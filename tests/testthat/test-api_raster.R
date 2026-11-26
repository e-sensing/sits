# Serves one file over HTTP/1.1 with keep-alive, HEAD and byte ranges, as a
# cloud provider does, from a sits worker, so the main process can open it
# with GDAL; a request without the header "Authorization: Bearer <token>"
# gets 401 when a token is given. Returns the URL and the port of the file,
# and a function that stops the server and the worker.
http_file <- function(file, token = "") {
    serve <- function(file, port, token) {
        bytes <- readBin(file, "raw", file.size(file))
        size <- length(bytes)
        server <- serverSocket(port)
        on.exit(close(server))
        clients <- list()
        deadline <- Sys.time() + 120
        while (Sys.time() < deadline) {
            ready <- socketSelect(c(list(server), clients), timeout = 0.2)
            if (ready[[1L]]) {
                clients <- c(clients, list(
                    socketAccept(server, blocking = TRUE, open = "r+b")
                ))
            }
            for (i in which(ready[-1L])) {
                con <- clients[[i]]
                request <- readLines(con, n = 1L)
                if (length(request) == 0L) {
                    close(con)
                    clients[[i]] <- NA
                    next
                }
                headers <- character(0L)
                repeat {
                    line <- readLines(con, n = 1L)
                    if (length(line) == 0L || !nzchar(line)) break
                    headers <- c(headers, line)
                }
                if (grepl("^GET /quit ", request)) {
                    return(invisible(NULL))
                }
                auth <- grep("^authorization:", headers,
                    ignore.case = TRUE, value = TRUE
                )
                range <- grep("^range:", headers,
                    ignore.case = TRUE, value = TRUE
                )
                extra <- ""
                if (nzchar(token) &&
                    !any(grepl(paste("Bearer", token), auth, fixed = TRUE))) {
                    status <- "401 Unauthorized"
                    body <- raw(0L)
                } else if (length(range) > 0L) {
                    ends <- strsplit(sub(".*bytes=", "", range[[1L]]), "-")
                    from <- as.numeric(ends[[1L]][[1L]])
                    to <- min(as.numeric(ends[[1L]][2L]), size - 1.0,
                        na.rm = TRUE
                    )
                    status <- "206 Partial Content"
                    body <- bytes[seq(from + 1.0, to + 1.0)]
                    extra <- sprintf(
                        "Content-Range: bytes %.0f-%.0f/%d\r\n", from, to, size
                    )
                } else {
                    status <- "200 OK"
                    body <- bytes
                }
                is_head <- startsWith(request, "HEAD ")
                writeBin(charToRaw(sprintf(paste0(
                    "HTTP/1.1 %s\r\nContent-Length: %d\r\n",
                    "Accept-Ranges: bytes\r\n%sConnection: keep-alive\r\n\r\n"
                ), status, length(body), extra)), con)
                if (!is_head) writeBin(body, con)
                flush(con)
            }
            clients <- Filter(function(x) inherits(x, "connection"), clients)
        }
        invisible(NULL)
    }
    # a cluster of the caller, or the debug mode, leaves no worker of its own
    started <- .parallel_start(workers = 1L, force = TRUE)
    testthat::skip_if_not(started, "a sits cluster is open or debug is on")
    node <- sits_env[["cluster"]][[1L]]
    # a port of the worker's own, so two runs of this file do not meet
    port <- 20000L + parallel::clusterCall(
        sits_env[["cluster"]], Sys.getpid
    )[[1L]] %% 20000L
    parallel:::sendCall(node, serve, list(file, port, token))
    stop_server <- function() {
        quit <- tryCatch(
            socketConnection("127.0.0.1", port, open = "r+b"),
            error = function(e) NULL
        )
        if (!is.null(quit)) {
            writeLines("GET /quit HTTP/1.1\r\n\r", quit)
            close(quit)
        }
        parallel:::recvResult(node)
        .parallel_stop(TRUE)
    }
    # wait for the server to listen
    listening <- FALSE
    for (i in seq_len(50L)) {
        probe <- tryCatch(
            suppressWarnings(socketConnection("127.0.0.1", port, open = "r+b")),
            error = function(e) NULL
        )
        if (!is.null(probe)) {
            close(probe)
            listening <- TRUE
            break
        }
        Sys.sleep(0.1)
    }
    if (!listening) {
        stop_server()
        stop("the local HTTP server did not start on port ", port)
    }
    list(
        url = paste0("/vsicurl/http://127.0.0.1:", port, "/", basename(file)),
        port = port,
        stop = stop_server
    )
}

test_that("The check of the files closes its connection to a remote image", {
    # the connections of a process are read from /proc
    testthat::skip_on_os(c("windows", "mac", "solaris"))
    # ESTABLISHED TCP connections of this process to a port on this machine
    established <- function(port) {
        links <- vapply(list.files("/proc/self/fd", full.names = TRUE),
            Sys.readlink, character(1L)
        )
        inodes <- sub("^socket:\\[([0-9]+)\\]$", "\\1",
            grep("^socket:", links, value = TRUE)
        )
        rows <- unlist(lapply(c("/proc/self/net/tcp", "/proc/self/net/tcp6"),
            function(path) {
                if (file.exists(path)) readLines(path)[-1L] else character(0L)
            }
        ))
        fields <- strsplit(trimws(rows), "[[:space:]]+")
        sum(vapply(fields, function(x) {
            x[[10L]] %in% inodes && x[[4L]] == "01" &&
                strtoi(sub(".*:", "", x[[3L]]), 16L) == port
        }, logical(1L)))
    }
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    served <- http_file(cube[["file_info"]][[1L]][["path"]][[1L]])
    on.exit(served[["stop"]](), add = TRUE)
    # an open of the image keeps its connection, which the count sees
    rast <- .raster_open_rast(served[["url"]])
    expect_gt(established(served[["port"]]), 0L)
    cube[["file_info"]][[1L]][["path"]][[1L]] <- served[["url"]]
    .check_raster_cube_files(cube)
    # a connection kept after the server closes it makes the exit of R
    # fail with SIGPIPE
    expect_identical(established(served[["port"]]), 0L)
})

test_that("A remote image that failed to open opens once the cause is fixed", {
    # GDAL_HTTP_HEADERS came in GDAL 3.6
    testthat::skip_if(
        numeric_version(terra::gdal()) < "3.6.0", "GDAL older than 3.6"
    )
    file <- system.file(
        "extdata/raster/mod13q1/TERRA_MODIS_012010_NDVI_2013-09-14.jp2",
        package = "sits"
    )
    served <- http_file(file, token = "good")
    on.exit(served[["stop"]](), add = TRUE)
    headers <- terra::getGDALconfig("GDAL_HTTP_HEADERS")
    on.exit(terra::setGDALconfig("GDAL_HTTP_HEADERS", headers), add = TRUE)
    terra::setGDALconfig("GDAL_HTTP_HEADERS", "Authorization: Bearer bad")
    expect_error(.raster_open_rast(served[["url"]]))
    # a renewed token, in the same session
    terra::setGDALconfig("GDAL_HTTP_HEADERS", "Authorization: Bearer good")
    # the server and the driver work for a URL that GDAL has not seen
    fresh <- .raster_open_rast(paste0(served[["url"]], "?fresh"))
    expect_identical(dim(fresh), dim(terra::rast(file)))
    rast <- .raster_open_rast(served[["url"]])
    expect_identical(dim(rast), dim(terra::rast(file)))
})

test_that("A local image keeps the cache of GDAL", {
    clears <- 0L
    local_mocked_bindings(.raster_clear_vsi_cache = function() {
        clears <<- clears + 1L
        invisible(NULL)
    })
    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    .check_raster_cube_files(cube)
    expect_error(
        .raster_open_rast(file.path(tempdir(), "no_such_image.tif")),
        "no_such_image"
    )
    # several files open in one call, as a block of a tile is read, and
    # fail with the error of terra, which names the file (its text changes
    # with the version of terra)
    expect_error(
        .raster_open_rast(file.path(tempdir(), c("no_a.tif", "no_b.tif"))),
        "no_a"
    )
    expect_identical(clears, 0L)
})
