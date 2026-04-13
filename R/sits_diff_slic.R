DifferentiableSLIC <- torch::nn_module(
    "DifferentiableSLIC",

    initialize = function(step = 30L, compactness = 1.0, iter = 10L) {
        self$step <- step
        self$compactness <- compactness
        self$iter <- iter
    },

    forward = function(x, hard = FALSE) {
        # x: [1, C, H, W]
        b <- x$size(1)  # batch
        c <- x$size(2)  # n_times*n_bands
        h <- x$size(3)  # raster chunk nrows
        w <- x$size(4)  # raster chunk ncols

        # make normalized coordinates [H*W, 2]
        yy <- torch::torch_tensor(
            as.numeric(seq_len(h)),
            dtype = torch::torch_float(),
            device = x$device
        )
        xx <- torch::torch_tensor(
            as.numeric(seq_len(w)),
            dtype = torch::torch_float(),
            device = x$device
        )
        grid_y <- yy$unsqueeze(2)$expand(c(h, w))
        grid_x <- xx$unsqueeze(1)$expand(c(h, w))
        coords <- torch::torch_stack(
            list(
                grid_y$reshape(c(h * w)),
                grid_x$reshape(c(h * w))
            ),
            dim = 2
        )

        # flatten features to [H*W, C]
        feat <- x$squeeze(1)$permute(c(2, 3, 1))$reshape(c(h * w, c))

        # initialize centers on regular grid
        centers <- initialize_grid_centers(
            feat = feat,
            h = h,
            w = w,
            step = self$step
        )

        # MAIN SOFT SLIC ITERATION
        for (i in seq_len(self$iter)) {
            # Assign pixels to clusters
            q <- soft_slic_assignment(
                feat = feat,
                coords = coords,
                center_feat = centers$feat,
                center_coords = centers$coords,
                compactness = self$compactness,
                temperature = self$temperature
            )
            # Update centroids
            centers <- update_centers(
                feat = feat,
                coords = coords,
                q = q
            )
        }

        out <- list(
            assign = q,
            center_feat = centers$feat,
            center_coords = centers$coords
        )

        if (hard) {
            out$labels <- torch::torch_argmax(q, dim = 2)
        }
        out
    }
)

initialize_grid_centers <- function(feat, h, w, step) {
    row_start <- max(1L, step %/% 2L + 1L)
    col_start <- max(1L, step %/% 2L + 1L)

    if (row_start > h) {
        rows <- as.integer((h + 1L) %/% 2L)
    } else {
        rows <- seq(from = row_start, to = h, by = step)
    }

    if (col_start > w) {
        cols <- as.integer((w + 1L) %/% 2L)
    } else {
        cols <- seq(from = col_start, to = w, by = step)
    }

    grid <- expand.grid(row = rows, col = cols)

    #lin_idx <- grid$row + (grid$col - 1L) * h
    lin_idx <- (grid$row - 1L) * w + grid$col

    lin_idx_t <- torch::torch_tensor(
        as.integer(lin_idx),
        dtype = torch::torch_long(),
        device = feat$device
    )

    center_feat <- feat[lin_idx_t, ]
    center_coords <- torch::torch_tensor(
        as.matrix(grid[, c("row", "col")]),
        dtype = torch::torch_float(),
        device = feat$device
    )
    list(
        feat = center_feat,
        coords = center_coords,
        index = lin_idx_t
    )
}


soft_slic_assignment <- function(feat,
                                 coords,
                                 center_feat,
                                 center_coords,
                                 step = 30L,
                                 compactness = 1.0,
                                 temperature = 1.0,
                                 eps = 1e-8) {
    # feat:          [N, C]
    # coords:        [N, 2]
    # center_feat:   [K, C]
    # center_coords: [K, 2]
    #
    # returns: [N, K], rows sum to 1

    # [N, 1, C] - [1, K, C] -> [N, K, C]
    feat_diff <- feat$unsqueeze(2) - center_feat$unsqueeze(1)
    feat_dist2 <- (feat_diff * feat_diff)$sum(dim = 3)
    # [N, 1, 2] - [1, K, 2] -> [N, K, 2]
    coord_diff <- coords$unsqueeze(2) - center_coords$unsqueeze(1)
    spatial_dist2 <- (coord_diff * coord_diff)$sum(dim = 3)
    # joint distance
    S <- step
    w_sp <- (compactness / max(S, eps))^2 # Normalizing spatial term with step.
    dist2 <- feat_dist2 + w_sp * spatial_dist2
    # numerical stability
    logits <- -dist2 / max(temperature, eps)
    q <- torch::nnf_softmax(logits, dim = 2)
    # return
    q
}

update_centers <- function(feat, coords, q, eps = 1e-8) {
    # feat:   [N, C]
    # coords: [N, 2]
    # q:      [N, K]

    q_t <- q$transpose(1, 2)  # [K, N]

    feat_weighted_sum <- q_t$matmul(feat)     # [K, C]
    coord_weighted_sum <- q_t$matmul(coords)  # [K, 2]

    mass <- q_t$sum(dim = 2)$clamp_min(eps)   # [K]

    center_feat <- feat_weighted_sum / mass$unsqueeze(2)
    center_coords <- coord_weighted_sum / mass$unsqueeze(2)

    list(
        feat = center_feat,
        coords = center_coords,
        mass = mass
    )
}

.rast_patch_to_tensor <- function(data, block, n_times) {
    x <- as.matrix(data)
    storage.mode(x) <- "double"

    H <- block[["nrows"]]
    W <- block[["ncols"]]

    stopifnot(nrow(x) == H * W)
    stopifnot(ncol(x) %% n_times == 0L)

    n_bands <- ncol(x) / n_times

    xt <- torch::torch_tensor(x, dtype = torch::torch_float())
    # [H*W, n_bands, n_times]
    xt <- xt$reshape(c(H * W, n_bands, n_times))
    # [H, W, n_bands, n_times]
    xt <- xt$reshape(c(H, W, n_bands, n_times))
    # [1, n_bands*n_times, H, W]
    xt <- xt$permute(c(3, 4, 1, 2))$reshape(c(n_bands * n_times, H, W))$unsqueeze(1)
    # [1, n_bands*n_times, H, W]
    xt
}


sits_differentiable_slic <- function(data = NULL,
                                     step = 30L,
                                     compactness = 1.0,
                                     iter = 10L,
                                     minarea = 10L, # TODO Post-processing check area
                                     verbose = FALSE,
                                     device = "cpu",
                                     n_times = 23L) {
    # set caller for error msg
    .check_set_caller("sits_differentiable_slic")
    # step is OK?
    .check_int_parameter(step, min = 1L, max = 500L)
    # compactness is OK?
    .check_num_parameter(compactness, min = 0.1, max = 50.0)
    # iter is OK?
    .check_int_parameter(iter, min = 1L, max = 100L)
    # minarea is OK?
    .check_int_parameter(minarea, min = 1L, max = 10000L)
    # n_times is OK?
    .check_int_parameter(n_times, min = 1L, max = 1000L)
    # documentation mode? verbose is FALSE
    verbose <- .message_verbose(verbose)

    function(data, block, bbox) {
        # Create a template rast
        v_temp <- .raster_new_rast(
            nrows = block[["nrows"]],
            ncols = block[["ncols"]],
            xmin = bbox[["xmin"]],
            xmax = bbox[["xmax"]],
            ymin = bbox[["ymin"]],
            ymax = bbox[["ymax"]],
            nlayers = 1L,
            crs = bbox[["crs"]]
        )

        x <- .rast_patch_to_tensor(
            data = data,
            block = block,
            n_times = n_times
        )
        # Segment Raster Cube With Soft Slic
        segs <- DifferentiableSLIC(
            step = step,
            compactness = compactness,
            iter = iter
        )

        if (device == "cuda" && torch::cuda_is_available()) {
            x <- x$cuda()
            segs <- segs$cuda()
        }

        out <- segs(x, hard = TRUE)

        labels <- as.integer(as.array(out$labels$cpu()))
        labels <- c(labels) + 1L

        stopifnot(length(labels) == block[["nrows"]] * block[["ncols"]])

        v_obj <- .raster_set_values(v_temp, labels)
        v_obj <- .raster_set_na(v_obj, -1L)
        v_obj <- .raster_extract_polygons(v_obj, dissolve = TRUE)
        v_obj <- sf::st_as_sf(v_obj)

        if (nrow(v_obj) == 0L) {
            return(v_obj)
        }

        centroids <- suppressWarnings(sf::st_centroid(v_obj))
        centroids_xy <- sf::st_coordinates(centroids)
        # Bind valid centers with segments table
        v_obj <- cbind(v_obj, centroids_xy)
        # Rename columns
        names(v_obj) <- c("supercells", "x", "y", "geometry")
        # Get only polygons segments
        v_obj <- suppressWarnings(sf::st_collection_extract(v_obj, "POLYGON"))
        # Return the segment object
        v_obj
    }
}
