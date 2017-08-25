#' @title Relabel raster stack object
#' @name sits_stack_relabel
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @include RcppExports.R
#'
#' @param x raster stack .... tbl raster or wms???
#' @param old_values an integer vector with raster values (label) to be updated
#' @param new_values an integer vector with new values (label). It must have the same length as \code{old_label}
#' @param filename a character with the file name. Optional
#' @param progress a character. See \code{\link[raster]{pbCreate}}. Default is \code{'text'}
#' @param parallel perform parallel processing. Default is TRUE
#' @param ... other arguments to pass to  \code{\link[raster]{beginCluster}} and \code{\link[raster]{writeStart}}
#'
#' @description This function updates categorical raster values (labels).
#'
#' @export
sits_stack_relabel <- function(x,
                               old_values,
                               new_values,
                               filename = "",
                               progress = 'text',
                               parallel = TRUE, ...) {

     if(length(new_values)!=length(old_values))
          stop("new_values must have the same length as old_values", call. = TRUE)

     if(parallel){
          raster::beginCluster(...)
          our <- .apply_stack_parallel(x,
                                       fun = .stack_relabel,
                                       args.list = list(old_values, new_values),
                                       filename = "", progress = 'text', ...)
          raster::endCluster()
     } else {
          stop("Not implemented yet", call. = TRUE)
          #out <- .apply_stack(x, fun = .stack_relabel, args.list = list(old_values, new_values), filename = "", progress = 'text', ...)
     }

     return(our)

}

#' @title Relabel raster stack based on temporal transitions
#' @name sits_stack_transition_relabel
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @param x raster stack object
#' @param A transition matrix. This gives the possible transitions among classes.
#' The first line and the first column of the table has the raster class followed
#' by the class label, such that <class_number>.<class_label>. The rows are the
#' Class(t) and columns Class(t-1), the cobination between a row (Class(t)) and a
#' column (Class(t-1)) gives the updated class number.
#' @param filename a character with the file name. Optional
#' @param progress a character. See \code{\link[raster]{pbCreate}}. Default is \code{'text'}
#' @param parallel perform parallel processing. Default is TRUE
#' @param ... other arguments to pass to \code{\link[raster]{beginCluster}} and \code{\link[raster]{writeStart}}
#'
#' @description This function updates categorical raster values (labels) based on the temporal transitions,
#' assuming Class(t) = f(Class(t-1), Class(t)) subject to the initial condition Class(t=1) = Class(t=1)
#'
#' @export
sits_stack_transition_relabel <- function(x,
                                          A,
                                          filename = "",
                                          progress = 'text',
                                          parallel = TRUE, ...) {

     if(parallel){
          raster::beginCluster(...)
          our <- .apply_stack_parallel(x,
                                       fun = .stack_transition_relabel,
                                       args.list = list(A),
                                       filename = "", progress = 'text', ...)
          raster::endCluster()
     } else {
          stop("Not implemented yet", call. = TRUE)
          #out <- .apply_stack(x, fun = .stack_transition_relabel, args.list = list(old_values, new_values), filename = "", progress = 'text', ...)
     }

     return(our)

}


#' @title Parallel processing of raster stack objects
#' @name .apply_stack_parallel
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @param x raster stack
#' @param fun is a function to be applied to each chunk of the raster stack object and can
#' be defined by the user. The function must have four arguments: k (is the chunk index),
#' x (is the raster stack object), bs (is the chnuk information created by raster::blockSize),
#' and args.list is a list of other arguments used in the processing. The function must return
#' a matrix with the same dimensions as returned by raster::getValues for each chunk.
#' @param filename a character with the file name. Optional
#' @param progress a character. See \code{\link[raster]{pbCreate}}. Default is \code{'text'}
#' @param ... other arguments to pass to \code{\link[raster]{beginCluster}} and \code{\link[raster]{writeStart}}
#'
#' @description This function performs the parallel processing of raster stack objects.
#'
#' @noRd
.apply_stack_parallel <- function(x, fun, args.list, filename = "", progress = 'text', ...) {

     # Create output raster
     out <- raster::brick(x, values = FALSE)
     names(out) <- names(x)

     # Create cluster
     cl <- raster::getCluster()
     on.exit(raster::returnCluster())
     nodes <- length(cl)

     # Compute raster tiles
     bs <- raster::blockSize(x, minblocks = nodes * 4)
     bs$array_rows <- cumsum(c(1, bs$nrows * out@ncols))
     pb <- raster::pbCreate(bs$n, progress = progress)

     # Export global variables to cluster
     # snow::clusterExport(cl, list = c("x", "bs"), envir = environment())

     # Get all nodes going
     for (k in 1:nodes) {
          snow::sendCall(cl[[k]], fun, list(k, x, bs, args.list), tag = k)
     }

     # If needed create raster files
     filename <- raster::trim(filename)
     if (!raster::canProcessInMemory(out) & filename == "") {
          filename <- raster::rasterTmpFile()
     }

     if (filename != "") {
          out <- raster::writeStart(out, filename = filename, ... )
     } else {
          vv <- matrix(out, ncol = raster::nlayers(out))
     }

     # Process raster tiles
     for (k in 1:bs$n) {
          # receive results from a node
          d <- snow::recvOneData(cl)

          # error?
          if (! d$value$success) {
               stop('cluster error')
          }

          # which block is this?
          b <- d$value$tag
          # cat('received block: ',b," / ",bs$n,'\n'); flush.console();

          if (filename != "") {
               out <- raster::writeValues(out, d$value$value, bs$row[b])
          } else {
               # cols <- bs$row[b]:(bs$row[b] + bs$nrows[b]-1)
               # vv[,cols] <- matrix(d$value$value, nrow=out@ncols)
               rows <- seq(from = bs$array_rows[b], by = 1, length.out = bs$nrows[b]*out@ncols)
               vv[rows,] <- d$value$value
          }

          # need to send more data?
          ni <- nodes + k
          if (ni <= bs$n) {
               snow::sendCall(cl[[d$node]], fun, list(ni, x, bs, args.list), tag = ni)
          }

          # Progess bar
          raster::pbStep(pb, k)

     }

     # Create output raster
     if (filename != "") {
          out <- raster::writeStop(out)
     } else {
          out <- raster::setValues(out, as.vector(vv))
     }

     # Close cluster
     raster::pbClose(pb)

     # Assign layers names
     names(out) <- names(x)

     return(out)
}

#' @title Relabel a chunk from raster stack object
#' @name .stack_relabel
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @param k is the chunk index
#' @param x is the raster stack object
#' @param bs is is the chnuk information created by raster::blockSize
#' @param args.list is a list of other arguments used in the processing.
#'
#' @description Relabel a chunk from raster stack object according old label
#'
#' @noRd
.stack_relabel <- function(k, x, bs, args.list){

     A <- tibble::tibble(old_values = args.list[[1]], new_values = args.list[[2]])

     v <- raster::getValues(x, bs$row[k], bs$nrows[k])

     v <- v %>% as.vector() %>%
          tibble::as.tibble() %>%
          dplyr::left_join(., y = A, by = c("value" = "old_values")) %>%
          dplyr::mutate(new_values = dplyr::if_else(is.na(new_values), value, new_values)) %>%
          .$new_values %>%
          matrix(nrow = nrow(v))

     return(v)

}

#' @title Relabel a chunk from raster stack object
#' @name .stack_transition_relabel
#' @author Victor Maus, \email{vwmaus1@@gmail.com}
#'
#' @param k is the chunk index
#' @param x is the raster stack object
#' @param bs is is the chnuk information created by raster::blockSize
#' @param args.list is a list of other arguments used in the processing.
#'
#' @description Relabel a chunk from raster stack object according to transition rules.
#'
#' @noRd
.stack_transition_relabel <- function(k, x, bs, args.list){

     A <- args.list[[1]]

     cpp_fun <- function(v, A, I) {
          .Call('_sits_apply_first_order_transition_rules', v, A, I)
     }

     v <- raster::getValues(x, bs$row[k], bs$nrows[k])

     # Skip NA
     I <- tibble::as.tibble(v) %>%
          dplyr::rowwise() %>%
          dplyr::do(na = !anyNA(.)) %>%
          unlist()

     # Update label
     if(any(I)){
          A <- A %>% dplyr::select(-label) %>% as.matrix()
          colnames(A) <- NULL
          I <- as.vector(which(I))
          v <- cpp_fun(v, A, I)
     }

     return(v)
}

