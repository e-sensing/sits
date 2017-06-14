#!/usr/bin/env Rscript

con_in <- file("stdin", "rb")
con_out <- pipe("cat", "wb")

while(TRUE) {

  input_list <- unserialize(con_in)
  ncol <- length(input_list)
  if(ncol == 0) { #this is the last message
    sits::exitConnection(list(), con_out)
    break
  }

  attach(input_list)

  # read and parse arguments
  args <- commandArgs(trailingOnly=TRUE)
  lapply(args, function(x) {l <- unlist(strsplit(x, "=")); assign(l[1], l[2], envir = .GlobalEnv)})

  # get unique column and row values
  sits.tb <- sits::createColRowSequence(i32col_id, i32row_id)

  # read patterns and get label names
  patterns.tb <- sits::sits_getdata(patterns.json)
  label_names <- dplyr::select(patterns.tb, label)

  split_processing <- function(line_sits.tb, patterns.tb, scale_factor, bands, dist.method, alpha, beta, theta, span, keep, interval, overlap, dates) {

    # get time series idx
    idx <- which(i32col_id == line_sits.tb$longitude & i32row_id == line_sits.tb$latitude)

    if(length(dates) < 9)
       return(list())

    # build time series object using attribute values and the dates
    line_sits.tb$time_series[[1]] <- sits::createZooObject(bands = bands,
                                                           dates = dates,
                                                           scale_factor = scale_factor,
                                                           idx = idx)
    # align twdtw
    alignments.tb <- sits::sits_TWDTW(line_sits.tb,
                                      patterns.tb,
                                      bands = bands,
                                      alpha = alpha,
                                      beta = beta,
                                      dist.method = dist.method,
                                      theta = theta,
                                      span = span,
                                      keep = keep,
                                      interval = interval,
					             start_date = dates[1],
					             end_date = dates[length(dates)])

     if(class(alignments.tb)=="try-error")
       return (list())

     k = nrow(alignments.tb$best.alignments[[1]])

     data.frame(
            colid = as.double(rep(alignments.tb$longitude, k)),
            rowid = as.double(rep(alignments.tb$latitude, k)),
            time  = as.double(seq_len(k)),
            from  = as.double(as.integer(alignments.tb$best.alignments[[1]]$from)),
            to    = as.double(as.integer(alignments.tb$best.alignments[[1]]$to)),
            label = as.double(match(alignments.tb$best.alignments[[1]]$label[], label_names[[1]])),
            dist  = as.double(alignments.tb$best.alignments[[1]]$distance)
      )

  }

  # Output
  out = do.call("rbind", parallel::mclapply(X = split(sits.tb, seq(nrow(sits.tb))),
					                   mc.cores = parallel::detectCores(),
					                   FUN = split_processing,
					                   patterns.tb = patterns.tb,
					                   scale_factor = as.numeric(scale_factor),
					                   bands = unlist(strsplit(bands, split=",")),
					                   dist.method = dist.method,
					                   alpha = as.numeric(alpha),
					                   beta = as.numeric(beta),
					                   theta = as.numeric(theta),
					                   span = as.numeric(span),
					                   keep = as.logical(keep),
					                   interval = gsub(",", " ", interval),
					                   overlap = as.numeric(overlap),
					                   dates = as.Date(unlist(strsplit(dates, split=",")))))
  out

  writeBin(serialize(c(out), NULL, xdr=FALSE), con_out)
  flush(con_out)

}

close(con_in)
