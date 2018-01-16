#   Title: satveg2sits
#   Description: This function returns a sits table retrieved from SATVeg service.
#   Version: 1
#   Date: 2017-10-31
#   Author: julio.esquerdo@embrapa.br
#           Embrapa Informatica Agropecuaria


# Required parameters

#   longitude and latitude (in decimal degrees)
#   sat: 'terra', 'aqua', 'comb'
#   indice: 'ndvi', 'evi'
#   pre-filter:   0 - none
#                 1 - no data correction
#                 2 - cloud correction
#                 3 - no data and cloud correction

# Optional parameters

#   filter: 'flt', 'wav', 'sav'
#   filter_par:   0 | 10 | 20 | 30
#                 2 | 3 | 4 | 5 | 6
#   start_date: 'YYYYMMDD'
#   end_date: 'YYYYMMDD'


# Calling examples:

# Complete EVI time series from both Terra and Aqua satellites using no data and cloud correction
# and flat-bottom filtering.

# satveg2sits(longitude = -51, latitude = -23, sat = "comb", indice = "evi", pre_filter = 3,
#            filter = 'flt', filter_par = 0)

# One year NDVI time series from both Terra satellite using no data and cloud correction
# and no filtering.

# satveg2sits(longitude = -51, latitude = -10, sat = "terra", indice = "ndvi", pre_filter = 3,
#            start_date = "2010-01-01", end_date = "2010-12-31")





satveg2sits <- function(longitude = longitude, latitude = latitude, sat = sat,
                        indice = indice, pre_filter = pre_filter, filter = filter,
                        filter_par = filter_par, start_date = start_date,
                        end_date = end_date)

  {

  library(RCurl)
  library(stringr)
  library(lubridate)
  library(tibble)
  library(sits)
  library(birk)

  if (missing(filter)) filter <- ''
  if (missing(pre_filter)) pre_filter <- ''
  if (missing(filter_par)) filter_par <- ''

    # Building the URL
  url_satveg <- "https://www.satveg.cnptia.embrapa.br/satvegws/ws/perfil/ZW46IXzr4pRzJlX/"
  sl <- "/"

  URL <- paste0(url_satveg, indice, "/ponto", sl, longitude, sl, latitude, sl, sat, sl, pre_filter, sl, filter, sl, filter_par)


  # Get the data from SATVeg service
  satveg.txt = getURL(url = URL)


  # Retrieve the time series
  pos1 <- regexpr("listaDatas", satveg.txt)
  pos1 <- pos1[1]-4
  satveg.profile <- substr(satveg.txt, 16, pos1)
  satveg.profile <- as.double(unlist(strsplit(satveg.profile,",")))
  n_points <- length(satveg.profile)

  # Retrieve the timeline
  pos2 <- regexpr("]}", satveg.txt)
  satveg.timeline <- substr(satveg.txt, pos1+17, pos2-2)
  satveg.timeline <- unlist(strsplit(satveg.timeline,'\",\"'))
  satveg.timeline[1] <- substr(satveg.timeline[1], 2, 13)


  # Converting timeline to YYYY-MM-DD
  Sys.setlocale("LC_TIME", "C")
  satveg.timeline <- as.Date(satveg.timeline, "%b %d, %Y")
  satveg.timeline <- as.Date(satveg.timeline, "%Y-%m%-%d")


  # Find the start and end closest dates
  if (hasArg(start_date)) {
      start_date_pos <- which.closest(satveg.timeline, as.Date(start_date))
    } else {start_date_pos <- 1 }


  if (hasArg(end_date)) {
      end_date_pos <- which.closest(satveg.timeline, as.Date(end_date))
    } else {end_date_pos <- n_points }


  # Temporal subset
  satveg.profile <- satveg.profile[start_date_pos:end_date_pos]
  satveg.timeline <- satveg.timeline[start_date_pos:end_date_pos]


  # Bulding time series tibble
  ndates <- length(satveg.timeline)
  time_series <- tibble(Index = satveg.timeline, indice = satveg.profile)
  names(time_series)[names(time_series) == "indice"] <- indice


  # Building tibble for sits
  satveg.tb <- tibble(longitude = as.character(longitude), latitude = as.character(latitude),
                      start_date = satveg.timeline[1],
                      end_date = satveg.timeline[ndates],
                      label = "NoClass", coverage = "satveg-mod13q1",
                      time_series = list(time_series))

  class(satveg.tb) <- append(class(satveg.tb), "sits")

  return(satveg.tb)

  }
