library(sf)
library(dplyr)
library(magrittr)
library(tidyverse)
library(sits)



# Get sample points from PRODES
#
# @param prodes_shp A character. Path to PRODES shp
# @param year       A numeric. PRODES year
# @param landsatid  A numeric. An ID of a landasat image. e.g. 22664
# @param nsamples   A numeric. Number of samples for each PRODES polygon. If NULL, then it returns the polygons' centroids
# @return           A sits' tibble plus PRODES' viewdate
sits_sample_prodes <- function(prodes_shp, year, landsatid, nsamples = NULL){
    min_area <- 9 * 30^2                         # minimum PRODES area to take into account e.g. 9 landsat pixels = 9 * 30^2
    dest_crs <- 4326                             # destination spatial reference. e.g. WTSS uses 4326 to retrieve samples
    use_centroids <- F                           # Use polygons centroids instead of random samples.
    if(is.null(nsamples)){
        use_centroids <- T
    }
    #---- read prodes & filter ----
    prodes <- sf::st_read(prodes_shp, quiet = T, stringsAsFactors = F) %>%
        dplyr::filter(pathrow == landsatid) %>%
        dplyr::filter(areameters > min_area) %>%
        dplyr::mutate(label = dplyr::recode(class_name,
                                            FLORESTA = "Forest",
                                            HIDROGRAFIA = "Water",
                                            NAO_FLORESTA = "Non-forest",
                                            NAO_FLORESTA2 = "Non-forest",
                                            d2012 = "Deforestation_before_2012",
                                            d2013 = "Deforestation_2013",
                                            d2014 = "Deforestation_2014",
                                            d2015 = "Deforestation_2015",
                                            d2016 = "Deforestation_2016",
                                            d2017 = "Deforestation_2017",
                                            d2018 = "Deforestation_2018",
                                            r2012 = "Def_remainder_before_2012",
                                            r2013 = "Def_remainder_2013",
                                            r2014 = "Def_remainder_2014",
                                            r2015 = "Def_remainder_2015",
                                            r2016 = "Def_remainder_2016",
                                            r2017 = "Def_remainder_2017",
                                            r2018 = "Def_remainder_2018")) %>%
        dplyr::filter(label %in% c("Forest", "Water", "Non-forest",
                                   paste0("Deforestation_", year)))
    #---- compute statistics ----
    # cast sf to data.frame
    prod.df <- prodes %>% dplyr::select(label, areameters)
    prod.df$geometry <- NULL
    prod.df <- dplyr::as_tibble(prod.df)
    (prod.df %>% dplyr::group_by(label) %>%
         dplyr::summarise(
             count = n(),
             area_km2 = sum(areameters, na.rm = T)/(10^6),
             area_mean = mean(areameters, na.rm = T)/(10^6),
             area_sd = sd(areameters, na.rm = T)/(10^6)
         ))
    #---- get sample points ----
    prodes_samples <- tibble()
    if(use_centroids){
        # NOTE: package sf only supports centroid computation for projected SRS
        inter_crs <- 29101
        prodes_proj <- prodes %>%
            sf::st_transform(inter_crs) %>%
            sf::st_centroid() %>%
            sf::st_coordinates() %>%
            as.data.frame(stringsAsFactors = F)
        prodes_proj_wgs84 <- sf::st_as_sf(prodes_proj, coords = colnames(prodes_proj), crs = inter_crs) %>%
            sf::st_transform(4326) %>%
            sf::st_coordinates() %>%
            dplyr::as_tibble() %>%
            dplyr::rename(longitude = X, latitude = Y)
        #
        prodes_samples <- prodes
        prodes_samples$geometry <- NULL
        prodes_samples <- prodes_samples %>%
            dplyr::as_tibble() %>%
            dplyr::bind_cols(prodes_samples, prodes_proj_wgs84)
    }else{
        #xy_mat <- sf::st_sample(prodes, size = nsamples) # FAIL!
        prodes$tmpid <- 1:nrow(prodes)
        xy_ls <- lapply(prodes$tmpid, function(tmpid, prodes, nsamples){
            g <- prodes$geometry[[tmpid]]
            xy_mat <- sf::st_sample(g, size = nsamples) %>% sf::st_coordinates()
            xy_mat <- cbind(xy_mat, tmpid = rep(tmpid, nrow(xy_mat)))
            return(xy_mat)
        },
        prodes = prodes,
        nsamples = nsamples)
        xy_tb <- dplyr::as_tibble(do.call(rbind, xy_ls)) %>%
            dplyr::rename(longitude = X, latitude = Y)

        prodes_samples <- prodes
        prodes_samples$geometry <- NULL
        prodes_samples <- dplyr::as_tibble(prodes_samples)
        prodes_samples <- dplyr::right_join(prodes_samples, xy_tb, by = "tmpid")
    }
    prodes_samples$start_date <- rep(as.Date(paste(year,"08-01", sep = "-")), nrow(prodes_samples))
    prodes_samples$end_date <- rep(as.Date(paste(year + 1,"07-31", sep = "-")), nrow(prodes_samples))
    prodes_samples$coverage <- NA
    prodes_samples$time_series <- NA
    prodes_samples$view_date <- as.Date(prodes_samples$view_date)
    prodes_samples <- prodes_samples[, c("longitude", "latitude", "start_date", "end_date", "label", "coverage", "time_series", "view_date", "areameters")]
    colnames(prodes_samples) <- c(colnames(sits::sits_tibble()), "view_date", "areameters")
    return(dplyr::as_tibble(prodes_samples))
}


# USAGE
prodes_shp <- "/home/alber/Documents/data/prodes/prodes2017/PDigital2000_2017_AMZ_shp/PDigital2017_AMZ_props.shp"
landsatid <- 22664                      # landsat image id (path & row)
year <- 2013

# get the PRODES' polygon centroids as sample points
#prodes_samples <- sits_sample_prodes(prodes_shp, year, landsatid)
#write.csv(prodes_samples, file = "/home/alber/Documents/Dropbox/alberLocal/inpe/projects/deepLearning/prodes_centroids_20180206.csv")

# get nsamples of each PRODES' polygon
nsamples <- 5
prodes_samples <- sits_sample_prodes(prodes_shp, year, landsatid, nsamples)
write.csv(prodes_samples, file = "/home/alber/Documents/Dropbox/alberLocal/inpe/projects/deepLearning/prodes_samples_20180206.csv")

