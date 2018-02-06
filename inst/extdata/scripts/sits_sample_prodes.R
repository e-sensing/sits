library(sf)
library(dplyr)
library(magrittr)
library(tidyverse)




# Get sample points from PRODES
#
# @param prodes_shp A character. Path to PRODES shp
# @param landsatid  A numeric. An ID of a landasat image. e.g. 22664
# @param nsamples   A numeric. Number of samples for each PRODES polygon. If NULL, then it returns the polygons' centroids
# @return           A data frame with PRODES data and WGS84 coordinates
sits_sample_prodes <- function(prodes_shp, landsatid, nsamples = NULL){
    min_area <- 9 * 30^2                         # minimum PRODES area to take into account e.g. 9 landsat pixels = 9 * 30^2
    dest_crs <- 4326                             # destination spatial reference. e.g. WTSS uses 4326 to retrieve samples
    use_centroids <- F                           # Use polygons centroids instead of random samples.
    if(is.null(nsamples)){
        use_centroids <- T
    }
    #---- read prodes ----
    prodes <- sf::st_read(prodes_shp, quiet = T) %>%
        dplyr::filter(pathrow == landsatid) %>%
        dplyr::filter(areameters > min_area)
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
    return(as.data.frame(prodes_samples))
}


# USAGE
prodes_shp <- "/home/alber/Documents/data/prodes/prodes2017/PDigital2000_2017_AMZ_shp/PDigital2017_AMZ_props.shp"
landsatid <- 22664                      # landsat image id (path & row)

# get the PRODES' polygon centroids as sample points
pordes_centroids <- sits_sample_prodes(prodes_shp, landsatid)

# get nsamples of each PRODES' polygon
nsamples <- 5
pordes_samples <- sits_sample_prodes(prodes_shp, landsatid, nsamples)
