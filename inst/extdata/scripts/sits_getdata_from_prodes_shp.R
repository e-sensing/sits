library(sf)

wtss.obj <- wtss::WTSS("http://www.terrama2.dpi.inpe.br/esensing/wtss/")

wtss::describeCoverage(wtss.obj, "mixl8mod")

l8_modis_226_064 <- sits_coverage(service = "WTSS-INPE-2", product = "LC8-MOD13-MYD13", name = "mixl8mod")

data_dir <- paste0("/Users/gilberto/Dropbox/TWDTWAmazoniaCerrado/MapasReferencia/Prodes/Prodes-226-064/")

prodes_226064 <- sf::st_read(paste0(data_dir,"prodes_226064.shp"))

prodes_226064_d2013 <- dplyr::filter(prodes_226064, class_name == "d2013")

bbox <- sf::st_bbox(prodes_226064_d2013[1,])
