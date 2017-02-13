#------------------------------------------------------------------
#' Obtain timeSeries from WTSS server, based on a SHP file.
#'
#' \code{sits_fromSHP} reads a shapefile and retrieves a SITS table
#' containing time series from a coverage that are inside the SHP file.
#' The script uses the WTSS service, taking information about coverage, spatial and
#' temporal resolution from the WTSS configuration.
#'
#'
#'
#' @param shp_file   string  - name of a SHP file which provides the boundaries of a region of interest
#' @param crs        string  - the coordinate reference system used by the shapefile
#' @return table     tibble  - a SITS table
#' @keywords SITS
#' @family   SITS main functions
#' @examples sits_fromSHP ("municipality.shp")
#' @export
#'
sits_fromSHP <- function (shp_file, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {

     # is the WTSS service working?
     sits_testWTSS()

     create_polygon <- function (file, crs) {

          # read shapefile from a full path
          area_shp <- shapefile::read.shapefile(file)

          # create spatial polygons from polygon
          polygon <- sp::Polygon(area_shp$shp$shp[[1]]$points)
          polygons <- sp::Polygons(list(polygon), paste(1))
          spatial.polygons <- sp::SpatialPolygons(list(polygons))

          # define CRS of the spatial polygon
          proj4string(spatial.polygons) <- sp::CRS(crs)

          # return spatial.polygons
          return (spatial.polygons)

     }
     # What points are inside a given polygon?
     inside_polygon <- function(coord.df, plg, crs) {

          # create a spatial points data frame with the same CRS as the SHP polygon
          pts.df <- sp::SpatialPoints(coord.df, CRS(crs))

          # find the coordinates of the data frame that are inside the polygon
          coord.df <- subset(coord.df, !is.na(sp::over(pts.df, plg) == 1))
          # return the coordinates that are inside the polygon
          return (coord.df)

     }
     # create a point grid of coverage pixels that are inside the polygon
     create_grid <- function(plg) {

          # Para MOD13Q1 use pixelsize = 231.6564
          # center coordinates resolution
          resolution = 0.0019

          # get first point from the polygon bounding box boundaries
          minimum.bb = bbox(plg)
          first.point <- getTimeSeries(minimum.bb, dates, class)

          # define list of latitudes and longitudes by resolution based on the bounding box
          list.long <- seq(from=first.point$longitude, to=minimum.bb[3], by=resolution)
          list.lat <- seq(from=first.point$latitude, to=minimum.bb[4], by=resolution)

          # build the latitude and longitude
          df.coordinates <- data.frame(longitude = rep(list.long, each=length(list.lat)), latitude = rep(list.lat, length(list.long)))

          df.coordinates

     }

}
