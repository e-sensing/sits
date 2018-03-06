# Access to EMBRAPA SATVEG services
message("SATVEG has three coverages with MODIS collection 6 data")
message("terra has data from MODIS TERRA satellite")
message("aqua has data from MODIS AQUA satellite")
message("comb combines data from TERRA and AQUA satellites")
message("the bands available are ndvi and evi")

# create three coverages
terra <- sits_coverage(service = "SATVEG",  name = "terra")
aqua <- sits_coverage(service = "SATVEG",  name = "aqua")
comb <- sits_coverage(service = "SATVEG",  name = "comb")

# retrieve the same point from three different coverages
point_terra.tb <- sits_getdata(terra, longitude = -55.50563, latitude = -11.71557)
point_aqua.tb <- sits_getdata(aqua, longitude = -55.50563, latitude = -11.71557)
point_comb.tb <- sits_getdata(comb, longitude = -55.50563, latitude = -11.71557)

# plot the three points
sits_plot(point_terra.tb)
sits_plot(point_aqua.tb)
sits_plot(point_comb.tb)

# retrieve a set of points based on a CSV file
csv_file <- system.file ("extdata/samples/samples_matogrosso.csv", package = "sits")
points.tb <- sits_getdata (comb, file = csv_file)
# show the points retrieved for the SATVEG server
sits_plot (points.tb)
