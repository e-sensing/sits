# Access to EMBRAPA SATVEG services
message("SATVEG has three data cubes with MODIS collection 6 data")
message("terra has data from MODIS TERRA satellite")
message("aqua has data from MODIS AQUA satellite")
message("comb combines data from TERRA and AQUA satellites")
message("the bands available are ndvi and evi")

# create three data cubes
cube_terra <- sits_cube(service = "SATVEG",  name = "terra")
cube_aqua <- sits_cube(service = "SATVEG",  name = "aqua")
cube_comb <- sits_cube(service = "SATVEG",  name = "comb")

# retrieve the same point from three different data cubes
point_terra.tb <- sits_get_data(cube_terra, longitude = -55.50563, latitude = -11.71557)
point_aqua.tb <- sits_get_data(cube_aqua, longitude = -55.50563, latitude = -11.71557)
point_comb.tb <- sits_get_data(cube_comb, longitude = -55.50563, latitude = -11.71557)

# plot the three points
plot(point_terra.tb)
plot(point_aqua.tb)
plot(point_comb.tb)

# retrieve a set of points based on a CSV file
csv_file <- system.file("extdata/samples/samples_matogrosso.csv", package = "sits")
points.tb <- sits_get_data(cube_comb, file = csv_file)
# show the points retrieved for the SATVEG server
plot(points.tb)

