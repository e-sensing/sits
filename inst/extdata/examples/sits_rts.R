library(rts)

# location of files

path <- "/Users/gilbertocamara/Dropbox/TWDTWAmazoniaCerrado/Classificacoes/MATO_GROSSO_15classes"

# list of raster files:

lst <- list.files(path=path,pattern='.tif$',full.names=TRUE)

lst

# creating a RasterStack object

raster.st <- stack(lst)

dates <- c("2001-09-01","2002-09-01","2003-09-01","2004-09-01",
       "2005-09-01","2006-09-01","2007-09-01","2008-09-01",
       "2009-09-01","2010-09-01","2011-09-01","2012-09-01",
       "2013-09-01","2014-09-01","2015-09-01","2016-09-01") # corresponding dates

dates <- as.Date(d)

# creating a RasterStackTS object:

raster.ts <- rts(raster.st, dates)

raster.ts

plot (raster.ts)
