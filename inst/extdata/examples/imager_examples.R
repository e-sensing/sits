library(imager)
plot(boats)
boats
plot(log(boats))

fpath <- file ("/Users/gilbertocamara/Dropbox/TWDTWAmazoniaCerrado/Classificacoes/MATO_GROSSO_15classes/MT_2001.tif")

mt2001 <- load.image(fpath)
