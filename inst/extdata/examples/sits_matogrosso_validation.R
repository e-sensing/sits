# R script 2017-03-09
# sits package by Gilberto Câmara
# https://github.com/gilbertocamara/sits

# cross-validation for FAO Bolivia Study

library(sits)

# retrieve a set of samples from a JSON file
matogrosso.tb <- sits_getdata(file = system.file("extdata/samples/matogrosso.json", package="sits"))

matogrosso.tb <- sits_prune(matogrosso.tb)
# perform accuracy assessment - já foi feito
#cm_gam <- sits_cross_validate (matogrosso.tb, method = "gam", bands = c("ndvi","evi", "nir"),
                    #times = 100, perc = 0.5, file = "./mt_cm.json")


mt_cm <- sits_relabel (matogrosso.tb, file = system.file("extdata/results/conf_matrix.json", package = "sits"))

mt_conv.lst <- tibble::lst(Soja_Sorgo	Soybean_Commerc
                           Soja_Nada	Soybean_Fallow
                           Soja_Brachiaria	Soybean_NonCommerc
                           Soja	Soybean_Fallow
                           Pasto	Pasture
                           Milheto_Alg	NonCommerc_Cotton
                           SojaSA	Soybean_Cotton
                           Soja_Milheto	Soybean_NonCommerc
                           Cana	Sugarcane
                           Soja_Milho	Soybean_Commerc
                           Alg	NonCommerc_Cotton
                           Arroz	Rice
                           Milheto	Millet
                           Soja_Milho.3	Soybean_Commerc
                           Soja_Sorgo.1	Soybean_Commerc
                           Soja_Brachiaria.2	Soybean_NonCommerc
                           Soja_Sorgo.2	Soybean_Commerc
                           Soja_Nada.3	Soybean_Fallow
                           Pasto.2	Pasture
                           SojaSA.1	Soybean_Cotton
                           Soja_Milho.2	Soybean_Commerc
                           Soja.1	Soybean_Fallow
                           Soja_Milheto.1	Soybean_NonCommerc
                           Soja_Nada.1	Soybean_Fallow
                           Soja_Brachiaria.3	Soybean_NonCommerc
                           Arroz.1	Rice
                           Soja_Nada.2	Soybean_Fallow
                           Arroz.2	Rice
                           Soja_Milho.1	Soybean_Commerc
                           Soja_Milheto.3	Soybean_NonCommerc
                           Soja.3	Soybean_Fallow
                           Pasto.1	Pasture
                           Cana.2	Sugarcane
                           Milheto_Alg.3	NonCommerc_Cotton
                           Cana.3	Sugarcane
                           Pasto.3	Pasture
                           Milheto_Alg.1	NonCommerc_Cotton
                           Cana.1	Sugarcane
                           Alg.2	NonCommerc_Cotton
                           Milheto.1	Millet
                           Alg.3	NonCommerc_Cotton
                           Alg.1	NonCommerc_Cotton
                           SojaSA.2	Soybean_Cotton
                           Milheto_Alg.2	NonCommerc_Cotton
                           Milheto.3	Millet
                           Soja_Brachiaria.1	Soybean_NonCommerc
                           Milheto.2	Millet
                           Soja_Milheto.2	Soybean_NonCommerc
                           SojaSA.3	Soybean_Cotton
                           Soja.2	Soybean_Fallow
                           Arroz.3	Rice

mt_cm <- sits_relabel (matogrosso.tb,
                       file = system.file("extdata/results/mt_cm.json", package = "sits"),
                       conv = mt_conv.lst)

cm_centroids <- sits_cross_validate (matogrosso.tb, method = "centroids", bands = c("ndvi","evi", "nir"),
                                         times = 1, perc = 0.5, file = "./mt_cm_centroids.json")
