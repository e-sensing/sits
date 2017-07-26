#save the data
prodes_2.tb <- sits_getdata(file = "./inst/extdata/samples/prodes_226_64_apr_set.json")

#cross_validate raw series
cv_apr_set <- sits_cross_validate (prodes_2.tb, method = "gam", bands = bands,
                                   from = "2016-04-03", to = "2016-09-10", freq = 8, interval = "5 month", formula = y ~ s(x),
                                   tw_alpha = -0.1, tw_beta = 60, tw_theta = 0.5, overlap = 0.3,
                                   times = 50, perc = 0.8, file = "./inst/extdata/validation/cv_apr_set.json")

# relabel and see assessment
prodes_relabel.lst <-  tibble::lst("Forest" = "Forest",
                                   "ClearCut"  = "NonForest",
                                   "Pasture"  = "NonForest")

cv_apr_set_2 <- sits_reassess(file = "./inst/extdata/validation/cv_apr_set.json",
                              conv = prodes_relabel.lst)

# test savitsky golay filter
samples_sg.tb <- sits_sgolay(prodes_2.tb, order = 2, scale = 1)
bands_sg = c("ndvi.sg", "evi.sg")
cv_sg <- sits_cross_validate (samples_sg.tb, method = "gam", bands = bands_sg,
                              from = "2016-04-03", to = "2016-09-10", freq = 8, interval = "5 month", formula = y ~ s(x),
                              tw_alpha = -0.1, tw_beta = 60, tw_theta = 0.5, overlap = 0.3,
                              times = 30, perc = 0.8, file = "./inst/extdata/validation/cv_sg.json")

cv_sg_2 <- sits_reassess(file = "./inst/extdata/validation/cv_sg.json",
                              conv = prodes_relabel.lst)

# test whitakker filter
samples_whit.tb <- sits_whittaker(prodes_2.tb, lambda = 2.0)
bands_whit = c("ndvi.whit", "evi.whit")
cv_whit <- sits_cross_validate (samples_whit.tb, method = "gam", bands = bands_whit,
                                from = "2016-04-03", to = "2016-09-10", freq = 8, interval = "5 month", formula = y ~ s(x),
                                tw_alpha = -0.1, tw_beta = 60, tw_theta = 0.5, overlap = 0.3,
                                times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_whit.json")

cv_whit_2 <- sits_reassess(file = "./inst/extdata/validation/cv_whit.json",
                              conv = prodes_relabel.lst)

sg1 <- sits_sgolay(prodes_2.tb[1:10,], order = 2, scale = 1)

sg1 %>%
     sits_merge (prodes_2.tb[1:10,]) %>%
     sits_select (bands = c("ndvi", "ndvi.sg")) %>%
     sits_plot(type="one_by_one")

w1 <- sits_whittaker(prodes_2.tb[1:10,], lambda = 2.0)

w1 %>%
     sits_merge (prodes_2.tb[1:10,]) %>%
     sits_select (bands = c("ndvi", "ndvi.whit")) %>%
     sits_plot(type="one_by_one")

<<<<<<< HEAD
cv_env <- sits_cross_validate (samples_env.tb, method = "gam", bands = bands_env,
                               times = 50, perc = 0.5, file = "./inst/extdata/validation/cv_env.json")

prodes_relabel.lst <-  tibble::lst("primary_forest" = "Forest",
                                   "clear_cut2015"  = "NonForest",
                                   "clear_cut2016"  = "NonForest",
                                   "pasture"        = "NonForest")
=======
>>>>>>> a5b13e545ef2d54c34abd832e64dfc64fe3c03ac
