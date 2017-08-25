library (sits)

# get the data from embrapa
cerrado.tb <- sits_getdata(file = system.file("extdata/samples/cerrado.json.gz", package="sits"))


conf.tb <- sits_kfold_validate(cerrado.tb, folds = 2, multicores = 1,
                                pt_method = sits_gam(),
                                dist_method = sits_TWDTW_distances(),
                                tr_method = sits_rfor ())

sits_accuracy(conf.tb)
