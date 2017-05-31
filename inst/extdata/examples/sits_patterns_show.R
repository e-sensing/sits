# adjusting patterns

library("sits")

# retrieve a set of samples from a JSON file
patterns1.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_MatoGrosso.json")
sits_plot (patterns.tb, type = "patterns")

# retrieve a set of samples from a JSON file
patterns2.tb <- sits_getdata(file = "./inst/extdata/patterns/patterns_MT_18052017.json")
