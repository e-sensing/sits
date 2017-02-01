sits_info_WTSS()
sits_config_WTSS()
load("../Assinaturas/new_patterns_VictorRodrigo_ndvi_evi_nir.RData")
plot(new_patterns_VictorRodrigo, type = "patterns")
patterns <- new_patterns_VictorRodrigo
patterns.tb <- sits_patterns_fromTWDTW(patterns)

