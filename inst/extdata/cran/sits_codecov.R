withr::with_envvar(new = c("SITS_RUN_TESTS" = "YES"),
                   covr::codecov(
                       token = "96cd2c35-378d-4237-9f64-20f3b9682a31",
                       quiet = FALSE,
                       exclusions = c(
                           "R/sits_detect_change.R",
                           "R/sits_bayts.R",
                           "R/api_dtw.R",
                           "R/api_detect_change.R",
                           "R/sits_dtw.R",
                           "R/api_bayts.R",
                           "R/sits_detect_change_method.R",
                           "R/api_patterns.R"
                       )
                   )
)

