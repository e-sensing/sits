withr::with_envvar(new = c("SITS_RUN_TESTS" = "YES"),
            covr::codecov(token = "96cd2c35-378d-4237-9f64-20f3b9682a31", quiet = FALSE))

