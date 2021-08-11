library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  record = "once",
  filter_request_headers = c("Authorization", "User-Agent"),
  filter_query_parameters = list("access_token" = "<BDC_ACCESS_KEY>")
))
vcr::check_cassette_names()

