test_that("Test environment variables swap", {
    # Setup - Environment configuration
    env_config <- list(
        name = "SITS_TEST",
        variables = list(
            "A" = "B",
            "C" = "D"
        )
    )
    # Setup - Env variables
    Sys.setenv("A" = 123)
    Sys.setenv("B" = 321)
    Sys.setenv("C" = "ABC")
    Sys.setenv("D" = "CBA")
    # Test - Swapped variables
    .environment_patch(env_config)

    expect_equal(Sys.getenv("B"), "123")
    expect_equal(Sys.getenv("D"), "ABC")
    expect_equal(Sys.getenv("SITS_TEST_SWAP_B"), "321")
    expect_equal(Sys.getenv("SITS_TEST_SWAP_D"), "CBA")
    # Test - Rollback variables
    .environment_rollback(env_config)

    expect_equal(Sys.getenv("B"), "321")
    expect_equal(Sys.getenv("D"), "CBA")
})
