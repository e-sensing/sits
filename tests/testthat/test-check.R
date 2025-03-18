test_that("Caller", {
    # .check_set_caller, .check_identify_caller
    .check_set_caller(".test_check")
    expect_equal(.check_identify_caller(), ".test_check")
    error_msg <- .conf("messages", ".test_check")
    expect_equal(error_msg, "expected error during testing")


    # .check_null
    input <- NULL
    expect_error(
        .check_null_parameter(input),
         ".test_check: NULL value not allowed for input
        - expected error during testing"
    )
    # .check_na
    input <- c(1, NA, 3)
    expect_error(
        .check_na_parameter(input),
        ".test_check: NA value not allowed for input
        - expected error during testing"
    )

    # .check_num_paramter
    input <- c(1, "MPC")
    expect_error(
        .check_num_parameter(input),
        ".test_check: invalid input parameter - expected error during testing"
    )
    expect_error(
        .check_int_parameter(input),
        ".test_check: invalid input parameter - expected error during testing"
    )
    input <- "TRUE"
    expect_error(
        .check_lgl_parameter(input),
        ".test_check: invalid input parameter - expected error during testing"
    )
    expect_error(
        .check_date_parameter("2023-301-01"),
        ".check_date_parameter: invalid date format
        - dates should follow year-month-day: YYYY-MM-DD"
    )
    legends <- c("Pasture", "Cerrado", "Soy")
    expect_error(
        .check_chr_parameter(legends, len_max = 2),
        ".test_check: invalid legends parameter - expected error during testing"
    )
    sources <- .conf("sources")
    expect_error(
        .check_lst_parameter(sources, len_max = 4),
        ".test_check: invalid sources parameter - expected error during testing"
    )
    period <- "P2Y6M"
    expect_error(
        .check_period(period),
        ".check_period: invalid period format
        - valid examples are P16D, P1M, P1Y"
    )
    crs <- "EPSG:9999"
    expect_error(
        .check_crs(crs),
        ".check_crs: invalid crs information in image files"
    )
    output_dir <- paste0("/mydir/123/test")
    expect_error(
        .check_output_dir(output_dir),
        ".check_output_dir: invalid output_dir variable
        - file does not exist: '/mydir/123/test'"
    )
    version <- c("1", "2")
    expect_error(
        .check_version(version),
        ".check_version: version should be
        lower case character vector with no underlines"
    )
    progress <- "TRUE"
    expect_error(
        .check_progress(progress),
        ".check_progress: progress must be either TRUE or FALSE"
    )
    # .check_chr_within
    expect_equal(
        .check_chr_within(c("a", "a"),
            within = c("a", "b", "c"),
            discriminator = "one_of"
        ),
        c("a", "a")
    )
    expect_equal(
        .check_chr_within(c("a", "b"),
            within = c("a", "b", "c"),
            discriminator = "any_of"
        ),
        c("a", "b")
    )
    expect_equal(
        .check_chr_within(c("a", "b"),
            within = c("a", "b", "c"),
            discriminator = "all_of"
        ),
        c("a", "b")
    )
    expect_equal(
        .check_chr_within(c("a", "b", "b", "c"),
            within = c("a", "b", "c"),
            discriminator = "all_of"
        ),
        c("a", "b", "b", "c")
    )
    expect_equal(
        .check_chr_within(c("a", "b", "c"),
            within = c("d"),
            discriminator = "none_of"
        ),
        c("a", "b", "c")
    )
    expect_error(
        .check_chr_within(c("a", "b", "b", "c"),
            within = c("a", "b", "c"),
            discriminator = "true_of"
        ),
        ".check_chr_within: discriminator should be one of"
    )
    expect_error(
        .check_chr_within(c("a", "b"),
                          within = c("a", "b", "c"),
                          discriminator = "exactly"
        )
    )
    # .check_chr_contains
    expect_error(
        .check_chr_contains(c("a", "b", "c"),
            contains = c("a", "b"),
            discriminator = "one_of"
        )
    )
    expect_error(
        .check_chr_contains(c("a", "b", "c"),
                            contains = c("a", "b"),
                            discriminator = "none_of"
        )
    )
    expect_error(
        .check_chr_contains(c("a", "b", "c"),
                            contains = c("a", "b"),
                            discriminator = "exactly"
        )
    )
    expect_equal(
        .check_chr_contains(c("a", "b", "c"),
            contains = c("a", "b"),
            discriminator = "any_of"
        ),
        c("a", "b", "c")
    )
    expect_error(
        .check_chr_contains(c("a", "b", "c"),
            contains = c("a", "b"),
            discriminator = "one_of"
        )
    )
    expect_equal(
        .check_chr_contains(c("a", "b", "c"),
            contains = c("a", "b"),
            discriminator = "any_of"
        ),
        c("a", "b", "c")
    )
    expect_equal(
        .check_chr_contains(c("a", "b", "c"),
            contains = c("a", "b", "c"),
            discriminator = "all_of"
        ),
        c("a", "b", "c")
    )
    expect_error(
        .check_chr_contains(c("a", "b", "b", "b"),
            contains = c("a", "b", "c"),
            discriminator = "all_of"
        )
    )

    # .check_lgl
    expect_equal(
        .check_lgl(c(TRUE, FALSE, FALSE), allow_na = FALSE),
        c(TRUE, FALSE, FALSE)
    )
    expect_error(
        .check_lgl(c(TRUE, NA, FALSE), allow_na = FALSE)
    )
    expect_equal(
        .check_lgl(c(TRUE, NA, FALSE), allow_na = TRUE),
        c(TRUE, NA, FALSE)
    )
    expect_equal(
        .check_lgl(logical(0)),
        logical(0)
    )
    expect_error(
        .check_lgl(logical(0), len_min = 1)
    )
    expect_error(
        .check_lgl(logical(1), len_max = 0)
    )
    expect_error(
        .check_lgl(NULL, msg = "NULL value is not allowed")
    )
    expect_equal(
        .check_lgl(NULL, allow_null = TRUE),
        NULL
    )
    expect_error(
        .check_lgl(c(a = TRUE, b = FALSE))
    )
    expect_equal(
        .check_lgl(c(a = TRUE, b = FALSE), is_named = TRUE),
        c(a = TRUE, b = FALSE)
    )
    expect_error(
        .check_lgl(c(TRUE, FALSE), is_named = TRUE)
    )

    # .check_num
    expect_equal(
        .check_num(c(1, 2, 3), allow_na = FALSE),
        c(1, 2, 3)
    )
    expect_error(
        .check_num(c(1, NA, 3), allow_na = FALSE)
    )
    expect_error(
        .check_num(c(1, 2, 3), min = "a")
    )
    expect_error(
        .check_num(c(1, 2, 3), max = "a")
    )
    expect_equal(
        .check_num(c(1, NA, 3), allow_na = TRUE),
        c(1, NA, 3)
    )
    expect_equal(
        .check_num(c(0, 1, 2, 3, 4), min = -9, max = 9),
        c(0, 1, 2, 3, 4)
    )
    expect_error(
        .check_num(c(0, 1, 2, 3, 4), exclusive_min = 0)
    )
    expect_error(
        .check_num(c(0, 1, 2, 3, 4, 9), exclusive_max = 9)
    )
    expect_error(
        .check_num(c(0, 1, 2, 3, 4), min = 5, max = 9)
    )
    expect_error(
        .check_num(c(0, 1, 2, 3, 4), min = -9, max = -5)
    )
    expect_error(
        .check_num(c(0, 1, 2, 3, 4), min = 3, max = 9)
    )
    expect_error(
        .check_num(c(0, 1, 2, 3, 4), min = -9, max = 1)
    )
    expect_error(
        .check_num(c(0, 1, 2, 3, 4), min = 1, max = 3)
    )
    expect_equal(
        .check_num(c(0, 1, 2, 3, 4), min = 0, max = 4),
        c(0, 1, 2, 3, 4)
    )
    expect_equal(
        .check_num(numeric(0)),
        numeric(0)
    )
    expect_error(
        .check_num(numeric(0), len_min = 1)
    )
    expect_error(
        .check_num(numeric(1), len_max = 0)
    )
    expect_error(
        .check_num(NULL, msg = "NULL value is not allowed")
    )
    expect_equal(
        .check_num(NULL, allow_null = TRUE),
        NULL
    )
    expect_equal(
        .check_num(c(1, 1.23, 2)),
        c(1, 1.23, 2)
    )
    expect_equal(
        .check_num(x = 1, min = 1.1, max = 1.1, tolerance = 0.1),
        1
    )
    expect_error(
        .check_num(x = 1, min = 1.1, max = 1.1, tolerance = 0)
    )
    expect_equal(
        .check_num(x = -1, min = -0.99, max = -1, tolerance = 0.1),
        -1
    )
    expect_error(
        .check_num(x = -1, min = -0.99, max = -1),
    )
    expect_error(
        .check_num(c(1, 1.23, 2), is_integer = TRUE)
    )
    expect_error(
        .check_num(c(a = 1, b = 2))
    )
    expect_equal(
        .check_num(c(a = 1, b = 2), is_named = TRUE),
        c(a = 1, b = 2)
    )
    expect_error(
        .check_num(c(1, 2), is_named = TRUE)
    )

    # .check_chr
    expect_equal(
        .check_chr(c("a", "b", "c")),
        c("a", "b", "c")
    )
    expect_error(
        .check_chr(c("a", NA, "c"))
    )
    expect_equal(
        .check_chr(c("a", NA, "c"), allow_na = TRUE),
        c("a", NA, "c")
    )
    expect_equal(
        .check_chr(c("a", "", "c")),
        c("a", "", "c")
    )
    expect_error(
        .check_chr(c("a", "", "c"), allow_empty = FALSE)
    )
    expect_error(
        .check_chr(c(NA, "", "c"))
    )
    expect_equal(
        .check_chr(c(NA, "", "c"), allow_na = TRUE, allow_empty = TRUE),
        c(NA, "", "c")
    )
    expect_equal(
        .check_chr(character(0)),
        character(0)
    )
    expect_error(
        .check_chr(character(0), len_min = 1)
    )
    expect_error(
        .check_chr(character(1), len_max = 0)
    )
    expect_error(
        .check_chr(NULL, msg = "NULL value is not allowed")
    )
    expect_equal(
        .check_chr(NULL, allow_null = TRUE),
        NULL
    )
    expect_error(
        .check_chr(c(a = "a", b = "b"))
    )
    expect_equal(
        .check_chr(c(a = "a", b = "b"), is_named = TRUE),
        c(a = "a", b = "b")
    )
    expect_error(
        .check_chr(c("a", "b"), is_named = TRUE)
    )
    expect_equal(
        .check_chr(c("http://example.com"), regex = "^[^ \"]+://[^ \"]+$"),
        c("http://example.com")
    )
    expect_error(
        .check_chr(c("http://example com"), regex = "^[^ \"]+://[^ \"]+$")
    )
    expect_error(
        .check_chr(c("example.com"), regex = "^[^ \"]+://[^ \"]+$")
    )
    # .check_lst
    expect_equal(
        .check_lst(list()),
        list()
    )
    expect_error(
        .check_lst(list(), len_min = 1)
    )
    expect_error(
        .check_lst(list(a = 1), len_max = 0)
    )
    expect_error(
        .check_lst(NULL, msg = "NULL value is not allowed")
    )
    expect_equal(
        .check_lst(NULL, allow_null = TRUE),
        NULL
    )
    expect_equal(
        .check_lst(list(a = 1, b = 2)),
        list(a = 1, b = 2)
    )
    expect_error(
        .check_lst(list(a = 1, b = 2), is_named = FALSE)
    )
    expect_equal(
        .check_lst(list(1, 2), is_named = FALSE),
        list(1, 2)
    )
    expect_equal(
        .check_lst(list(a = 1, b = 2), fn_check = .check_num_type),
        list(a = 1, b = 2)
    )
    expect_error(
        .check_lst(list(a = "a", b = "b"), fn_check = .check_num_type)
    )

    # .check_file
    expect_error(
        .check_file(character(0))
    )
    expect_error(
        .check_file("file_does_not_exist")
    )
    expect_error(
        .check_file("file_does_not_exist", extensions = "xyz")
    )

    # .check_warn
    expect_warning(
        .check_warn(.check_that(FALSE))
    )
    expect_equal(
        .check_warn(.check_num(123)),
        123
    )
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "TRUE")
    expect_false(.check_warnings())
    expect_false(.check_documentation(progress = TRUE))
    expect_false(.check_messages())
    Sys.setenv("SITS_DOCUMENTATION_MODE" = "FALSE")

})
