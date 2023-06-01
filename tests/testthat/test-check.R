test_that("Caller", {

    # .check_set_caller, .check_identify_caller
    .check_set_caller("zzz")
    expect_equal(
        .check_caller,
        "zzz"
    )
    expect_equal(
        tryCatch(
            .check_that(FALSE, local_msg = "abc"),
            error = function(e) {
                return(e$message)
            }
        ),
        "zzz: abc"
    )
    rm(.check_caller)
    f1 <- function() {
        .check_set_caller("f1")
        f2()
    }
    f2 <- function() {
        .check_identify_caller()
    }
    expect_equal(
        f1(),
        "f1"
    )
    rm(f1, f2)

    # .check_that, .check_error
    expect_error(
        .check_that(FALSE)
    )
    expect_equal(
        tryCatch(
            {
                .check_set_caller("zzz")
                .check_that(FALSE, local_msg = "abc", msg = "def")
            },
            error = function(e) {
                return(e$message)
            }
        ),
        "zzz: def (abc)"
    )
    expect_equal(
        tryCatch(
            {
                .check_set_caller("zzz")
                .check_error(
                    {
                        .check_that(FALSE, local_msg = "abc")
                    },
                    msg = "def"
                )
            },
            error = function(e) {
                return(e$message)
            }
        ),
        "zzz: def (zzz: abc)"
    )
    expect_equal(
        tryCatch(
            {
                .check_set_caller("zzz")
                .check_error(
                    {
                        .check_set_caller("yyy")
                        .check_that(FALSE, local_msg = "abc")
                    },
                    msg = "def"
                )
            },
            error = function(e) {
                return(e$message)
            }
        ),
        "zzz: def (yyy: abc)"
    )
})

test_that("Checks", {
    .check_set_caller("test")

    # .check_null
    expect_error(
        .check_null(NULL, msg = "NULL value is not allowed"),
        "test: NULL value is not allowed"
    )
    expect_equal(
        .check_null("abc"),
        "abc"
    )

    # .check_na
    expect_error(
        .check_na(c(1, NA, 3)),
        "NA value is not allowed"
    )
    expect_error(
        .check_na(list(1, NA, 3)),
        "NA value is not allowed"
    )
    expect_equal(
        .check_na("abc"),
        "abc"
    )

    # .check_names
    expect_equal(
        .check_names(character(0)),
        character(0)
    )
    expect_equal(
        .check_names(character(0), is_named = FALSE),
        character(0)
    )
    expect_error(
        .check_names("abc"),
        "value should have names"
    )
    expect_equal(
        .check_names("abc", is_named = FALSE),
        "abc"
    )
    expect_error(
        .check_names(c(abc = "abc"), is_named = FALSE),
        "value should be unnamed"
    )
    expect_equal(
        .check_names(c(abc = "abc")),
        c(abc = "abc")
    )

    # .check_length
    expect_equal(
        .check_length(character(0)),
        character(0)
    )
    expect_error(
        .check_length(character(0), len_min = 1),
        "length should be >= 1"
    )
    expect_error(
        .check_length("abc", len_max = 0),
        "length should be 0"
    )
    expect_error(
        .check_length(c("a", "b", "c", "d"), len_min = 1, len_max = 3),
        "length should be <= 3"
    )

    # .check_apply
    expect_equal(
        .check_apply(c(1, 2, 3), fn_check = .check_null),
        c(1, 2, 3)
    )
    expect_error(
        .check_apply(list(1, NULL, 3),
            fn_check = .check_null,
            msg = "NULL value is not allowed"
        ),
        "NULL value is not allowed"
    )

    # .check_lgl_type, .check_num_type, .check_int_type,
    # .check_chr_type, .check_lst_type
    expect_equal(
        .check_lgl_type(logical(0)),
        logical(0)
    )
    expect_error(
        .check_lgl_type(""),
        "value is not logical"
    )
    expect_error(
        .check_lgl_type(NULL),
        "value is not logical"
    )
    expect_equal(
        .check_num_type(numeric(0)),
        numeric(0)
    )
    expect_error(
        .check_num_type("0"),
        "value is not a number"
    )
    expect_error(
        .check_num_type(NULL),
        "value is not a number"
    )
    expect_equal(
        .check_num_type(numeric(0)),
        numeric(0)
    )
    expect_equal(
        .check_num_type(numeric(0), is_integer = TRUE),
        numeric(0)
    )
    expect_error(
        .check_num_type(c(1, 1.23, 2), is_integer = TRUE),
        "value is not integer"
    )
    expect_error(
        .check_num_type("0"),
        "value is not a number"
    )
    expect_equal(
        .check_chr_type(character(0)),
        character(0)
    )
    expect_error(
        .check_chr_type(0),
        "value is not character type"
    )
    expect_error(
        .check_chr_type(NULL),
        "value is not character type"
    )
    expect_equal(
        .check_lst_type(list()),
        list()
    )
    expect_error(
        .check_lst_type(0),
        "value is not a list"
    )
    expect_error(
        .check_lst_type(NULL),
        "value is not a list"
    )

    # .check_chr_within
    expect_error(
        .check_chr_within(character(0),
            within = character(0),
            discriminator = "one_of"
        ),
        "invalid 'within' parameter"
    )
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
        .check_chr_within(c("a", "b", "c"),
            within = c("a", "b"),
            discriminator = "exactly"
        ),
        "values should be exactly"
    )
    expect_error(
        .check_chr_within(c("a", "b", "b", "c"),
            within = c("a", "b", "c"),
            discriminator = "true_of"
        ),
        ".check_chr_within: discriminator should be one of"
    )
    # .check_chr_contains
    expect_error(
        .check_chr_contains(character(0),
            contains = character(0),
            discriminator = "one_of"
        ),
        "invalid 'contains' parameter"
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
        .check_lst(list(), min_len = 1)
    )
    expect_error(
        .check_lst(list(a = 1), max_len = 0)
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
    expect_equal(
        .check_file(.conf_file()),
        .conf_file()
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
})
