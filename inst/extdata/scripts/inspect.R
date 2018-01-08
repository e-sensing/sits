inspect <- function(file){

    lintr::lint(file, with_defaults(line_length_linter(120),
                                     camel_case_linter = NULL))
}
