# ---- expressions ----

.expr_names <- function(expr) {
    if (is.call(expr)) {
        unique(unlist(lapply(as.list(expr)[-1], .expr_names)))
    } else if (is.name(expr)) {
        .as_chr(expr)
    } else {
        character()
    }
}
.expr_calls <- function(expr) {
    if (is.call(expr)) {
        unique(c(
            paste0(expr[[1]]), unlist(lapply(as.list(expr)[-1], .expr_calls))
        ))
    } else {
        character()
    }
}
