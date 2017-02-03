#
#' Verify if the sits table has been created
#'
#' \code{sits_assert_table} tests if the sits table exists
#' @param table_name  a string - name of table (must exist)
#'
#' @export
sits_assert_table <- function (table_obj) {
     tryCatch (
          exists(deparse(substitute(table_obj))),
          error = function (cond) {
               message (paste ("Missing SITS table name!!","\n",
                               "Please provide the name of an existing sits table","\n",
                               sep=""))
               stop(cond)
          }
     )
}
