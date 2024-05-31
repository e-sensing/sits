.merge_diff_timelines <- function(t1, t2) {
    abs(as.Date(t1) - as.Date(t2))
}

.cube_merge <- function(data1, data2) {
    data1 <- slider::slide2_dfr(data1, data2, function(x, y) {
        .fi(x) <- dplyr::arrange(
            dplyr::bind_rows(.fi(x), .fi(y)),
            .data[["date"]],
            .data[["band"]],
            .data[["fid"]]
        )
        # remove duplicates
        .fi(x) <- dplyr::distinct(
            .fi(x),
            .data[["band"]],
            .data[["date"]],
            .keep_all = TRUE
        )

        return(x)
    })
    return(data1)
}
