── Failed tests ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Error (test-segmentation.R:18:5): Segmentation
<purrr_error_indexed/rlang_error/error/condition>
    Error in `purrr::map(rounds, function(round) {
        if (!is.null(sync_fn)) {
            sync_fn(round)
        }
        round <- slider::slide(round, identity)
        .parallel_map(round, fn, ..., progress = progress)
    })`: i In index: 1.
Caused by error in `.check_remote_errors()`:
    ! one node produced an error: Invalid input type, expected 'integer' actual 'double'
Backtrace:
    ▆
1. ├─sits::sits_segment(...) at test-segmentation.R:18:5
2. │ └─sits:::.cube_foreach_tile(...) at sits/R/sits_segmentation.R:183:5
3. │   └─slider::slide_dfr(cube, fn, ...) at sits/R/api_cube.R:918:5
4. │     └─slider::slide(...)
5. │       └─slider:::slide_impl(...)
6. │         ├─slider:::slide_common(...)
7. │         └─sits (local) .f(.x, ...)
8. │           └─sits:::.segments_tile(...) at sits/R/sits_segmentation.R:185:9
9. │             └─sits:::.jobs_map_parallel_chr(...) at sits/R/api_segments.R:62:5
10. │               └─sits:::.jobs_map_parallel(jobs, fn, ..., progress = progress) at sits/R/api_jobs.R:155:5
11. │                 ├─base::unlist(...) at sits/R/api_jobs.R:138:5
12. │                 └─purrr::map(...)
13. │                   └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
14. │                     ├─purrr:::with_indexed_errors(...)
15. │                     │ └─base::withCallingHandlers(...)
16. │                     ├─purrr:::call_with_cleanup(...)
17. │                     └─sits (local) .f(.x[[i]], ...)
18. │                       └─sits:::.parallel_map(round, fn, ..., progress = progress) at sits/R/api_jobs.R:143:9
19. │                         └─sits:::.parallel_cluster_apply(x, fn, ..., pb = pb) at sits/R/api_parallel.R:296:5
20. │                           └─parallel (local) .check_remote_errors(val) at sits/R/api_parallel.R:245:9
21. │                             └─base::stop("one node produced an error: ", firstmsg, domain = NA)
22. └─base::.handleSimpleError(...)
23.   └─purrr (local) h(simpleError(msg, call))
24.     └─cli::cli_abort(...)
25.       └─rlang::abort(...)

Failure (test-segmentation.R:205:5): Segmentation of large files
.check_cube_is_regular(modis_cube_local) is not TRUE

`actual` is NULL
`expected` is a logical vector (TRUE)

Error (test-segmentation.R:207:5): Segmentation of large files
<purrr_error_indexed/rlang_error/error/condition>
    Error in `purrr::map(rounds, function(round) {
        if (!is.null(sync_fn)) {
            sync_fn(round)
        }
        round <- slider::slide(round, identity)
        .parallel_map(round, fn, ..., progress = progress)
    })`: i In index: 1.
Caused by error in `.check_remote_errors()`:
    ! one node produced an error: Invalid input type, expected 'integer' actual 'double'
Backtrace:
    ▆
1. ├─sits::sits_segment(...) at test-segmentation.R:207:5
2. │ └─sits:::.cube_foreach_tile(...) at sits/R/sits_segmentation.R:183:5
3. │   └─slider::slide_dfr(cube, fn, ...) at sits/R/api_cube.R:918:5
4. │     └─slider::slide(...)
5. │       └─slider:::slide_impl(...)
6. │         ├─slider:::slide_common(...)
7. │         └─sits (local) .f(.x, ...)
8. │           └─sits:::.segments_tile(...) at sits/R/sits_segmentation.R:185:9
9. │             └─sits:::.jobs_map_parallel_chr(...) at sits/R/api_segments.R:62:5
10. │               └─sits:::.jobs_map_parallel(jobs, fn, ..., progress = progress) at sits/R/api_jobs.R:155:5
11. │                 ├─base::unlist(...) at sits/R/api_jobs.R:138:5
12. │                 └─purrr::map(...)
13. │                   └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
14. │                     ├─purrr:::with_indexed_errors(...)
15. │                     │ └─base::withCallingHandlers(...)
16. │                     ├─purrr:::call_with_cleanup(...)
17. │                     └─sits (local) .f(.x[[i]], ...)
18. │                       └─sits:::.parallel_map(round, fn, ..., progress = progress) at sits/R/api_jobs.R:143:9
19. │                         └─sits:::.parallel_cluster_apply(x, fn, ..., pb = pb) at sits/R/api_parallel.R:296:5
20. │                           └─parallel (local) .check_remote_errors(val) at sits/R/api_parallel.R:245:9
21. │                             └─base::stop("one node produced an error: ", firstmsg, domain = NA)
22. └─base::.handleSimpleError(...)
23.   └─purrr (local) h(simpleError(msg, call))
24.     └─cli::cli_abort(...)
25.       └─rlang::abort(...)

 └─rlang::abort(message, class = error_class, parent = parent, call = error_call)


[ FAIL 8 | WARN 0 | SKIP 5 | PASS 1431 ]
