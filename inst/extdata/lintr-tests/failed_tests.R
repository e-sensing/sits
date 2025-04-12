── Failed tests ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Error (test-samples.R:61:5): Sampling design
Error in `dplyr::inner_join(x = sampling_design, y = cube_labels, by = "labels")`: Join columns in `y` must be present in the data.
x Problem with `labels`.
Backtrace:
    ▆
1. └─sits::sits_stratified_sampling(...) at test-samples.R:61:5
2.   ├─dplyr::rename(...) at sits/R/sits_sample_functions.R:479:5
3.   ├─dplyr::select(...)
4.   ├─dplyr::inner_join(x = sampling_design, y = cube_labels, by = "labels")
5.   └─dplyr:::inner_join.data.frame(x = sampling_design, y = cube_labels, by = "labels")
6.     └─dplyr:::join_mutate(...)
7.       └─dplyr:::join_cols(...)
8.         └─dplyr:::check_join_vars(by$y, y_names, by$condition, "y", error_call = error_call)
9.           └─rlang::abort(bullets, call = error_call)

Error (test-samples.R:112:5): Sampling design with class cube from STAC
Error in `dplyr::inner_join(x = sampling_design, y = cube_labels, by = "labels")`: Join columns in `y` must be present in the data.
x Problem with `labels`.
Backtrace:
    ▆
1. └─sits::sits_stratified_sampling(...) at test-samples.R:112:5
2.   ├─dplyr::rename(...) at sits/R/sits_sample_functions.R:479:5
3.   ├─dplyr::select(...) at sits/R/sits_sample_functions.R:479:5
4.   ├─dplyr::inner_join(x = sampling_design, y = cube_labels, by = "labels") at sits/R/sits_sample_functions.R:479:5
5.   └─dplyr:::inner_join.data.frame(x = sampling_design, y = cube_labels, by = "labels")
6.     └─dplyr:::join_mutate(...)
7.       └─dplyr:::join_cols(...)
8.         └─dplyr:::check_join_vars(by$y, y_names, by$condition, "y", error_call = error_call)
9.           └─rlang::abort(bullets, call = error_call)

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

Error (test-summary.R:73:5): summary sits area accuracy
Error in `dplyr::reframe(var_values, dplyr::across(.cols = dplyr::all_of(labels),
                                                   function(x) {
                                                       stats::quantile(x, probs = seq(0, 1, intervals))
                                                   }))`: i In argument: `dplyr::across(...)`.
Caused by error in `across()`:
    i In argument: `dplyr::all_of(labels)`.
Caused by error in `dplyr::all_of()`:
    ! Can't subset elements.
x Subscript must be numeric or character, not a function.
Backtrace:
     ▆
  1. ├─utils::capture.output(suppressWarnings(summary(variance_cube))) at test-summary.R:73:5
  2. │ └─base::withVisible(...elt(i))
  3. ├─base::suppressWarnings(summary(variance_cube))
  4. │ └─base::withCallingHandlers(...)
  5. ├─base::summary(variance_cube)
  6. ├─sits:::summary.variance_cube(variance_cube)
  7. │ ├─dplyr::reframe(...) at sits/R/sits_summary.R:321:5
  8. │ └─dplyr:::reframe.data.frame(...)
  9. │   └─dplyr:::summarise_cols(.data, dplyr_quosures(...), by, "reframe")
 10. │     ├─base::withCallingHandlers(...)
 11. │     └─dplyr:::expand_across(dot)
 12. │       └─dplyr:::across_setup(...)
 13. │         └─tidyselect::eval_select(cols, data = data, error_call = error_call)
 14. │           └─tidyselect:::eval_select_impl(...)
 15. │             ├─tidyselect:::with_subscript_errors(...)
 16. │             │ └─base::withCallingHandlers(...)
 17. │             └─tidyselect:::vars_select_eval(...)
 18. │               └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
 19. │                 └─tidyselect:::eval_context(expr, context_mask, call = error_call)
 20. │                   ├─tidyselect:::with_chained_errors(...)
 21. │                   │ └─base::withCallingHandlers(...)
 22. │                   └─rlang::eval_tidy(as_quosure(expr, env), context_mask)
 23. ├─dplyr::all_of(labels)
 24. │ └─tidyselect:::as_indices_impl(x, vars = vars, strict = TRUE)
 25. │   └─vctrs::vec_as_subscript(x, logical = "error", call = call, arg = arg)
 26. ├─rlang::cnd_signal(x)
 27. │ └─rlang:::signal_abort(cnd)
 28. │   └─base::signalCondition(cnd)
 29. ├─tidyselect (local) `<fn>`(`<vctrs___>`)
 30. │ └─cli::cli_abort(c(i = msg), call = call, parent = cnd)
 31. │   └─rlang::abort(...)
 32. │     └─rlang:::signal_abort(cnd, .file)
 33. │       └─base::signalCondition(cnd)
 34. └─dplyr (local) `<fn>`(`<rlng_rrr>`)
 35.   └─dplyr (local) handler(cnd)
 36.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)

Error (test-view.R:1:1): View
<CStackOverflowError/stackOverflowError/error/condition>
Error: C stack usage  7974040 is too close to the limit

Error (test-view.R:176:1): View BDC cube
<CStackOverflowError/stackOverflowError/error/condition>
Error: C stack usage  7970280 is too close to the limit

[ FAIL 8 | WARN 0 | SKIP 5 | PASS 1431 ]
