# Documentation review: exported `sits` functions

Scope: the roxygen documentation of the **112 exported functions** (documented
across 73 `R/sits_*.R` files, 111 help topics after `@rdname` grouping).
Internal `api_*` / `.dot` helpers are out of scope, except one leaked man page
noted as an aside.

Method: an authoritative mechanical audit (`tools::checkDocFiles()`,
`tools::Rd_db()`, tag scans over the generated `man/` and the roxygen source)
plus a qualitative deep-dive on a sample of core and flagged functions.

---

## 1. Executive summary

Overall the documentation is in good shape: **every exported topic has
`@title`, `@description`, `@param` and — with one exception — `@return`, and
`tools::checkDocFiles()` reports no parameter/usage mismatches** (documented
arguments all match the function signatures). The gaps are concentrated and
mostly mechanical.

Top recommendations, in priority order:

1. **Add cross-references.** No exported topic uses `@family` or `@seealso`
   (0 of 111). This is the single biggest opportunity — users cannot navigate
   between related functions from the help pages.
2. **Fix the one missing `@return`** (`sits_snic`) — this is a CRAN
   `R CMD check` NOTE risk.
3. **Add `@examples`** to the handful of exported functions that lack them,
   most notably the `impute_*` family and `sits_encode`.
4. **Close small tag gaps** (`@name`, `@author`) and **normalize `@return`
   vs `@returns`** for consistency.

---

## 2. Systemic issues

### 2.1 No cross-referencing (highest impact)
`@family` / `@seealso` appear in **0** exported topics. Related functions form
natural clusters that should link to each other. Suggested `@family` groups:

| Proposed family | Example members |
|---|---|
| Data cube creation & management | `sits_cube`, `sits_cube_copy`, `sits_cube_replace_path`, `sits_regularize`, `sits_mosaic` |
| Machine learning models | `sits_tempcnn`, `sits_lstm_fcn`, `sits_resnet`, `sits_lighttae`, `sits_rfor`, `sits_svm`, `sits_mlp`, `sits_xgboost` |
| Self-supervised learning | `sits_pre_train`, `sits_encode`, `sits_ssl_vicreg`, `sits_ssl_lejepa`, `sits_ssl_barlow_twins`, `sits_ssl_mae`, `sits_barlow_twins`, `sits_contrastive_learning` |
| Post-processing | `sits_smooth`, `sits_label_classification`, `sits_uncertainty`, `sits_clean`, `sits_combine_predictions` |
| Accuracy & validation | `sits_accuracy`, `sits_accuracy_summary`, `sits_kfold_validate` |
| Colors | `sits_colors`, `sits_colors_qgis`, `sits_colors_reset`, `sits_colors_set`, `sits_colors_show` |
| Imputation | `impute_linear`, `impute_mean`, `impute_median`, `impute_mean_window` |

Adding `@family` would improve both the man pages and the auto-generated pkgdown
reference index (the project's `_pkgdown.yml` has no curated `reference:`
section, so it relies on defaults).

### 2.2 Missing `@return` (CRAN risk)
`sits_snic` (in `R/sits_segmentation.R`) has no `@return`. It returns a
segmentation closure to be passed to `sits_segment(seg_fn = ...)`; document that.
This is the only exported topic missing `\value` and will trigger an
`R CMD check` NOTE.

### 2.3 Missing `@examples`
Exported topics with no `@examples`:

| Topic | Assessment |
|---|---|
| `impute_linear`, `impute_mean`, `impute_median`, `impute_mean_window` | Should add a short shared example (fast, uses vectors/bundled data). |
| `sits_encode` | Should add an example (`sits_pre_train()` → `sits_encode()`). |
| `sits_sf_to_tibble` | Should add an example. |
| `sits_accuracy_summary` | Acceptable — it is a print/summary helper (`@return Called for side effects`). |
| `sits_classify` | Acceptable — examples live on the method pages (`sits_classify.raster_cube`, `sits_classify.sits`) via `@rdname`. |
| `sits_run_examples`, `sits_run_tests` | Acceptable — test/CI toggles. |

### 2.4 Tag-style consistency
`@return` is used in 154 files and `@returns` in 23 across `R/`. Both are valid
roxygen aliases, but standardizing on one (`@return` is the majority) improves
consistency.

### 2.5 Examples not guarded by `sits_run_examples()`
`R/sits_csv.R`, `R/sits_select.R`, `R/sits_timeline.R` have `@examples` not
wrapped in `if (sits_run_examples()) { ... }`. These operate on bundled data
(`cerrado_2classes`, `samples_modis_ndvi`) and are fast/CRAN-safe, so this is
**likely intentional** — confirm whether the project wants uniform guarding.

### 2.6 Source-level tag gaps
Present in the roxygen source (harmless to R CMD check, but inconsistent):
- Missing `@name`: `R/sits_pre_train.R`, `R/sits_variance.R`.
- Missing `@author`: `R/sits_cube_local.R`, `R/sits_parallel.R`.

### 2.7 Aside (internal, out of primary scope)
`.check_date_parameter` (`R/api_check.R`) has `@keywords internal` but is missing
`@noRd`, so roxygen generates a stray `man/dot-check_date_parameter.Rd` for a
non-exported helper. Adding `@noRd` removes the leaked page. (Worth a quick scan
for other internal helpers with the same omission.)

---

## 3. Qualitative deep-dive (sampled functions)

Functions read in full for this review: `sits_cube`, `sits_classify`,
`sits_encode`, `sits_snic`, `sits_accuracy_summary`, and the `impute_*` family.

- **`sits_cube`** — strong. The `@description` enumerates the dispatch targets
  with `\code{\link{}}` cross-links to each method page. Good model to emulate.
- **`sits_classify`** — good structure, but a wording bug in the description:
  "takes three types of data as input and produce there types of output" →
  "takes three types of input and produces three types of output". Minor typo
  worth fixing.
- **`sits_encode`** — high quality (clear `@description`, precise `@return`
  describing both output classes, a `@note` workflow). Only gap: no `@examples`.
- **`sits_snic`** — otherwise complete (params, `@references`, `@examples`) but
  missing `@return` (see 2.2). Also clarify that it returns a function.
- **`sits_accuracy_summary`** — carries both `@keywords internal` and `@export`.
  Reconcile: if it is a supported user-facing summary, drop `@keywords internal`;
  if truly internal, it should not be exported. `@return Called for side effects`
  is appropriate.
- **`impute_*` family** — terse. Each `@description` merely restates the
  `@title` ("Remove NA by linear interpolation"), the `@param data` note is
  minimal, and there are no examples. Suggest: (a) group under one topic with
  `@rdname impute` + `@family imputation`; (b) expand each description to say
  what the function does and when to use it; (c) add one shared runnable example.

---

## 4. Prioritized action list

### Quick wins (low risk, mechanical)
1. Add `@return` to `sits_snic` (`R/sits_segmentation.R`).
2. Add `@name` to `sits_pre_train.R`, `sits_variance.R`.
3. Add `@author` to `sits_cube_local.R`, `sits_parallel.R`.
4. Fix the `sits_classify` description typo ("produce there types").
5. Add `@noRd` to `.check_date_parameter` (remove leaked man page).
6. Normalize `@returns` → `@return` package-wide.

### Medium
7. Add `@examples` to `impute_*`, `sits_encode`, `sits_sf_to_tibble`.
8. Enrich the `impute_*` descriptions and group them with `@rdname`/`@family`.
9. Confirm and, if desired, standardize example guarding in
   `sits_csv.R`, `sits_select.R`, `sits_timeline.R`.

### Larger (systemic)
10. Roll out an `@family` taxonomy across all exported functions (see 2.1),
    which also seeds a cleaner pkgdown reference.
11. Optionally add a curated `reference:` section to `_pkgdown.yml` aligned with
    the families.

---

## Appendix: audit provenance

- `tools::checkDocFiles(dir = ".")` → no param/usage mismatches.
- `tools::Rd_db(dir = ".")` over 111 exported topics →
  missing `\value`: `sits_snic`; missing `\examples`: the 10 topics in 2.3;
  `\seealso`/`@family`: 0.
- Source tag scans over `R/sits_*.R` → the `@name`/`@author` gaps (2.6),
  `@return`/`@returns` split (2.4), unguarded examples (2.5).
