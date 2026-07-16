# `sits` package — engineering audit (2026-07-15)

Software-engineering audit of the `sits` package as it stands today. This
updates the earlier `plan/code-audit.md`, which predates the refactors made in
the interim and is now partly stale.

**Method.** Read-only inspection (`grep`/reads). Findings are tagged
**[confirmed]** (read directly) or **[scan]** (grep-level signal — verify before
acting). Line counts and site counts are approximate.

---

## 1. Executive summary

| # | Theme | Severity | Status |
|---|-------|----------|--------|
| 1 | classify vs encode pipeline duplication | High | **Open** |
| 2 | STAC source API duplication | Medium | **Open** |
| 3 | Indentation: AGENTS.md (tabs) vs codebase (4 spaces) | Medium | **Open** (new) |
| 4 | Base `apply`/`for` vs `purrr`/`slider` | Low–Medium | Open |
| 5 | Direct `sits_env[[...]]` vs `.conf()` | Low | Open (mostly legit) |
| 6 | Tests skip on training failure (masking) | Medium | Open |
| 7 | No invariant guard for caller↔message keys | Low–Medium | Open (new) |
| 8 | Torch model boilerplate duplication | — | **Resolved** |
| 9 | SSL / pretraining duplication | — | **Resolved** |
| 10 | Missing `.check_set_caller` messages | — | **Resolved** |
| 11 | `terra::` calls outside `api_raster.R` | — | **Resolved** |

### Resolved since the previous audit
- **Torch models** consolidated onto shared helpers in `api_torch.R`
  (`.torch_sample_info`, `.torch_split_train_test`, `.torch_build_arrays`,
  `.torch_set_seed`, `.torch_callbacks`, `.torch_fit_model`,
  `.torch_optim_params`); the prediction closure was intentionally kept inline
  per maintainer preference.
- **SSL wrappers** consolidated behind `api_ssl.R` (`.ssl_projector_head`,
  `.ssl_callbacks`, `.ssl_stub_data`, `.ssl_wrap_encoder`) and the two identical
  resampling functions merged into `.ssl_apply_resampling`. The encoder
  prediction closure was intentionally kept inline.
- **Latent fixes:** removed the double `ml_stats` computation and the
  `sample_labels`/`labels` naming drift in the torch models; fixed a
  `.check_samples_validation(labels = labels)` reference that used an undefined
  variable in `sits_tempcnn`/`sits_resnet`.
- **Messages:** every `.check_set_caller("X")` now has an `X:` entry in
  `config_messages.yml` (**0 caller gaps, 0 duplicate keys** [confirmed]), and a
  load-breaking duplicate key (`sits_labels_raster_cube`) was removed.
- **`terra::`** is no longer called outside `api_raster.R` (see §4).

---

## 2. Architecture & conventions (AGENTS.md adherence)

Overall the package follows its documented conventions well: S3 generics with
per-class methods, the two-tier `sits_*` (public) / `api_*` (internal) file
split, layered domain APIs (`api_raster`/`api_cube`/`api_tile`/`api_bbox`),
`@noRd` internals, and centralized config/messages via `.conf()`.

Confirmed-good:
- **Raster API boundary respected** — `.raster_*` wraps `terra`; no feature-code
  `terra::` calls remain (§4).
- **Config/messages complete** — `.conf("messages", caller)` resolves for every
  caller; no missing or duplicate keys [confirmed].
- **`.check_set_caller()` coverage** — present on exported generics.

**Finding 3 — indentation drift [confirmed].** AGENTS.md §"Code style" states
*"Indentation: 1 tab, no spaces"*, but the codebase is uniformly **4-space**
indented:

| file | leading-tab lines | leading-4-space lines |
|------|------------------|-----------------------|
| `api_check.R` | 0 | 1736 |
| `api_torch.R` | 0 | 293 |
| `sits_classify.R` | 0 | 409 |
| `api_ssl_lejepa.R` | 0 | 71 |
| `api_ssl.R` | **149** | 2 |

`api_ssl.R` (added this session, following the literal AGENTS.md rule) is the
sole tab-indented file and is now the outlier.
**Recommendation:** pick one source of truth — either reindent `api_ssl.R` to
4 spaces to match the de-facto standard (preferred, least churn), **or** update
AGENTS.md to say "4 spaces" and reformat the tree. Do not leave both.

---

## 3. Code duplication — current status

### 3.1 classify vs encode pipeline — **Open, High** [confirmed]
`api_classify.R` and `api_encode.R` are structurally parallel: ~13 near-mirror
helper pairs `.classify_*` / `.encode_*`, including `data_read`, `read_block`,
`chunk_cpu`, `tile_cpu`, `tile_gpu`, `ts`, `ts_cpu`, `ts_gpu`, `write_block`,
and byte-identical `verbose_start`/`verbose_end`
(`api_classify.R:1079` vs `api_encode.R:759`). Estimated ~480 shared lines.
Intentional differences to preserve: encode skips `.ml_normalize`, scales
embeddings vs probabilities, produces a different output cube class, and (today)
classify supports `exclusion_mask` while encode does not.
**Recommendation:** extract a shared `api_pipeline.R` (e.g.
`.pipeline_verbose_start/end`, `.pipeline_data_read`, `.pipeline_ts_cpu/gpu`
with a `normalize` flag, `.pipeline_write_block`); start with the identical
verbose pair and `data_read` (lowest risk). Note: the encode-side `exclusion_mask`
gap is worth confirming as intended vs a feature omission.

### 3.2 STAC source API — **Open, Medium** [scan]
`api_source_*.R` repeat pagination/fetch, tile→bbox prep, item-property reaping,
collection access tests, platform filtering, and GDAL VSI href handling.
**Recommendation:** grow `api_source_stac.R` with shared helpers parameterized by
per-source property names; migrate one helper per PR, leaning on source tests.

---

## 4. Convention violations

- **`terra::` outside `api_raster.R` — Resolved** [confirmed]. Only two matches
  remain and both are **comments** (`api_segments.R:343` `@note`,
  `api_cube.R:1814`), not code.
- **Base `apply`/`for` vs `purrr`/`slider` — Open** [scan]. Rough counts across
  `R/`: 48 `lapply`, 17 `vapply`, 16 `apply`, plus `for` hotspots in
  `sits_view.R` (14), `api_parallel.R` (5), `api_reclassify.R` (4),
  `api_data.R`/`api_check.R` (3 each). Some are performance-critical or
  index-heavy and best left; convert the clearly-mechanical ones.
  **Caution:** exclude sits' own `.apply()` helper from any sweep.
- **Direct `sits_env[[...]]` — Open, Low** [scan]. Present in 28 files. Many are
  legitimate *runtime state* (`batch_size`, GPU settings) rather than config;
  only genuine configuration reads should move to `.conf()`. Triage before
  changing.

---

## 5. Correctness / bug risks

- **SSL loss normalization contract** [confirmed]. `.contrastive_learning_loss`
  (SupCon) requires L2-normalized views; raw inputs yield `NaN`. This is honored
  in production (`forward()` normalizes) and now covered by tests. Keep the
  contract documented on the loss.
- **`sits_lstm_fcn` Apple-MPS handling** [confirmed, intentional]. It forces CPU
  for training (`!.torch_cuda_enabled()`) and keeps a custom `predict_fun` that
  avoids MPS. Do not "unify" this with `.torch_cpu_train()`, which would enable
  MPS and hit the known luz/MPS issue.
- **Config-key uniqueness** — a duplicate `sits_labels_raster_cube` recently
  broke config loading (the loader errors on duplicate keys). Fixed, but there
  is **no guard** — see §7.

---

## 6. Testing

- Convention is public-API-first, with internals covered implicitly (SSL test
  files are an exception and test internals directly).
- Recently corrected stale references (this session): `sits_mae` → `sits_ssl_mae`,
  a non-existent `pair_smp_method` argument, and the contrastive loss name.
- **Skip-masks-error risk [confirmed].** All five SSL test files
  (`test-ssl-lejepa/vicreg`, `test-barlow-twins`, `test-contrastive-learning`,
  `test-mae`) wrap training in `.try(..., .default = NULL)` + `skip_if(is.null(...))`.
  A real training regression surfaces as a *skip*, not a failure.
  **Recommendation:** keep the skip for resource-less CI, but add at least one
  non-skipping smoke test (guarded by `skip_if_not_installed("torch")`) that
  asserts training produces a `sits_encoder`.

---

## 7. Documentation & CRAN / build health

- Internals are `@noRd`; `DESCRIPTION` `Collate` includes `api_ssl.R` [confirmed].
- `plan/` is build-ignored (`^plan$` in `.Rbuildignore`) [confirmed] — this audit
  file does not affect `R CMD check`.
- **Recommendation (new invariant guards):**
  1. a test asserting **every `.check_set_caller("X")` has a message key** (locks
     in the fix from this session);
  2. a test asserting **`config_messages.yml` has no duplicate keys** (prevents a
     recurrence of the load-breaking duplicate).
  Both are cheap `grep`/`yaml`-level checks.
- Acceptance gate for any change: `devtools::document()`, targeted
  `devtools::test()`, then `R CMD check` (keep CRAN-clean).

---

## 8. Performance / hot paths (brief)

- Compiled `Rcpp` routines in `src/` (dtw, glcm, smoothing, interpolation) cover
  the numeric hot paths — appropriate.
- classify/encode use block/chunk processing with memory sizing and
  parallelism via `api_parallel.R`; the shared consolidation in §3.1 should
  preserve the block-memory optimization.
- For remote/DB-backed cubes, keep pipelines lazy and `collect()` only at the
  end (already the documented guidance).

---

## 9. Prioritized remediation roadmap

**Quick wins (low risk)**
- Resolve the indentation contradiction (§3): reindent `api_ssl.R` to 4 spaces,
  or amend AGENTS.md. 
- Add the two invariant tests (§7): caller↔message coverage and message-key
  uniqueness.
- Add one non-skipping SSL training smoke test (§6).

**Medium**
- Extract `api_pipeline.R` and migrate the classify/encode helpers (§3.1),
  starting with the identical `verbose`/`data_read` helpers.
- Convert the clearly-mechanical base loops (`sits_view.R`, etc.) to
  `purrr`/`slider` (§4), skipping performance-critical ones.

**Larger / staged**
- Consolidate the STAC `api_source_*` helpers into `api_source_stac.R` (§3.2),
  one helper per PR.
- Triage `sits_env[[...]]` reads (§4) into config vs runtime state; migrate the
  config reads to `.conf()`.

All changes should follow AGENTS.md: test only the affected functions, run
`devtools::document()` when roxygen moves, and keep the package CRAN-clean.
