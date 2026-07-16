# `sits` code audit: duplication & inconsistencies

Audit of duplicated / near-duplicated code and AGENTS.md convention drift in the
`sits` R package. Each item ends with a **Recommended change**.

Findings are marked **[confirmed]** (verified by reading the code directly) or
**[scan]** (surfaced by a broad scan; line numbers approximate, verify before
acting). Line counts are estimates.

Likely-intentional differences are called out so they are **not** collapsed
during any future refactor: the encoder path deliberately skips `.ml_normalize`,
`sits_mlp` uses 2D tensors while sequence models use 3D, and MAE has a unique
reconstruction loss.

---

## Summary

| # | Theme | Severity | Approx. lines | Key files |
|---|-------|----------|---------------|-----------|
| 1 | Redundant `ml_stats` copy-paste | Low (waste, not a bug) | ~12 | torch model files |
| 2 | Torch model boilerplate duplication | High | ~900–1000 | `sits_tempcnn/tae/lighttae/mlp/resnet/lstm_fcn.R` |
| 3 | Classify vs encode pipeline duplication | High | ~480 | `api_classify.R`, `api_encode.R` |
| 4 | SSL / pretraining duplication | Medium–High | ~430–500 | `api_ssl_*.R`, `sits_ssl_*.R`, `*barlow_twins*`, `*contrastive_learning*` |
| 5 | STAC source API duplication | Medium | ~600 | `api_source_*.R` |
| 6 | `terra::` calls outside `api_raster.R` | Medium | ~8 sites | `sits_terra.R`, `api_smooth.R`, `api_variance.R` |
| 7 | Base `apply`/`for` vs `purrr`/`slider` | Low–Medium | ~80 sites | many `api_*.R`, `sits_view.R` |
| 8 | Direct `sits_env[[...]]` vs `.conf()` | Low | ~15–20 | `sits_colors.R`, `api_conf.R` |
| 9 | Missing/misplaced `.check_set_caller()` | Low–Medium | ~12–15 | `sits_colors.R`, `sits_labels.R`, `sits_histogram.R` |
| 10 | Variable-naming drift | Low | — | torch model files |

---

## 1. Redundant `ml_stats` computation (copy-paste) — [confirmed]

In the torch model files, `ml_stats <- .samples_stats(samples)` runs twice in a
row, with duplicated comments. This is not a correctness bug (the value is
identical) but it is wasteful and signals copy-paste.

- `sits_tempcnn.R:181-187` — computes `ml_stats` at line 182 **and** 187; the
  comment `# Organize train and the test data` is repeated (184–185), as is
  `# Data normalization`.
- `sits_resnet.R:180-185` — same double computation (180 and 185).
- The same pattern was reported by scan in `sits_tae.R`, `sits_lighttae.R`,
  `sits_mlp.R`, `sits_lstm_fcn.R` **[scan]**.

**Recommended change:** delete the second `ml_stats <- .samples_stats(samples)`
and the duplicated comment lines in each affected file. When the shared training
helper in item 2 is introduced, this computation lives there once.

---

## 2. Torch supervised-model boilerplate — [scan, spot-confirmed]

`sits_tempcnn.R`, `sits_tae.R`, `sits_lighttae.R`, `sits_mlp.R`, `sits_resnet.R`,
and `sits_lstm_fcn.R` repeat large near-identical blocks inside their
`train_fun`. Spot-confirmed in `sits_tempcnn.R` and `sits_resnet.R`; the rest
match by scan.

Repeated blocks:

- **Optimizer hparam handling** — `formals(optimizer)[-1L]` +
  `.check_opt_hparams()` + `utils::modifyList()` (~7 lines × 6 files).
  `sits_lstm_fcn.R` reportedly hand-rolls this differently **[scan]**.
- **Label / band / timeline extraction + `code_labels`** (~13 lines × 6).
- **Train/test split, validation handling, shuffle** (~33 lines × 6) —
  confirmed identical in `sits_tempcnn.R:188-220`.
- **3D array assembly of `train_x`/`test_x`** (~14 lines × 5; `sits_mlp` uses 2D
  — intentional).
- **Torch seed setup** (`.torch_seed()` + `torch::torch_manual_seed()`).
- **`luz::setup() |> set_opt_hparams() |> set_hparams() |> fit()` loop** (~48
  lines × 6); `sits_mlp`/`sits_lstm_fcn` omit the LR scheduler callback.
- **Cleanup + serialize** (`rm(...)`, `gc()`, `.torch_serialize_model()`).
- **`predict_fun` closure** (unserialize → normalize → array → GPU/CPU branch →
  `as_array` → set colnames) (~40 lines × 6).
- **`.set_class(...)` + `.factory_function(samples, train_fun)`** tail.

**Recommended change:** add helpers to `api_torch.R` and route all six models
through them:
- `.torch_optim_params(optimizer, opt_hparams)`
- `.torch_prepare_train_test(samples, samples_validation, validation_split, ml_stats)` (returns shuffled train/test predictors; handles the split/validation branch once)
- `.torch_build_arrays(train_samples, test_samples, code_labels, n_times, n_bands, dims)` (parameterize 2D vs 3D)
- `.torch_fit(module, hparams, opt_params, train, test, epochs, callbacks, ...)`
- `.torch_predict_closure(torch_model, serialized_model, ml_stats, samples, bands, labels, reshape)`

Keep model-specific pieces (module definition, `set_hparams` payload, callback
set) in each `sits_*` file.

---

## 3. Classify vs encode pipeline — [confirmed on key helpers]

`api_classify.R` and `api_encode.R` are structurally parallel (~85% overlap).
Confirmed byte-identical: `.classify_verbose_start/end` (`api_classify.R:1095-1124`)
vs `.encode_verbose_start/end` (`api_encode.R:855-890`).

Parallel functions that differ mainly by model call and output class:

| Concern | classify | encode |
|---------|----------|--------|
| Data read/preprocess | `.classify_data_read` (`api_classify.R:801`) | `.encode_data_read` (`api_encode.R:1089`) |
| Verbose timing | `.classify_verbose_start/end` (1095/1114) | `.encode_verbose_start/end` (855/880) |
| Chunk create + ROI/mask filter | `api_classify.R:73-108`, `328-362` | `api_encode.R:109-127`, `417-434` |
| Block-memory optimization | `api_classify.R:401-439` | `api_encode.R:355-392` |
| TS CPU | `.classify_ts_cpu` (`~1003`) | `.encode_ts_cpu` (`~759`) |
| TS GPU | `.classify_ts_gpu` (`~1049`) | `.encode_ts_gpu` (`~808`) |
| Block write | `.classify_write_block` (`597`) | `.encode_write_block` (`979`) |

Intentional differences to preserve: encode skips `.ml_normalize`; scaling
(probs vs embeddings) and output cube class differ; classify supports
`exclusion_mask`, encode currently does not (worth flagging as a possible
feature gap).

**Recommended change:** create `api_pipeline.R` (new internal API) with shared
helpers, following the two-tier layout:
- `.pipeline_verbose_start(verbose, block)` / `.pipeline_verbose_end(...)` — replace both verbose pairs outright (identical).
- `.pipeline_data_read(tile, block, bands, base_bands, ml_features_name, impute_fn, filter_fn)` — replace `.classify_data_read`/`.encode_data_read`.
- `.pipeline_block_optimization(data, model, memsize, multicores, bands, base_bands)`.
- `.pipeline_prepare_chunks(tile, block, roi, exclusion_mask = NULL)`.
- `.pipeline_ts_cpu/gpu(pred, model, ..., normalize = TRUE)` with a `normalize`
  flag so encode passes `FALSE`.

Start with the verbose pair and `data_read` (lowest-risk, verified identical).

---

## 4. SSL / pretraining duplication — [scan]

`api_ssl_lejepa.R`, `api_ssl_vicreg.R`, `api_ssl_mae.R`, `api_barlow_twins.R`,
`api_contrastive_learning.R` and their `sits_*` wrappers repeat:

- **Projector head** — identical 3-layer `nn_sequential` (Linear/BN/ReLU ×2 +
  Linear) across lejepa/vicreg/barlow_twins. (OK)
  
- **Encoder + seed setup** — `.torch_seed()` + `torch_manual_seed()` +
  `encoder_model(...)`.
- **Two-view forward** — `x[, 1, , ]$contiguous() |> encoder() |> projector()`
  (contrastive adds L2 normalize — intentional).
- **Resampling augmentation + dataset** — ~95% identical between lejepa/vicreg
  (`.lejepa_apply_resampling` vs `.vicreg_apply_resampling`; one uses
  `purrr::walk`, the other a base `for`).
- **Train/val split + pair counting** — repeated across all four pair-based
  files.
- **Stub-data creation, callback assembly, `luz::fit`, stub-module weight
  injection, `predict_fun`** — near-identical across all files.

**Recommended change:** add SSL helpers (e.g. in a new `api_ssl.R` or extend
`api_torch.R`):
- `.ssl_projector_head(embedding_dim, proj_dim)`
- `.ssl_setup_encoder(samples, embedding_dim, seed)`
- `.ssl_two_view_forward(x, encoder, projector, l2 = FALSE)`
- `.ssl_apply_resampling(...)` shared by lejepa/vicreg
- `.ssl_wrap_encoder_for_sits(...)` and `.ssl_encoder_predict_closure(...)`
- `.train_val_split(n, validation_split)` and `.resolve_num_pairs(...)`

---

## 5. STAC source API duplication — [scan]

Across `api_source_aws/bdc/cdse/cdse_os/deafrica/deaustralia/hls/mpc/ogh/sdc/`
`terrascope/usgs/stac/local.R`, these patterns recur:

- **Pagination/fetch** — `items_matched()` threshold → `.message_progress()` →
  `items_fetch()`.
- **Tile → bbox query prep** — the `if (!is.null(tiles)) .s2_mgrs_to_roi(...)`
  branch setting `intersects <- NULL` and `bbox <- ...`.
- **Tile extraction** — `rstac::items_reap(items, field = c("properties", ...))`
  with per-source property names.
- **Collection access test** — build query → `post_request` → `.check_stac_items`
  → select bands → get hrefs → optional raster open.
- **Platform filter**, **href + GDAL VSI prefix**, **`NoTilingSystem`** repeats.

**Recommended change:** extend `api_source_stac.R` with shared helpers:
`.stac_fetch_items_with_progress()`, `.stac_prepare_bbox_query()`,
`.stac_extract_tile_property()`, `.stac_test_collection_access()`,
`.stac_add_platform_filter()`, `.stac_get_item_hrefs_with_gdal()`,
`.stac_tiles_no_tiling_system()`. Per-source methods pass their property
names / converters as arguments. This is the largest surface — stage per
helper, one PR each, and lean on existing source tests.

---

## 6. `terra::` calls outside `api_raster.R` — [confirmed in sits_terra.R]

AGENTS.md: all raster I/O goes through `api_raster.R`; never call `terra::`
from feature code.

- `sits_terra.R:77, 93, 113, 137, 153` — direct `terra::rast(...)` **[confirmed]**.
- `api_smooth.R:404-405`, `api_variance.R:326` — direct `terra::rast()` /
  `terra::values()` **[scan]**.

Note: `sits_terra.R` is a terra-interop layer, so some direct use may be
deliberate; still worth reconciling with the stated rule.

**Recommended change:** route these through `.raster_*` accessors (e.g.
`.raster_open_rast()`, `.raster_values()`); if `sits_terra.R` is an intentional
exception, document that exception in AGENTS.md.

---

## 7. Base `apply` / `for` vs `purrr` / `slider` — [scan, needs filtering]

~80 sites use base `for` / `apply` / `lapply` / `vapply`. **Caution:** the scan
conflates sits' own internal `.apply()` helper (legitimate) with base
`apply()` — those must be excluded. Genuine hotspots reported: `sits_view.R`
(many `for` loops), `api_parallel.R`, `api_reclassify.R`, `api_ssl_vicreg.R`.

**Recommended change:** convert genuine base-loop sites to `purrr`/`slider`
where it improves clarity; leave performance-critical or index-heavy loops
alone. Verify each hit is base R, not `.apply()`, before changing.

---

## 8. Direct `sits_env[[...]]` instead of `.conf()` — [scan]

~15–20 sites read `sits_env[["color_table"]]`, `sits_env[["legends"]]`,
`sits_env$config[[...]]` directly (e.g. `sits_colors.R:26-42`, `api_conf.R`),
bypassing the config abstraction.

**Recommended change:** access these through `.conf(...)` accessors; if some are
genuinely runtime state (not config), leave them but keep the distinction clear.

---

## 9. Missing / misplaced `.check_set_caller()` — [scan]

Exported functions that appear to lack (or misplace) `.check_set_caller()`:
`sits_colors()`, `sits_colors_show()`, `sits_colors_reset()`, the `sits_labels`
generic/methods, `hist.sits()`. Some place it after `UseMethod()`, where it
never runs.

**Recommended change:** add `.check_set_caller("<fn>")` as the first statement
of each exported generic (before `UseMethod`), matching `sits_bbox.R` /
`sits_bands.R`.

---

## 10. Variable-naming drift — [confirmed]

Within the torch models, the same concept is named inconsistently:
`sample_labels` (`sits_tempcnn.R:169`, `sits_resnet.R:168`) vs `labels`
elsewhere; comment wording for the GPU branch varies ("Do GPU" / "GPU
classification").

**Recommended change:** standardize on one name (`sample_labels`) when the
shared torch helpers in item 2 are introduced.

---

## Suggested order of work

1. **Quick wins (low risk):** item 1 (delete redundant `ml_stats`), item 3
   verbose pair + `data_read` (verified identical), item 9.
2. **Medium:** item 2 torch helpers, item 3 remaining pipeline helpers, item 4
   SSL helpers.
3. **Larger / staged:** item 5 STAC consolidation (one helper per PR), items
   6–8 convention cleanups.

Per AGENTS.md, each change should test only the affected public functions and
keep the package CRAN-clean; regenerate docs with `devtools::document()` if any
roxygen blocks move.
