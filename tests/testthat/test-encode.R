# Structure tests for raster-cube encoding. Every encode path must return
# the same cube structure that sits_cube() builds from the output files:
# one cube row per tile, with all embedding bands in that row's file_info
# (canonical column and row order of .local_cube_file_info).

# Rebuild the encoder output as a local cube: the reference structure.
# sits_cube() classes it as embeddings_cube on its own (all bands EMB*)
encode_ref_cube <- function(output_dir) {
    sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = output_dir,
        parse_info = c("X1", "X2", "tile", "band", "date"),
        delim = "_",
        progress = FALSE
    )
}

# Force the CPU or auto-detected orchestration branch of sits_encode()
# through the public API by toggling SITS_FORCE_CPU (read by
# .torch_gpu_classification()), restoring the variable afterwards. Both
# branches run on CPU tensors when no CUDA/MPS device is present, so this
# exercises both code paths on any machine.
encode_forcing <- function(force, cube, encoder, output_dir) {
    old <- Sys.getenv("SITS_FORCE_CPU", unset = NA)
    Sys.setenv(SITS_FORCE_CPU = force)
    on.exit(
        if (is.na(old)) {
            Sys.unsetenv("SITS_FORCE_CPU")
        } else {
            Sys.setenv(SITS_FORCE_CPU = old)
        }
    )
    sits_encode(
        data = cube, encoder = encoder, memsize = 4L, multicores = 2L,
        output_dir = output_dir, progress = FALSE
    )
}


# Cheap pre-trained encoder; embedding_dim >= 10 so band names reach two
# digits (EMB10...) and row ordering is actually discriminated
encode_test_encoder <- function() {
    .try(
        sits_pre_train(
            samples        = samples_modis_ndvi,
            encoder_method = sits_ssl_mae(
                embedding_dim  = 12L,
                decoder_width  = 32L,
                mask_ratio     = 0.5,
                epochs         = 2L,
                batch_size     = 32L,
                verbose        = FALSE
            )
        ),
        .default = NULL
    )
}

test_that("cube encoding returns the same structure across CPU/GPU pipelines", {
    skip_on_cran()
    skip_if_not_installed("torch")
    skip_if_not_installed("luz")

    set.seed(42)
    torch::torch_manual_seed(42)
    encoder <- encode_test_encoder()
    skip_if(is.null(encoder), "torch training failed")

    cube <- sits_cube(
        source = "BDC",
        collection = "MOD13Q1-6.1",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        progress = FALSE
    )
    out_bands <- .encode_band_names(encoder)

    cpu_dir <- file.path(tempdir(), "encode_struct_cpu")
    gpu_dir <- file.path(tempdir(), "encode_struct_gpu")
    unlink(c(cpu_dir, gpu_dir), recursive = TRUE)
    dir.create(cpu_dir)
    dir.create(gpu_dir)
    on.exit(unlink(c(cpu_dir, gpu_dir), recursive = TRUE), add = TRUE)

    # Drive both pipelines through the public API by forcing each branch
    emb_cpu <- encode_forcing("TRUE", cube, encoder, cpu_dir)
    emb_gpu <- encode_forcing("FALSE", cube, encoder, gpu_dir)

    # Both pipelines yield the canonical embeddings_cube structure:
    # one cube row per tile, all embedding bands in that row's file_info
    expect_s3_class(emb_cpu, "embeddings_cube")
    expect_s3_class(emb_gpu, "embeddings_cube")
    # tolerance absorbs sub-pixel bbox rounding when the written rasters are
    # re-read; structural differences are still reported exactly
    expect_equal(emb_cpu, encode_ref_cube(cpu_dir), tolerance = 1e-6)
    expect_equal(emb_gpu, encode_ref_cube(gpu_dir), tolerance = 1e-6)
    expect_equal(nrow(emb_cpu), 1L)
    expect_equal(nrow(emb_gpu), 1L)
    expect_equal(nrow(.fi(emb_cpu)), 12L)
    expect_equal(nrow(.fi(emb_gpu)), 12L)

    # Bands must keep their numeric dimension order (EMB01...EMB12)
    expected_bands <- 1:12
    expected_bands <- ifelse(
        test = expected_bands < 10,
        yes = paste0("0", expected_bands),
        no = expected_bands
    )
    expected_bands <- paste0(.conf("embedding_band_prefix"), expected_bands)

    expect_equal(out_bands, expected_bands)
    expect_equal(.cube_bands(emb_cpu), out_bands)
    expect_equal(.cube_bands(emb_gpu), out_bands)
})
