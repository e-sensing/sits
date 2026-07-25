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

# The embeddings_cube class is added by the public sits_encode() wrapper,
# not by the tile functions; align tile results the same way
encode_as_emb <- function(tile) {
    .cube_set_class(tile, c("embeddings_cube", class(tile)))
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

test_that("cube encoding returns the same structure as sits_cube()", {
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
    tile <- .tile(cube)
    out_bands <- .encode_band_names(encoder)
    # several chunks so block merging is actually exercised
    block <- c(ncols = 255L, nrows = 50L)
    sits_env[["batch_size"]] <- 32L

    cpu_dir <- file.path(tempdir(), "encode_struct_cpu")
    gpu_dir <- file.path(tempdir(), "encode_struct_gpu")
    pub_dir <- file.path(tempdir(), "encode_struct_pub")
    unlink(c(cpu_dir, gpu_dir, pub_dir), recursive = TRUE)
    dir.create(cpu_dir)
    dir.create(gpu_dir)
    dir.create(pub_dir)
    on.exit(unlink(c(cpu_dir, gpu_dir, pub_dir), recursive = TRUE),
        add = TRUE
    )

    emb_cpu <- .encode_tile_cpu(
        tile = tile, out_bands = out_bands, bands = "NDVI",
        base_bands = NULL, encoder = encoder, block = block, roi = NULL,
        impute_fn = impute_linear(), output_dir = cpu_dir,
        verbose = FALSE, progress = FALSE
    )
    expect_equal(nrow(emb_cpu), 1L)
    expect_equal(nrow(emb_cpu$file_info[[1L]]), 12L)
    expect_equal(encode_as_emb(emb_cpu), encode_ref_cube(cpu_dir))

    # Bands must keep their numeric dimension order
    expected_bands <- 1:12
    expected_bands <- ifelse(
        test = expected_bands < 10,
        yes = paste0("0", expected_bands),
        no = expected_bands
    )
    expected_bands <- paste0(.conf("embedding_band_prefix"), expected_bands)

    expect_equal(out_bands, expected_bands)
    expect_equal(.cube_bands(encode_ref_cube(cpu_dir)), out_bands)

    # direct call: dispatch via sits_encode() requires a GPU device,
    # but the tile structure under test is device-agnostic
    sits_env[["multicores"]] <- 2

    .torch_model_to_device(encoder)
    emb_gpu <- .encode_tile_gpu(
        tile = tile, out_bands = out_bands, bands = "NDVI",
        base_bands = NULL, encoder = encoder, block = block, roi = NULL,
        impute_fn = impute_linear(), output_dir = gpu_dir,
        verbose = FALSE, progress = FALSE
    )

    expect_equal(nrow(emb_gpu), 12L)
    expect_equal(nrow(emb_gpu$file_info[[1L]]), 1)

    emb <- sits_encode(
        data = cube, encoder = encoder, memsize = 4L, multicores = 2L,
        output_dir = pub_dir, progress = FALSE
    )
    expect_s3_class(emb, "embeddings_cube")
    expect_equal(emb, encode_ref_cube(pub_dir))
})

test_that("streaming encoding returns the same structure as sits_cube()", {
    # NOTE: run against an *installed* dev sits. Stage closures are
    # serialized to workers with a namespace reference, so workers resolve
    # sits internals in the installed package; under pkgload::load_all()
    # over an older installed sits, workers execute mixed code.
    skip_on_cran()
    skip_if_not_installed("siphon")
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
    tile <- .tile(cube)
    sits_env[["batch_size"]] <- 32L

    stream_dir <- file.path(tempdir(), "encode_struct_stream")
    unlink(stream_dir, recursive = TRUE)
    dir.create(stream_dir)
    on.exit(unlink(stream_dir, recursive = TRUE), add = TRUE)

    emb_stream <- .encode_tile_stream(
        tile = tile, out_bands = .encode_band_names(encoder),
        bands = "NDVI", base_bands = NULL, encoder = encoder,
        block = c(ncols = 255L, nrows = 50L), roi = NULL,
        impute_fn = impute_linear(), output_dir = stream_dir,
        multicores = 2L, verbose = FALSE, progress = FALSE
    )
    expect_equal(nrow(emb_stream), 1L)
    expect_equal(nrow(emb_stream$file_info[[1L]]), 12L)
    expect_equal(encode_as_emb(emb_stream), encode_ref_cube(stream_dir))
})
