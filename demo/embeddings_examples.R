# =============================================================================
# Comprehensive Example: Embedding Algorithms in sits
# =============================================================================
# This script demonstrates the use of different embedding algorithms for
# encoding time series, training a classifier, and classifying a data cube.
# Inspired by Chapter 18 of the sits book.
#
# Available embedding algorithms:
# 1. sits_ssl_mae() - Masked Autoencoder (self-supervised)
# 2. sits_ssl_lejepa() - LeJEPA (self-supervised with resampling)
# 3. sits_ssl_vicreg() - VICReg (self-supervised with time-warping)
# 4. sits_barlow_twins() - Barlow Twins (supervised)
# 5. sits_contrastive_learning() - Supervised contrastive learning
# =============================================================================

library(sits)
library(sitsdata)

# =============================================================================
# Part 1: Load Data
# =============================================================================

cat("=== Part 1: Loading Data ===\n")

# Load training samples from sitsdata
data("samples_deforestation_rondonia")
cat("Training samples loaded:", nrow(samples_deforestation_rondonia), "samples\n")

# Show sample summary
print(summary(samples_deforestation_rondonia))

# Load data cube from sitsdata
data_dir <- system.file("extdata/Rondonia-20LMR/", package = "sitsdata")
rondonia_20LMR <- sits_cube(
  source = "MPC",
  collection = "SENTINEL-2-L2A",
  data_dir = data_dir
)
cat("Data cube loaded\n")

# Plot the cube
plot(rondonia_20LMR, date = "2022-07-16", band = "NDVI")

# =============================================================================
# Part 2: Pre-train Encoders with Different Algorithms
# =============================================================================

cat("\n=== Part 2: Pre-training Encoders ===\n")

# Set seed for reproducibility
set.seed(03022024)

# Common parameters for all algorithms
embedding_dim <- 32L
epochs <- 20L  # Reduced for demonstration; use 100+ for production
batch_size <- 64L

# -----------------------------------------------------------------------------
# Algorithm 1: Masked Autoencoder (MAE) - Self-supervised
# -----------------------------------------------------------------------------
cat("\n--- Algorithm 1: MAE (Masked Autoencoder) ---\n")

encoder_mae <- sits_pre_train(
  samples = samples_deforestation_rondonia,
  rl_method = sits_ssl_mae(
    embedding_dim = embedding_dim,
    mask_ratio = 0.6,
    epochs = epochs,
    batch_size = batch_size,
    verbose = TRUE
  )
)

# Plot training history
plot(encoder_mae)

# -----------------------------------------------------------------------------
# Algorithm 2: LeJEPA - Self-supervised with resampling augmentation
# -----------------------------------------------------------------------------
cat("\n--- Algorithm 2: LeJEPA ---\n")

encoder_lejepa <- sits_pre_train(
  samples = samples_deforestation_rondonia,
  rl_method = sits_ssl_lejepa(
    embedding_dim = embedding_dim,
    epochs = epochs,
    batch_size = batch_size,
    verbose = TRUE
  )
)

# Plot training history
plot(encoder_lejepa)

# -----------------------------------------------------------------------------
# Algorithm 3: VICReg - Self-supervised with time-warping augmentation
# -----------------------------------------------------------------------------
cat("\n--- Algorithm 3: VICReg ---\n")

encoder_vicreg <- sits_pre_train(
  samples = samples_deforestation_rondonia,
  rl_method = sits_ssl_vicreg(
    embedding_dim = embedding_dim,
    epochs = epochs,
    batch_size = batch_size,
    verbose = TRUE
  )
)

# Plot training history
plot(encoder_vicreg)

# -----------------------------------------------------------------------------
# Algorithm 4: Barlow Twins - Supervised
# -----------------------------------------------------------------------------
cat("\n--- Algorithm 4: Barlow Twins ---\n")

encoder_barlow <- sits_pre_train(
  samples = samples_deforestation_rondonia,
  rl_method = sits_barlow_twins(
    embedding_dim = embedding_dim,
    epochs = epochs,
    batch_size = batch_size,
    verbose = TRUE
  )
)

# Plot training history
plot(encoder_barlow)

# -----------------------------------------------------------------------------
# Algorithm 5: Supervised Contrastive Learning
# -----------------------------------------------------------------------------
cat("\n--- Algorithm 5: Supervised Contrastive Learning ---\n")

encoder_contrastive <- sits_pre_train(
  samples = samples_deforestation_rondonia,
  rl_method = sits_contrastive_learning(
    embedding_dim = embedding_dim,
    proj_dim = 64L,
    num_pairs = 500L,
    epochs = epochs,
    batch_size = batch_size,
    verbose = TRUE
  )
)

# Plot training history
plot(encoder_contrastive)

# =============================================================================
# Part 3: Complete Workflow for Every Algorithm
# =============================================================================
# For each pre-trained encoder we run the full pipeline:
#   1. encode all training samples
#   2. train a Random Forest on the encoded samples
#   3. encode the data cube
#   4. classify the embeddings cube
#   5. label the classification
# Each encoder writes an embeddings cube with the same band names
# (EMB1..EMBn), tile and date, so results are kept in a per-method output
# directory to avoid file-name collisions (and false "recovery" from a
# previous encoder's files).

cat("\n=== Part 3: Complete Workflow for Every Algorithm ===\n")

# Collect the encoders pre-trained in Part 2 (named by algorithm)
encoders <- list(
  mae         = encoder_mae,
  lejepa      = encoder_lejepa,
  vicreg      = encoder_vicreg,
  barlow      = encoder_barlow,
  contrastive = encoder_contrastive
)

# Base output directory (one subdirectory per algorithm is created below)
output_dir <- tempdir()

# Store the classified maps so they can be compared afterwards
label_cubes <- list()

for (method in names(encoders)) {
  cat("\n--- Workflow for encoder:", method, "---\n")
  encoder <- encoders[[method]]

  # Per-method output directory (keeps each encoder's cubes separate)
  method_dir <- file.path(output_dir, method)
  dir.create(method_dir, showWarnings = FALSE, recursive = TRUE)

  # Step 1: Encode all training samples
  cat("Encoding training samples...\n")
  samples_encoded <- sits_encode(
    data = samples_deforestation_rondonia,
    encoder = encoder,
    progress = TRUE
  )

  # Step 2: Train a Random Forest classifier on encoded samples
  cat("Training Random Forest on encoded samples...\n")
  rf_model <- sits_train(
    samples_encoded,
    ml_method = sits_rfor(num_trees = 100)
  )

  # Step 3: Encode the data cube
  cat("Encoding data cube...\n")
  emb_cube <- sits_encode(
    data = rondonia_20LMR,
    encoder = encoder,
    output_dir = method_dir,
    memsize = 8,
    multicores = 2,
    progress = TRUE
  )

  # Step 4: Classify the embeddings cube
  cat("Classifying embeddings cube...\n")
  probs_cube <- sits_classify(
    data = emb_cube,
    ml_model = rf_model,
    output_dir = method_dir,
    version = paste0(method, "_probs"),
    memsize = 8,
    multicores = 2,
    progress = TRUE
  )

  # Step 5: Label the classification
  cat("Labeling classification...\n")
  label_cube <- sits_label_classification(
    cube = probs_cube,
    output_dir = method_dir,
    version = paste0(method, "_label")
  )

  # Keep the result and plot the final classified map for this encoder
  label_cubes[[method]] <- label_cube
}

cat("Classified map for encoder: MAE\n")
plot(label_cubes$mae)

cat("Classified map for encoder: LeJEPA\n")
plot(label_cubes$lejepa)

cat("Classified map for encoder: VICReg\n")
plot(label_cubes$vicreg)

cat("Classified map for encoder: Barlow Twins\n")
plot(label_cubes$barlow)

cat("Classified map for encoder: Constrative\n")
plot(label_cubes$contrastive)

cat("\n=== Done: classified maps available for",
    paste(names(label_cubes), collapse = ", "), "===\n")

