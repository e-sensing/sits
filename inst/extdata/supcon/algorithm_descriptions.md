# Algorithm Descriptions

## Supervised Contrastive Learning (`sits_contrastive_learning`)

This implements the **Supervised Contrastive Learning** framework (Khosla et al., 2020) adapted for satellite image time series. The goal is to pre-train an encoder that maps time series into an embedding space where same-class samples cluster together and different-class samples are pushed apart.

### Pipeline

**1. Pair construction** (`api_contrastive_learning.R`, `.contrastive_learning_data_split`)

Samples are normalized, split into train/validation, and then assembled into *view pairs* (A, B). Two pairing strategies are available:
- **"label"** (default): view B is a randomly chosen sample from the *same class* as the anchor. Singletons self-pair.
- **"random"**: view B is drawn uniformly at random regardless of class.

Each dataset item is a tensor of shape `[2, n_times, n_bands]` plus the anchor's integer label.

**2. Model architecture**

- A shared **encoder backbone** (default: LightTAE) maps each view to an embedding of dimension `embedding_dim`.
- An **MLP projection head** (Linear → ReLU → Linear) maps embeddings to `proj_dim`-dimensional vectors.
- Both views are **L2-normalized** after projection.

The forward pass produces a tensor of shape `[batch, 2, proj_dim]`.

**3. Cross-entropy contrastive loss**

For a batch of size B:
1. Compute the **cosine similarity matrix** S = z_A · z_B^T (shape `[B, B]`), where z_A and z_B are the L2-normalized projections of views A and B.
2. Build a **positive mask**: `mask[i,j] = 1` iff `label[i] == label[j]`.
3. Apply **temperature-scaled softmax** row-wise: p_ij = softmax(S_i / τ)_j
4. Loss per anchor: ℓ_i = -(1/|P_i|) Σ_{j ∈ P_i} log p_ij, where P_i is the set of positives for anchor i.
5. Final loss = mean over all anchors.

The softmax denominator sums over all references (positives + negatives), so each anchor is contrasted against every negative in the batch — larger batches yield a richer training signal.

**4. Training**

Uses `luz::fit` with AdamW, step-LR decay, and early stopping on validation loss.

**5. Post-training**

The projection head is **discarded**. The encoder alone is wrapped into a `sits_encoder` closure compatible with `sits_encode()`, which normalizes new data using the training statistics and produces embeddings of dimension `embedding_dim`.

---

## Contrastive Learning vs Barlow Twins: Comparison

### Shared structure

Both follow the same overall pattern: build view pairs → pass through a shared encoder + projection head → compute a loss that encourages same-class representations to be similar → discard the projection head and keep the encoder for downstream use via `sits_encode()`. They share identical train/val splitting logic, the same pair construction strategies (`"label"` and `"random"`), the same training infrastructure (luz + AdamW + step-LR + early stopping), and the same stub-wrapping post-training step.

### Key differences

| Aspect | Contrastive Learning | Barlow Twins |
|---|---|---|
| **Supervision** | **Supervised** — labels used in loss (positive mask) | **Self-supervised** — labels ignored by loss (`target` unused) |
| **Loss function** | Cross-entropy over temperature-scaled cosine similarities; contrasts each anchor against all references in the batch | Cross-correlation matrix of batch-standardized projections pushed toward the identity: diagonal → 1 (invariance), off-diagonal → 0 (redundancy reduction) |
| **Normalization in loss** | L2-normalization of projections (in forward pass) | Batch-wise standardization (mean 0, std 1) inside the loss |
| **Projection head** | Simple MLP: `Linear → ReLU → Linear` (2 layers) | Deeper MLP with batch norm: `Linear → BN → ReLU → Linear → BN → ReLU → Linear` (3 linear layers, no bias) |
| **Default `proj_dim`** | 128 | 256 |
| **Negatives** | Implicit negatives via softmax denominator — batch size matters for the quality of the contrastive signal | No negatives at all — loss is purely based on the cross-correlation structure |
| **Loss hyperparameters** | `temperature` (0.07) controls softmax sharpness | `bt_lambda` (5e-3) weights the off-diagonal penalty |
| **Dataset** | Returns `(x, y)` where `y` is the integer class label (needed for positive mask) | Returns `(x, y)` where `y` is a dummy zero tensor |

In short: the contrastive learner uses class labels *during training* to define which batch elements are positives in a softmax-based contrastive loss, while Barlow Twins is label-agnostic at the loss level, relying only on view-pair co-occurrence and a redundancy-reduction objective. Labels in Barlow Twins only influence which samples get *paired* (when `pair_smp_method = "label"`), not the loss computation itself.
