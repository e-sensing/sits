# R oracle of the GLCM kernel, written from the definitions; it shares no
# code with src/glcm_fns.cpp
# reflect index, as torch pads with "reflect": the edge pixel is not repeated
glcm_refl <- function(k, n) {
    k <- ifelse(k < 1L, 2L - k, k)
    ifelse(k > n, 2L * n - k, k)
}
# symmetric co-occurrence probabilities of one window for one angle
glcm_prob <- function(w, a, ng) {
    dr <- round(sin(a))
    dc <- round(cos(a))
    n <- nrow(w)
    rows <- seq(max(1L, 1L - dr), min(n, n - dr))
    cols <- seq(max(1L, 1L - dc), min(n, n - dc))
    v1 <- as.vector(w[rows, cols])
    v2 <- as.vector(w[rows + dr, cols + dc])
    lv <- seq_len(ng) - 1L
    # both orders of a pair count
    p <- unclass(table(factor(c(v1, v2), lv), factor(c(v2, v1), lv)))
    # no valid pair gives an empty GLCM, so every sum over it is 0
    if (sum(p) == 0) {
        return(p)
    }
    p / sum(p)
}
glcm_variance_r <- function(p, i) sum(p * (i - sum(p * i))^2)
glcm_metrics <- list(
    contrast = function(p, i, j) sum(p * (i - j)^2),
    dissimilarity = function(p, i, j) sum(p * abs(i - j)),
    homogeneity = function(p, i, j) sum(p / (1 + (i - j)^2)),
    energy = function(p, i, j) sqrt(sum(p^2)),
    asm = function(p, i, j) sum(p^2),
    mean = function(p, i, j) sum(p * i),
    variance = function(p, i, j) glcm_variance_r(p, i),
    std = function(p, i, j) sqrt(glcm_variance_r(p, i)),
    correlation = function(p, i, j) {
        mu <- sum(p * i)
        v <- glcm_variance_r(p, i)
        # skimage convention for a constant window
        if (v < 1e-15) 1 else sum(p * (i - mu) * (j - mu)) / v
    }
)
# each oracle returns the metric of every pixel in row-major order
glcm_oracle <- purrr::map(glcm_metrics, function(metric) {
    function(x, ws, angles, ng) {
        leg <- ws %/% 2L
        i <- matrix(seq_len(ng) - 1, ng, ng)
        j <- t(i)
        pix <- expand.grid(c = seq_len(ncol(x)), r = seq_len(nrow(x)))
        purrr::map2_dbl(pix[["r"]], pix[["c"]], function(r, c) {
            w <- x[glcm_refl(r + (-leg:leg), nrow(x)),
                   glcm_refl(c + (-leg:leg), ncol(x)), drop = FALSE]
            mean(purrr::map_dbl(angles, function(a) {
                metric(glcm_prob(w, a, ng), i, j)
            }))
        })
    }
})
test_that("Texture kernel - equals the R oracle on every metric and angle", {
    # hand-computed values at the center of a 3 x 3 window: angle 0 has
    # pairs 00 01 00 01 22 23, angle pi/2 has pairs 00 02 00 02 11 13
    w <- rbind(c(0L, 0L, 1L), c(0L, 0L, 1L), c(2L, 2L, 3L))
    expect_equal(glcm_oracle[["contrast"]](w, 3L, 0, 4L)[[5L]], 0.5)
    expect_equal(glcm_oracle[["contrast"]](w, 3L, pi / 2, 4L)[[5L]], 2)
    expect_equal(glcm_oracle[["contrast"]](w, 3L, c(0, pi / 2), 4L)[[5L]], 1.25)
    expect_equal(glcm_oracle[["asm"]](w, 3L, 0, 4L)[[5L]], 30 / 144)
    set.seed(42)
    ng <- 8L
    inputs <- list(
        list(x = matrix(sample(0:(ng - 1L), 42L, TRUE), 6L, 7L), ws = 3L),
        list(x = matrix(sample(0:(ng - 1L), 42L, TRUE), 6L, 7L), ws = 5L),
        # levels ng and ng + 1 are out of range, so their pairs are dropped;
        # one window has no valid pair at angle 0
        list(x = matrix(sample(0:(ng + 1L), 42L, TRUE), 6L, 7L), ws = 3L),
        # 4 rows is leg + 1, the smallest block the reflect mirror accepts
        list(x = matrix(sample(0:(ng - 1L), 20L, TRUE), 4L, 5L), ws = 7L)
    )
    angle_sets <- list(0, pi / 4, pi / 2, 3 * pi / 4,
                       c(0, pi / 4, pi / 2, 3 * pi / 4))
    cases <- expand.grid(
        k = seq_along(inputs), a = seq_along(angle_sets),
        m = names(glcm_oracle), stringsAsFactors = FALSE
    )
    c_glcm <- function(m, x, a, ws) {
        as.vector(get(paste0("C_glcm_", m), asNamespace("sits"))(
            as.numeric(t(x)), a, nrow(x), ncol(x), ng, ws
        ))
    }
    got <- purrr::pmap(cases, function(k, a, m) {
        c_glcm(m, inputs[[k]][["x"]], angle_sets[[a]], inputs[[k]][["ws"]])
    })
    expected <- purrr::pmap(cases, function(k, a, m) {
        glcm_oracle[[m]](inputs[[k]][["x"]], inputs[[k]][["ws"]],
                         angle_sets[[a]], ng)
    })
    # one expectation, so a failure names every case that differs
    names(got) <- paste(cases[["m"]], "input", cases[["k"]], "angles",
                        cases[["a"]])
    names(expected) <- names(got)
    expect_equal(got, expected, tolerance = 1e-12)
})
