# Every hit-finding method must find enriched genes with direction = "Up"
# and depleted genes with direction = "Down", without mixing them.

simulate_two_directions <- function() {
    old <- if (exists(".Random.seed", envir = globalenv())) {
        get(".Random.seed", envir = globalenv())
    }
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
    set.seed(42)
    genes <- sprintf("Gene_%03d", seq_len(200))
    gene <- rep(genes, each = 8)
    n <- length(gene)
    barcode <- sprintf("bc_%04d", seq_len(n))
    up <- genes[1:12]
    down <- genes[13:24]
    effect <- 1.5 * stats::rbeta(n, 4, 2)
    log2fc <- ifelse(gene %in% up, effect, ifelse(gene %in% down, -effect, 0))
    abundance <- stats::rlnorm(n, 0, 0.5)
    samples <- c(paste0("Day1_CTRL_", 1:3), paste0("Day1_TRT_", 1:3))
    counts <- vapply(samples, function(s) {
        mu <- abundance * 2^(grepl("TRT", s) * log2fc)
        stats::rnbinom(n, mu = mu / sum(mu) * 500 * n, size = 50)
    }, numeric(n))
    counts <- data.frame(Barcode = barcode, counts, check.names = FALSE)
    object <- suppressMessages(create_screenr_object(
        table = counts, annotation = data.frame(Barcode = barcode, Gene = gene),
        groups = factor(rep(c("CTRL", "TRT"), each = 3))
    ))
    object <- compute_data_table(normalize_data(object))
    list(object = object, up = up, down = down)
}

sim <- simulate_two_directions()
design <- create_design_matrix(sim$object)
contrast <- create_contrast(design, treatment = "TRT", control = "CTRL")
metrics <- compute_metrics(sim$object,
    control = "CTRL", treatment = "TRT", day = "Day1"
)

expect_direction <- function(hits, truth, wrong, precision = 0.8) {
    hits <- as.character(hits)
    expect_gt(length(hits), 0)
    expect_gte(mean(hits %in% truth), precision)
    expect_false(any(hits %in% wrong))
}

test_that("find_gene_zscore_hit finds enriched and depleted genes", {
    up <- find_gene_zscore_hit(metrics, direction = "Up", seed = 1,
        n_permutations = 5000)
    down <- find_gene_zscore_hit(metrics, direction = "Down", seed = 1,
        n_permutations = 5000)
    expect_direction(up$Gene, sim$up, sim$down)
    expect_direction(down$Gene, sim$down, sim$up)
})

test_that("find_camera_hit finds enriched and depleted genes", {
    up <- suppressWarnings(find_camera_hit(sim$object, design, contrast,
        direction = "Up"))
    down <- suppressWarnings(find_camera_hit(sim$object, design, contrast,
        direction = "Down"))
    expect_direction(up$Gene, sim$up, sim$down)
    expect_direction(down$Gene, sim$down, sim$up)
    expect_true(all(up$Direction == "Up"))
})

test_that("find_roast_hit (fry) finds enriched and depleted genes", {
    up <- suppressWarnings(find_roast_hit(sim$object, design, contrast,
        direction = "Up", method = "fry"))
    down <- suppressWarnings(find_roast_hit(sim$object, design, contrast,
        direction = "Down", method = "fry"))
    # FRY is self-contained: it also reports some noisy genes without effect
    expect_direction(up$Gene, sim$up, sim$down, precision = 0.6)
    expect_direction(down$Gene, sim$down, sim$up, precision = 0.6)
})

test_that("the legacy rules accept direction = 'Up'", {
    down <- find_zscore_hit(metrics)
    expect_identical(down, find_zscore_hit(metrics, direction = "Down"))
    up <- find_zscore_hit(metrics, direction = "Up")
    expect_gt(sum(up$Gene %in% sim$up), 0)
    expect_false(any(up$Gene %in% sim$down))
    up_robust <- find_robust_zscore_hit(metrics, direction = "Up")
    expect_false(any(up_robust$Gene %in% sim$down))
})

test_that("an invalid direction gives an error", {
    expect_error(find_camera_hit(sim$object, design, contrast,
        direction = "up"), "should be one of")
    expect_error(find_roast_hit(sim$object, design, contrast,
        direction = "enriched"), "should be one of")
    expect_error(find_gene_zscore_hit(metrics, direction = "up"),
        "should be one of")
    expect_error(find_zscore_hit(metrics, direction = "up"),
        "should be one of")
})
