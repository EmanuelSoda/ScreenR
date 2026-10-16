# A simulated screen: 2000 genes with 4 barcodes, 10% of them depleted.
# gene_sd adds a shift common to the barcodes of a gene, so that the genes
# without effect vary more than random sets of barcodes
simulate_screen <- function(gene_sd = 0, n_hits = 200, seed = 1) {
    set.seed(seed)
    n_genes <- 2000
    hit <- seq_len(n_genes) <= n_hits
    gene_effect <- ifelse(hit, -2.5, 0) + stats::rnorm(n_genes, sd = gene_sd)
    genes <- paste0("G", seq_len(n_genes))
    data.frame(
        Gene = rep(genes, each = 4),
        Zscore = rep(gene_effect, each = 4) + stats::rnorm(4 * n_genes),
        Log2FC = 0, Hit = rep(hit, each = 4)
    )
}

null_rate <- function(result, table) {
    null_genes <- unique(table$Gene[!table$Hit])
    mean(result$PValue[result$Gene %in% null_genes] <= 0.05)
}

test_that("the empirical null is calibrated and finds the hits", {
    table <- simulate_screen()
    result <- find_gene_zscore_hit(table, fdr = 1, reference = "empirical")
    null <- attr(result, "null")
    # The barcodes of null genes are N(0, 1): their standardized statistic
    # is about N(0.16, 1) (the hits move the median of all the barcodes)
    expect_lt(abs(null[["sd"]] - 1), 0.15)
    expect_gt(null_rate(result, table), 0.02)
    expect_lt(null_rate(result, table), 0.08)
    hits <- result$Gene[result$FDR <= 0.05]
    expect_gt(mean(paste0("G", 1:200) %in% hits), 0.8)
})

test_that("the empirical null includes the variation between genes", {
    # No hits: with hits the random barcodes get a wider null, which can
    # hide the problem (as in the K562 subset)
    table <- simulate_screen(gene_sd = 0.6, n_hits = 0)
    empirical <- find_gene_zscore_hit(table, fdr = 1, reference = "empirical")
    random <- find_gene_zscore_hit(table, fdr = 1, min_barcodes = 1,
        n_permutations = 2000, seed = 1)
    # The null of the gene statistic is wider than N(0, 1): sd about
    # sqrt(1 + 4 * 0.6^2) = 1.56
    expect_gt(attr(empirical, "null")[["sd"]], 1.35)
    expect_lt(null_rate(empirical, table), 0.08)
    # Random sets of barcodes ignore it and give too small p-values
    expect_gt(null_rate(random, table), 0.1)
})

test_that("the one-sided fit and direction Up work", {
    table <- simulate_screen()
    one_sided <- find_gene_zscore_hit(table, fdr = 1, reference = "empirical",
        empirical_fit = "one-sided")
    expect_lt(abs(attr(one_sided, "null")[["sd"]] - 1), 0.15)
    expect_lt(null_rate(one_sided, table), 0.08)

    # Enriched instead of depleted genes
    up <- table
    up$Zscore <- -up$Zscore
    result <- find_gene_zscore_hit(up, fdr = 1, reference = "empirical",
        direction = "Up", empirical_fit = "one-sided")
    hits <- result$Gene[result$FDR <= 0.05]
    expect_gt(mean(paste0("G", 1:200) %in% hits), 0.8)
})

test_that("the empirical null returns the usual columns and checks inputs", {
    table <- simulate_screen()
    result <- find_gene_zscore_hit(table, reference = "empirical")
    expect_true(all(c("Gene", "NBarcodes", "Score", "Log2FC", "Statistic",
        "PValue", "FDR") %in% colnames(result)))
    expect_true(all(result$FDR <= 0.05))
    expect_false(is.unsorted(result$PValue))
    expect_error(find_gene_zscore_hit(table, reference = "empirical",
        control_genes = "G1"), "control_genes")
    expect_warning(find_gene_zscore_hit(table[table$Gene %in%
        paste0("G", 1:50), ], reference = "empirical", fdr = 1), "hundreds")
})

test_that("plot_empirical_null", {
    table <- simulate_screen()
    plot <- plot_empirical_null(table)
    expect_true(ggplot2::is_ggplot(plot))
    expect_match(plot$labels$subtitle, "Empirical null")
    expect_equal(nrow(plot$data), 2000)
})
