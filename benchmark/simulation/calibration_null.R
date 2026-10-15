# Calibration of the ScreenR p-values on null screens (no true hits).
# Usage (from benchmark/simulation): Rscript calibration_null.R <ScreenR lib>
# A calibrated method has about 5%, 10% and 50% of p-values below 0.05,
# 0.1 and 0.5.
suppressMessages({library(ScreenR, lib.loc = commandArgs(TRUE)[1]); library(dplyr)})
source("simulate.R")
res <- list()
for (seed in 1:3) {
  sim <- simulate_screen(n_genes = 500, hit_fraction = 0, seed = seed)
  counts <- sim$counts; colnames(counts) <- c("Barcode", sub("^(CTRL|TRT)_([0-9]+)$", "Day1_\\1_\\2", sim$samples))
  o <- suppressMessages(create_screenr_object(table = counts, annotation = sim$annotation, groups = factor(sub("_[0-9]+$", "", sim$samples))))
  o <- compute_data_table(normalize_data(o)); d <- create_design_matrix(o); ct <- create_contrast(d, "TRT", "CTRL")
  m <- compute_metrics(o, control = "CTRL", treatment = "TRT", day = "Day1")
  st <- compute_barcode_statistics(o, d, ct)
  p <- list(
    CAMERA = suppressWarnings(find_camera_hit(o, d, ct, fdr = 1, number_barcode = 1)),
    ROAST = suppressWarnings(find_roast_hit(o, d, ct, fdr = 1, number_barcode = 1, seed = 1)),
    FRY = suppressWarnings(find_roast_hit(o, d, ct, fdr = 1, number_barcode = 1, method = "fry")),
    Z_mean = find_gene_zscore_hit(m, fdr = 1, seed = 1, n_permutations = 2e4),
    Z_median = find_gene_zscore_hit(m, fdr = 1, seed = 1, statistic = "median", n_permutations = 2e4),
    Zedger_mean = find_gene_zscore_hit(st, fdr = 1, seed = 1, n_permutations = 2e4),
    Zedger_median = find_gene_zscore_hit(st, fdr = 1, seed = 1, statistic = "median", n_permutations = 2e4))
  for (k in names(p)) {
    pv <- p[[k]]$PValue
    # camera/roast/fry return only the Down genes: the Up genes have p > 0.5
    n_missing <- length(unique(sim$annotation$Gene)) - length(pv); pv <- c(pv, rep(0.75, n_missing))
    res[[length(res)+1]] <- data.frame(method = k, p05 = mean(pv < 0.05), p10 = mean(pv < 0.1), p50 = mean(pv < 0.5))
  }
}
print(bind_rows(res) %>% group_by(method) %>% summarise(across(everything(), ~ round(mean(.x), 3))), n = 20)
