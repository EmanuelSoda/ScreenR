# ScreenR methods on the K562 shRNA screen, compared with MAGeCK RRA on the
# core essential / non-essential reference genes.
# Usage: Rscript k562_screenr_methods.R <ScreenR lib> <ScreenR repo> \
#            <MAGeCK RRA gene_summary.txt of the K562 screen>
# The MAGeCK output is produced by run_realdata.R.
suppressMessages({library(ScreenR, lib.loc = commandArgs(TRUE)[1]); library(dplyr)})
repo <- commandArgs(TRUE)[2]
for (f in c("k562_count_table", "k562_annotation_table", "k562_reference_genes"))
    load(file.path(repo, "data", paste0(f, ".rda")))
rra <- read.delim(commandArgs(TRUE)[3], check.names = FALSE)
res <- list(MAGeCK_RRA = data.frame(Gene = rra$id, PValue = rra[["neg|p-value"]], FDR = rra[["neg|fdr"]], time = NA))
samples <- data.frame(Sample = c("Plasmid", "T14_A", "T14_B"), Day = "Screen", Treatment = c("Plasmid", "T14", "T14"))
t0 <- system.time({
obj <- suppressMessages(create_screenr_object(table = k562_count_table, annotation = k562_annotation_table,
    groups = factor(c("Plasmid", "T14", "T14")), sample_table = samples))
obj <- compute_data_table(normalize_data(obj))})[["elapsed"]]
design <- create_design_matrix(obj); contrast <- create_contrast(design, "T14", "Plasmid")
metrics <- compute_metrics(obj, control = "Plasmid", treatment = "T14", day = "Screen")
st <- compute_barcode_statistics(obj, design, contrast)
tm <- function(name, expr) { t <- system.time(x <- suppressWarnings(expr))[["elapsed"]]
    res[[name]] <<- data.frame(Gene = x$Gene, PValue = x$PValue, FDR = x$FDR, time = t0 + t) }
tm("CAMERA", find_camera_hit(obj, design, contrast, fdr = 1, number_barcode = 1))
tm("CAMERA_storey", find_camera_hit(obj, design, contrast, fdr = 1, number_barcode = 1, fdr_method = "storey"))
tm("FRY", find_roast_hit(obj, design, contrast, method = "fry", fdr = 1, number_barcode = 1))
tm("ROAST", find_roast_hit(obj, design, contrast, fdr = 1, number_barcode = 1, seed = 1))
tm("Zscore_mean", find_gene_zscore_hit(metrics, fdr = 1, min_barcodes = 1, seed = 1, n_permutations = 2e4))
tm("Zscore_median", find_gene_zscore_hit(metrics, statistic = "median", fdr = 1, min_barcodes = 1, seed = 1, n_permutations = 2e4))
tm("Zscore_controls", find_gene_zscore_hit(metrics, control_genes = "non-targeting", fdr = 1, min_barcodes = 1, seed = 1, n_permutations = 2e4))
tm("Zscore_edgeR", find_gene_zscore_hit(st, fdr = 1, min_barcodes = 1, seed = 1, n_permutations = 2e4))
leg <- find_zscore_hit(metrics, number_barcode = 6)
res$legacy_rule <- data.frame(Gene = unique(leg$Gene), PValue = NA, FDR = 0, time = NA)
ev <- function(r, ref = k562_reference_genes) {
    m <- merge(ref, r, by = "Gene", all.x = TRUE); m$PValue[is.na(m$PValue)] <- 1; m$FDR[is.na(m$FDR)] <- 1
    ess <- m$Class == "core essential"
    out <- lapply(c(0.05, 0.1), function(a) { cl <- m$FDR <= a
        data.frame(alpha = a, essential_called = sum(cl & ess), nonessential_called = sum(cl & !ess),
            recall = mean(cl[ess]), false_frac = if (any(cl)) mean(!ess[cl]) else NA) })
    out <- do.call(rbind, out)
    hit <- ess[order(m$PValue)]; prec <- cumsum(hit) / seq_along(hit)
    out$AUPRC <- if (all(is.na(r$PValue))) NA else sum(prec[hit]) / sum(hit)
    out$time <- r$time[1]; out }
e <- bind_rows(lapply(names(res), function(n) cbind(method = n, ev(res[[n]]))))
saveRDS(list(res = res, eval = e), "k562_now.rds")
options(width = 200); print(mutate(e, across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
