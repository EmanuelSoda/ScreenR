# Feasibility test of ScreenR on a CRISPR-Cas9 screen: HAP1 cells, TKOv3
# library (Hart et al. 2017, example data of the BAGEL2 repository, MIT),
# T18 vs T0, evaluated on CEGv2 / NEGv1, with MAGeCK RRA, BAGEL2, gscreend
# and the edgeR/limma workflows.
# Usage (from benchmark/crispr/): Rscript hap1_crispr.R <dir>, where <dir>
# contains libdir/ (ScreenR), competitors/bagel/ (BAGEL2 clone),
# bagel_env/ and mageck_env/ (python environments)
args <- commandArgs(TRUE); S <- args[1]
suppressMessages({library(ScreenR, lib.loc = file.path(S, "libdir")); library(dplyr)})
source("../competitors/competitor_methods.R")
raw <- read.delim(file.path(S, "competitors/bagel/pipeline-script-example/HAP1-TKOv3-EXAMPLE.txt"), check.names = FALSE)
ess <- readLines(file.path(S, "competitors/bagel/CEGv2.txt")); ess <- sub("\t.*", "", ess[-1])
non <- readLines(file.path(S, "competitors/bagel/NEGv1.txt")); non <- sub("\t.*", "", non[-1])
ref <- rbind(data.frame(Gene = ess, Class = "core essential"), data.frame(Gene = non, Class = "non-essential"))
ref <- ref[ref$Gene %in% raw$GENE, ]
cat("guides:", nrow(raw), " genes:", length(unique(raw$GENE)), " guides/gene:", median(table(raw$GENE)), "\n")
cat("reference genes in library:", table(ref$Class), "\n")
counts <- raw[, c("GENE_CLONE", "T0_-", "T18_A", "T18_B", "T18_C")]
colnames(counts) <- c("Barcode", "T0", "T18_A", "T18_B", "T18_C")
annotation <- data.frame(Barcode = raw$GENE_CLONE, Gene = raw$GENE)
samples <- data.frame(Sample = c("T0", "T18_A", "T18_B", "T18_C"), Day = "Screen", Treatment = c("T0", "T18", "T18", "T18"))
res <- list(); timing <- list()
t0 <- system.time({
obj <- create_screenr_object(table = counts, annotation = annotation,
    groups = factor(c("T0", "T18", "T18", "T18")), sample_table = samples)
obj <- compute_data_table(normalize_data(obj))})[["elapsed"]]
design <- create_design_matrix(obj); contrast <- create_contrast(design, "T18", "T0")
metrics <- compute_metrics(obj, control = "T0", treatment = "T18", day = "Screen")
stats <- compute_barcode_statistics(obj, design, contrast)
tm <- function(name, expr) { t <- system.time(x <- suppressWarnings(expr))[["elapsed"]]
    res[[name]] <<- data.frame(Gene = x$Gene, PValue = x$PValue, FDR = x$FDR, time = t0 + t) }
tm("ScreenR_Zscore_mean", find_gene_zscore_hit(metrics, fdr = 1, min_barcodes = 1, seed = 1, n_permutations = 2e4))
tm("ScreenR_Zscore_edgeR", find_gene_zscore_hit(stats, fdr = 1, min_barcodes = 1, seed = 1, n_permutations = 2e4))
tm("ScreenR_CAMERA", find_camera_hit(obj, design, contrast, fdr = 1, number_barcode = 1))
tm("ScreenR_FRY", find_roast_hit(obj, design, contrast, method = "fry", fdr = 1, number_barcode = 1))
ctrl <- "T0"; trt <- c("T18_A", "T18_B", "T18_C"); g <- annotation$Gene
res$edgeR_gene_QL <- run_edger_gene(counts, g, ctrl, trt)
res$limma_voom_camera <- run_voom_camera(counts, g, ctrl, trt)
res$gscreend <- run_gscreend(counts, g, ctrl, trt)
res$BAGEL2 <- run_bagel2(counts, g, ctrl, trt, ess[ess %in% g], non[non %in% g], "bagel_work",
    file.path(S, "competitors/bagel/BAGEL.py"), file.path(S, "bagel_env/bin/python"))
# MAGeCK RRA
mageck <- file.path(S, "mageck_env/bin/mageck"); Sys.setenv(PATH = paste(dirname(mageck), Sys.getenv("PATH"), sep = ":"))
write.table(data.frame(sgRNA = counts$Barcode, Gene = g, counts[, c(ctrl, trt)], check.names = FALSE), "hap1_counts.txt", sep = "\t", quote = FALSE, row.names = FALSE)
t <- system.time(system2(mageck, c("test", "-k", "hap1_counts.txt", "-t", paste(trt, collapse = ","), "-c", ctrl, "-n", "hap1_rra"), stdout = FALSE, stderr = FALSE))[["elapsed"]]
x <- read.delim("hap1_rra.gene_summary.txt", check.names = FALSE)
res$MAGeCK_RRA <- data.frame(Gene = x$id, PValue = x[["neg|p-value"]], FDR = x[["neg|fdr"]], time = t)
ev <- function(r) {
    m <- merge(ref, r, by = "Gene", all.x = TRUE); m$PValue[is.na(m$PValue)] <- 1; m$FDR[is.na(m$FDR)] <- 1
    e <- m$Class == "core essential"; cl <- m$FDR <= 0.05
    hit <- e[order(m$PValue)]; prec <- cumsum(hit) / seq_along(hit)
    data.frame(essential_called = sum(cl & e), nonessential_called = sum(cl & !e), recall = mean(cl[e]),
        AUPRC = sum(prec[hit]) / sum(hit), time = r$time[1])
}
out <- bind_rows(lapply(res, ev), .id = "method") %>% arrange(desc(AUPRC))
saveRDS(list(res = res, eval = out, reference = ref), "hap1_crispr.rds")
options(width = 160); print(mutate(out, across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
