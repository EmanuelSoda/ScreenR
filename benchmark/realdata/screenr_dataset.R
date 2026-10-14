# ScreenR example dataset (count_table): Time4 treated vs control.
# There is no ground truth: the script measures the agreement between
# MAGeCK and the ScreenR methods (overlap of the hits at 5% FDR and rank
# correlation of the p-values).
# Usage: Rscript screenr_dataset.R <ScreenR lib> <path/to/mageck> <out.rds>
args <- commandArgs(TRUE)
suppressMessages({
    library(ScreenR, lib.loc = args[1])
    library(dplyr)
})
mageck <- args[2]
Sys.setenv(PATH = paste(dirname(mageck), Sys.getenv("PATH"), sep = ":"))
dir.create("screenr_work", showWarnings = FALSE)

data(count_table, package = "ScreenR")
data(annotation_table, package = "ScreenR")
counts <- count_table
colnames(counts) <- c("Barcode", "Time1", "Time2", "Time3_TRT_A",
    "Time3_TRT_B", "Time3_TRT_C", "Time3_CTRL_A", "Time3_CTRL_B",
    "Time3_CTRL_C", "Time4_TRT_A", "Time4_TRT_B", "Time4_TRT_C",
    "Time4_CTRL_A", "Time4_CTRL_B", "Time4_CTRL_C")
counts <- filter(counts, Barcode != "*")
gene <- annotation_table$Gene[match(as.character(counts$Barcode),
    as.character(annotation_table$Barcode))]
ctrl <- c("Time4_CTRL_A", "Time4_CTRL_B", "Time4_CTRL_C")
trt <- c("Time4_TRT_A", "Time4_TRT_B", "Time4_TRT_C")
count_file <- "screenr_work/counts.txt"
write.table(data.frame(sgRNA = counts$Barcode, Gene = gene,
    counts[, c(ctrl, trt)], check.names = FALSE), count_file, sep = "\t",
    quote = FALSE, row.names = FALSE)

res <- list()
time <- system.time(system2(mageck, c("test", "-k", count_file, "-t",
    paste(trt, collapse = ","), "-c", paste(ctrl, collapse = ","), "-n",
    "screenr_work/rra"), stdout = FALSE, stderr = FALSE))[["elapsed"]]
x <- read.delim("screenr_work/rra.gene_summary.txt", check.names = FALSE)
res$MAGeCK_RRA <- data.frame(Gene = x$id, PValue = x[["neg|p-value"]],
    FDR = x[["neg|fdr"]], time = time)
write.table(data.frame(Samples = c(ctrl, trt), baseline = 1,
    treatment = rep(0:1, each = 3)), "screenr_work/design.txt", sep = "\t",
    quote = FALSE, row.names = FALSE)
time <- system.time(system2(mageck, c("mle", "-k", count_file, "-d",
    "screenr_work/design.txt", "-n", "screenr_work/mle", "--threads", "1",
    "--no-permutation-by-group", "--permutation-round", "1"),
    stdout = FALSE, stderr = FALSE))[["elapsed"]]
x <- read.delim("screenr_work/mle.gene_summary.txt", check.names = FALSE)
p <- pnorm(x[["treatment|z"]])
res$MAGeCK_MLE <- data.frame(Gene = x$Gene, PValue = p,
    FDR = p.adjust(p, "BH"), time = time)

groups <- factor(c("T1/T2", "T1/T2", rep("Time3_TRT", 3),
    rep("Time3_CTRL", 3), rep("Time4_TRT", 3), rep("Time4_CTRL", 3)))
t0 <- system.time({
    object <- suppressMessages(create_screenr_object(table = counts,
        annotation = annotation_table, groups = groups))
    object <- compute_data_table(normalize_data(remove_all_zero_row(object)))
})[["elapsed"]]
design <- create_design_matrix(object)
contrast <- create_contrast(design, "Time4_TRT", "Time4_CTRL")
metrics <- compute_metrics(object, control = "CTRL", treatment = "TRT",
    day = "Time4")
statistics <- compute_barcode_statistics(object, design, contrast)
timed <- function(name, expr) {
    time <- system.time(x <- suppressWarnings(expr))[["elapsed"]]
    res[[name]] <<- data.frame(Gene = x$Gene, PValue = x$PValue,
        FDR = x$FDR, time = t0 + time)
}
timed("CAMERA", find_camera_hit(object, design, contrast, fdr = 1,
    number_barcode = 1))
timed("ROAST", find_roast_hit(object, design, contrast, fdr = 1,
    number_barcode = 1, seed = 1))
timed("FRY", find_roast_hit(object, design, contrast, method = "fry",
    fdr = 1, number_barcode = 1))
timed("Zscore_mean", find_gene_zscore_hit(metrics, fdr = 1,
    min_barcodes = 1, seed = 1))
timed("Zscore_edgeR", find_gene_zscore_hit(statistics, fdr = 1,
    min_barcodes = 1, seed = 1))
legacy <- find_zscore_hit(metrics, number_barcode = 6)
legacy_hits <- unique(as.character(legacy$Gene))

genes <- Reduce(intersect, lapply(res, function(r) as.character(r$Gene)))
hits <- c(lapply(res, function(r) as.character(r$Gene[r$FDR <= 0.05])),
    list(legacy_rule = legacy_hits))
ranks <- sapply(res, function(r) {
    r <- r[match(genes, r$Gene), ]
    rank(r$PValue)
})
summary <- data.frame(method = names(hits),
    n_hits = lengths(hits),
    overlap_with_MAGeCK_RRA = vapply(hits, function(h)
        length(intersect(h, hits$MAGeCK_RRA)), integer(1)),
    overlap_with_MAGeCK_MLE = vapply(hits, function(h)
        length(intersect(h, hits$MAGeCK_MLE)), integer(1)))
correlation <- cor(ranks, method = "spearman")
saveRDS(list(res = res, hits = hits, summary = summary,
    correlation = correlation, n_genes = length(genes)), args[3])
options(width = 150)
print(summary, row.names = FALSE)
print(round(correlation, 2))
