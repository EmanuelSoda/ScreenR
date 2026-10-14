# Real-data comparison of ScreenR and MAGeCK.
# Usage: Rscript run_realdata.R <ScreenR lib> <mageck executable> <repo dir>
args <- commandArgs(TRUE)
suppressMessages({
    library(ScreenR, lib.loc = args[1])
    library(dplyr)
})
mageck <- args[2]
repo <- args[3]
Sys.setenv(PATH = paste(dirname(mageck), Sys.getenv("PATH"), sep = ":"))
dir.create("realdata_work", showWarnings = FALSE)

run_mageck_table <- function(counts, gene, ctrl, trt, prefix, mle = TRUE) {
    count_file <- paste0(prefix, "_counts.txt")
    write.table(data.frame(sgRNA = counts$Barcode, Gene = gene,
        counts[, c(ctrl, trt)], check.names = FALSE), count_file, sep = "\t",
        quote = FALSE, row.names = FALSE)
    out <- list()
    t_rra <- system.time(system2(mageck, c("test", "-k", count_file,
        "-t", paste(trt, collapse = ","), "-c", paste(ctrl, collapse = ","),
        "-n", paste0(prefix, "_rra")), stdout = FALSE,
        stderr = FALSE))[["elapsed"]]
    rra <- read.delim(paste0(prefix, "_rra.gene_summary.txt"),
        check.names = FALSE)
    out$MAGeCK_RRA <- data.frame(Gene = rra$id, PValue = rra[["neg|p-value"]],
        FDR = rra[["neg|fdr"]], time = t_rra)
    if (mle) {
        design_file <- paste0(prefix, "_design.txt")
        write.table(data.frame(Samples = c(ctrl, trt), baseline = 1,
            treatment = rep(0:1, c(length(ctrl), length(trt)))), design_file,
            sep = "\t", quote = FALSE, row.names = FALSE)
        t_mle <- system.time(system2(mageck, c("mle", "-k", count_file,
            "-d", design_file, "-n", paste0(prefix, "_mle"), "--threads",
            "3"), stdout = FALSE, stderr = FALSE))[["elapsed"]]
        mle <- read.delim(paste0(prefix, "_mle.gene_summary.txt"),
            check.names = FALSE)
        p <- pnorm(mle[["treatment|z"]])
        out$MAGeCK_MLE <- data.frame(Gene = mle$Gene, PValue = p,
            FDR = p.adjust(p, "BH"), time = t_mle)
    }
    out
}

run_screenr_object <- function(object, treatment_group, control_group,
    treatment, control, day) {
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, treatment_group, control_group)
    out <- list()
    t <- system.time(cam <- suppressWarnings(find_camera_hit(object, design,
        contrast, fdr = 1, number_barcode = 1)))[["elapsed"]]
    out$ScreenR_CAMERA <- data.frame(Gene = cam$Gene, PValue = cam$PValue,
        FDR = cam$FDR, time = t)
    t <- system.time({
        st <- compute_barcode_statistics(object, design, contrast)
        ze <- find_gene_zscore_hit(st, fdr = 1, min_barcodes = 1)
    })[["elapsed"]]
    out$ScreenR_Zscore_edgeR <- data.frame(Gene = ze$Gene, PValue = ze$PValue,
        FDR = ze$FDR, time = t)
    t <- system.time({
        m <- compute_metrics(object, control = control, treatment = treatment,
            day = day)
        z <- find_gene_zscore_hit(m, fdr = 1, min_barcodes = 1)
    })[["elapsed"]]
    out$ScreenR_Zscore <- data.frame(Gene = z$Gene, PValue = z$PValue,
        FDR = z$FDR, time = t)
    t <- system.time(ro <- suppressWarnings(find_roast_hit(object, design,
        contrast, fdr = 1, seed = 1, number_barcode = 1)))[["elapsed"]]
    out$ScreenR_ROAST <- data.frame(Gene = ro$Gene, PValue = ro$PValue,
        FDR = ro$FDR, time = t)
    legacy <- find_zscore_hit(m, number_barcode = 6)
    out$ScreenR_legacy_rule <- data.frame(Gene = legacy$Gene, PValue = NA,
        FDR = 0, time = NA)
    out
}

# ---------------------------------------------------------------- K562 ---
load(file.path(repo, "data/k562_count_table.rda"))
load(file.path(repo, "data/k562_annotation_table.rda"))
load(file.path(repo, "data/k562_reference_genes.rda"))
k562_mageck <- run_mageck_table(k562_count_table,
    k562_annotation_table$Gene, "Plasmid", c("T14_A", "T14_B"),
    "realdata_work/k562")
samples <- data.frame(Sample = c("Plasmid", "T14_A", "T14_B"),
    Day = "Screen", Treatment = c("Plasmid", "T14", "T14"))
object <- suppressMessages(create_screenr_object(table = k562_count_table,
    annotation = k562_annotation_table,
    groups = factor(c("Plasmid", "T14", "T14")), sample_table = samples))
object <- compute_data_table(normalize_data(object))
k562_screenr <- run_screenr_object(object, "T14", "Plasmid",
    treatment = "T14", control = "Plasmid", day = "Screen")
k562 <- c(k562_mageck, k562_screenr)

evaluate_reference <- function(result, reference, alpha = 0.05) {
    merged <- merge(reference, result, by = "Gene", all.x = TRUE)
    merged$PValue[is.na(merged$PValue)] <- 1
    merged$FDR[is.na(merged$FDR)] <- 1
    essential <- merged$Class == "core essential"
    called <- merged$FDR <= alpha
    ord <- order(merged$PValue)
    hit <- essential[ord]
    precision <- cumsum(hit) / seq_along(hit)
    data.frame(
        essential_called = sum(called & essential),
        nonessential_called = sum(called & !essential),
        recall = mean(called[essential]),
        precision = if (sum(called) == 0) NA else mean(essential[called]),
        AUPRC = if (all(is.na(result$PValue))) NA else
            sum(precision[hit]) / sum(hit),
        time = result$time[1]
    )
}
k562_eval <- bind_rows(lapply(names(k562), function(m) {
    cbind(method = m, evaluate_reference(k562[[m]], k562_reference_genes))
}))

# ------------------------------------------------------ ScreenR dataset ---
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
screenr_mageck <- run_mageck_table(counts, gene,
    c("Time4_CTRL_A", "Time4_CTRL_B", "Time4_CTRL_C"),
    c("Time4_TRT_A", "Time4_TRT_B", "Time4_TRT_C"), "realdata_work/screenr")
groups <- factor(c("T1/T2", "T1/T2", rep("Time3_TRT", 3),
    rep("Time3_CTRL", 3), rep("Time4_TRT", 3), rep("Time4_CTRL", 3)))
object <- suppressMessages(create_screenr_object(table = counts,
    annotation = annotation_table, groups = groups))
object <- compute_data_table(normalize_data(remove_all_zero_row(object)))
screenr_screenr <- run_screenr_object(object, "Time4_TRT", "Time4_CTRL",
    treatment = "TRT", control = "CTRL", day = "Time4")
screenr_data <- c(screenr_mageck, screenr_screenr)

saveRDS(list(k562 = k562, k562_eval = k562_eval,
    screenr_data = screenr_data), "realdata_results.rds")
options(width = 200)
cat("\n=== K562: core essential vs non-essential (FDR <= 0.05) ===\n")
print(k562_eval %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
    row.names = FALSE)
