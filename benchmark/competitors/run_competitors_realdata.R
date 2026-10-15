# BAGEL2, gscreend and the plain edgeR/limma workflows on the real screens:
# - K562 essentiality screen (k562_count_table), Plasmid vs T14, evaluated on
#   the core essential / non-essential reference genes like the other methods
#   (realdata/k562_screenr_methods.R);
# - ScreenR example dataset (count_table), Time4 treated vs control, rank
#   agreement with the other methods (no ground truth; BAGEL2 is not
#   applicable: it needs reference essential genes).
# Usage (from benchmark/competitors/):
#   Rscript run_competitors_realdata.R <ScreenR repo> <BAGEL.py> <python>
args <- commandArgs(TRUE)
suppressMessages(library(dplyr))
source("competitor_methods.R")
repo <- args[1]
bagel <- normalizePath(args[2])
python <- args[3]
dir.create("realdata_work", showWarnings = FALSE)

# ------------------------------------------------------------------ K562 ---
for (f in c("k562_count_table", "k562_annotation_table",
    "k562_reference_genes")) {
    load(file.path(repo, "data", paste0(f, ".rda")))
}
counts <- k562_count_table
gene <- k562_annotation_table$Gene[match(counts$Barcode,
    k562_annotation_table$Barcode)]
ctrl <- "Plasmid"
trt <- c("T14_A", "T14_B")
k562 <- list(
    edgeR_gene_QL = run_edger_gene(counts, gene, ctrl, trt),
    limma_voom_camera = run_voom_camera(counts, gene, ctrl, trt),
    gscreend = run_gscreend(counts, gene, ctrl, trt),
    # Trained on the same reference genes used for the evaluation, scored by
    # 10-fold cross-validation (the standard BAGEL2 use)
    BAGEL2 = run_bagel2(counts, gene, ctrl, trt,
        essentials = k562_reference_genes$Gene[
            k562_reference_genes$Class == "core essential"],
        nonessentials = k562_reference_genes$Gene[
            k562_reference_genes$Class == "non-essential"],
        workdir = "realdata_work/k562_bagel", bagel = bagel,
        python = python)
)

# Same evaluation as realdata/k562_screenr_methods.R
evaluate_reference <- function(r, ref = k562_reference_genes) {
    m <- merge(ref, r, by = "Gene", all.x = TRUE)
    m$PValue[is.na(m$PValue)] <- 1
    m$FDR[is.na(m$FDR)] <- 1
    ess <- m$Class == "core essential"
    out <- do.call(rbind, lapply(c(0.05, 0.1), function(a) {
        cl <- m$FDR <= a
        data.frame(alpha = a, essential_called = sum(cl & ess),
            nonessential_called = sum(cl & !ess), recall = mean(cl[ess]),
            false_frac = if (any(cl)) mean(!ess[cl]) else NA)
    }))
    hit <- ess[order(m$PValue)]
    prec <- cumsum(hit) / seq_along(hit)
    out$AUPRC <- sum(prec[hit]) / sum(hit)
    out$time <- r$time[1]
    out
}
k562_eval <- bind_rows(lapply(names(k562), function(n) {
    cbind(method = n, evaluate_reference(k562[[n]]))
}))

# ------------------------------------------------------ ScreenR dataset ---
load(file.path(repo, "data", "count_table.rda"))
load(file.path(repo, "data", "annotation_table.rda"))
counts <- count_table
colnames(counts) <- c("Barcode", "Time1", "Time2", "Time3_TRT_A",
    "Time3_TRT_B", "Time3_TRT_C", "Time3_CTRL_A", "Time3_CTRL_B",
    "Time3_CTRL_C", "Time4_TRT_A", "Time4_TRT_B", "Time4_TRT_C",
    "Time4_CTRL_A", "Time4_CTRL_B", "Time4_CTRL_C")
counts <- counts[counts$Barcode != "*", ]
gene <- as.character(annotation_table$Gene[match(
    as.character(counts$Barcode), as.character(annotation_table$Barcode))])
ctrl <- c("Time4_CTRL_A", "Time4_CTRL_B", "Time4_CTRL_C")
trt <- c("Time4_TRT_A", "Time4_TRT_B", "Time4_TRT_C")
keep <- rowSums(counts[, c(ctrl, trt)]) > 0
screenr_data <- list(
    edgeR_gene_QL = run_edger_gene(counts[keep, ], gene[keep], ctrl, trt),
    limma_voom_camera = run_voom_camera(counts[keep, ], gene[keep], ctrl,
        trt),
    gscreend = run_gscreend(counts[keep, ], gene[keep], ctrl, trt)
)

saveRDS(list(k562 = k562, k562_eval = k562_eval,
    screenr_data = screenr_data), "realdata_competitors.rds")
options(width = 200)
print(k562_eval %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
    row.names = FALSE)
print(vapply(screenr_data, function(r) sum(r$FDR <= 0.05), integer(1)))
