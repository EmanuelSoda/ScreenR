# Run MAGeCK and the ScreenR methods on a simulated screen; one row per gene
# with the p-value and FDR of depletion.
run_mageck <- function(sim, workdir, mageck, mle = TRUE) {
    Sys.setenv(PATH = paste(dirname(mageck), Sys.getenv("PATH"), sep = ":"))
    dir.create(workdir, showWarnings = FALSE, recursive = TRUE)
    count_file <- file.path(workdir, "counts.txt")
    write.table(data.frame(sgRNA = sim$counts$Barcode,
        Gene = sim$annotation$Gene, sim$counts[, sim$samples],
        check.names = FALSE), count_file, sep = "\t", quote = FALSE,
        row.names = FALSE)
    control_file <- file.path(workdir, "controls.txt")
    writeLines(sim$annotation$Barcode[sim$annotation$Gene == "non-targeting"],
        control_file)
    ctrl <- grep("^CTRL", sim$samples, value = TRUE)
    trt <- grep("^TRT", sim$samples, value = TRUE)
    results <- list()
    rra <- function(prefix, extra = character()) {
        time <- system.time(system2(mageck, c("test", "-k", count_file,
            "-t", paste(trt, collapse = ","), "-c", paste(ctrl, collapse = ","),
            "-n", prefix, extra), stdout = FALSE, stderr = FALSE))[["elapsed"]]
        x <- read.delim(paste0(prefix, ".gene_summary.txt"), check.names = FALSE)
        data.frame(Gene = x$id, PValue = x[["neg|p-value"]],
            FDR = x[["neg|fdr"]], LFC = x[["neg|lfc"]], time = time)
    }
    results$MAGeCK_RRA <- rra(file.path(workdir, "rra"))
    results$MAGeCK_RRA_controls <- rra(file.path(workdir, "rra_ctrl"),
        c("--control-sgrna", control_file))
    if (mle) {
        design_file <- file.path(workdir, "design.txt")
        write.table(data.frame(Samples = sim$samples, baseline = 1,
            treatment = as.integer(sim$samples %in% trt)), design_file,
            sep = "\t", quote = FALSE, row.names = FALSE)
        prefix <- file.path(workdir, "mle")
        time <- system.time(system2(mageck, c("mle", "-k", count_file,
            "-d", design_file, "-n", prefix), stdout = FALSE,
            stderr = FALSE))[["elapsed"]]
        x <- read.delim(paste0(prefix, ".gene_summary.txt"), check.names = FALSE)
        p <- pnorm(x[["treatment|z"]])
        results$MAGeCK_MLE <- data.frame(Gene = x$Gene, PValue = p,
            FDR = p.adjust(p, "BH"), LFC = x[["treatment|beta"]], time = time)
    }
    results
}

as_result <- function(x, time) {
    data.frame(Gene = x$Gene, PValue = x$PValue, FDR = x$FDR,
        LFC = x$Log2FC, time = time)
}

run_screenr <- function(sim, nrot = 999) {
    groups <- factor(sub("_[0-9]+$", "", sim$samples))
    counts <- sim$counts
    colnames(counts) <- c("Barcode",
        sub("^(CTRL|TRT)_([0-9]+)$", "Day1_\\1_\\2", sim$samples))
    out <- list()
    t0 <- system.time({
        object <- suppressMessages(create_screenr_object(table = counts,
            annotation = sim$annotation, groups = groups))
        object <- compute_data_table(normalize_data(object))
    })[["elapsed"]]
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, treatment = "TRT", control = "CTRL")
    timed <- function(expr) {
        time <- system.time(value <- suppressWarnings(expr))[["elapsed"]]
        as_result(value, t0 + time)
    }
    metrics <- compute_metrics(object, control = "CTRL", treatment = "TRT",
        day = "Day1")
    statistics <- compute_barcode_statistics(object, design, contrast)
    out$ScreenR_CAMERA <- timed(find_camera_hit(object, design, contrast,
        fdr = 1, number_barcode = 1))
    out$ScreenR_CAMERA_storey <- timed(find_camera_hit(object, design,
        contrast, fdr = 1, number_barcode = 1, fdr_method = "storey"))
    out$ScreenR_ROAST <- timed(find_roast_hit(object, design, contrast,
        nrot = nrot, seed = 1, fdr = 1, number_barcode = 1))
    out$ScreenR_FRY <- timed(find_roast_hit(object, design, contrast,
        method = "fry", fdr = 1, number_barcode = 1))
    out$ScreenR_Zscore_mean <- timed(find_gene_zscore_hit(metrics, fdr = 1,
        min_barcodes = 1, seed = 1, n_permutations = 2e4))
    out$ScreenR_Zscore_median <- timed(find_gene_zscore_hit(metrics,
        statistic = "median", fdr = 1, min_barcodes = 1, seed = 1,
        n_permutations = 2e4))
    out$ScreenR_Zscore_controls <- timed(find_gene_zscore_hit(metrics,
        control_genes = "non-targeting", fdr = 1, min_barcodes = 1, seed = 1,
        n_permutations = 2e4))
    out$ScreenR_Zscore_edgeR <- timed(find_gene_zscore_hit(statistics,
        fdr = 1, min_barcodes = 1, seed = 1, n_permutations = 2e4))
    legacy <- find_zscore_hit(metrics, number_barcode = 6)
    out$ScreenR_legacy_rule <- data.frame(Gene = legacy$Gene,
        PValue = NA_real_, FDR = 0, LFC = NA_real_, time = NA_real_)
    out
}

evaluate <- function(result, truth, levels = c(0.01, 0.05, 0.1, 0.2)) {
    result <- result[result$Gene %in% truth$Gene, ]
    merged <- merge(truth, result, by = "Gene", all.x = TRUE)
    merged$FDR[is.na(merged$FDR)] <- 1
    merged$PValue[is.na(merged$PValue)] <- 1
    out <- do.call(rbind, lapply(levels, function(alpha) {
        called <- merged$FDR <= alpha
        data.frame(alpha = alpha, n_called = sum(called),
            FDP = if (sum(called) == 0) 0 else mean(!merged$hit[called]),
            TPR = if (any(merged$hit)) sum(called & merged$hit) / sum(merged$hit) else NA,
            # Null p-values: fraction below alpha among the non-hits
            null_p_below = mean(merged$PValue[!merged$hit] <= alpha))
    }))
    if (all(is.na(result$PValue)) || !any(merged$hit)) {
        out$AUPRC <- NA_real_
    } else {
        lfc <- merged$LFC
        lfc[is.na(lfc)] <- 0
        hit <- merged$hit[order(merged$PValue, lfc)]
        precision <- cumsum(hit) / seq_along(hit)
        out$AUPRC <- sum(precision[hit]) / sum(hit)
    }
    out$time <- result$time[1]
    out
}
