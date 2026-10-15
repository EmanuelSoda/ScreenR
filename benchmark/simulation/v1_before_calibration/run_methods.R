# Run MAGeCK and the ScreenR methods on a simulated screen and return, for
# each method, one row per gene with its p-value and FDR (depletion).

run_mageck <- function(sim, workdir, mageck) {
    # MAGeCK calls its compiled RRA program by name
    Sys.setenv(PATH = paste(dirname(mageck), Sys.getenv("PATH"), sep = ":"))
    dir.create(workdir, showWarnings = FALSE, recursive = TRUE)
    count_file <- file.path(workdir, "counts.txt")
    table <- data.frame(sgRNA = sim$counts$Barcode, Gene = sim$annotation$Gene,
        sim$counts[, sim$samples], check.names = FALSE)
    write.table(table, count_file, sep = "\t", quote = FALSE,
        row.names = FALSE)
    ctrl <- grep("^CTRL", sim$samples, value = TRUE)
    trt <- grep("^TRT", sim$samples, value = TRUE)
    results <- list()

    # MAGeCK RRA (default median normalization)
    prefix <- file.path(workdir, "rra")
    time_rra <- system.time(system2(mageck, c("test", "-k", count_file,
        "-t", paste(trt, collapse = ","), "-c", paste(ctrl, collapse = ","),
        "-n", prefix), stdout = FALSE, stderr = FALSE))[["elapsed"]]
    rra <- read.delim(paste0(prefix, ".gene_summary.txt"), check.names = FALSE)
    results$MAGeCK_RRA <- data.frame(Gene = rra$id,
        PValue = rra[["neg|p-value"]], FDR = rra[["neg|fdr"]],
        LFC = rra[["neg|lfc"]], time = time_rra)

    # MAGeCK MLE: design with an intercept and the treatment
    design_file <- file.path(workdir, "design.txt")
    write.table(data.frame(Samples = sim$samples, baseline = 1,
        treatment = as.integer(sim$samples %in% trt)), design_file,
        sep = "\t", quote = FALSE, row.names = FALSE)
    prefix <- file.path(workdir, "mle")
    time_mle <- system.time(system2(mageck, c("mle", "-k", count_file,
        "-d", design_file, "-n", prefix), stdout = FALSE,
        stderr = FALSE))[["elapsed"]]
    mle <- read.delim(paste0(prefix, ".gene_summary.txt"), check.names = FALSE)
    # One-sided p-value for depletion from the Wald z statistic
    z <- mle[["treatment|z"]]
    p_down <- pnorm(z)
    results$MAGeCK_MLE <- data.frame(Gene = mle$Gene, PValue = p_down,
        FDR = p.adjust(p_down, "BH"), LFC = mle[["treatment|beta"]],
        time = time_mle)
    results
}

run_screenr <- function(sim, nrot = 999) {
    groups <- factor(sub("_[0-9]+$", "", sim$samples))
    counts <- sim$counts
    colnames(counts) <- c("Barcode",
        sub("^(CTRL|TRT)_([0-9]+)$", "Day1_\\1_\\2", sim$samples))
    results <- list()
    t0 <- system.time({
        object <- suppressMessages(create_screenr_object(table = counts,
            annotation = sim$annotation, groups = groups))
        object <- normalize_data(object)
        object <- compute_data_table(object)
    })[["elapsed"]]
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, treatment = "TRT", control = "CTRL")

    time_z <- system.time({
        metrics <- compute_metrics(object, control = "CTRL",
            treatment = "TRT", day = "Day1")
        z <- find_gene_zscore_hit(metrics, fdr = 1, min_barcodes = 1)
    })[["elapsed"]]
    results$ScreenR_Zscore <- data.frame(Gene = z$Gene, PValue = z$PValue,
        FDR = z$FDR, LFC = z$Log2FC, time = t0 + time_z)

    time_ze <- system.time({
        statistics <- compute_barcode_statistics(object, design, contrast)
        ze <- find_gene_zscore_hit(statistics, fdr = 1, min_barcodes = 1)
    })[["elapsed"]]
    results$ScreenR_Zscore_edgeR <- data.frame(Gene = ze$Gene,
        PValue = ze$PValue, FDR = ze$FDR, LFC = ze$Log2FC,
        time = t0 + time_ze)

    time_c <- system.time(camera <- suppressWarnings(find_camera_hit(object,
        design, contrast, fdr = 1, number_barcode = 1)))[["elapsed"]]
    # find_camera_hit returns only the Down direction: genes going up get p = 1
    results$ScreenR_CAMERA <- data.frame(Gene = camera$Gene,
        PValue = camera$PValue, FDR = camera$FDR, LFC = camera$Log2FC,
        time = t0 + time_c)

    time_r <- system.time(roast <- suppressWarnings(find_roast_hit(object,
        design, contrast, nrot = nrot, seed = 1, fdr = 1,
        number_barcode = 1)))[["elapsed"]]
    results$ScreenR_ROAST <- data.frame(Gene = roast$Gene,
        PValue = roast$PValue, FDR = roast$FDR, LFC = roast$Log2FC,
        time = t0 + time_r)

    # Legacy rule: no p-value, only a list of hits
    legacy <- find_zscore_hit(metrics, number_barcode = 6)
    results$ScreenR_legacy_rule <- data.frame(Gene = legacy$Gene,
        PValue = NA_real_, FDR = 0, LFC = NA_real_, time = t0 + time_z)
    results
}

# Evaluate a method against the truth
evaluate <- function(result, truth, levels = c(0.05, 0.1)) {
    merged <- merge(truth, result, by = "Gene", all.x = TRUE)
    merged$FDR[is.na(merged$FDR)] <- 1
    merged$PValue[is.na(merged$PValue)] <- 1
    rows <- lapply(levels, function(alpha) {
        called <- merged$FDR <= alpha
        data.frame(
            alpha = alpha,
            n_called = sum(called),
            FDP = if (sum(called) == 0) 0 else mean(!merged$hit[called]),
            TPR = sum(called & merged$hit) / sum(merged$hit)
        )
    })
    out <- do.call(rbind, rows)
    # Area under the precision-recall curve of the p-value ranking
    if (all(is.na(result$PValue))) {
        out$AUPRC <- NA_real_
    } else {
        ord <- order(merged$PValue, -abs(merged$LFC %||% 0))
        hit <- merged$hit[ord]
        precision <- cumsum(hit) / seq_along(hit)
        out$AUPRC <- sum(precision[hit]) / sum(hit)
    }
    out$time <- result$time[1]
    out
}

`%||%` <- function(a, b) if (is.null(a)) b else a
