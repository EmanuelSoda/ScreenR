# find_time_hit() with the real times of the time points

# A screen with three time points that are not equally spaced (days 0, 2
# and 20), control and treated cultures with three replicates each. The
# barcodes of the genes H1-H20 lose 0.1 log2 units per day in the treated
# cultures only, so their treatment effect grows linearly with time.
simulate_time_course <- function(seed = 1) {
    set.seed(seed)
    genes <- c(paste0("H", 1:20), paste0("N", 1:130))
    barcode_gene <- rep(genes, each = 5)
    n <- length(barcode_gene)
    days <- c(D0 = 0, D2 = 2, D20 = 20)
    design <- expand.grid(Replicate = 1:3, Treatment = c("CTRL", "TRT"),
        Day = names(days), stringsAsFactors = FALSE)
    samples <- paste(design$Day, design$Treatment, design$Replicate,
        sep = "_")
    base <- stats::rgamma(n, shape = 3, rate = 3 / 300)
    hit <- startsWith(barcode_gene, "H")
    counts <- vapply(seq_along(samples), function(i) {
        effect <- if (design$Treatment[i] == "TRT") {
            2^(-0.1 * days[[design$Day[i]]] * hit)
        } else {
            1
        }
        stats::rnbinom(n, mu = base * effect, size = 30)
    }, numeric(n))
    colnames(counts) <- samples
    object <- suppressMessages(create_screenr_object(
        table = data.frame(Barcode = paste0("b", seq_len(n)), counts,
            check.names = FALSE),
        annotation = data.frame(Barcode = paste0("b", seq_len(n)),
            Gene = barcode_gene),
        groups = factor(paste(design$Day, design$Treatment, sep = "_")),
        sample_table = data.frame(Sample = samples, Day = design$Day,
            Treatment = design$Treatment)))
    list(object = normalize_data(object), days = days,
        hits = paste0("H", 1:20))
}

screen <- simulate_time_course()

test_that("the default numbers the time points 0, 1, 2", {
    steps <- c(D0 = 0, D2 = 1, D20 = 2)
    expect_equal(
        find_time_hit(screen$object, "TRT", "CTRL", names(steps), fdr = 1),
        find_time_hit(screen$object, "TRT", "CTRL", names(steps), fdr = 1,
            time = steps))
})

test_that("with two time points the times only change the units", {
    days <- c("D2", "D20")
    by_step <- find_time_hit(screen$object, "TRT", "CTRL", days, fdr = 1)
    by_day <- find_time_hit(screen$object, "TRT", "CTRL", days, fdr = 1,
        time = screen$days[days])
    expect_equal(by_day$Gene, by_step$Gene)
    expect_equal(by_day$PValue, by_step$PValue, tolerance = 1e-6)
    # Log2FC is per day instead of per time point: 18 days between them
    expect_equal(by_day$Log2FC, by_step$Log2FC / 18, tolerance = 1e-6)
})

test_that("real times fit an effect that grows linearly with time", {
    days <- names(screen$days)
    by_day <- find_time_hit(screen$object, "TRT", "CTRL", days, fdr = 1,
        time = screen$days)
    by_step <- find_time_hit(screen$object, "TRT", "CTRL", days, fdr = 1)
    hits_by_day <- by_day[by_day$Gene %in% screen$hits, ]
    # The change of the treatment effect is estimated per day: -0.1
    expect_equal(stats::median(hits_by_day$Log2FC), -0.1, tolerance = 0.25)
    # All the genes whose effect grows are found
    expect_true(all(screen$hits %in% by_day$Gene[by_day$FDR <= 0.05]))
    # With equally spaced steps the line does not follow the effect, which
    # is close to 0 at the second time point (day 2) and large at the third
    # (day 20): most hits get larger p-values (here about twice as large)
    hits_by_step <- by_step[match(hits_by_day$Gene, by_step$Gene), ]
    log_ratio <- log10(hits_by_day$PValue / hits_by_step$PValue)
    expect_lt(stats::median(log_ratio), -0.1)
    expect_gt(mean(log_ratio < 0), 0.5)
})

test_that("find_time_hit agrees with camera on the same model", {
    object <- screen$object
    info <- get_sample_table(object)
    time <- unname(screen$days[info$Day])
    treated <- as.numeric(info$Treatment == "TRT")
    design <- cbind(Intercept = 1, Time = time, Treated = treated,
        TimeTreated = time * treated)
    counts <- SummarizedExperiment::assay(object, "counts")
    genes <- SummarizedExperiment::rowData(object)$Gene
    y <- edgeR::DGEList(counts, lib.size = colSums(counts))
    y <- edgeR::estimateDisp(y, design)
    index <- ScreenR:::unique_gene_symbols(genes, 3)
    direct <- limma::camera(y, index, design, "TimeTreated")
    result <- find_time_hit(object, "TRT", "CTRL", names(screen$days),
        fdr = 1, time = screen$days)
    expect_true(all(direct[result$Gene, "Direction"] == "Down"))
    expect_equal(result$PValue, direct[result$Gene, "PValue"] / 2,
        tolerance = 1e-6)
})

test_that("invalid times give informative errors", {
    days <- names(screen$days)
    expect_error(find_time_hit(screen$object, "TRT", "CTRL", days,
        time = c(D0 = 0, D2 = 2)), "no value for the time point\\(s\\): D20")
    expect_error(find_time_hit(screen$object, "TRT", "CTRL", days,
        time = c(0, 2, 20)), "named numeric")
    expect_error(find_time_hit(screen$object, "TRT", "CTRL", days,
        time = c(D0 = 0, D2 = NA, D20 = 20)), "finite")
    expect_error(find_time_hit(screen$object, "TRT", "CTRL", days,
        time = c(D0 = "0", D2 = "2", D20 = "20")), "named numeric")
    # Two time points with the same time cannot give a slope
    expect_error(find_time_hit(screen$object, "TRT", "CTRL", c("D0", "D2"),
        time = c(D0 = 5, D2 = 5)), "at least two time points")
})
