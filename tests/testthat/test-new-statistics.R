get_object <- function() get0("object", envir = asNamespace("ScreenR"))

test_that("create_design_matrix has one column per group and no intercept", {
    object <- get_object()
    design <- create_design_matrix(object)
    expect_equal(colnames(design), make.names(levels(get_groups(object))))
    expect_true(all(rowSums(design) == 1))
    expect_equal(nrow(design), length(get_groups(object)))
})

test_that("create_contrast compares groups by name", {
    design <- create_design_matrix(get_object())
    contrast <- create_contrast(design, "Treated", "Control")
    expect_equal(unname(contrast[c("Treated", "Control")]), c(1, -1))
    expect_equal(sum(contrast), 0)

    # Syntactic and original names both work
    both <- create_contrast(design, "Treated", c("Control", "T1/T2"))
    expect_equal(unname(both[c("Control", "T1.T2")]), c(-0.5, -0.5))

    expect_error(create_contrast(design, "Nope", "Control"), "not found")
    expect_error(create_contrast(design, "Treated", "Treated"), "both")
})

test_that("as_contrast accepts names, vectors and makeContrasts matrices", {
    design <- create_design_matrix(get_object())
    matrix <- limma::makeContrasts(Treated - Control, levels = design)
    vector <- ScreenR:::as_contrast(matrix, design)
    expect_equal(unname(vector), unname(create_contrast(design, "Treated",
        "Control")))
    expect_equal(ScreenR:::as_contrast("Treated", design), "Treated")
    expect_error(ScreenR:::as_contrast("Nope", design), "not a column")
    expect_error(ScreenR:::as_contrast(c(1, -1), design), "one value")
})

test_that("find_camera_hit applies the FDR cut off and reports Log2FC", {
    object <- get_object()
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, "Treated", "Control")

    all_genes <- suppressWarnings(find_camera_hit(object, design, contrast,
        fdr = 1))
    expect_true("Log2FC" %in% colnames(all_genes))
    expect_true(all(all_genes$Direction == "Down"))

    hits <- suppressWarnings(find_camera_hit(object, design, contrast,
        fdr = 0.2))
    expect_true(all(hits$FDR <= 0.2))
    expect_equal(nrow(hits), sum(all_genes$FDR <= 0.2))

    # A makeContrasts matrix gives the same result as the vector
    matrix <- limma::makeContrasts(Treated - Control, levels = design)
    expect_equal(
        suppressWarnings(find_camera_hit(object, design, matrix, fdr = 1)),
        all_genes
    )
})

test_that("find_roast_hit is reproducible with a seed and uses the FDR", {
    object <- get_object()
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, "Treated", "Control")

    first <- suppressWarnings(find_roast_hit(object, design, contrast,
        nrot = 99, seed = 1, fdr = 1))
    second <- suppressWarnings(find_roast_hit(object, design, contrast,
        nrot = 99, seed = 1, fdr = 1))
    expect_equal(first, second)
    expect_true("Log2FC" %in% colnames(first))

    hits <- suppressWarnings(find_roast_hit(object, design, contrast,
        nrot = 99, seed = 1, fdr = 0.5))
    expect_true(all(hits$FDR <= 0.5))
})

test_that("compute_metrics uses a pseudocount", {
    object <- get_object()
    metrics <- compute_metrics(object,
        control = "Time3", treatment = "TRT",
        day = "Time3", pseudocount = 0.5
    )
    expect_equal(
        metrics$Log2FC,
        log2((metrics$Treated + 0.5) / (metrics$Control + 0.5))
    )
    expect_true(all(is.finite(metrics$Log2FC)))
    expect_true(all(metrics$Treated > 0 | metrics$Control > 0))
    expect_error(compute_metrics(object, "TRT", "Time3", "Time3",
        pseudocount = 0), "positive")
})

test_that("normalize_data supports cpm, tmm and control barcodes", {
    object <- get_object()
    counts <- as.matrix(ScreenR:::count_table_of(object)[, -1])

    cpm <- ScreenR:::normalized_table_of(normalize_data(object))
    expect_equal(unname(colSums(as.matrix(cpm[, -1]))), rep(1e6, ncol(counts)))

    tmm <- suppressMessages(ScreenR:::normalized_table_of(normalize_data(object, method = "tmm")))
    expect_equal(dim(tmm), dim(cpm))
    expect_equal(tmm$Barcode, cpm$Barcode)

    controls <- ScreenR:::count_table_of(object)$Barcode[1:20]
    control <- normalize_data(object, method = "control",
        control_barcodes = controls) |> ScreenR:::normalized_table_of()
    expect_equal(
        unname(colSums(as.matrix(control[1:20, -1]))),
        rep(1e6, ncol(counts))
    )
    expect_error(normalize_data(object, method = "control"), "control_barcodes")
    expect_error(normalize_data(object, method = "control",
        control_barcodes = "nope"), "None")
})

test_that("find_gene_zscore_hit computes binomial p-values and FDR", {
    # Gene A: 8 of 10 barcodes strongly depleted; genes B-J: no effect
    set.seed(1)
    table <- tibble::tibble(
        Gene = rep(LETTERS[1:10], each = 10),
        Barcode = paste0("b", 1:100),
        Zscore = c(rep(-3, 8), rep(0, 2), rnorm(90, sd = 0.5)),
        Log2FC = c(rep(-2, 8), rep(0, 2), rnorm(90, sd = 0.2))
    )
    result <- find_gene_zscore_hit(table, fdr = 1)
    expect_equal(result$Gene[1], "A")
    expect_equal(result$NBarcodesHit[1], 8L)

    proportion <- mean(table$Zscore < -2)
    expect_equal(result$PValue[1],
        pbinom(7, 10, proportion, lower.tail = FALSE))
    expect_equal(result$FDR, p.adjust(result$PValue, "BH"))

    hits <- find_gene_zscore_hit(table, fdr = 0.05)
    expect_equal(hits$Gene, "A")

    expect_error(find_gene_zscore_hit(table, score = "ZscoreRobust"),
        "Missing")
    expect_error(find_gene_zscore_hit(table, zscore_threshold = -2),
        "positive")
})

test_that("compute_barcode_statistics returns one row per barcode", {
    object <- get_object()
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, "Treated", "Control")
    statistics <- compute_barcode_statistics(object, design, contrast)

    expect_equal(nrow(statistics), nrow(ScreenR:::count_table_of(object)))
    expect_equal(statistics$Barcode, as.character(ScreenR:::count_table_of(object)$Barcode))
    expect_equal(sign(statistics$Zscore), sign(statistics$Log2FC))
    expect_true(all(statistics$PValue >= 0 & statistics$PValue <= 1))
})

test_that("find_time_hit fits the time course and checks its input", {
    object <- get_object()
    hits <- suppressWarnings(find_time_hit(object,
        treatment = "TRT", control = c("Time3", "Time4"),
        days = c("Time3", "Time4"), fdr = 1
    ))
    expect_true(all(c("Gene", "PValue", "FDR", "Log2FC") %in% colnames(hits)))
    expect_true(all(hits$Direction == "Down"))

    expect_error(find_time_hit(object, "TRT", "Time3", days = "Time3"),
        "two time points")
    expect_error(find_time_hit(object, "nope", "Time3",
        days = c("Time3", "Time4")), "No treated")
})

test_that("summarise_hits combines the FDRs with their maximum", {
    a <- data.frame(Gene = c("A", "B", "C"), FDR = c(0.01, 0.2, 0.03))
    b <- data.frame(Gene = c("A", "B", "D"), FDR = c(0.02, 0.01, 0.01))
    summary <- summarise_hits(list(x = a, y = b), fdr = 0.05)

    expect_equal(summary$Gene[1], "A")
    expect_equal(summary$MaxFDR[summary$Gene == "A"], 0.02)
    expect_equal(summary$NMethods[summary$Gene == "B"], 1)
    expect_true(is.na(summary$MaxFDR[summary$Gene == "C"]))
    expect_equal(summary$Gene[summary$Hit], "A")

    expect_error(summarise_hits(list(a, b)), "named list")
    expect_error(summarise_hits(list(x = data.frame(Gene = "A"))), "FDR")
})

test_that("compute_gini is 0 for an even library and handles big counts", {
    object <- get_object()
    gini <- compute_gini(object)
    expect_true(all(gini$Gini > 0 & gini$Gini < 1))

    object <- create_screenr_object(table = tibble::tibble(
        Barcode = c("a", "b", "c", "d"),
        even = rep(1e9, 4), uneven = c(0, 0, 0, 1e9)
    ))
    gini <- compute_gini(object)
    expect_equal(gini$Gini, c(0, 0.75))
})

test_that("plot_replicate_correlation returns a ggplot", {
    plot <- plot_replicate_correlation(get_object(), method = "spearman")
    expect_true(ggplot2::is_ggplot(plot))
})

test_that("find_discordant_barcodes flags outlying barcodes", {
    table <- tibble::tibble(
        Barcode = paste0("b", 1:10),
        Gene = "A",
        Log2FC = c(seq(-0.2, 0.2, length.out = 9), 5)
    )
    discordant <- find_discordant_barcodes(table, threshold = 3)
    expect_equal(discordant$Barcode, "b10")
    expect_error(find_discordant_barcodes(table[, -1]), "Missing")
})

test_that("plot_gini plots the Gini index of each sample", {
    object <- get_object()
    plot <- plot_gini(object)
    expect_true(ggplot2::is_ggplot(plot))
    expect_equal(plot$data$Gini, compute_gini(object)$Gini)

    palette <- rep("grey50", ncol(ScreenR:::count_table_of(object)) - 1)
    expect_true(ggplot2::is_ggplot(plot_gini(object, palette = palette)))
})

test_that("discordant barcodes are found when the gene MAD is 0", {
    table <- tibble::tibble(
        Barcode = paste0("b", 1:6), Gene = "A",
        Log2FC = c(0, 0, 0, 0, 0, 4)
    )
    discordant <- find_discordant_barcodes(table)
    expect_equal(discordant$Barcode, "b6")
    expect_equal(discordant$Deviation, Inf)
})

test_that("find_time_hit does not call a constant effect a growing one", {
    # 40 genes x 5 barcodes; genes 1-10 have a constant treatment effect
    # (the same at both time points), so the interaction is 0
    set.seed(10)
    n_genes <- 40
    barcodes <- paste0("b", seq_len(n_genes * 5))
    genes <- rep(paste0("G", seq_len(n_genes)), each = 5)
    samples <- c(paste0("D1_CTRL_", 1:3), paste0("D1_TRT_", 1:3),
        paste0("D2_CTRL_", 1:3), paste0("D2_TRT_", 1:3))
    base <- rgamma(length(barcodes), shape = 5, rate = 0.01)
    effect <- ifelse(genes %in% paste0("G", 1:10), 2^-1.5, 1)
    counts <- sapply(samples, function(sample) {
        mu <- base * if (grepl("TRT", sample)) effect else 1
        rnbinom(length(mu), mu = mu, size = 20)
    })
    count_table <- data.frame(Barcode = barcodes, counts)
    annotation <- data.frame(Barcode = barcodes, Gene = genes)
    object <- suppressMessages(create_screenr_object(
        table = count_table, annotation = annotation,
        groups = factor(sub("_[0-9]$", "", samples))
    ))
    hits <- find_time_hit(object, treatment = "TRT", control = "CTRL",
        days = c("D1", "D2"), fdr = 1)
    expect_false(any(hits$FDR[hits$Gene %in% paste0("G", 1:10)] <= 0.05))

    expect_error(
        find_time_hit(object, treatment = "TRT", control = "CTRL",
            days = c("D1", "D2"), number_barcode = 10),
        "lower number_barcode"
    )
})

test_that("find_camera_hit works when a single gene can be tested", {
    object <- get_object()
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, "Treated", "Control")
    counts <- table(object@annotation_table$Gene)
    skip_if(sum(counts == max(counts)) != 1, "No gene with a unique maximum")
    threshold <- max(counts) - 1
    result <- suppressWarnings(find_camera_hit(object, design, contrast,
        number_barcode = threshold, fdr = 1, direction = "Down"))
    expect_true("FDR" %in% colnames(result))
    expect_error(
        suppressWarnings(find_camera_hit(object, design, contrast,
            number_barcode = 1000)),
        "lower number_barcode"
    )
})

test_that("the seed of find_roast_hit does not change the user's random numbers", {
    object <- get_object()
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, "Treated", "Control")
    set.seed(123)
    expected <- runif(3)
    set.seed(123)
    invisible(suppressWarnings(find_roast_hit(object, design, contrast,
        nrot = 99, seed = 1, fdr = 1)))
    expect_equal(runif(3), expected)
})
