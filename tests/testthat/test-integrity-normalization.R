# Data integrity of the ScreenR object (SummarizedExperiment as the single
# source of truth) and propagation of one normalization to every analysis.

# A simulated time-course screen with a strong compositional imbalance: at
# day 2, the barcodes of 20% of the genes (n_hits of 200) are depleted 8
# times in the treated samples, so with counts per million every other
# barcode looks enriched. The barcodes of the null genes N1-N60 can serve as
# control barcodes.
simulate_screen <- function(seed = 1, numeric_ids = FALSE, n_hits = 40) {
    set.seed(seed)
    n_genes <- 200
    genes <- c(paste0("H", seq_len(n_hits)),
        paste0("N", seq_len(n_genes - n_hits)))
    barcode_gene <- rep(genes, each = 4)
    n <- length(barcode_gene)
    barcodes <- if (numeric_ids) 100000 + seq_len(n) else
        as.character(100000 + seq_len(n))
    samples <- c("D1_CTRL_A", "D1_CTRL_B", "D1_TRT_A", "D1_TRT_B",
        "D2_CTRL_A", "D2_CTRL_B", "D2_TRT_A", "D2_TRT_B")
    base <- stats::rgamma(n, shape = 3, rate = 3 / 300)
    hit <- startsWith(barcode_gene, "H")
    counts <- vapply(samples, function(sample) {
        depleted <- grepl("D2_TRT", sample) & hit
        mu <- base * ifelse(depleted, 1 / 8, 1)
        stats::rnbinom(n, mu = mu, size = 30)
    }, numeric(n))
    count_table <- data.frame(Barcode = barcodes, counts, check.names = FALSE)
    annotation <- data.frame(Barcode = barcodes, Gene = barcode_gene)
    sample_table <- data.frame(Sample = samples,
        Day = sub("_.*", "", samples),
        Treatment = sub("^D[12]_([A-Z]+)_.*", "\\1", samples))
    groups <- factor(sub("_[AB]$", "", samples))
    object <- suppressMessages(create_screenr_object(table = count_table,
        annotation = annotation, groups = groups, sample_table = sample_table))
    list(object = object, count_table = count_table, annotation = annotation,
        controls = barcodes[barcode_gene %in% paste0("N", 1:60)],
        null_genes = paste0("N", seq_len(n_genes - n_hits)),
        hit_genes = paste0("H", seq_len(n_hits)))
}

screen <- simulate_screen()

normalized <- function(method) {
    if (method == "control") {
        normalize_data(screen$object, "control",
            control_barcodes = screen$controls)
    } else {
        normalize_data(screen$object, method)
    }
}

# The same normalization, set directly in edgeR as a conventional workflow
direct_dge <- function(object, method) {
    counts <- SummarizedExperiment::assay(object, "counts")
    genes <- data.frame(Barcode = rownames(object),
        Gene = SummarizedExperiment::rowData(object)$Gene)
    y <- edgeR::DGEList(counts = counts, group = object$Group, genes = genes)
    if (method == "tmm") {
        y <- edgeR::normLibSizes(y)
    } else if (method == "control") {
        y$samples$lib.size <- colSums(counts[screen$controls, ])
    }
    y
}

design_contrast <- function(object) {
    design <- create_design_matrix(object)
    list(design = design,
        contrast = create_contrast(design, "D2_TRT", "D2_CTRL"))
}

gene_index <- function(genes, number_barcode) {
    index <- split(seq_along(genes), genes)
    index[lengths(index) > number_barcode]
}


# --------------------------------------------------------------------------
# Barcode identifiers
# --------------------------------------------------------------------------

test_that("barcode identifiers never become count columns", {
    object <- screen$object
    counts <- SummarizedExperiment::assay(object, "counts")
    expect_equal(colnames(counts), screen$object$Sample)
    expect_false("Barcode" %in% colnames(counts))
    expect_equal(rownames(counts), as.character(screen$count_table$Barcode))
    expect_identical(rownames(object),
        as.character(SummarizedExperiment::rowData(object)$Barcode))

    # Numeric identifiers in a column with another name are not a sample
    numeric <- simulate_screen(numeric_ids = TRUE)
    table <- numeric$count_table
    colnames(table)[1] <- "id"
    annotation <- numeric$annotation
    colnames(annotation)[1] <- "id"
    object <- suppressMessages(create_screenr_object(table = table,
        annotation = annotation, groups = screen$object$Group,
        barcode_column = "id"))
    expect_equal(ncol(object), 8L)
    expect_false(any(c("id", "Barcode") %in%
        colnames(SummarizedExperiment::assay(object))))
    expect_type(rownames(object), "character")
    count_table <- suppressMessages(get_count_table(object))
    expect_equal(colnames(count_table), c("Barcode", object$Sample))
})

test_that("numeric and character identifiers give the same object and results", {
    numeric <- simulate_screen(numeric_ids = TRUE)$object
    character <- screen$object
    expect_identical(rownames(numeric), rownames(character))
    expect_identical(SummarizedExperiment::assay(numeric, "counts"),
        SummarizedExperiment::assay(character, "counts"))
    expect_identical(SummarizedExperiment::rowData(numeric),
        SummarizedExperiment::rowData(character))

    numeric <- normalize_data(numeric, "tmm")
    character <- normalize_data(character, "tmm")
    dc <- design_contrast(character)
    expect_equal(
        compute_barcode_statistics(numeric, dc$design, dc$contrast),
        compute_barcode_statistics(character, dc$design, dc$contrast))
    expect_equal(
        compute_metrics(numeric, control = "CTRL", treatment = "TRT",
            day = "D2"),
        compute_metrics(character, control = "CTRL", treatment = "TRT",
            day = "D2"))
    # Large numbers are not written in scientific notation
    expect_equal(ScreenR:::as_barcode_id(c(1e5, 123456789012)),
        c("100000", "123456789012"))
})


# --------------------------------------------------------------------------
# Alignment by identifier
# --------------------------------------------------------------------------

test_that("reordering the annotation keeps the gene of each barcode", {
    object <- normalized("cpm")
    shuffled <- screen$annotation[sample(nrow(screen$annotation)), ]
    reordered <- set_annotation_table(object, shuffled)
    expect_identical(SummarizedExperiment::rowData(reordered),
        SummarizedExperiment::rowData(object))
    expect_identical(create_edger_obj(reordered)$genes$Gene,
        create_edger_obj(object)$genes$Gene)
})

test_that("reordering samples and barcodes keeps everything aligned", {
    object <- normalized("tmm")
    sizes <- get_normalization(object)$effective_library_size
    reordered <- object[rev(seq_len(nrow(object))), rev(colnames(object))]

    expect_equal(get_normalization(reordered)$effective_library_size,
        rev(sizes))
    expect_equal(reordered$Treatment, rev(object$Treatment))
    expect_equal(as.character(get_groups(reordered)),
        rev(as.character(get_groups(object))))
    expect_equal(ScreenR:::genes_of(reordered),
        rev(ScreenR:::genes_of(object)))
    # The normalized counts of each barcode and sample are unchanged
    expect_equal(
        ScreenR:::normalized_counts(reordered)[rownames(object),
            colnames(object)],
        ScreenR:::normalized_counts(object))
    # And so are the analyses, matched by barcode and gene
    metrics <- compute_metrics(object, "CTRL", "TRT", "D2")
    metrics_reordered <- compute_metrics(reordered, "CTRL", "TRT", "D2")
    expect_equal(
        metrics_reordered$Log2FC[match(as.character(metrics$Barcode),
            as.character(metrics_reordered$Barcode))],
        metrics$Log2FC)
    dc <- design_contrast(object)
    statistics <- compute_barcode_statistics(object, dc$design, dc$contrast)
    statistics_reordered <- compute_barcode_statistics(reordered,
        create_design_matrix(reordered),
        create_contrast(create_design_matrix(reordered), "D2_TRT", "D2_CTRL"))
    matched <- match(statistics$Barcode, statistics_reordered$Barcode)
    expect_equal(statistics_reordered$Log2FC[matched], statistics$Log2FC,
        tolerance = 1e-6)
    expect_equal(statistics_reordered$Gene[matched], statistics$Gene)
})


# --------------------------------------------------------------------------
# Invalidation of stale derived data
# --------------------------------------------------------------------------

test_that("replacing the counts removes the normalization", {
    object <- normalized("tmm")
    counts <- SummarizedExperiment::assay(object, "counts")

    # assay<- with new values
    changed <- object
    expect_message(SummarizedExperiment::assay(changed, "counts") <-
        counts + 1, "normalization has been removed")
    expect_null(get_normalization(changed))
    expect_error(get_data_table(changed), "normalize_data")
    expect_error(create_edger_obj(changed), "normalize_data")
    expect_equal(nrow(suppressMessages(get_normalized_count_table(changed))),
        0L)

    # assays<- and assay<- without the name
    changed <- object
    expect_message(SummarizedExperiment::assays(changed) <-
        list(counts = counts * 2), "normalization has been removed")
    expect_null(get_normalization(changed))
    changed <- object
    expect_message(SummarizedExperiment::assay(changed) <- counts + 2,
        "normalization has been removed")
    expect_null(get_normalization(changed))

    # set_count_table with new values
    table <- suppressMessages(get_count_table(object))
    table[1, 2] <- table[1, 2] + 5
    expect_message(changed <- set_count_table(object, table),
        "normalization has been removed")
    expect_null(get_normalization(changed))

    # The same counts, or another assay, keep the normalization
    same <- object
    SummarizedExperiment::assay(same, "counts") <- counts
    expect_identical(get_normalization(same), get_normalization(object))
    SummarizedExperiment::assay(same, "other") <- counts * 0
    expect_identical(get_normalization(same), get_normalization(object))
    same <- set_count_table(object, suppressMessages(get_count_table(object)))
    expect_identical(get_normalization(same), get_normalization(object))
})

test_that("normalizing again changes every view immediately", {
    cpm <- normalized("cpm")
    tmm <- normalize_data(cpm, "tmm")
    for (object in list(cpm, tmm)) {
        sizes <- get_normalization(object)$effective_library_size
        counts <- SummarizedExperiment::assay(object, "counts")
        expected <- sweep(counts, 2, sizes, "/") * 1e6
        # normalized table, data table, edgeR library sizes and edgeR cpm
        table <- ScreenR:::normalized_table_of(object)
        expect_equal(as.matrix(table[, -1]), unname(expected),
            ignore_attr = TRUE)
        data_table <- ScreenR:::data_table_of(object)
        expect_equal(data_table$Frequency, as.vector(t(expected)))
        dge <- create_edger_obj(object)
        expect_equal(dge$samples$lib.size * dge$samples$norm.factors,
            unname(sizes))
        expect_equal(edgeR::cpm(dge), expected, ignore_attr = TRUE)
        expect_equal(ScreenR:::log_normalized_counts(object),
            log2(expected[rowSums(expected) > 0, ] + 1))
    }
    expect_false(isTRUE(all.equal(
        ScreenR:::data_table_of(cpm)$Frequency,
        ScreenR:::data_table_of(tmm)$Frequency)))
    expect_false(isTRUE(all.equal(compute_explained_variance(cpm),
        compute_explained_variance(tmm))))
})

test_that("replacing the annotation changes the gene mappings", {
    object <- normalized("cpm")
    annotation <- screen$annotation
    annotation$Gene <- ifelse(annotation$Gene == "N1", "Renamed",
        annotation$Gene)
    renamed <- set_annotation_table(object, annotation)
    expect_true("Renamed" %in% ScreenR:::data_table_of(renamed)$Gene)
    expect_false("N1" %in% ScreenR:::data_table_of(renamed)$Gene)
    expect_true("Renamed" %in% create_edger_obj(renamed)$genes$Gene)
    expect_true("Renamed" %in% barcodes_per_gene(renamed)$Gene)
    expect_true("Renamed" %in%
        compute_metrics(renamed, "CTRL", "TRT", "D2")$Gene)

    # rowData<- directly works the same way
    row_data <- SummarizedExperiment::rowData(object)
    row_data$Gene[row_data$Gene == "N2"] <- "Direct"
    SummarizedExperiment::rowData(object) <- row_data
    expect_true("Direct" %in% ScreenR:::data_table_of(object)$Gene)
    expect_true("Direct" %in% create_edger_obj(object)$genes$Gene)
})

test_that("the replacement methods reject inconsistent objects", {
    object <- normalized("cpm")
    row_data <- SummarizedExperiment::rowData(object)
    row_data$Barcode <- rev(row_data$Barcode)
    expect_error(SummarizedExperiment::rowData(object) <- row_data,
        "Barcode column")
    # A rowData without Barcode gets it from the row names
    SummarizedExperiment::rowData(object) <- row_data[, "Gene", drop = FALSE]
    expect_identical(SummarizedExperiment::rowData(object)$Barcode,
        rownames(object))

    expect_error(object$EffectiveLibSize <- NULL, "effective library sizes")
    expect_error(object$EffectiveLibSize <- -1, "positive")

    # Renaming the barcodes keeps the Barcode column equal to the row names
    renamed <- object
    rownames(renamed) <- paste0("b", rownames(object))
    expect_identical(SummarizedExperiment::rowData(renamed)$Barcode,
        rownames(renamed))
    expect_error(rownames(renamed) <- rep("x", nrow(renamed)), "Duplicated")
})

test_that("subsetting keeps the normalization of the retained samples", {
    object <- normalized("control")
    sizes <- get_normalization(object)$effective_library_size
    subset <- object[1:100, c("D2_TRT_B", "D1_CTRL_A")]
    expect_equal(get_normalization(subset)$effective_library_size,
        sizes[c("D2_TRT_B", "D1_CTRL_A")])
    expect_equal(get_normalization(subset)$method, "control")
    expect_equal(get_normalization(subset)$reference, screen$controls)
    expect_equal(ScreenR:::normalized_counts(subset),
        ScreenR:::normalized_counts(object)[1:100,
            c("D2_TRT_B", "D1_CTRL_A")])
    # remove_all_zero_row is a subset too
    expect_equal(get_normalization(remove_all_zero_row(object)),
        get_normalization(object))
    # Normalizing again after filtering is explicit and changes the sizes
    renormalized <- normalize_data(object[1:100, ], "cpm")
    expect_equal(unname(get_normalization(renormalized)$effective_library_size),
        unname(colSums(SummarizedExperiment::assay(object, "counts")[1:100, ])))
})


# --------------------------------------------------------------------------
# Deliberate behaviour with problematic inputs
# --------------------------------------------------------------------------

test_that("invalid inputs give informative errors", {
    table <- screen$count_table[1:20, ]
    annotation <- screen$annotation[1:20, ]
    build <- function(table, annotation = NULL, ...) {
        suppressMessages(create_screenr_object(table = table,
            annotation = annotation, ...))
    }
    # Duplicated identifiers, also after converting numbers to text
    duplicated <- table
    duplicated$Barcode[2] <- duplicated$Barcode[1]
    expect_error(build(duplicated), "duplicated barcodes")
    mixed <- table
    mixed$Barcode <- c(as.numeric(table$Barcode[1]), table$Barcode[-1])
    mixed$Barcode[2] <- table$Barcode[1]
    expect_error(build(mixed), "duplicated barcodes")
    expect_equal(nrow(build(duplicated, duplicate_barcodes = "sum")), 19L)
    expect_equal(
        unname(SummarizedExperiment::assay(build(duplicated,
            duplicate_barcodes = "sum"))[1, ]),
        unname(unlist(table[1, -1]) + unlist(table[2, -1])))
    # Missing identifiers and invalid counts
    missing_id <- table
    missing_id$Barcode[3] <- NA
    expect_error(build(missing_id), "missing or empty")
    with_na <- table
    with_na[4, 2] <- NA
    expect_error(build(with_na), "missing values")
    negative <- table
    negative[4, 2] <- -1
    expect_error(build(negative), "negative")
    infinite <- table
    infinite[4, 2] <- Inf
    expect_error(build(infinite), "infinite")
    # Conflicting annotation of the same barcode
    conflicting <- rbind(annotation, data.frame(Barcode = annotation$Barcode[1],
        Gene = "Other"))
    expect_error(build(table, conflicting), "different annotations")
    expect_warning(object <- build(table, conflicting,
        duplicate_annotation = "first"), "first row")
    expect_equal(SummarizedExperiment::rowData(object)$Gene[1],
        annotation$Gene[1])
    # Exact copies of an annotation row are not a conflict
    expect_silent(build(table, rbind(annotation, annotation[1, ])))
})

test_that("normalization problems give informative errors", {
    object <- screen$object
    # A sample without reads
    empty <- object
    counts <- SummarizedExperiment::assay(empty, "counts")
    counts[, "D1_CTRL_A"] <- 0
    SummarizedExperiment::assay(empty, "counts") <- counts
    expect_error(normalize_data(empty), "D1_CTRL_A have no reads")
    expect_error(normalize_data(empty, "tmm"), "no reads")
    # Control barcodes: none given, some missing, none present, no reads
    expect_error(normalize_data(object, "control"), "control_barcodes must")
    expect_warning(partial <- normalize_data(object, "control",
        control_barcodes = c(screen$controls, "absent1", "absent2")),
        "2 of the 242 control barcodes are not in the object")
    expect_equal(get_normalization(partial)$reference, screen$controls)
    expect_error(normalize_data(object, "control",
        control_barcodes = c("absent1", "absent2")), "None of the")
    no_control_reads <- object
    counts <- SummarizedExperiment::assay(object, "counts")
    counts[screen$controls[1:3], "D2_TRT_A"] <- 0
    SummarizedExperiment::assay(no_control_reads, "counts") <- counts
    expect_error(normalize_data(no_control_reads, "control",
        control_barcodes = screen$controls[1:3]),
        "no reads in sample\\(s\\) D2_TRT_A")
    # Operations that need normalized counts say so; raw-count quality
    # control does not need them
    expect_error(compute_metrics(object, "CTRL", "TRT", "D2"),
        "normalize_data")
    expect_error(find_camera_hit(object, create_design_matrix(object),
        design_contrast(object)$contrast), "normalize_data")
    expect_s3_class(compute_gini(object), "tbl_df")
    expect_s3_class(mapped_reads(object), "tbl_df")
    expect_s3_class(barcode_lost(object), "tbl_df")
})

test_that("empty subsets behave deliberately", {
    object <- normalized("cpm")
    no_rows <- object[0, ]
    expect_equal(dim(no_rows), c(0L, 8L))
    expect_equal(get_normalization(no_rows)$effective_library_size,
        get_normalization(object)$effective_library_size)
    expect_equal(nrow(ScreenR:::data_table_of(no_rows)), 0L)
    expect_error(create_edger_obj(no_rows), "no barcodes")
    expect_error(normalize_data(no_rows), "no barcodes")
    no_columns <- object[, 0]
    expect_equal(dim(no_columns), c(nrow(object), 0L))
    expect_error(normalize_data(no_columns), "no samples")
})


# --------------------------------------------------------------------------
# Agreement with direct edgeR and limma workflows
# --------------------------------------------------------------------------

for (method in c("cpm", "tmm", "control")) {
    test_that(paste("ScreenR agrees with edgeR and limma,", method), {
        object <- normalized(method)
        y <- direct_dge(object, method)
        dc <- design_contrast(object)
        design <- dc$design
        contrast <- dc$contrast
        y <- edgeR::estimateDisp(y, design)
        index <- gene_index(y$genes$Gene, 3)

        # compute_barcode_statistics: QL F-test
        test <- edgeR::glmQLFTest(edgeR::glmQLFit(y, design),
            contrast = contrast)
        statistics <- compute_barcode_statistics(object, design, contrast)
        expect_equal(statistics$Log2FC, test$table$logFC, tolerance = 1e-6)
        expect_equal(statistics$PValue, test$table$PValue, tolerance = 1e-6)

        # find_camera_hit: one-sided p-values from limma::camera
        camera <- limma::camera(y, index, design, contrast)
        result <- find_camera_hit(object, design, contrast, fdr = 1,
            direction = "Down")
        direct <- camera[result$Gene, ]
        expect_true(all(direct$Direction == "Down"))
        expect_equal(result$PValue, direct$PValue / 2, tolerance = 1e-6)

        # find_roast_hit: FRY and ROAST
        fry <- limma::fry(y, index, design, contrast)
        result <- find_roast_hit(object, design, contrast, method = "fry",
            fdr = 1, direction = "Down")
        expect_equal(result$PValue, fry[result$Gene, "PValue"] / 2,
            tolerance = 1e-6)
        # ROAST is random: the same sets, in the same order, and the same seed
        roast_index <- ScreenR:::unique_gene_symbols(y$genes$Gene, 3)
        set.seed(7)
        roast <- limma::mroast(y, roast_index, design, contrast, nrot = 199)
        result <- find_roast_hit(object, design, contrast, nrot = 199,
            seed = 7, fdr = 1, direction = "Down")
        expect_equal(result$PValue, roast[result$Gene, "PValue"] / 2,
            tolerance = 1e-6)

        # Gene log2 fold changes, filter_by_variance and plot_barcode_hit
        # come from the edgeR negative binomial GLM
        lrt <- edgeR::glmLRT(edgeR::glmFit(y, design), contrast = contrast)
        expect_equal(result$Log2FC,
            as.numeric(tapply(lrt$table$logFC, y$genes$Gene,
                stats::median)[result$Gene]), tolerance = 1e-6)
        filtered <- filter_by_variance(object, genes = "N7",
            matrix_model = design, contrast = contrast, variance = Inf)
        expect_equal(unique(filtered$Variance),
            stats::var(lrt$table$logFC[y$genes$Gene == "N7"]),
            tolerance = 1e-6)
        plot <- plot_barcode_hit(object, design, contrast, gene = "N7")
        expect_equal(plot$data$Log2FC, sort(lrt$table$logFC),
            tolerance = 1e-6)

        # find_time_hit: camera on the time-course model
        info <- get_sample_table(object)
        time <- match(info$Day, c("D1", "D2")) - 1
        treated <- as.numeric(info$Treatment == "TRT")
        time_design <- cbind(Intercept = 1, Time = time, Treated = treated,
            TimeTreated = time * treated)
        y_time <- edgeR::estimateDisp(direct_dge(object, method), time_design)
        camera_time <- limma::camera(y_time, index, time_design,
            "TimeTreated")
        result <- find_time_hit(object, treatment = "TRT", control = "CTRL",
            days = c("D1", "D2"), fdr = 1)
        expect_equal(result$PValue,
            camera_time[result$Gene, "PValue"] / 2, tolerance = 1e-6)

        # compute_metrics and the data table use the same normalized counts
        metrics <- compute_metrics(object, "CTRL", "TRT", "D2")
        normalized_cpm <- edgeR::cpm(direct_dge(object, method))
        treated_mean <- rowMeans(normalized_cpm[, c("D2_TRT_A", "D2_TRT_B")])
        control_mean <- rowMeans(normalized_cpm[, c("D2_CTRL_A",
            "D2_CTRL_B")])
        barcodes <- as.character(metrics$Barcode)
        expect_equal(metrics$Log2FC,
            unname(log2((treated_mean[barcodes] + 0.5) /
                (control_mean[barcodes] + 0.5))), tolerance = 1e-8)
    })
}

test_that("the TMM sizes of the plots and of the models agree", {
    object <- normalized("tmm")
    y <- edgeR::normLibSizes(edgeR::DGEList(
        SummarizedExperiment::assay(object, "counts")))
    sizes <- get_normalization(object)$effective_library_size
    expect_equal(unname(sizes), y$samples$lib.size * y$samples$norm.factors)
    dge <- create_edger_obj(object)
    expect_equal(dge$samples$lib.size * dge$samples$norm.factors,
        unname(sizes))
    # Applying TMM again would be a second normalization: the factors of
    # the DGEList are 1, so that the effective sizes are used once
    expect_equal(dge$samples$norm.factors, rep(1, ncol(object)))
    expect_equal(unname(get_normalization(object)$norm_factors),
        y$samples$norm.factors)
})


# --------------------------------------------------------------------------
# The normalization changes plots and inference as intended
# --------------------------------------------------------------------------

test_that("TMM and control normalization correct a compositional imbalance", {
    null_log2fc <- function(object) {
        dc <- design_contrast(object)
        statistics <- compute_barcode_statistics(object, dc$design,
            dc$contrast)
        stats::median(statistics$Log2FC[statistics$Gene %in%
            screen$null_genes])
    }
    null_ratio <- function(object) {
        metrics <- compute_metrics(object, "CTRL", "TRT", "D2")
        stats::median(metrics$Log2FC[metrics$Gene %in% screen$null_genes])
    }
    enriched_null_barcodes <- function(object) {
        dc <- design_contrast(object)
        statistics <- compute_barcode_statistics(object, dc$design,
            dc$contrast)
        null <- statistics$Gene %in% screen$null_genes
        sum(statistics$FDR[null] < 0.05 & statistics$Log2FC[null] > 0)
    }
    cpm <- normalized("cpm")
    tmm <- normalized("tmm")
    control <- normalized("control")

    # With CPM the depleted hits push every other barcode up, by about
    # log2(1 / (1 - 0.2 * 7 / 8)) = 0.28. The control barcodes, known to be
    # without effect, remove the bias. TMM reduces it: with changes that all
    # go in one direction its trimmed mean is biased towards no correction,
    # so it corrects about half of it here.
    expect_gt(null_log2fc(cpm), 0.2)
    expect_lt(abs(null_log2fc(control)), 0.05)
    expect_lt(null_log2fc(tmm), null_log2fc(cpm) - 0.08)
    # The descriptive statistics (and so the plots) follow the same
    # normalization
    expect_gt(null_ratio(cpm), 0.2)
    expect_lt(abs(null_ratio(control)), 0.05)
    expect_lt(null_ratio(tmm), null_ratio(cpm) - 0.08)
    # And so does the inference: with CPM many barcodes without effect are
    # called enriched
    expect_gt(enriched_null_barcodes(cpm), 20)
    expect_lt(enriched_null_barcodes(tmm), enriched_null_barcodes(cpm) / 2)
    expect_lte(enriched_null_barcodes(control), 3)
    # The depleted genes are found with every normalization
    for (object in list(cpm, tmm, control)) {
        dc <- design_contrast(object)
        found <- find_roast_hit(object, dc$design, dc$contrast,
            method = "fry", fdr = 0.05, direction = "Down")$Gene
        expect_gt(mean(screen$hit_genes %in% found), 0.9)
    }
    # The sample plots change too
    expect_false(isTRUE(all.equal(plot_mds(cpm)$data, plot_mds(tmm)$data)))
})


# --------------------------------------------------------------------------
# Objects of older versions
# --------------------------------------------------------------------------

test_that("objects of the 1.15.8-1.15.15 layout are converted", {
    counts <- SummarizedExperiment::assay(screen$object, "counts")[1:40, ]
    rownames(counts)[2] <- "*.1"
    normalized_counts <- sweep(counts, 2, colSums(counts), "/") * 1e6
    se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = counts, normalized = normalized_counts),
        rowData = S4Vectors::DataFrame(Barcode = c(rownames(counts)[1], "*",
            rownames(counts)[-(1:2)]),
            Gene = screen$annotation$Gene[1:40]),
        colData = SummarizedExperiment::colData(screen$object)[,
            c("Sample", "Day", "Treatment", "Group")])
    rownames(se) <- rownames(counts)
    SummarizedExperiment::rowData(se)$Barcode <- rownames(counts)
    old <- methods::new("screenr_object", se,
        annotation_table = screen$annotation[1:45, ], replicates = c(""),
        data_table = data.frame(Barcode = "x", Frequency = 1),
        column_names = list())
    # Those versions made duplicated barcodes unique only in the row names
    # ("*" and "*.1"); saved objects are read without validation
    old@elementMetadata$Barcode[2] <- "*"
    expect_message(object <- update_screenr_object(old), "normalize_data")
    expect_equal(SummarizedExperiment::assayNames(object), "counts")
    expect_null(get_normalization(object))
    expect_equal(nrow(object@data_table), 0L)
    expect_identical(unname(SummarizedExperiment::assay(object, "counts")),
        unname(counts))
    expect_identical(rownames(object), rownames(counts))
    expect_equal(object$Treatment, screen$object$Treatment)
    expect_equal(get_groups(object), get_groups(screen$object))
    # Annotation rows of barcodes that are not in the counts are kept apart
    annotation <- suppressMessages(get_annotation_table(object))
    expect_true(all(screen$annotation$Barcode[1:45] %in% annotation$Barcode))
    expect_false(anyDuplicated(annotation$Barcode) > 0)
    expect_identical(update_screenr_object(object), object)
    # The analyses ask for an explicit normalization
    expect_error(suppressMessages(compute_metrics(old, "CTRL", "TRT", "D2")),
        "normalize_data")
    expect_equal(get_normalization(normalize_data(object))$method, "cpm")
})
