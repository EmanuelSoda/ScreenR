data("count_table", package = "ScreenR")
data("annotation_table", package = "ScreenR")

sample_names <- c(
    "Time1", "Time2", "Time3_TRT_A", "Time3_TRT_B", "Time3_TRT_C",
    "Time3_CTRL_A", "Time3_CTRL_B", "Time3_CTRL_C", "Time4_TRT_A",
    "Time4_TRT_B", "Time4_TRT_C", "Time4_CTRL_A", "Time4_CTRL_B",
    "Time4_CTRL_C"
)
input_groups <- factor(c(
    "T1/T2", "T1/T2", rep("Time3_TRT", 3), rep("Time3_CTRL", 3),
    rep("Time4_TRT", 3), rep("Time4_CTRL", 3)
))

clean_counts <- function() {
    counts <- count_table[count_table$Barcode != "*", ]
    colnames(counts) <- c("Barcode", sample_names)
    counts
}

build <- function(object) {
    compute_data_table(normalize_data(object))
}

test_that("the standard input gives the same data table as before", {
    object <- suppressMessages(create_screenr_object(
        table = clean_counts(), annotation = annotation_table,
        groups = input_groups
    ))
    data_table <- ScreenR:::data_table_of(build(object))

    expect_equal(
        colnames(data_table),
        c("Barcode", "Gene", "Sample", "Frequency", "Sequence", "Library",
            "Gene_ID", "Day", "Treatment")
    )
    expected_day <- stringr::str_split_fixed(
        as.character(data_table$Sample), "_", 3
    )[, 1]
    expect_equal(data_table$Day, expected_day)
    expect_equal(
        unique(data_table$Treatment[data_table$Sample == "Time3_TRT_A"]),
        "TRT"
    )
    expect_equal(get_column_names(object), c(Barcode = "Barcode",
        Gene = "Gene"))
})

test_that("other barcode and gene column names are mapped", {
    counts <- clean_counts()
    colnames(counts)[1] <- "shRNA_ID"
    annotation <- annotation_table
    colnames(annotation)[colnames(annotation) == "Barcode"] <- "shRNA_ID"
    colnames(annotation)[colnames(annotation) == "Gene"] <- "gene_symbol"

    object <- suppressMessages(create_screenr_object(
        table = counts, annotation = annotation, groups = input_groups,
        barcode_column = "shRNA_ID", gene_column = "gene_symbol"
    ))
    reference <- suppressMessages(create_screenr_object(
        table = clean_counts(), annotation = annotation_table,
        groups = input_groups
    ))

    expect_equal(ScreenR:::data_table_of(build(object)),
        ScreenR:::data_table_of(build(reference)))
    expect_equal(
        get_column_names(object),
        c(Barcode = "shRNA_ID", Gene = "gene_symbol")
    )
    expect_equal(
        create_edger_obj(build(object))$samples$lib.size,
        create_edger_obj(build(reference))$samples$lib.size
    )
})

test_that("a missing column gives an error with a suggestion", {
    annotation <- annotation_table
    colnames(annotation)[colnames(annotation) == "Gene"] <- "gene_symbol"
    expect_error(
        create_screenr_object(
            table = clean_counts(), annotation = annotation,
            groups = input_groups
        ),
        "Did you mean 'gene_symbol'\\? Use gene_column = \"gene_symbol\""
    )

    counts <- clean_counts()
    colnames(counts)[1] <- "id"
    expect_error(
        create_screenr_object(table = counts, groups = input_groups),
        "no column called 'Barcode'.*barcode_column"
    )
})

test_that("Sequence, Library and Gene_ID are optional", {
    object <- suppressMessages(create_screenr_object(
        table = clean_counts(),
        annotation = annotation_table[, c("Gene", "Barcode")],
        groups = input_groups
    ))
    data_table <- ScreenR:::data_table_of(build(object))
    expect_equal(
        colnames(data_table),
        c("Barcode", "Gene", "Sample", "Frequency", "Day", "Treatment")
    )
})

test_that("a sample table replaces the sample names convention", {
    counts <- clean_counts()
    new_names <- paste0("s", seq_along(sample_names))
    colnames(counts) <- c("Barcode", new_names)
    parsed <- stringr::str_split_fixed(sample_names, "_", 3)
    samples <- data.frame(
        id = new_names,
        time = parsed[, 1],
        condition = ifelse(parsed[, 2] == "", parsed[, 1], parsed[, 2])
    )
    object <- suppressMessages(create_screenr_object(
        table = counts, annotation = annotation_table, groups = input_groups,
        sample_table = samples[rev(seq_len(nrow(samples))), ],
        sample_columns = c(Sample = "id", Day = "time",
            Treatment = "condition")
    ))

    sample_table <- get_sample_table(object)
    expect_equal(sample_table$Sample, new_names)
    expect_equal(sample_table$Day, parsed[, 1])

    reference <- suppressMessages(create_screenr_object(
        table = clean_counts(), annotation = annotation_table,
        groups = input_groups
    ))
    expect_equal(
        compute_metrics(build(object), control = "CTRL", treatment = "TRT",
            day = "Time3")$Log2FC,
        compute_metrics(build(reference), control = "CTRL",
            treatment = "TRT", day = "Time3")$Log2FC
    )

    expect_error(
        create_screenr_object(
            table = counts, annotation = annotation_table,
            sample_table = samples[-1, ],
            sample_columns = c(Sample = "id", Day = "time",
                Treatment = "condition")
        ),
        "does not describe"
    )
    expect_error(
        create_screenr_object(
            table = counts, sample_table = samples,
            sample_columns = c(Sample = "id", Day = "day",
                Treatment = "condition")
        ),
        "no column called 'day'.*Its columns are"
    )
    expect_error(
        create_screenr_object(
            table = counts, sample_table = samples,
            sample_columns = c(Sample = "ID", Day = "time",
                Treatment = "condition")
        ),
        "Did you mean 'id'"
    )
})

test_that("unparsable sample names give a message", {
    counts <- clean_counts()
    colnames(counts) <- c("Barcode", paste0("s", seq_along(sample_names)))
    expect_message(
        create_screenr_object(table = counts, groups = input_groups),
        "sample_table"
    )
})

test_that("groups must have one value per sample", {
    expect_error(
        suppressMessages(create_screenr_object(
            table = clean_counts(), groups = input_groups[1:5]
        )),
        "one value for each sample"
    )
})

test_that("text counts are converted and other text columns removed", {
    counts <- clean_counts()
    counts$Time1 <- as.character(counts$Time1)
    counts$Gene <- "a gene"
    expect_message(
        object <- create_screenr_object(table = counts, groups = input_groups),
        "converted to numbers"
    )
    expect_true(is.numeric(ScreenR:::count_table_of(object)$Time1))
    expect_false("Gene" %in% colnames(ScreenR:::count_table_of(object)))
})

test_that("duplicated barcodes are an error unless combined", {
    counts <- count_table
    colnames(counts) <- c("Barcode", sample_names)
    expect_error(
        suppressMessages(create_screenr_object(
            table = counts, annotation = annotation_table,
            groups = input_groups
        )),
        "duplicated barcodes \\(\\*\\).*duplicate_barcodes"
    )
    # The three "*" rows are combined into one, which is not annotated
    expect_warning(
        object <- suppressMessages(create_screenr_object(
            table = counts, annotation = annotation_table,
            groups = input_groups, duplicate_barcodes = "sum"
        )),
        "not in the annotation"
    )
    expect_equal(sum(rownames(object) == "*"), 1L)
    expect_equal(nrow(object), nrow(clean_counts()) + 1L)
})

test_that("objects created before the new slots still work", {
    object <- readRDS(test_path("fixtures", "legacy_object.rds"))
    expect_false(methods::.hasSlot(object, "sample_table"))
    expect_output(show(object), "screenr_object")
    annotation <- suppressMessages(get_annotation_table(object))
    object <- suppressMessages(set_annotation_table(object, annotation))
    expect_equal(get_column_names(object), c(Barcode = "Barcode",
        Gene = "Gene"))
    expect_equal(nrow(get_sample_table(object)),
        ncol(ScreenR:::count_table_of(object)) - 1)
    expect_s3_class(
        ScreenR:::data_table_of(compute_data_table(normalize_data(object))),
        "tbl_df")
})

test_that("the object is a SummarizedExperiment with consistent parts", {
    object <- suppressMessages(create_screenr_object(
        table = clean_counts(), annotation = annotation_table,
        groups = input_groups
    ))
    expect_s4_class(object, "SummarizedExperiment")
    expect_s4_class(object, "screenr_object")
    expect_equal(dim(object), c(nrow(clean_counts()), length(sample_names)))
    expect_equal(SummarizedExperiment::assayNames(object), "counts")
    expect_equal(colnames(object), sample_names)
    expect_equal(as.character(SummarizedExperiment::rowData(object)$Gene),
        annotation_table$Gene[match(
            as.character(clean_counts()$Barcode),
            as.character(annotation_table$Barcode)
        )])
    expect_equal(object$Treatment[3], "TRT")
    expect_equal(object$Group, input_groups)

    object <- compute_data_table(normalize_data(object))
    # The normalized counts are not stored: only the raw counts are an assay
    expect_equal(SummarizedExperiment::assayNames(object), "counts")
    expect_false(is.null(object$EffectiveLibSize))

    # Subsetting keeps counts, normalized counts and data table together
    treated <- object[1:50, object$Treatment == "TRT"]
    expect_equal(dim(treated), c(50L, 6L))
    expect_true(all(get_data_table(treated)$Treatment == "TRT"))
    expect_equal(nrow(get_data_table(treated)), 50L * 6L)
    expect_equal(nrow(get_normalized_count_table(treated)), 50L)
    expect_equal(
        as.character(get_groups(treated)),
        rep(c("Time3_TRT", "Time4_TRT"), each = 3)
    )
})

test_that("old objects are converted with the same content", {
    legacy <- readRDS(test_path("fixtures", "legacy_object.rds"))
    expect_message(object <- update_screenr_object(legacy),
        "normalize_data")
    expect_s4_class(object, "SummarizedExperiment")
    expect_identical(update_screenr_object(object), object)
    expected_counts <- tibble::as_tibble(attr(legacy, "count_table"))
    expected_counts$Barcode <- as.character(expected_counts$Barcode)
    expect_equal(ScreenR:::count_table_of(object), expected_counts)
    expect_equal(get_groups(object), attr(legacy, "groups"))
    # The legacy normalization is not recorded, so it is not kept ...
    expect_null(get_normalization(object))
    expect_error(get_data_table(object), "normalize_data")
    # ... and normalizing again with "cpm" gives the legacy values up to one
    # factor per sample (the fixture keeps 300 of the barcodes, while the
    # legacy values are per million reads of all of them)
    data_table <- ScreenR:::data_table_of(normalize_data(object))
    legacy_table <- attr(legacy, "data_table")
    key <- function(table) paste(table$Barcode, table$Sample)
    matched <- match(key(data_table), key(legacy_table))
    expect_false(anyNA(matched))
    ratio <- legacy_table$Frequency[matched] / data_table$Frequency
    used <- data_table$Frequency > 0
    per_sample <- tapply(ratio[used], data_table$Sample[used],
        function(x) diff(range(x)) / mean(x))
    expect_true(all(per_sample < 1e-8))
})

test_that("the setters keep working", {
    object <- suppressMessages(create_screenr_object(
        table = clean_counts(), annotation = annotation_table,
        groups = input_groups
    ))
    counts <- ScreenR:::count_table_of(object)[1:10, ]
    object <- set_count_table(object, counts)
    expect_equal(nrow(object), 10L)
    expect_equal(ScreenR:::count_table_of(object), counts)

    object <- set_groups(object, rev(input_groups))
    expect_equal(get_groups(object), as.factor(rev(input_groups)))

    # A normalized table that scales each sample by one factor is stored as
    # effective library sizes ...
    normalized <- counts
    normalized[, -1] <- as.matrix(counts[, -1]) * 2
    object <- set_normalized_count_table(object, normalized)
    expect_equal(ScreenR:::normalized_table_of(object), normalized,
        ignore_attr = TRUE)
    expect_equal(get_normalization(object)$method, "custom")
    expect_equal(unname(get_normalization(object)$effective_library_size),
        rep(5e5, length(sample_names)))
    # ... any other table is refused
    normalized[1, 2] <- normalized[1, 2] + 1
    expect_error(set_normalized_count_table(object, normalized),
        "single factor")
})
