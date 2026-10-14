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
    data_table <- build(object)@data_table

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

    expect_equal(build(object)@data_table, build(reference)@data_table)
    expect_equal(
        get_column_names(object),
        c(Barcode = "shRNA_ID", Gene = "gene_symbol")
    )
    expect_equal(
        create_edger_obj(object)$samples$lib.size,
        create_edger_obj(reference)$samples$lib.size
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
    data_table <- build(object)@data_table
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
    expect_true(is.numeric(object@count_table$Time1))
    expect_false("Gene" %in% colnames(object@count_table))
})

test_that("unmapped and duplicated barcodes give warnings", {
    counts <- count_table
    colnames(counts) <- c("Barcode", sample_names)
    expect_warning(
        expect_warning(
            suppressMessages(create_screenr_object(
                table = counts, annotation = annotation_table,
                groups = input_groups
            )),
            "duplicated"
        ),
        "not in the annotation"
    )
})

test_that("objects created before the new slots still work", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    expect_false(methods::.hasSlot(object, "sample_table"))
    expect_output(show(object), "count_table")
    object <- set_annotation_table(object, object@annotation_table)
    expect_equal(get_column_names(object), c(Barcode = "Barcode",
        Gene = "Gene"))
    expect_equal(nrow(get_sample_table(object)), ncol(object@count_table) - 1)
    expect_s3_class(compute_data_table(object)@data_table, "tbl_df")
})
