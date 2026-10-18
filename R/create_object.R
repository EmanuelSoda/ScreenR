#' @title Create a ScreenR object
#' @description Creates the ScreenR object, the starting point of every
#'     analysis, from a count table and an annotation table. The object stores
#'     the counts, the annotation, the groups of the samples and, later, the
#'     normalized counts and the data table.
#' @details The tables can use any column names: the names of the barcode and
#'     gene columns are given with barcode_column and gene_column, and the
#'     object renames them internally to Barcode and Gene (the original names
#'     are kept, see \code{\link{get_column_names}}). When a column is not
#'     found, the error suggests the most similar column name.
#'
#'     The input is checked when the object is created:
#'     \itemize{
#'       \item the barcode identifiers are converted to character (numbers
#'             to their decimal form: 10208 and "10208" are the same barcode)
#'             and become the row names of the counts; they never become a
#'             count column. Missing or empty identifiers are an error;
#'       \item sample columns stored as text but containing only numbers are
#'             converted to numbers;
#'       \item other non-numeric columns of the count table (for example a
#'             gene column) are removed with a message, since they are not
#'             samples;
#'       \item missing, infinite or negative counts are an error;
#'       \item duplicated barcodes are an error, unless
#'             duplicate_barcodes says how to combine them;
#'       \item barcodes annotated more than once with different values are
#'             an error, unless duplicate_annotation = "first" (rows that are
#'             exact copies are merged silently);
#'       \item groups must have one value for each sample;
#'       \item barcodes missing from the annotation (for example the
#'             unmapped reads "*") give a warning.
#'     }
#'
#'     The object stores the raw counts as \code{assay(object, "counts")},
#'     the annotation of the barcodes in its rowData and the sample
#'     information in its colData (see \code{\link{screenr_object-class}}).
#'     Annotation rows of barcodes that are not in the count table are kept
#'     apart and returned by \code{\link{get_annotation_table}}.
#'
#'     The Day (time point) and the Treatment of each sample are used by
#'     several functions. By default they are taken from the sample names,
#'     which must have the form Day_Treatment_Replicate (for example
#'     Time3_TRT_A). When the sample names have another form, give them
#'     explicitly with sample_table, and check the result with
#'     \code{\link{get_sample_table}}.
#' @param table The count table obtained from the read alignment, with one
#'     row for each barcode and one column for each sample, plus the barcode
#'     column.
#' @param annotation The annotation table, with one row for each barcode and
#'     at least the barcode column and the gene column (the gene targeted by
#'     the shRNA).
#' @param groups A factor with the experimental group of each sample (for
#'     example "Control" and "Treated"), with one value for each sample
#'     (column of the count table other than the barcode column). It is used
#'     by the edgeR/limma based methods, see
#'     \code{\link{create_design_matrix}}.
#' @param replicates An optional vector with the replicate labels. When it
#'     has one value for each sample, it is stored in the Replicate column of
#'     the colData. Default c("").
#' @param barcode_column The name of the column containing the barcode
#'     identifiers, in both the count and the annotation table. Default
#'     "Barcode".
#' @param gene_column The name of the column of the annotation table
#'     containing the gene names. Default "Gene".
#' @param sample_table An optional data frame with one row for each sample
#'     and columns giving the sample name, the Day (time point) and the
#'     Treatment of the sample. When given, these values are used instead of
#'     the ones derived from the sample names.
#' @param sample_columns A named character vector giving the names of the
#'     columns of sample_table that contain the sample names, the Day and the
#'     Treatment. Default c(Sample = "Sample", Day = "Day",
#'     Treatment = "Treatment").
#' @param duplicate_barcodes What to do when a barcode appears more than once
#'     in the count table: "error" (default), "sum" (add up the counts of the
#'     copies) or "first" (keep the first copy).
#' @param duplicate_annotation What to do when a barcode is annotated more
#'     than once with different values: "error" (default) or "first" (keep
#'     the first row).
#' @param palettes Optional colors for the values of the sample variables
#'     (Sample, Day, Treatment, Group, Replicate or other columns of
#'     sample_table): a named list with one vector of colors for each
#'     variable, for example
#'     \code{list(Treatment = c(CTRL = "#1B9E77", TRT = "#D95F02"))}. All the
#'     plots that color the samples by one of these variables then use these
#'     colors, so that they are consistent across the analysis (see
#'     \code{\link{get_palettes}}).
#' @importFrom rlang .data
#' @importFrom methods new
#' @importFrom tibble tibble
#' @concept objects
#' @return A ScreenR object (class
#'     \code{\link[=screenr_object-class]{screenr_object}}), not normalized:
#'     run \code{\link{normalize_data}} before the analyses that need
#'     normalized counts.
#' @seealso \code{\link{normalize_data}}, \code{\link{compute_data_table}},
#'     \code{\link{get_sample_table}}, \code{\link{get_count_table}} and the
#'     other accessors.
#' @export
#' @examples
#' count_table <-
#'     data.frame(
#'         Barcode = c("Code_1", "Code_2", "Code_3", "Code_4"),
#'         Time3_CTRL_rep1 = c(3520, 3020, 1507, 1400),
#'         Time3_CTRL_rep2 = c(3500, 3000, 1457, 1490),
#'         Time3_TRT_rep1 = c(1200, 1100, 1300, 1350),
#'         Time3_TRT_rep2 = c(1250, 1000, 1400, 1375)
#'     )
#' annotation_table <-
#'     data.frame(
#'         Gene = c("Gene_1", "Gene_1", "Gene_2", "Gene_2"),
#'         Barcode = c("Code_1", "Code_2", "Code_3", "Code_4")
#'     )
#'
#' groups <- factor(c("Control", "Control", "Treated", "Treated"))
#' obj <- create_screenr_object(
#'     table = count_table,
#'     annotation = annotation_table,
#'     groups = groups, replicates = c("")
#' )
#' obj
#'
#' # Tables with other column names and sample names without the
#' # Day_Treatment_Replicate form
#' counts <- count_table
#' colnames(counts) <- c("shRNA", "c1", "c2", "t1", "t2")
#' annotation <- data.frame(
#'     symbol = annotation_table$Gene,
#'     shRNA = annotation_table$Barcode
#' )
#' samples <- data.frame(
#'     id = c("c1", "c2", "t1", "t2"),
#'     time = "Day3",
#'     condition = c("CTRL", "CTRL", "TRT", "TRT")
#' )
#' obj <- create_screenr_object(
#'     table = counts, annotation = annotation, groups = groups,
#'     barcode_column = "shRNA", gene_column = "symbol",
#'     sample_table = samples,
#'     sample_columns = c(Sample = "id", Day = "time",
#'         Treatment = "condition")
#' )
#' get_sample_table(obj)
#' get_column_names(obj)
create_screenr_object <- function(table = NULL, annotation = NULL,
                                  groups = NULL, replicates = c(""),
                                  barcode_column = "Barcode",
                                  gene_column = "Gene",
                                  sample_table = NULL,
                                  sample_columns = c(
                                      Sample = "Sample", Day = "Day",
                                      Treatment = "Treatment"
                                  ),
                                  duplicate_barcodes = c("error", "sum",
                                      "first"),
                                  duplicate_annotation = c("error",
                                      "first"),
                                  palettes = NULL) {
    duplicate_barcodes <- match.arg(duplicate_barcodes)
    duplicate_annotation <- match.arg(duplicate_annotation)
    column_names <- list(Barcode = barcode_column, Gene = gene_column)

    if (!is.null(table)) {
        table <- check_count_table(table, barcode_column,
            duplicate_barcodes)
        samples <- setdiff(colnames(table), "Barcode")

        if (!is.null(groups) && length(groups) != length(samples)) {
            stop(
                "groups has ", length(groups), " values but the count table ",
                "has ", length(samples), " samples (", 
                paste(utils::head(samples, 5), collapse = ", "),
                if (length(samples) > 5) ", ..." else "", "). groups must ",
                "have one value for each sample.",
                call. = FALSE
            )
        }
        if (!is.null(annotation)) {
            annotation <- check_annotation_table(
                annotation, barcode_column, gene_column, duplicate_annotation
            )
            missing <- setdiff(
                as.character(table$Barcode), as.character(annotation$Barcode)
            )
            if (length(missing) > 0) {
                warning(
                    length(missing), " barcode(s) of the count table are ",
                    "not in the annotation table (",
                    paste(utils::head(missing, 5), collapse = ", "),
                    if (length(missing) > 5) ", ..." else "", "). Remove ",
                    "them (for example the unmapped reads \"*\") before the ",
                    "edgeR based analyses.",
                    call. = FALSE
                )
            }
        }
        if (!is.null(sample_table)) {
            sample_table <- check_sample_table(
                sample_table, sample_columns, samples
            )
            column_names <- c(column_names, as.list(sample_columns))
        } else {
            message_unparsed_samples(samples)
        }
    } else if (!is.null(sample_table)) {
        stop("sample_table can only be given together with table.",
            call. = FALSE
        )
    }

    object <- build_screenr_object(
        count_table = table,
        annotation_table = annotation,
        groups = groups,
        replicates = replicates,
        sample_table = sample_table,
        column_names = column_names
    )
    if (!is.null(palettes)) {
        object <- set_palettes(object, palettes)
    }
    return(object)
}

#' @title Get the sample table
#' @description Returns the Day (time point) and the Treatment of each sample,
#'     as used by the analysis. They come from the sample_table given to
#'     \code{\link{create_screenr_object}} or, when it was not given, from the
#'     sample names (Day_Treatment_Replicate). Use this function to check that
#'     the samples are interpreted correctly, and to find the values to give
#'     to the day, treatment and control arguments of functions such as
#'     \code{\link{compute_metrics}} and \code{\link{find_time_hit}}.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A tibble with the columns Sample, Day and Treatment (and any other
#'     column of the sample_table).
#' @seealso \code{\link{create_screenr_object}}, \code{\link{get_column_names}}
#' @concept objects
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' get_sample_table(object)
get_sample_table <- function(screenR_Object) {
    sample_table_of(update_screenr_object(screenR_Object))
}

#' @title Get the original column names
#' @description Returns the names that the columns had in the tables given to
#'     \code{\link{create_screenr_object}}. Internally the barcode and gene
#'     columns are always called Barcode and Gene, and the sample information
#'     Sample, Day and Treatment.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A named character vector: the names are the internal column names
#'     and the values the original ones.
#' @seealso \code{\link{create_screenr_object}}, \code{\link{get_sample_table}}
#' @concept objects
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' get_column_names(object)
get_column_names <- function(screenR_Object) {
    column_names <- update_screenr_object(screenR_Object)@column_names
    if (length(column_names) > 0) {
        return(unlist(column_names))
    }
    c(Barcode = "Barcode", Gene = "Gene")
}

#' @title Parse Sample Names
#' @description Internal function that derives the Day and the Treatment of
#'              each sample from names of the form Day_Treatment_Replicate.
#' @param samples The sample names.
#' @importFrom stringr str_split_fixed
#' @return A tibble with the columns Sample, Day and Treatment.
#' @keywords internal
parse_sample_names <- function(samples) {
    tibble::tibble(
        Sample = samples,
        Day = stringr::str_split_fixed(samples, pattern = "_", n = 3)[, 1],
        Treatment = gsub(".*_", "", gsub("(.*)_\\w+", "\\1", samples))
    )
}

#' @title Message Unparsed Samples
#' @description Internal function that informs the user when some sample
#'              names do not have the form Day_Treatment_Replicate, so that
#'              their Day and Treatment are the whole sample name.
#' @param samples The sample names.
#' @return NULL, invisibly.
#' @keywords internal
message_unparsed_samples <- function(samples) {
    parsed <- parse_sample_names(samples)
    unparsed <- parsed$Sample[parsed$Day == parsed$Treatment]
    if (length(unparsed) > 0) {
        message(
            "The Day and the Treatment of each sample are taken from the ",
            "sample names (Day_Treatment_Replicate). For ",
            length(unparsed), " sample(s) (",
            paste(utils::head(unparsed, 5), collapse = ", "),
            if (length(unparsed) > 5) ", ..." else "",
            ") they cannot be separated and are both set to ",
            "the first part of the name. If this is not intended, give them ",
            "with the sample_table argument. Check them with ",
            "get_sample_table()."
        )
    }
    invisible(NULL)
}

#' @title Find a Column
#' @description Internal function that returns the name of a column of a
#'              table, or stops with an error that suggests the most
#'              similar column name and the argument to use.
#' @param table The table.
#' @param wanted The column name given by the user.
#' @param default The default (internal) column name.
#' @param table_name The name of the table, for the error message.
#' @param argument The name of the argument, for the error message.
#' @return The column name.
#' @keywords internal
find_column <- function(table, wanted, default, table_name, argument) {
    if (!is.character(wanted) || length(wanted) != 1) {
        stop(argument, " must be a single column name.", call. = FALSE)
    }
    columns <- colnames(table)
    if (wanted %in% columns) {
        return(wanted)
    }
    lower <- tolower(columns)
    candidates <- unique(c(
        columns[lower %in% tolower(c(wanted, default))],
        columns[grepl(tolower(default), lower, fixed = TRUE)],
        columns[grepl(tolower(wanted), lower, fixed = TRUE)],
        agrep(wanted, columns, ignore.case = TRUE, value = TRUE,
            max.distance = 0.2)
    ))
    message <- paste0(
        "The ", table_name, " has no column called '", wanted, "'."
    )
    if (length(candidates) > 0) {
        message <- paste0(
            message, " Did you mean '", candidates[1], "'? Use ", argument,
            " = \"", candidates[1], "\"."
        )
    } else {
        message <- paste0(
            message, " Its columns are: ",
            paste(utils::head(columns, 10), collapse = ", "),
            if (length(columns) > 10) ", ..." else "", ". Use ", argument,
            " to give the name of the column."
        )
    }
    stop(message, call. = FALSE)
}

#' @title Rename a Column
#' @description Internal function that renames a column to its internal
#'              name, checking that the internal name is not already used by
#'              another column.
#' @param table The table.
#' @param from The current column name.
#' @param to The internal column name.
#' @param table_name The name of the table, for the error message.
#' @return The table with the column renamed.
#' @keywords internal
rename_column <- function(table, from, to, table_name) {
    if (from == to) {
        return(table)
    }
    if (to %in% colnames(table)) {
        stop(
            "The ", table_name, " has both a column '", from, "' and a ",
            "column '", to, "'. ScreenR uses the name '", to, "' internally: ",
            "rename or remove the column '", to, "'.",
            call. = FALSE
        )
    }
    colnames(table)[colnames(table) == from] <- to
    table
}

#' @title Check the Count Table
#' @description Internal function that finds and renames the barcode column
#'              of the count table, converts the identifiers to character,
#'              converts sample columns stored as text, removes the other
#'              non-numeric columns, rejects invalid counts and resolves (or
#'              rejects) duplicated barcodes.
#' @param table The count table.
#' @param barcode_column The name of the barcode column.
#' @param duplicate_barcodes "error", "sum" or "first".
#' @return The checked count table, with the barcode column first.
#' @keywords internal
check_count_table <- function(table, barcode_column,
    duplicate_barcodes = "error") {
    if (!is.data.frame(table)) {
        stop("table must be a data frame.", call. = FALSE)
    }
    table <- tibble::as_tibble(table)
    column <- find_column(
        table, barcode_column, "Barcode", "count table", "barcode_column"
    )
    table <- rename_column(table, column, "Barcode", "count table")
    table <- table[, c("Barcode", setdiff(colnames(table), "Barcode"))]
    table$Barcode <- as_barcode_id(table$Barcode,
        "barcode identifiers of the count table")

    removed <- character()
    for (sample in setdiff(colnames(table), "Barcode")) {
        values <- table[[sample]]
        if (is.numeric(values)) {
            next
        }
        converted <- suppressWarnings(as.numeric(as.character(values)))
        if (all(is.na(converted) == is.na(values))) {
            table[[sample]] <- converted
            message("Column '", sample, "' of the count table was stored as ",
                "text and has been converted to numbers.")
        } else {
            removed <- c(removed, sample)
        }
    }
    if (length(removed) > 0) {
        table <- table[, setdiff(colnames(table), removed)]
        message(
            "Column(s) ", paste0("'", removed, "'", collapse = ", "),
            " of the count table are not numeric, so they are not samples, ",
            "and have been removed. Put the information on the barcodes in ",
            "the annotation table."
        )
    }
    if (ncol(table) < 2) {
        stop("The count table has no numeric sample columns.", call. = FALSE)
    }
    counts <- as.matrix(table[, -1])
    if (anyNA(counts)) {
        stop("The count table contains missing values (NA): replace them ",
            "with 0 if the barcodes were not observed.", call. = FALSE)
    }
    if (any(!is.finite(counts))) {
        stop("The count table contains infinite values.", call. = FALSE)
    }
    if (any(counts < 0)) {
        stop("The count table contains negative values: ScreenR needs raw ",
            "read counts.", call. = FALSE)
    }
    resolve_duplicated_barcodes(table, duplicate_barcodes)
}

#' @title Resolve Duplicated Barcodes
#' @description Internal function that stops when barcodes appear more than
#'              once in the count table, or combines their rows when a policy
#'              is given.
#' @param table The count table, with a character Barcode column.
#' @param policy "error", "sum" (add up the counts) or "first".
#' @return The count table without duplicated barcodes.
#' @keywords internal
resolve_duplicated_barcodes <- function(table, policy = "error") {
    duplicated_barcodes <- unique(table$Barcode[duplicated(table$Barcode)])
    if (length(duplicated_barcodes) == 0) {
        return(table)
    }
    if (policy == "error") {
        stop(
            "The count table has duplicated barcodes (",
            paste(utils::head(duplicated_barcodes, 5), collapse = ", "),
            if (length(duplicated_barcodes) > 5) ", ..." else "", "). ",
            "Remove them (for example the unmapped reads \"*\"), or say how ",
            "to combine them with duplicate_barcodes = \"sum\" or \"first\".",
            call. = FALSE
        )
    }
    message(length(duplicated_barcodes), " duplicated barcode(s) (",
        paste(utils::head(duplicated_barcodes, 5), collapse = ", "),
        if (length(duplicated_barcodes) > 5) ", ..." else "",
        ") have been combined (duplicate_barcodes = \"", policy, "\").")
    if (policy == "first") {
        return(table[!duplicated(table$Barcode), ])
    }
    order <- unique(table$Barcode)
    summed <- rowsum(as.matrix(table[, -1]), table$Barcode, reorder = FALSE)
    combined <- tibble::as_tibble(summed[order, , drop = FALSE],
        .name_repair = "minimal")
    dplyr::bind_cols(tibble::tibble(Barcode = order), combined)
}

#' @title Check the Annotation Table
#' @description Internal function that finds and renames the barcode and
#'              gene columns of the annotation table, converts the
#'              identifiers to character and resolves (or rejects) barcodes
#'              annotated more than once.
#' @param annotation The annotation table.
#' @param barcode_column The name of the barcode column.
#' @param gene_column The name of the gene column.
#' @param duplicate_annotation "error" or "first".
#' @return The checked annotation table.
#' @keywords internal
check_annotation_table <- function(annotation, barcode_column, gene_column,
    duplicate_annotation = "error") {
    if (!is.data.frame(annotation)) {
        stop("annotation must be a data frame.", call. = FALSE)
    }
    annotation <- tibble::as_tibble(annotation)
    barcode <- find_column(
        annotation, barcode_column, "Barcode", "annotation table",
        "barcode_column"
    )
    gene <- find_column(
        annotation, gene_column, "Gene", "annotation table", "gene_column"
    )
    if (barcode == gene) {
        stop("barcode_column and gene_column must be different columns.",
            call. = FALSE
        )
    }
    annotation <- rename_column(annotation, barcode, "Barcode",
        "annotation table")
    annotation <- rename_column(annotation, gene, "Gene", "annotation table")
    annotation <- annotation_frame(annotation)
    resolve_duplicated_annotation(annotation, duplicate_annotation)
}

#' @title Resolve Duplicated Annotation
#' @description Internal function that merges the rows of the annotation
#'              that are exact copies, and stops when a barcode is annotated
#'              more than once with different values (unless policy is
#'              "first", which keeps the first row of each barcode).
#' @param annotation The annotation table, with a character Barcode column.
#' @param policy "error" or "first".
#' @return The annotation table with one row for each barcode.
#' @keywords internal
resolve_duplicated_annotation <- function(annotation, policy = "error") {
    annotation <- annotation[!duplicated(as.data.frame(annotation)), ,
        drop = FALSE]
    conflicts <- unique(annotation$Barcode[duplicated(annotation$Barcode)])
    if (length(conflicts) == 0) {
        return(annotation)
    }
    if (policy == "error") {
        stop(
            "The annotation table gives different annotations for the same ",
            "barcode(s) (", paste(utils::head(conflicts, 5), collapse = ", "),
            if (length(conflicts) > 5) ", ..." else "", "). Correct the ",
            "table, or keep the first row of each barcode with ",
            "duplicate_annotation = \"first\".", call. = FALSE
        )
    }
    warning(
        "The annotation table gives different annotations for the same ",
        "barcode(s) (", paste(utils::head(conflicts, 5), collapse = ", "),
        if (length(conflicts) > 5) ", ..." else "", "): only the first row ",
        "of each barcode is kept.", call. = FALSE
    )
    annotation[!duplicated(annotation$Barcode), , drop = FALSE]
}

#' @title Check the Sample Table
#' @description Internal function that finds and renames the columns of the
#'              sample table and checks that it describes all the samples of
#'              the count table.
#' @param sample_table The sample table.
#' @param sample_columns The named vector with the names of the Sample, Day
#'                       and Treatment columns.
#' @param samples The sample names of the count table.
#' @return The checked sample table, in the order of the count table.
#' @keywords internal
check_sample_table <- function(sample_table, sample_columns, samples) {
    if (!is.data.frame(sample_table)) {
        stop("sample_table must be a data frame.", call. = FALSE)
    }
    needed <- c("Sample", "Day", "Treatment")
    if (is.null(names(sample_columns)) ||
        !all(needed %in% names(sample_columns))) {
        stop(
            "sample_columns must be a named vector with the names Sample, ",
            "Day and Treatment, for example c(Sample = \"id\", ",
            "Day = \"time\", Treatment = \"condition\").",
            call. = FALSE
        )
    }
    sample_table <- tibble::as_tibble(sample_table)
    for (internal in needed) {
        column <- find_column(
            sample_table, sample_columns[[internal]], internal,
            "sample table", paste0("sample_columns[\"", internal, "\"]")
        )
        sample_table <- rename_column(sample_table, column, internal,
            "sample table")
    }
    sample_table$Sample <- as.character(sample_table$Sample)
    sample_table$Day <- as.character(sample_table$Day)
    sample_table$Treatment <- as.character(sample_table$Treatment)

    missing <- setdiff(samples, sample_table$Sample)
    if (length(missing) > 0) {
        stop(
            "The sample table does not describe the sample(s) ",
            paste(utils::head(missing, 5), collapse = ", "),
            if (length(missing) > 5) ", ..." else "",
            " of the count table.",
            call. = FALSE
        )
    }
    if (anyDuplicated(sample_table$Sample)) {
        stop("The sample table has duplicated samples.", call. = FALSE)
    }
    sample_table <- sample_table[match(samples, sample_table$Sample), ]
    sample_table[, c(needed, setdiff(colnames(sample_table), needed))]
}

#' @title Create an edgeR object
#' @description Converts a ScreenR object into an edgeR
#'     \code{\link[edgeR]{DGEList}}, for example to use the object with edgeR
#'     or limma functions directly. All the edgeR/limma based functions of
#'     ScreenR (\code{\link{find_camera_hit}}, \code{\link{find_roast_hit}},
#'     \code{\link{compute_barcode_statistics}}, \code{\link{find_time_hit}},
#'     \code{\link{filter_by_variance}} and \code{\link{plot_barcode_hit}})
#'     use it, so they all model the counts with the same normalization.
#' @details The DGEList contains the raw counts, as required by the edgeR
#'     negative binomial model (never normalized counts), and the
#'     normalization of the object set by \code{\link{normalize_data}}: the
#'     library sizes (lib.size) are the effective library sizes of the
#'     samples and the normalization factors (norm.factors) are 1. The offsets
#'     of the models, log(lib.size * norm.factors), are therefore the
#'     logarithms of the effective library sizes, the same that give the
#'     normalized counts of the plots and of the data table (raw count /
#'     effective library size * 1e6). For the "tmm" method the effective
#'     library size already includes the TMM factor, so it is not applied a
#'     second time; do not call \code{\link[edgeR]{normLibSizes}} (or
#'     calcNormFactors) on the result, which would change the normalization.
#'
#'     The object must be normalized, and every barcode must be annotated
#'     with a gene. The genes table of the DGEList is the rowData of the
#'     object.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}, normalized with
#'     \code{\link{normalize_data}}.
#' @importFrom  edgeR DGEList
#' @return A \code{\link[edgeR]{DGEList}} with the raw counts, the effective
#'     library sizes, the groups of the samples and the annotation of the
#'     barcodes (genes).
#' @seealso \code{\link{normalize_data}}, \code{\link{create_design_matrix}}
#' @concept objects
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' dge <- create_edger_obj(object)
#' dge$samples
create_edger_obj <- function(screenR_Object) {
    object <- update_screenr_object(screenR_Object)
    sizes <- effective_lib_size(object, "The edgeR/limma based analysis")
    if (nrow(object) == 0 || ncol(object) == 0) {
        stop("The object has no barcodes or no samples.", call. = FALSE)
    }
    # edgeR models raw counts; the normalization enters through the library
    # sizes (offsets)
    counts <- SummarizedExperiment::assay(object, "counts",
        withDimnames = TRUE)
    genes <- as.data.frame(SummarizedExperiment::rowData(object),
        optional = TRUE)
    rownames(genes) <- rownames(object)
    if (!"Gene" %in% colnames(genes) || anyNA(genes$Gene)) {
        stop("Some barcodes of the count table are not in the annotation ",
            "table (no gene).", call. = FALSE)
    }
    groups <- groups_of(object)
    if (length(groups) != ncol(object)) {
        groups <- NULL
    }
    edgeR::DGEList(counts = counts, lib.size = unname(sizes),
        norm.factors = rep(1, ncol(object)), group = groups, genes = genes)
}
