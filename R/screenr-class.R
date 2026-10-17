#' @include  generics.R

#' @title The ScreenR object class
#' @description The screenr_object class is the main object of the package:
#'     it stores the data of a screen and is passed to the functions of the
#'     analysis. Create it with \code{\link{create_screenr_object}}.
#'
#'     It extends \code{\link[SummarizedExperiment]{SummarizedExperiment}},
#'     which is the single source of truth of the object:
#'     \itemize{
#'       \item \code{assay(object, "counts")}: the raw counts, one row for
#'             each barcode and one column for each sample. They are never
#'             modified by the analysis. The row names are the barcode
#'             identifiers (as character) and the column names the samples.
#'       \item \code{rowData(object)}: the annotation of each barcode (Gene,
#'             ...), aligned with the rows of the counts. Its Barcode column
#'             is kept equal to the row names, for compatibility.
#'       \item \code{colData(object)}: the Sample, Day and Treatment of each
#'             sample (see \code{\link{get_sample_table}}), its Group, its
#'             Replicate (when replicate labels were given for each sample)
#'             and, after \code{\link{normalize_data}}, its effective library
#'             size (EffectiveLibSize).
#'       \item \code{metadata(object)$normalization}: the method of the
#'             normalization and its provenance (see
#'             \code{\link{get_normalization}}).
#'     }
#'     The normalized counts, the data table and the edgeR objects are not
#'     stored: they are computed when needed from the raw counts, the
#'     annotation, the sample information and the effective library sizes, so
#'     they always agree with each other and with the counts (see
#'     \code{\link{normalize_data}}).
#'
#' @details \strong{Changing the object.} The ScreenR setters
#'     (\code{\link{set_count_table}}, \code{\link{set_annotation_table}},
#'     \code{\link{set_groups}}, ...) and the replacement functions of
#'     SummarizedExperiment keep the object consistent:
#'     \itemize{
#'       \item replacing the counts (\code{set_count_table},
#'             \code{assay(object, "counts") <- }, \code{assays(object) <- })
#'             with different values removes the normalization, which must be
#'             computed again with \code{\link{normalize_data}};
#'       \item replacing the annotation (\code{set_annotation_table},
#'             \code{rowData(object) <- }) changes the gene of each barcode
#'             used by all the following analyses;
#'       \item \code{rowData<-}, \code{colData<-} (and \code{$<-}) and
#'             \code{dimnames<-} check the object, and renaming the barcodes
#'             updates the Barcode column of the rowData;
#'       \item subsetting and reordering (\code{object[i, j]}) keep the
#'             effective library sizes of the retained samples: the
#'             normalization is not computed again on the retained barcodes.
#'             Call \code{\link{normalize_data}} after filtering to normalize
#'             on the retained barcodes.
#'     }
#'     Modifying the slots directly (\code{object@assays <- ...}) bypasses
#'     these safeguards and is not supported. Combining objects with
#'     \code{cbind} or \code{rbind} is only meaningful between objects
#'     normalized in the same way; the result is checked by
#'     \code{validObject()}.
#'
#' @slot annotation_table The rows of the annotation table given by the user
#'     whose barcodes are not in the count table (barcodes of the library that
#'     were not observed). The annotation of the observed barcodes is in the
#'     rowData. \code{\link{get_annotation_table}} returns both.
#' @slot replicates The replicate labels, when they were not given for each
#'     sample (sample-specific labels are in the Replicate column of the
#'     colData).
#' @slot data_table Not used any more (the data table is computed when
#'     needed, see \code{\link{get_data_table}}); kept, empty, so that saved
#'     objects can be read.
#' @slot column_names The names that the columns had in the tables given by
#'     the user (see \code{\link{get_column_names}}).
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @return \code{screenr_object()} (the class generator) returns a new
#'     object of class \code{screenr_object}; use
#'     \code{\link{create_screenr_object}} to build one from your data.
#' @exportClass screenr_object
#' @concept objects
#' @seealso \code{\link{create_screenr_object}},
#'     \code{\link{normalize_data}}, \code{\link{get_normalization}},
#'     \code{\link{update_screenr_object}}
#' @examples
#' data("count_table", package = "ScreenR")
#' data("annotation_table", package = "ScreenR")
#'
#' groups <- factor(c(
#'     "T1/T2", "T1/T2", "Treated", "Treated", "Treated",
#'     "Control", "Control", "Control", "Treated", "Treated",
#'     "Treated", "Control", "Control", "Control"
#' ))
#'
#' # The unmapped reads ("*") appear several times: remove them
#' counts <- count_table[count_table$Barcode != "*", ]
#' obj <- create_screenr_object(
#'     table = counts,
#'     annotation = annotation_table,
#'     groups = groups
#' )
#' obj
#' SummarizedExperiment::assay(obj, "counts")[1:3, 1:3]
#' SummarizedExperiment::rowData(obj)
#'
#' # Normalized values are derived from the counts and the normalization
#' obj <- normalize_data(obj, method = "tmm")
#' get_normalization(obj)$effective_library_size
screenr_object <- setClass("screenr_object",
  contains = "SummarizedExperiment",
  representation = methods::representation(
    annotation_table = "data.frame",
    replicates = "vector",
    data_table = "data.frame",
    column_names = "list"
  )
)

# The version of the object layout, stored in the metadata: objects without
# it were created by older versions of ScreenR and are converted by
# update_screenr_object()
screenr_layout_version <- 2L

#' @title Check a ScreenR Object
#' @description Internal validity method of the screenr_object class: the
#'              barcode identifiers must be unique and not missing, the
#'              Barcode column of the rowData must equal them, the counts
#'              must be finite and not negative, and the normalization must be
#'              complete (effective library sizes positive and finite, and
#'              recorded in the metadata).
#' @param object A ScreenR object.
#' @return TRUE or a character vector describing the problems.
#' @keywords internal
check_screenr_object <- function(object) {
    if (is_legacy_screenr(object)) {
        return(TRUE)
    }
    problems <- character()
    if (nrow(object) > 0 || ncol(object) > 0) {
        if (!"counts" %in% SummarizedExperiment::assayNames(object)) {
            return("The object has no 'counts' assay.")
        }
    }
    identifiers <- rownames(object)
    if (nrow(object) > 0) {
        if (is.null(identifiers) || anyNA(identifiers) ||
            any(identifiers == "")) {
            problems <- c(problems, "Some barcode identifiers are missing.")
        } else if (anyDuplicated(identifiers)) {
            problems <- c(problems, paste0("Duplicated barcode identifiers: ",
                paste(utils::head(unique(identifiers[
                    duplicated(identifiers)]), 5), collapse = ", "), "."))
        }
        row_data <- SummarizedExperiment::rowData(object)
        if ("Barcode" %in% colnames(row_data) && !is.null(identifiers) &&
            !identical(as.character(row_data$Barcode), identifiers)) {
            problems <- c(problems, paste("The Barcode column of the rowData",
                "differs from the row names (the barcode identifiers)."))
        }
    }
    if ("counts" %in% SummarizedExperiment::assayNames(object) &&
        length(object) > 0) {
        counts <- SummarizedExperiment::assay(object, "counts",
            withDimnames = FALSE)
        if (!is.numeric(counts)) {
            problems <- c(problems, "The counts must be numeric.")
        } else if (anyNA(counts) || any(!is.finite(counts))) {
            problems <- c(problems,
                "The counts contain missing or infinite values.")
        } else if (any(counts < 0)) {
            problems <- c(problems, "The counts contain negative values.")
        }
    }
    state <- S4Vectors::metadata(object)$normalization
    has_sizes <- "EffectiveLibSize" %in%
        colnames(SummarizedExperiment::colData(object))
    if (!is.null(state) && !has_sizes) {
        problems <- c(problems, paste("The normalization is recorded in the",
            "metadata but the effective library sizes (EffectiveLibSize) are",
            "missing from the colData."))
    }
    if (is.null(state) && has_sizes) {
        problems <- c(problems, paste("The colData has effective library",
            "sizes (EffectiveLibSize) but no normalization is recorded in",
            "the metadata."))
    }
    if (has_sizes) {
        sizes <- SummarizedExperiment::colData(object)$EffectiveLibSize
        if (!is.numeric(sizes) || any(!is.finite(sizes)) ||
            any(sizes <= 0)) {
            problems <- c(problems, paste("The effective library sizes must",
                "be positive and finite."))
        }
    }
    if (length(problems) > 0) problems else TRUE
}

methods::setValidity("screenr_object", check_screenr_object)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                     Barcode identifiers and construction
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Barcode Identifiers
#' @description Internal function that converts barcode identifiers to
#'              character, the type used for the row names of the object.
#'              Factors are converted to their labels and numbers to their
#'              decimal representation, without scientific notation (10208
#'              and "10208" give the same identifier, 1e5 gives "100000").
#' @param x The identifiers.
#' @param what A description of the identifiers, for the error message.
#' @param allow_missing When FALSE (default), missing or empty identifiers
#'                      are an error.
#' @return A character vector.
#' @keywords internal
as_barcode_id <- function(x, what = "barcode identifiers",
    allow_missing = FALSE) {
    if (is.factor(x)) {
        x <- as.character(x)
    }
    if (is.numeric(x)) {
        whole <- !is.na(x) & is.finite(x) & x == round(x)
        identifiers <- as.character(x)
        identifiers[whole] <- formatC(x[whole], format = "f", digits = 0,
            big.mark = "")
        x <- identifiers
    }
    x <- as.character(x)
    missing <- is.na(x) | x == ""
    if (!allow_missing && any(missing)) {
        stop(sum(missing), " of the ", what, " are missing or empty.",
            call. = FALSE)
    }
    x
}

#' @title Build a ScreenR Object
#' @description Internal function that builds a screenr_object from tables
#'              already checked by \code{\link{create_screenr_object}}: the
#'              counts become the counts assay (with the barcode identifiers
#'              as row names), the annotation of the observed barcodes the
#'              rowData and the sample information the colData.
#' @param count_table A table with a Barcode column and one numeric column
#'                    for each sample, without duplicated barcodes.
#' @param annotation_table The annotation table, with a Barcode column and no
#'                         conflicting duplicated barcodes, or NULL.
#' @param groups The groups of the samples.
#' @param replicates The replicate labels: one for each sample, or any other
#'                   value (kept for compatibility).
#' @param sample_table An optional sample table with the columns Sample, Day
#'                     and Treatment, in the order of the samples; when NULL
#'                     the sample names are parsed.
#' @param column_names The original column names.
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @return A screenr_object.
#' @keywords internal
build_screenr_object <- function(count_table = NULL, annotation_table = NULL,
    groups = NULL, replicates = c(""), sample_table = NULL,
    column_names = list()) {
    groups <- as.factor(groups)
    metadata <- list(screenr_layout = screenr_layout_version)
    if (is.null(count_table)) {
        se <- SummarizedExperiment::SummarizedExperiment(
            assays = list(counts = matrix(0, nrow = 0, ncol = 0)),
            metadata = metadata
        )
        if (length(groups) > 0) S4Vectors::metadata(se)$groups <- groups
        return(methods::new("screenr_object", se,
            annotation_table = annotation_frame(annotation_table),
            replicates = replicates, data_table = tibble(),
            column_names = column_names))
    }
    identifiers <- as_barcode_id(count_table$Barcode)
    samples <- setdiff(colnames(count_table), "Barcode")
    counts <- as.matrix(as.data.frame(count_table)[, samples, drop = FALSE])
    dimnames(counts) <- list(identifiers, samples)

    annotation <- annotation_frame(annotation_table)
    row_data <- annotation_for(identifiers, annotation)

    if (is.null(sample_table) || nrow(sample_table) == 0) {
        sample_table <- parse_sample_names(samples)
    }
    col_data <- S4Vectors::DataFrame(sample_table, check.names = FALSE)
    rownames(col_data) <- samples
    if (length(groups) == length(samples)) {
        col_data$Group <- groups
    } else if (length(groups) > 0) {
        metadata$groups <- groups
    }
    if (length(replicates) == length(samples) && length(samples) > 1) {
        col_data$Replicate <- replicates
        replicates <- c("")
    }
    se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = counts), rowData = row_data,
        colData = col_data, metadata = metadata
    )
    object <- methods::new("screenr_object", se,
        annotation_table = annotation[!annotation$Barcode %in% identifiers,
            , drop = FALSE],
        replicates = replicates,
        data_table = tibble(),
        column_names = column_names
    )
    methods::validObject(object)
    object
}

#' @title Annotation Table with Character Identifiers
#' @description Internal function that returns the annotation table as a
#'              data frame with the barcode identifiers as character (an
#'              empty data frame with a Barcode column when there is no
#'              annotation).
#' @param annotation The annotation table or NULL.
#' @return A tibble.
#' @keywords internal
annotation_frame <- function(annotation) {
    if (is.null(annotation) || !"Barcode" %in% colnames(annotation)) {
        return(tibble(Barcode = character()))
    }
    annotation <- tibble::as_tibble(annotation)
    annotation$Barcode <- as_barcode_id(annotation$Barcode,
        "barcode identifiers of the annotation table")
    annotation[, c("Barcode", setdiff(colnames(annotation), "Barcode"))]
}

#' @title Annotation of Observed Barcodes
#' @description Internal function that builds the rowData of the object: the
#'              annotation of each barcode, matched by identifier (NA for the
#'              barcodes missing from the annotation).
#' @param identifiers The barcode identifiers (row names of the counts).
#' @param annotation The annotation, as returned by annotation_frame.
#' @return A DataFrame with the Barcode column and the annotation columns.
#' @keywords internal
annotation_for <- function(identifiers, annotation) {
    row_data <- S4Vectors::DataFrame(Barcode = identifiers,
        check.names = FALSE)
    matched <- match(identifiers, annotation$Barcode)
    for (column in setdiff(colnames(annotation), "Barcode")) {
        row_data[[column]] <- annotation[[column]][matched]
    }
    rownames(row_data) <- identifiers
    row_data
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                       Objects of older versions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Is a ScreenR Object of an Older Version
#' @description Internal function that tells whether an object was created
#'              with a version of ScreenR older than 1.15.8, in which the
#'              screenr_object class was not a SummarizedExperiment.
#' @param object A ScreenR object.
#' @return TRUE or FALSE.
#' @keywords internal
is_legacy_screenr <- function(object) {
    !is.null(attr(object, "count_table", exact = TRUE))
}

#' @title Update a ScreenR object
#' @description Converts a ScreenR object created with an older version of
#'     ScreenR to the current layout. All the ScreenR functions do this
#'     automatically (with a message), so it is only needed to save the
#'     converted object, or to use it with other Bioconductor tools.
#' @details Two kinds of older objects are converted:
#'     \itemize{
#'       \item objects created before version 1.15.8, which were not
#'             SummarizedExperiment objects;
#'       \item objects created by versions 1.15.8 to 1.15.15, which stored
#'             the normalized counts as an assay and the data table as a
#'             slot.
#'     }
#'     The counts, the annotation, the groups and the sample information are
#'     kept. The normalized counts and the data table of older objects are
#'     \strong{not} kept: those versions did not record how the counts were
#'     normalized, and the edgeR based methods ignored the normalization, so
#'     it cannot be recovered reliably. Run \code{\link{normalize_data}} on
#'     the converted object (with the method used originally, "cpm" by
#'     default). Objects of the current layout are returned unchanged.
#' @param object A ScreenR object.
#' @param verbose Whether to give a message when the object is converted.
#'     Default TRUE.
#' @return A ScreenR object of the current layout.
#' @seealso \code{\link{create_screenr_object}}, \code{\link{normalize_data}}
#' @concept objects
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- update_screenr_object(object)
#' object
update_screenr_object <- function(object, verbose = TRUE) {
    if (is_legacy_screenr(object)) {
        legacy <- function(name) attr(object, name, exact = TRUE)
        converted <- rebuild_screenr_object(
            count_table = legacy("count_table"),
            annotation_table = legacy("annotation_table"),
            groups = legacy("groups"),
            replicates = legacy("replicates"),
            sample_table = legacy("sample_table"),
            column_names = legacy("column_names")
        )
        had_normalization <- !is.null(legacy("normalized_count_table")) &&
            nrow(legacy("normalized_count_table")) > 0
    } else if (!identical(
        S4Vectors::metadata(object)$screenr_layout, screenr_layout_version)) {
        had_normalization <- "normalized" %in%
            SummarizedExperiment::assayNames(object) ||
            nrow(object@data_table) > 0
        converted <- rebuild_screenr_object(
            count_table = assay_table(object, "counts", object@column_names),
            annotation_table = dplyr::bind_rows(
                as.data.frame(SummarizedExperiment::rowData(object)),
                annotation_frame(object@annotation_table)
            ),
            groups = groups_of(object),
            replicates = object@replicates,
            sample_table = sample_table_of(object),
            column_names = object@column_names,
            metadata = S4Vectors::metadata(object)
        )
    } else {
        return(object)
    }
    if (verbose) {
        message("This ScreenR object was created with an older version of ",
            "ScreenR and has been converted (see ?update_screenr_object).",
            if (had_normalization) paste0(" Its normalized counts were not ",
                "kept because their normalization is not recorded: run ",
                "normalize_data()."))
    }
    converted
}

#' @title Rebuild an Older ScreenR Object
#' @description Internal function used by \code{\link{update_screenr_object}}
#'              to build a current object from the tables of an older one.
#' @param count_table,annotation_table,groups,replicates,sample_table,column_names
#'        The tables of the older object.
#' @param metadata The metadata of the older object, if any.
#' @return A screenr_object of the current layout, not normalized.
#' @keywords internal
rebuild_screenr_object <- function(count_table, annotation_table, groups,
    replicates, sample_table, column_names, metadata = list()) {
    if (is.null(column_names)) column_names <- list()
    if (is.null(replicates)) replicates <- c("")
    if (!is.null(count_table)) {
        count_table <- tibble::as_tibble(count_table)
        count_table <- check_count_table(count_table, "Barcode")
    }
    if (!is.null(annotation_table) && nrow(annotation_table) > 0) {
        annotation_table <- annotation_frame(annotation_table)
        annotation_table <- annotation_table[
            !duplicated(annotation_table$Barcode), , drop = FALSE]
    }
    if (!is.null(sample_table) && nrow(sample_table) > 0 &&
        !is.null(count_table)) {
        samples <- setdiff(colnames(count_table), "Barcode")
        sample_table <- tibble::as_tibble(sample_table)
        sample_table <- sample_table[match(samples, sample_table$Sample), ]
    }
    object <- build_screenr_object(
        count_table = count_table, annotation_table = annotation_table,
        groups = groups, replicates = replicates, sample_table = sample_table,
        column_names = column_names
    )
    kept <- metadata[setdiff(names(metadata),
        c("groups", "normalization", "screenr_layout"))]
    if (length(kept) > 0) {
        S4Vectors::metadata(object) <- c(S4Vectors::metadata(object), kept)
    }
    object
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                              Normalization state
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Normalization State
#' @description Internal functions that read the normalization of an object:
#'              \code{is_normalized} tells whether it is normalized,
#'              \code{effective_lib_size} returns the effective library size
#'              of each sample (stopping with an informative error when the
#'              object is not normalized) and \code{normalized_counts} the
#'              normalized counts, raw counts / effective library size *
#'              1e6.
#' @param object A ScreenR object (of the current layout).
#' @param what What needs the normalization, for the error message.
#' @return TRUE/FALSE, a named numeric vector or a numeric matrix.
#' @keywords internal
#' @name normalization_state
is_normalized <- function(object) {
    !is.null(S4Vectors::metadata(object)$normalization) &&
        "EffectiveLibSize" %in% colnames(SummarizedExperiment::colData(object))
}

#' @rdname normalization_state
effective_lib_size <- function(object, what = "This function") {
    if (!is_normalized(object)) {
        stop(what, " needs normalized counts, but the object is not ",
            "normalized: run normalize_data() first (method \"cpm\", ",
            "\"tmm\" or \"control\").", call. = FALSE)
    }
    stats::setNames(SummarizedExperiment::colData(object)$EffectiveLibSize,
        colnames(object))
}

#' @rdname normalization_state
normalized_counts <- function(object, what = "This function") {
    sizes <- effective_lib_size(object, what)
    counts <- SummarizedExperiment::assay(object, "counts",
        withDimnames = TRUE)
    sweep(counts, 2, sizes, "/") * normalization_scale
}

# Normalized counts are per million reads of the effective library size
normalization_scale <- 1e6

#' @title Set or Remove the Normalization
#' @description Internal functions that record a normalization in the object
#'              (effective library sizes in the colData, method and
#'              provenance in the metadata) or remove it.
#' @param object A ScreenR object.
#' @param sizes The effective library size of each sample.
#' @param state A list with the method and the provenance.
#' @param reason Why the normalization is removed, for the message; NULL for
#'               no message.
#' @return The ScreenR object.
#' @keywords internal
#' @name set_normalization_state
set_normalization_state <- function(object, sizes, state) {
    object@colData$EffectiveLibSize <- as.numeric(sizes)
    object@metadata$normalization <- state
    methods::validObject(object)
    object
}

#' @rdname set_normalization_state
remove_normalization <- function(object, reason = NULL) {
    if (!is_normalized(object) &&
        is.null(S4Vectors::metadata(object)$normalization)) {
        return(object)
    }
    object@colData$EffectiveLibSize <- NULL
    object@metadata$normalization <- NULL
    if (!is.null(reason)) {
        message(reason, " The normalization has been removed: run ",
            "normalize_data() again.")
    }
    object
}

#' @title Get the normalization
#' @description Returns the normalization of a ScreenR object, as set by
#'     \code{\link{normalize_data}}: the method, the effective library size of
#'     each sample and how it was obtained. The same normalization is used by
#'     all the functions of the analysis: the normalized counts, the data
#'     table, the plots and the offsets of the edgeR and limma models.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return NULL when the object is not normalized; otherwise a list with:
#'     \itemize{
#'       \item method: "cpm", "tmm", "control" or "custom" (set with
#'             \code{\link{set_normalized_count_table}});
#'       \item effective_library_size: the effective library size of each
#'             sample of the object (the normalized count of a barcode is its
#'             raw count divided by the effective library size, times 1e6);
#'       \item reference: the identifiers of the control barcodes (method
#'             "control"), otherwise NULL;
#'       \item and the provenance recorded when the normalization was
#'             computed: library_size (the total raw counts of each sample),
#'             norm_factors (the TMM factors for "tmm", otherwise 1),
#'             n_barcodes (the number of barcodes of the object) and
#'             screenr_version.
#'     }
#'     The provenance describes the object when it was normalized: after
#'     subsetting, the effective library sizes of the retained samples are
#'     kept unchanged.
#' @seealso \code{\link{normalize_data}}
#' @concept objects
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' get_normalization(object)$method
#' get_normalization(normalize_data(object, method = "tmm"))
get_normalization <- function(object) {
    object <- update_screenr_object(object)
    if (!is_normalized(object)) {
        return(NULL)
    }
    state <- S4Vectors::metadata(object)$normalization
    c(list(method = state$method,
        effective_library_size = effective_lib_size(object)),
        state[setdiff(names(state), "method")])
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                     Internal accessors (views of the object)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Internal Accessors
#' @description Internal functions that return views of a ScreenR object as
#'              tables: they are built from the assays, rowData and colData
#'              each time and are never stored. \code{count_table_of} and
#'              \code{normalized_table_of} are for export and the public
#'              getters: the computations use the assays directly.
#' @param object A ScreenR object.
#' @param assay_name The name of the assay.
#' @param column_names Ignored (kept for older objects).
#' @importFrom SummarizedExperiment assay assayNames rowData colData
#' @return A tibble (count_table_of, normalized_table_of, sample_table_of,
#'         annotation_of), or a factor (groups_of).
#' @keywords internal
#' @name internal_accessors
count_table_of <- function(object) {
    assay_table(update_screenr_object(object), "counts")
}

#' @rdname internal_accessors
normalized_table_of <- function(object) {
    object <- update_screenr_object(object)
    if (!is_normalized(object)) {
        return(tibble())
    }
    values <- normalized_counts(object)
    dplyr::bind_cols(tibble(Barcode = rownames(object)),
        tibble::as_tibble(unname_rows(values), .name_repair = "minimal"))
}

#' @rdname internal_accessors
assay_table <- function(object, assay_name, column_names = NULL) {
    if (ncol(object) == 0) {
        return(tibble())
    }
    values <- SummarizedExperiment::assay(object, assay_name,
        withDimnames = TRUE)
    identifiers <- rownames(object)
    if (is.null(identifiers)) {
        identifiers <- SummarizedExperiment::rowData(object)$Barcode
    }
    dplyr::bind_cols(tibble(Barcode = as.character(identifiers)),
        tibble::as_tibble(unname_rows(values), .name_repair = "minimal"))
}

unname_rows <- function(values) {
    rownames(values) <- NULL
    values
}

#' @rdname internal_accessors
annotation_of <- function(object) {
    object <- update_screenr_object(object)
    observed <- tibble::as_tibble(as.data.frame(
        SummarizedExperiment::rowData(object), optional = TRUE))
    if (!"Barcode" %in% colnames(observed)) {
        observed$Barcode <- rownames(object)
    }
    unobserved <- annotation_frame(object@annotation_table)
    if (nrow(unobserved) == 0) {
        return(observed)
    }
    dplyr::bind_rows(observed, unobserved)
}

#' @rdname internal_accessors
genes_of <- function(object) {
    row_data <- SummarizedExperiment::rowData(object)
    if (!"Gene" %in% colnames(row_data)) {
        return(rep(NA_character_, nrow(object)))
    }
    as.character(row_data$Gene)
}

#' @rdname internal_accessors
groups_of <- function(object) {
    col_data <- SummarizedExperiment::colData(object)
    if ("Group" %in% colnames(col_data)) {
        return(col_data$Group)
    }
    groups <- S4Vectors::metadata(object)$groups
    if (!is.null(groups)) {
        return(groups)
    }
    factor()
}

# Columns of the colData that are not sample information
internal_sample_columns <- c("Group", "Replicate", "EffectiveLibSize")

#' @rdname internal_accessors
sample_table_of <- function(object) {
    col_data <- SummarizedExperiment::colData(object)
    columns <- setdiff(colnames(col_data), internal_sample_columns)
    if (ncol(object) == 0 || !"Sample" %in% columns) {
        return(tibble(Sample = character(), Day = character(),
            Treatment = character()))
    }
    tibble::as_tibble(as.data.frame(col_data[, columns, drop = FALSE]),
        rownames = NULL)
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                 S4 methods
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @rdname get_count_table
#' @aliases get_count_table,screenr_object
#' @export
setMethod(
  f = "get_count_table",
  signature = "screenr_object",
  definition = function(object) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    count_table <- count_table_of(object)
    message(
      "ScreenR count table: ", nrow(count_table), " rows, ",
      ncol(count_table), " columns"
    )
    return(count_table)
  }
)


#' @rdname get_annotation_table
#' @aliases get_annotation_table,screenr_object
#' @export
setMethod(
  f = "get_annotation_table",
  signature = "screenr_object",
  definition = function(object) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    annotation_table <- annotation_of(object)
    message(
      "ScreenR annotation table: ", nrow(annotation_table), " rows, ",
      ncol(annotation_table), " columns"
    )
    return(annotation_table)
  }
)


#' @export
#' @aliases get_groups,screenr_object
#' @rdname get_groups
setMethod(
  f = "get_groups",
  signature = "screenr_object",
  definition = function(object) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    return(groups_of(update_screenr_object(object)))
  }
)


#' @export
#' @aliases get_replicates,screenr_object
#' @rdname get_replicates
setMethod(
  f = "get_replicates",
  signature = "screenr_object",
  definition = function(object) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    object <- update_screenr_object(object)
    col_data <- SummarizedExperiment::colData(object)
    if ("Replicate" %in% colnames(col_data)) {
      return(col_data$Replicate)
    }
    return(object@replicates)
  }
)


#' @export
#' @aliases get_normalized_count_table,screenr_object
#' @rdname get_normalized_count_table
setMethod(
  f = "get_normalized_count_table",
  signature = "screenr_object",
  definition = function(object) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    normalized_count_table <- normalized_table_of(object)
    message(
      "ScreenR normalized count table: ", nrow(normalized_count_table), " rows, ",
      ncol(normalized_count_table), " columns"
    )
    return(normalized_count_table)
  }
)


#' @export
#' @aliases get_data_table,screenr_object
#' @rdname get_data_table
setMethod(
  f = "get_data_table",
  signature = "screenr_object",
  definition = function(object) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    data_table <- data_table_of(update_screenr_object(object),
      "get_data_table()")
    message(
      "ScreenR normalized data table: ", nrow(data_table), " rows, ",
      ncol(data_table), " columns"
    )
    return(data_table)
  }
)


#' @export
#' @aliases set_count_table,screenr_object
#' @rdname set_count_table
setMethod(
  f = "set_count_table",
  signature = "screenr_object",
  definition = function(object, count_table) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    object <- update_screenr_object(object)
    count_table <- check_count_table(count_table, "Barcode")
    new_samples <- setdiff(colnames(count_table), "Barcode")
    same_samples <- identical(colnames(object), new_samples)
    sample_table <- if (same_samples) sample_table_of(object) else NULL
    groups <- groups_of(object)
    replicates <- get_replicates(object)
    updated <- build_screenr_object(
      count_table = count_table,
      annotation_table = annotation_of(object),
      groups = groups,
      replicates = replicates,
      sample_table = sample_table,
      column_names = object@column_names
    )
    if (is_normalized(object)) {
      unchanged <- same_samples &&
        identical(rownames(updated), rownames(object)) &&
        isTRUE(all.equal(
          unname(SummarizedExperiment::assay(updated, "counts")),
          unname(SummarizedExperiment::assay(object, "counts")),
          check.attributes = FALSE, tolerance = 0))
      if (unchanged) {
        updated <- set_normalization_state(updated,
          effective_lib_size(object),
          S4Vectors::metadata(object)$normalization)
      } else {
        message("The counts have changed. The normalization has been ",
          "removed: run normalize_data() again.")
      }
    }
    updated
  }
)


#' @export
#' @aliases set_annotation_table,screenr_object
#' @rdname set_annotation_table
setMethod(
  f = "set_annotation_table",
  signature = "screenr_object",
  definition = function(object, annotation_table) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    object <- update_screenr_object(object)
    if (!"Barcode" %in% colnames(annotation_table)) {
      stop("The annotation table must have a Barcode column.", call. = FALSE)
    }
    annotation <- resolve_duplicated_annotation(
      annotation_frame(annotation_table), "error")
    object@annotation_table <- annotation[
      !annotation$Barcode %in% rownames(object), , drop = FALSE]
    SummarizedExperiment::rowData(object) <-
      annotation_for(rownames(object), annotation)
    return(object)
  }
)


#' @export
#' @aliases set_groups,screenr_object
#' @rdname set_groups
setMethod(
  f = "set_groups",
  signature = "screenr_object",
  definition = function(object, groups) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    object <- update_screenr_object(object)
    groups <- as.factor(groups)
    col_data <- SummarizedExperiment::colData(object)
    col_data$Group <- NULL
    metadata <- S4Vectors::metadata(object)
    metadata$groups <- NULL
    if (length(groups) == ncol(object)) {
      col_data$Group <- groups
    } else if (length(groups) > 0) {
      warning("groups has ", length(groups), " values but the object has ",
        ncol(object), " samples.", call. = FALSE)
      metadata$groups <- groups
    }
    SummarizedExperiment::colData(object) <- col_data
    S4Vectors::metadata(object) <- metadata
    return(object)
  }
)



#' @export
#' @aliases set_replicates,screenr_object
#' @rdname set_replicates
setMethod(
  f = "set_replicates",
  signature = "screenr_object",
  definition = function(object, replicates) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    object <- update_screenr_object(object)
    col_data <- SummarizedExperiment::colData(object)
    col_data$Replicate <- NULL
    if (length(replicates) == ncol(object) && ncol(object) > 1) {
      col_data$Replicate <- replicates
      object@replicates <- c("")
    } else {
      object@replicates <- replicates
    }
    SummarizedExperiment::colData(object) <- col_data
    return(object)
  }
)


#' @export
#' @aliases set_normalized_count_table,screenr_object
#' @rdname set_normalized_count_table
setMethod(
  f = "set_normalized_count_table",
  signature = "screenr_object",
  definition = function(object, normalized_count_table) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    object <- update_screenr_object(object)
    if (nrow(normalized_count_table) == 0) {
      return(remove_normalization(object))
    }
    sizes <- sizes_from_normalized_table(object, normalized_count_table)
    set_normalization_state(object, sizes, list(
      method = "custom", reference = NULL,
      library_size = colSums(SummarizedExperiment::assay(object, "counts")),
      norm_factors = stats::setNames(rep(1, ncol(object)), colnames(object)),
      n_barcodes = nrow(object),
      source = "set_normalized_count_table()",
      screenr_version = screenr_version()))
  }
)

#' @title Effective Library Sizes of a Normalized Table
#' @description Internal function used by
#'              \code{\link{set_normalized_count_table}}: it checks that a
#'              normalized count table is the raw counts scaled by one
#'              factor for each sample, and returns the corresponding
#'              effective library sizes (raw / normalized * 1e6).
#' @param object A ScreenR object.
#' @param table A table with a Barcode column and one column for each sample.
#' @return The effective library size of each sample.
#' @keywords internal
sizes_from_normalized_table <- function(object, table) {
  if (!"Barcode" %in% colnames(table)) {
    stop("The normalized count table must have a Barcode column.",
      call. = FALSE)
  }
  identifiers <- as_barcode_id(table$Barcode)
  missing_samples <- setdiff(colnames(object), colnames(table))
  if (length(missing_samples) > 0) {
    stop("The normalized count table has no column for the sample(s) ",
      paste(missing_samples, collapse = ", "), ".", call. = FALSE)
  }
  rows <- match(rownames(object), identifiers)
  if (anyNA(rows)) {
    stop("The normalized count table has no row for ", sum(is.na(rows)),
      " barcode(s) of the object.", call. = FALSE)
  }
  counts <- SummarizedExperiment::assay(object, "counts")
  vapply(colnames(object), function(sample) {
    normalized <- as.numeric(table[[sample]])[rows]
    raw <- as.numeric(counts[, sample])
    used <- raw > 0
    ratio <- raw[used] / normalized[used] * normalization_scale
    if (!any(used) || anyNA(normalized) || any(normalized[!used] != 0) ||
        any(!is.finite(ratio)) || any(ratio <= 0) ||
        diff(range(ratio)) > 1e-6 * max(ratio)) {
      stop("The normalized counts of sample ", sample, " are not the raw ",
        "counts divided by a single factor: ScreenR can only use ",
        "normalizations that scale each sample (effective library sizes). ",
        "Use normalize_data() for the \"cpm\", \"tmm\" and \"control\" ",
        "methods.", call. = FALSE)
    }
    stats::median(ratio)
  }, numeric(1))
}

screenr_version <- function() {
  as.character(utils::packageVersion("ScreenR"))
}


#' @export
#' @aliases set_data_table,screenr_object
#' @rdname set_data_table
setMethod(
  f = "set_data_table",
  signature = "screenr_object",
  definition = function(object, data_table) {
    if (is.null(object)) {
      stop("The object is not defined!")
    }
    object <- update_screenr_object(object)
    current <- if (is_normalized(object)) data_table_of(object) else tibble()
    if (isTRUE(all.equal(as.data.frame(current), as.data.frame(data_table),
      check.attributes = FALSE))) {
      warning("set_data_table() is deprecated: the data table is computed ",
        "from the object when needed.", call. = FALSE)
      return(object)
    }
    stop("set_data_table() is deprecated and can no longer change the data ",
      "table: it is computed from the counts, the annotation, the sample ",
      "table and the normalization of the object. Change those instead ",
      "(for example subset the object, or use set_annotation_table() or ",
      "normalize_data()).", call. = FALSE)
  }
)

#' @title Subset a ScreenR object
#' @description Subsets the barcodes (rows) and the samples (columns) of a
#'              ScreenR object, like any
#'              \code{\link[SummarizedExperiment]{SummarizedExperiment}}.
#'              The annotation (rowData) and the sample information
#'              (colData) are subset with the counts, so they stay aligned by
#'              identifier when the rows or the columns are reordered.
#' @details The normalization is kept: the retained samples keep their
#'          effective library sizes, which were computed on all the barcodes
#'          when \code{\link{normalize_data}} was called. To normalize on the
#'          retained barcodes only, call \code{\link{normalize_data}} again
#'          after subsetting.
#' @param x A ScreenR object.
#' @param i,j The barcodes and samples to keep (indices, names or logical
#'            vectors).
#' @param ... Not used.
#' @param drop Not used.
#' @return The subset ScreenR object.
#' @seealso \code{\link{remove_all_zero_row}}, \code{\link{normalize_data}}
#' @concept objects
#' @rdname subset-screenr_object
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object[1:100, ]
#' object[, object$Treatment == "TRT"]
setMethod(
  f = "[",
  signature = "screenr_object",
  definition = function(x, i, j, ..., drop = TRUE) {
    x <- update_screenr_object(x)
    x <- methods::callNextMethod()
    x@data_table <- tibble()
    x
  }
)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#          Replacement methods of SummarizedExperiment (safeguards)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Remove a Stale Normalization
#' @description Internal function called after the assays are replaced: when
#'              the raw counts have changed, the effective library sizes no
#'              longer describe them, so the normalization is removed (with
#'              a message). The object is then checked.
#' @param x The ScreenR object after the replacement.
#' @param old_counts The counts before the replacement.
#' @return The ScreenR object.
#' @keywords internal
counts_replaced <- function(x, old_counts) {
  new_counts <- if ("counts" %in% SummarizedExperiment::assayNames(x)) {
    SummarizedExperiment::assay(x, "counts", withDimnames = FALSE)
  }
  if (!identical(unname(new_counts), unname(old_counts))) {
    x <- remove_normalization(x, if (is_normalized(x)) {
      "The counts have changed."
    })
  }
  methods::validObject(x)
  x
}

old_counts_of <- function(x) {
  if ("counts" %in% SummarizedExperiment::assayNames(x)) {
    SummarizedExperiment::assay(x, "counts", withDimnames = FALSE)
  }
}

#' @title Replacement Methods of a ScreenR Object
#' @description The replacement functions of SummarizedExperiment work on
#'     ScreenR objects, with safeguards that keep the object consistent:
#'     \itemize{
#'       \item \code{assay<-} and \code{assays<-}: when the raw counts change,
#'             the normalization is removed (run \code{\link{normalize_data}}
#'             again); other assays can be added freely;
#'       \item \code{rowData<-}: the Barcode column must equal the row names
#'             (it is added when missing); the new annotation is used by all
#'             the following analyses;
#'       \item \code{colData<-} (and \code{$<-}): the object is checked, so
#'             for example the effective library sizes cannot be removed or
#'             made invalid while a normalization is recorded;
#'       \item \code{dimnames<-} (\code{rownames<-}, \code{colnames<-}): the
#'             Barcode column of the rowData follows the new barcode
#'             identifiers, which must be unique.
#'     }
#' @param x A ScreenR object.
#' @param i The assay (name or index).
#' @param withDimnames,... Passed to the SummarizedExperiment method.
#' @param value The new value.
#' @return The modified ScreenR object.
#' @seealso \code{\link{screenr_object-class}}
#' @concept objects
#' @name screenr_object-replace
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' counts <- SummarizedExperiment::assay(object, "counts")
#' # Changing the counts removes the normalization
#' SummarizedExperiment::assay(object, "counts") <- counts + 1
#' get_normalization(object)
NULL

#' @rdname screenr_object-replace
#' @importMethodsFrom SummarizedExperiment assay<- assays<- rowData<- colData<-
#' @export
setReplaceMethod("assay", c("screenr_object", "missing"),
  function(x, i, withDimnames = TRUE, ..., value) {
    old <- old_counts_of(x)
    x <- methods::callNextMethod()
    counts_replaced(x, old)
  }
)

#' @rdname screenr_object-replace
#' @export
setReplaceMethod("assay", c("screenr_object", "character"),
  function(x, i, withDimnames = TRUE, ..., value) {
    old <- old_counts_of(x)
    x <- methods::callNextMethod()
    counts_replaced(x, old)
  }
)

#' @rdname screenr_object-replace
#' @export
setReplaceMethod("assay", c("screenr_object", "numeric"),
  function(x, i, withDimnames = TRUE, ..., value) {
    old <- old_counts_of(x)
    x <- methods::callNextMethod()
    counts_replaced(x, old)
  }
)

#' @rdname screenr_object-replace
#' @export
setReplaceMethod("assays", c("screenr_object", "list"),
  function(x, withDimnames = TRUE, ..., value) {
    old <- old_counts_of(x)
    x <- methods::callNextMethod()
    counts_replaced(x, old)
  }
)

#' @rdname screenr_object-replace
#' @export
setReplaceMethod("assays", c("screenr_object", "SimpleList"),
  function(x, withDimnames = TRUE, ..., value) {
    old <- old_counts_of(x)
    x <- methods::callNextMethod()
    counts_replaced(x, old)
  }
)

#' @rdname screenr_object-replace
#' @export
setReplaceMethod("rowData", "screenr_object",
  function(x, ..., value) {
    if (!is.null(value) && !"Barcode" %in% colnames(value) &&
      nrow(x) > 0) {
      value$Barcode <- rownames(x)
      value <- value[, c("Barcode", setdiff(colnames(value), "Barcode")),
        drop = FALSE]
    }
    x <- methods::callNextMethod(x, ..., value = value)
    methods::validObject(x)
    x
  }
)

#' @rdname screenr_object-replace
#' @export
setReplaceMethod("colData", c("screenr_object", "DataFrame"),
  function(x, ..., value) {
    x <- methods::callNextMethod()
    methods::validObject(x)
    x
  }
)

#' @rdname screenr_object-replace
#' @export
setReplaceMethod("dimnames", c("screenr_object", "list"),
  function(x, value) {
    x <- methods::callNextMethod()
    row_data <- SummarizedExperiment::rowData(x)
    if (nrow(x) > 0 && "Barcode" %in% colnames(row_data)) {
      x@elementMetadata$Barcode <- rownames(x)
    }
    methods::validObject(x)
    x
  }
)

#' @title Show a ScreenR object
#' @description Prints a summary of a ScreenR object: the
#'              SummarizedExperiment summary and the normalization. Objects
#'              created with older versions of ScreenR are converted first
#'              (see \code{\link{update_screenr_object}}).
#' @param object A ScreenR object.
#' @importFrom methods show callNextMethod
#' @return NULL, invisibly.
#' @concept objects
#' @rdname show-screenr_object
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object
setMethod(
  f = "show",
  signature = "screenr_object",
  definition = function(object) {
    updated <- update_screenr_object(object, verbose = FALSE)
    if (!identical(updated, object)) {
      show(updated)
      return(invisible(NULL))
    }
    methods::callNextMethod()
    if (is_normalized(object)) {
      cat("normalization:", S4Vectors::metadata(object)$normalization$method,
        "(effective library sizes in colData(object)$EffectiveLibSize)\n")
    } else {
      cat("normalization: none (run normalize_data())\n")
    }
    invisible(NULL)
  }
)
