#' @include  generics.R

#' @title The ScreenR object class
#' @description The screenr_object class is the main object of the package:
#'     it stores the data of a screen and is passed to the functions of the
#'     analysis. Create it with \code{\link{create_screenr_object}}.
#'
#'     It extends \code{\link[SummarizedExperiment]{SummarizedExperiment}},
#'     so it can be used with all the Bioconductor tools that work with that
#'     class:
#'     \itemize{
#'       \item \code{assay(object, "counts")}: the raw counts, one row for
#'             each barcode and one column for each sample;
#'       \item \code{assay(object, "normalized")}: the normalized counts,
#'             after \code{\link{normalize_data}};
#'       \item \code{rowData(object)}: the barcode identifier (Barcode) and
#'             its annotation (Gene, ...);
#'       \item \code{colData(object)}: the Sample, Day and Treatment of each
#'             sample (see \code{\link{get_sample_table}}) and its Group.
#'     }
#'     The ScreenR accessors (\code{\link{get_count_table}},
#'     \code{\link{set_count_table}}, ...) work as in the previous versions
#'     of the package. Objects created with versions older than 1.15.8 are
#'     converted automatically (see \code{\link{update_screenr_object}}).
#'
#' @slot annotation_table The annotation table as given by the user (it can
#'     contain barcodes that are not in the count table).
#' @slot replicates The replicate labels.
#' @slot data_table The data table, a tidy (long) version of the normalized
#'     counts (see \code{\link{compute_data_table}}).
#' @slot column_names The names that the columns had in the tables given by
#'     the user (see \code{\link{get_column_names}}).
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @exportClass screenr_object
#' @concept objects
#' @seealso \code{\link{create_screenr_object}},
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
#' obj <- create_screenr_object(
#'     table = count_table,
#'     annotation = annotation_table,
#'     groups = groups,
#'     replicates = c("")
#' )
#' obj
screenr_object <- setClass("screenr_object",
  contains = "SummarizedExperiment",
  representation = methods::representation(
    annotation_table = "data.frame",
    replicates = "vector",
    data_table = "data.frame",
    column_names = "list"
  )
)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                  Construction and objects of older versions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Build a ScreenR Object
#' @description Internal function that builds a screenr_object from the
#'              tables, without checking them (the checks are done by
#'              \code{\link{create_screenr_object}}).
#' @param count_table A table with a Barcode column and one column for each
#'                    sample. Non-numeric columns other than Barcode are
#'                    ignored.
#' @param annotation_table The annotation table, with a Barcode column.
#' @param groups The groups of the samples.
#' @param replicates The replicates.
#' @param normalized_count_table An optional normalized count table, with a
#'                               Barcode column; it is matched to the count
#'                               table by barcode and sample.
#' @param data_table The data table.
#' @param sample_table An optional sample table with the columns Sample, Day
#'                     and Treatment, in the order of the samples; when NULL
#'                     the sample names are parsed.
#' @param column_names The original column names.
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @return A screenr_object.
#' @keywords internal
build_screenr_object <- function(count_table = NULL, annotation_table = NULL,
    groups = NULL, replicates = c(""), normalized_count_table = NULL,
    data_table = NULL, sample_table = NULL, column_names = list()) {
    groups <- as.factor(groups)
    metadata <- list()
    annotation_table <- tibble(annotation_table)
    data_table <- tibble(data_table)

    if (is.null(count_table) || !"Barcode" %in% colnames(count_table)) {
        se <- SummarizedExperiment::SummarizedExperiment(
            assays = list(counts = matrix(0L, nrow = 0, ncol = 0))
        )
        if (length(groups) > 0) metadata$groups <- groups
    } else {
        barcodes <- count_table$Barcode
        samples <- setdiff(colnames(count_table), "Barcode")
        samples <- samples[vapply(count_table[samples], is.numeric,
            logical(1))]
        counts <- as.matrix(as.data.frame(count_table)[, samples,
            drop = FALSE])
        row_names <- make.unique(as.character(barcodes))
        dimnames(counts) <- list(row_names, samples)

        row_data <- S4Vectors::DataFrame(Barcode = barcodes,
            check.names = FALSE)
        if (nrow(annotation_table) > 0 &&
            "Barcode" %in% colnames(annotation_table)) {
            matched <- match(as.character(barcodes),
                as.character(annotation_table$Barcode))
            for (column in setdiff(colnames(annotation_table), "Barcode")) {
                row_data[[column]] <- annotation_table[[column]][matched]
            }
        }

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

        assays <- list(counts = counts)
        if (!is.null(normalized_count_table) &&
            "Barcode" %in% colnames(normalized_count_table)) {
            assays$normalized <- match_to_counts(normalized_count_table,
                counts)
        }
        se <- SummarizedExperiment::SummarizedExperiment(
            assays = assays, rowData = row_data, colData = col_data,
            metadata = metadata
        )
    }
    methods::new("screenr_object", se,
        annotation_table = annotation_table,
        replicates = replicates,
        data_table = data_table,
        column_names = column_names
    )
}

#' @title Match a Table to the Counts
#' @description Internal function that puts the values of a table with a
#'              Barcode column in a matrix with the same rows and columns as
#'              the count matrix (NA where a barcode or sample is missing).
#' @param table A table with a Barcode column and one column per sample.
#' @param counts The count matrix.
#' @return A numeric matrix with the dimensions of counts.
#' @keywords internal
match_to_counts <- function(table, counts) {
    values <- matrix(NA_real_, nrow = nrow(counts), ncol = ncol(counts),
        dimnames = dimnames(counts))
    rows <- match(rownames(counts), make.unique(as.character(table$Barcode)))
    for (sample in intersect(colnames(counts), colnames(table))) {
        values[, sample] <- as.numeric(table[[sample]])[rows]
    }
    values
}

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
#' @description Converts a ScreenR object created with a version of ScreenR
#'     older than 1.15.8 to the current class, which extends
#'     \code{\link[SummarizedExperiment]{SummarizedExperiment}}. All the
#'     ScreenR functions do this automatically, so it is only needed to use
#'     the object with other Bioconductor tools. Objects of the current class
#'     are returned unchanged.
#' @param object A ScreenR object.
#' @return A ScreenR object of the current class.
#' @seealso \code{\link{create_screenr_object}}
#' @concept objects
#' @export
#' @examples
#' # The example object of the package was saved with an older version
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- update_screenr_object(object)
#' object
update_screenr_object <- function(object) {
    if (!is_legacy_screenr(object)) {
        return(object)
    }
    legacy <- function(name) attr(object, name, exact = TRUE)
    build_screenr_object(
        count_table = legacy("count_table"),
        annotation_table = legacy("annotation_table"),
        groups = legacy("groups"),
        replicates = legacy("replicates"),
        normalized_count_table = legacy("normalized_count_table"),
        data_table = legacy("data_table"),
        sample_table = legacy("sample_table"),
        column_names = if (is.null(legacy("column_names"))) {
            list()
        } else {
            legacy("column_names")
        }
    )
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                            Internal accessors
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Internal Accessors
#' @description Internal functions that return the tables of a ScreenR object
#'              in the format used by the rest of the package, for objects of
#'              the current and of older versions.
#' @param object A ScreenR object.
#' @param assay_name The name of the assay.
#' @importFrom SummarizedExperiment assay assayNames rowData colData
#' @return A tibble (count_table_of, normalized_table_of, sample_table_of), a
#'         data frame (annotation_of) or a factor (groups_of).
#' @keywords internal
#' @name internal_accessors
count_table_of <- function(object) {
    assay_table(update_screenr_object(object), "counts")
}

#' @rdname internal_accessors
normalized_table_of <- function(object) {
    object <- update_screenr_object(object)
    if (!"normalized" %in% SummarizedExperiment::assayNames(object)) {
        return(tibble())
    }
    table <- assay_table(object, "normalized")
    measured <- rowSums(!is.na(as.matrix(table[, -1, drop = FALSE]))) > 0
    table[measured, ]
}

#' @rdname internal_accessors
assay_table <- function(object, assay_name) {
    if (ncol(object) == 0) {
        return(tibble())
    }
    values <- SummarizedExperiment::assay(object, assay_name,
        withDimnames = TRUE)
    rownames(values) <- NULL
    dplyr::bind_cols(
        tibble(Barcode = SummarizedExperiment::rowData(object)$Barcode),
        tibble::as_tibble(values, .name_repair = "minimal")
    )
}

#' @rdname internal_accessors
annotation_of <- function(object) {
    update_screenr_object(object)@annotation_table
}

#' @rdname internal_accessors
groups_of <- function(object) {
    object <- update_screenr_object(object)
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

#' @rdname internal_accessors
sample_table_of <- function(object) {
    object <- update_screenr_object(object)
    col_data <- SummarizedExperiment::colData(object)
    columns <- setdiff(colnames(col_data), "Group")
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
    return(groups_of(object))
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
    return(update_screenr_object(object)@replicates)
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
    data_table <- update_screenr_object(object)@data_table
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
    old_samples <- colnames(object)
    new_samples <- setdiff(colnames(count_table), "Barcode")
    sample_table <- NULL
    if (identical(old_samples, new_samples)) {
      sample_table <- sample_table_of(object)
    }
    normalized <- NULL
    if ("normalized" %in% SummarizedExperiment::assayNames(object)) {
      normalized <- normalized_table_of(object)
    }
    build_screenr_object(
      count_table = count_table,
      annotation_table = object@annotation_table,
      groups = groups_of(object),
      replicates = object@replicates,
      normalized_count_table = normalized,
      data_table = object@data_table,
      sample_table = sample_table,
      column_names = object@column_names
    )
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
    object@annotation_table <- annotation_table
    if (nrow(object) > 0 && "Barcode" %in% colnames(annotation_table)) {
      row_data <- SummarizedExperiment::rowData(object)
      row_data <- row_data[, "Barcode", drop = FALSE]
      matched <- match(as.character(row_data$Barcode),
        as.character(annotation_table$Barcode))
      for (column in setdiff(colnames(annotation_table), "Barcode")) {
        row_data[[column]] <- annotation_table[[column]][matched]
      }
      SummarizedExperiment::rowData(object) <- row_data
    }
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
    object@replicates <- replicates
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
      SummarizedExperiment::assay(object, "normalized") <- NULL
      return(object)
    }
    if (!"Barcode" %in% colnames(normalized_count_table)) {
      stop("The normalized count table must have a Barcode column.")
    }
    counts <- SummarizedExperiment::assay(object, "counts",
      withDimnames = TRUE)
    SummarizedExperiment::assay(object, "normalized", withDimnames = FALSE) <-
      match_to_counts(normalized_count_table, counts)
    return(object)
  }
)


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
    object@data_table <- data_table
    return(object)
  }
)

#' @title Subset a ScreenR object
#' @description Subsets the barcodes (rows) and the samples (columns) of a
#'              ScreenR object, like any
#'              \code{\link[SummarizedExperiment]{SummarizedExperiment}}.
#'              The normalized counts and the data table are subset to the
#'              same barcodes and samples.
#' @param x A ScreenR object.
#' @param i,j The barcodes and samples to keep (indices, names or logical
#'            vectors).
#' @param ... Not used.
#' @param drop Not used.
#' @return The subset ScreenR object.
#' @seealso \code{\link{remove_all_zero_row}}
#' @concept objects
#' @rdname subset-screenr_object
#' @export
#' @examples
#' object <- update_screenr_object(
#'     get0("object", envir = asNamespace("ScreenR"))
#' )
#' object[1:100, ]
#' object[, object$Treatment == "TRT"]
setMethod(
  f = "[",
  signature = "screenr_object",
  definition = function(x, i, j, ..., drop = TRUE) {
    x <- update_screenr_object(x)
    x <- methods::callNextMethod()
    data_table <- x@data_table
    if (nrow(data_table) > 0 && all(c("Barcode", "Sample") %in%
      colnames(data_table))) {
      keep <- as.character(data_table$Barcode) %in%
        as.character(SummarizedExperiment::rowData(x)$Barcode) &
        as.character(data_table$Sample) %in% colnames(x)
      x@data_table <- data_table[keep, ]
    }
    x
  }
)

#' @title Show a ScreenR object
#' @description Prints a summary of a ScreenR object: the
#'              SummarizedExperiment summary and the size of the data table.
#'              Objects created with older versions of ScreenR are converted
#'              first (see \code{\link{update_screenr_object}}).
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
    if (is_legacy_screenr(object)) {
      object <- update_screenr_object(object)
      show(object)
      return(invisible(NULL))
    }
    methods::callNextMethod()
    cat("data table:", nrow(object@data_table), "rows,",
      ncol(object@data_table), "columns\n")
    invisible(NULL)
  }
)
