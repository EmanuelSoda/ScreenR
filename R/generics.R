###### Get Fuctions

#' @title Get the count table
#' @description Returns the raw count table of a ScreenR object, with one row
#'     for each barcode and one column for each sample. A short summary of the
#'     size of the table is printed.
#'
#'     The table is a view of the counts assay
#'     (\code{assay(object, "counts")}): the Barcode column is made from the
#'     row names and is not a sample. Changing the returned table does not
#'     change the object; use \code{\link{set_count_table}}.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A tibble with the Barcode column and one column of counts for each
#'     sample.
#' @seealso \code{\link{set_count_table}},
#'     \code{\link{get_normalized_count_table}},
#'     \code{\link{create_screenr_object}}
#' @concept objects
#' @importFrom methods slot
#' @rdname get_count_table
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' count_table <- get_count_table(object)
#' head(count_table)
setGeneric(
    "get_count_table",
    function(object) {
        standardGeneric("get_count_table")
    }
)


#' @title Get the annotation table
#' @description Returns the annotation table of a ScreenR object, with the
#'     gene (and any other information) of each barcode. A short summary of
#'     the size of the table is printed.
#'
#'     The annotation of the barcodes of the object is its rowData, which is
#'     what the analyses use; the returned table has those rows first,
#'     followed by the rows of the annotation given by the user whose barcodes
#'     are not in the count table (barcodes of the library that were not
#'     observed).
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return The annotation table, a data frame with at least the columns
#'     Barcode and Gene.
#' @seealso \code{\link{set_annotation_table}},
#'     \code{\link{create_screenr_object}}
#' @concept objects
#' @rdname get_annotation_table
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' annotation_table <- get_annotation_table(object)
#' head(annotation_table)
setGeneric(
    "get_annotation_table",
    function(object) {
        standardGeneric("get_annotation_table")
    }
)

#' @title Get the groups
#' @description Returns the experimental group of each sample of a ScreenR
#'     object, as given to \code{\link{create_screenr_object}}. The groups
#'     define the columns of the design matrix
#'     (\code{\link{create_design_matrix}}).
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A factor with one value for each sample.
#' @seealso \code{\link{set_groups}}, \code{\link{create_screenr_object}}
#' @concept objects
#' @export
#' @rdname get_groups
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' groups <- get_groups(object)
#' groups
setGeneric(
    "get_groups",
    function(object) {
        standardGeneric("get_groups")
    }
)


#' @title Get the replicates
#' @description Returns the replicate labels stored in a ScreenR object:
#'     the Replicate column of the colData when one label was given for each
#'     sample, otherwise the value given to
#'     \code{\link{create_screenr_object}}.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return The vector of replicate labels.
#' @seealso \code{\link{set_replicates}}, \code{\link{create_screenr_object}}
#' @export
#' @concept objects
#' @rdname get_replicates
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' replicates <- get_replicates(object)
#' replicates
setGeneric(
    "get_replicates",
    function(object) {
        standardGeneric("get_replicates")
    }
)

#' @title Get the normalized count table
#' @description Returns the normalized count table of a ScreenR object. A
#'     short summary of the size of the table is printed. The normalized
#'     counts are not stored: they are computed from the raw counts and the
#'     effective library sizes set by \code{\link{normalize_data}} (raw count
#'     / effective library size * 1e6), so they always follow the counts and
#'     the normalization.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A tibble with the Barcode column and one column of normalized
#'     counts for each sample (an empty tibble if the data have not been
#'     normalized).
#' @seealso \code{\link{normalize_data}},
#'     \code{\link{set_normalized_count_table}},
#'     \code{\link{get_count_table}}
#' @export
#' @rdname get_normalized_count_table
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' normalized_count_table
setGeneric(
    "get_normalized_count_table",
    function(object) {
        standardGeneric("get_normalized_count_table")
    }
)


#' @title Get the data table
#' @description Returns the data table of a ScreenR object, the tidy (long)
#'     version of the normalized counts. A short summary of the size of the
#'     table is printed. The data table is not stored: it is computed from the
#'     counts, the annotation (rowData), the sample table (colData) and the
#'     normalization each time, so it always follows them. The object must be
#'     normalized (\code{\link{normalize_data}}).
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A tibble with one row for each barcode and sample and the columns
#'     Barcode, Gene, Sample, Frequency (the normalized count), Day and
#'     Treatment (plus the other annotation columns).
#' @seealso \code{\link{compute_data_table}}, \code{\link{set_data_table}}
#' @export
#' @concept objects
#' @rdname get_data_table
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' data_table <- get_data_table(object)
#' data_table
setGeneric(
    "get_data_table",
    function(object) {
        standardGeneric("get_data_table")
    }
)




###### Set Fuctions

#' @title Set the count table
#' @description Replaces the raw count table of a ScreenR object. The
#'     annotation and the groups are kept (the annotation is matched to the
#'     new barcodes by identifier); the Day and Treatment of the samples are
#'     kept when the samples are the same, otherwise they are taken again from
#'     the sample names. The table is checked as in
#'     \code{\link{create_screenr_object}} (identifiers, invalid counts,
#'     duplicated barcodes). When the counts change, the normalization is
#'     removed (with a message): run \code{\link{normalize_data}} again. The
#'     same holds for \code{assay(object, "counts") <- } (see
#'     \code{\link{screenr_object-replace}}).
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param count_table A count table with a Barcode column and one column of
#'     counts for each sample.
#' @return The ScreenR object with the new count table.
#' @seealso \code{\link{get_count_table}}, \code{\link{create_screenr_object}}
#' @concept objects
#' @importFrom methods slot
#' @export
#' @rdname set_count_table
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' counts <- get_count_table(object)
#' set_count_table(object, counts)
setGeneric(
    "set_count_table",
    function(object, count_table) {
        standardGeneric("set_count_table")
    }
)

#' @title Set the annotation table
#' @description Replaces the annotation table of a ScreenR object. The
#'     annotation of the barcodes of the count table (the rowData of the
#'     object) is updated by matching the Barcode column, so the new genes are
#'     used by all the following analyses (data table, metrics, edgeR/limma
#'     methods). Barcodes annotated more than once with different values are
#'     an error.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param annotation_table A data frame with the annotation of each barcode,
#'     with at least the columns Barcode and Gene.
#' @return The ScreenR object with the new annotation table.
#' @seealso \code{\link{get_annotation_table}},
#'     \code{\link{create_screenr_object}}
#' @export
#' @rdname set_annotation_table
#' @concept objects
#' @importFrom methods slot<-
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' annotation <- get_annotation_table(object)
#' set_annotation_table(object, annotation)
setGeneric(
    "set_annotation_table",
    function(object, annotation_table) {
        standardGeneric("set_annotation_table")
    }
)

#' @title Set the groups
#' @description Replaces the experimental groups of the samples of a ScreenR
#'     object. A warning is given when the number of groups differs from the
#'     number of samples.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param groups A factor (or a vector, which is converted to a factor) with
#'     the group of each sample, for example "Control" and "Treated".
#' @return The ScreenR object with the new groups.
#' @seealso \code{\link{get_groups}}, \code{\link{create_design_matrix}}
#' @export
#' @concept objects
#' @rdname set_groups
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' groups <- get_groups(object)
#' set_groups(object, groups)
setGeneric(
    "set_groups",
    function(object, groups) {
        standardGeneric("set_groups")
    }
)


#' @title Set the replicates
#' @description Replaces the replicate labels of a ScreenR object.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param replicates A vector with the replicate labels.
#' @return The ScreenR object with the new replicates.
#' @seealso \code{\link{get_replicates}}
#' @export
#' @rdname set_replicates
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' replicates <- get_replicates(object)
#' set_replicates(object, replicates)
setGeneric(
    "set_replicates",
    function(object, replicates) {
        standardGeneric("set_replicates")
    }
)

#' @title Set the normalized count table
#' @description Sets the normalization of a ScreenR object from a table of
#'     normalized counts, for example counts normalized outside ScreenR.
#'     Usually the normalization is set with \code{\link{normalize_data}}.
#'     ScreenR uses one normalization for the plots and the edgeR/limma
#'     models, expressed as one effective library size for each sample, so
#'     the table must be the raw counts divided by one factor for each sample
#'     (times 1e6): the effective library sizes are computed from it and the
#'     method is recorded as "custom". Any other table is an error. The table
#'     is matched to the counts by barcode and sample; an empty table removes
#'     the normalization.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param normalized_count_table A table with a Barcode column and one column
#'     of normalized counts for each sample.
#' @return The ScreenR object with the new normalized count table.
#' @seealso \code{\link{normalize_data}},
#'     \code{\link{get_normalized_count_table}}
#' @export
#' @rdname set_normalized_count_table
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' set_normalized_count_table(object, normalized_count_table)
setGeneric(
    "set_normalized_count_table",
    function(object, normalized_count_table) {
        standardGeneric("set_normalized_count_table")
    }
)

#' @title Set the data table
#' @description Deprecated. The data table is no longer stored: it is
#'     computed from the counts, the annotation, the sample table and the
#'     normalization of the object when needed (see
#'     \code{\link{get_data_table}}), so it cannot be replaced. Given the
#'     current data table, the object is returned unchanged with a
#'     deprecation warning; any other table is an error. To change the data
#'     table, change the object (subset it, or use
#'     \code{\link{set_annotation_table}} or
#'     \code{\link{normalize_data}}).
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param data_table A data table in tidy (long) format, with the columns
#'     described in \code{\link{get_data_table}}.
#' @return The unchanged ScreenR object (with a warning).
#' @seealso \code{\link{compute_data_table}}, \code{\link{get_data_table}}
#' @export
#' @rdname set_data_table
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' data_table <- get_data_table(object)
#' set_data_table(object, data_table)
setGeneric(
    "set_data_table",
    function(object, data_table) {
        standardGeneric("set_data_table")
    }
)
