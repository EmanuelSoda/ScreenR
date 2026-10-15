###### Get Fuctions

#' @title Get the count table
#' @description Returns the raw count table of a ScreenR object, with one row
#'     for each barcode and one column for each sample. A short summary of the
#'     size of the table is printed.
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
#' @description Returns the replicate labels stored in a ScreenR object.
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
#' @description Returns the normalized count table of a ScreenR object,
#'     computed with \code{\link{normalize_data}}. A short summary of the size
#'     of the table is printed.
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
#'     version of the normalized counts computed with
#'     \code{\link{compute_data_table}}. A short summary of the size of the
#'     table is printed.
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
#'     annotation, the groups and the normalized counts are kept (the
#'     normalized counts are matched to the new table by barcode and sample);
#'     the Day and Treatment of the samples are kept when the samples are the
#'     same, otherwise they are taken again from the sample names. Unlike
#'     \code{\link{create_screenr_object}}, the table is not checked.
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
#'     object) is updated by matching the Barcode column.
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
#' @description Replaces the normalized count table of a ScreenR object, for
#'     example with counts normalized outside ScreenR. The table is matched to
#'     the count table by barcode and sample; an empty table removes the
#'     normalized counts. Usually the normalized counts are computed with
#'     \code{\link{normalize_data}}.
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
#' @description Replaces the data table of a ScreenR object. Usually the data
#'     table is computed with \code{\link{compute_data_table}}.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param data_table A data table in tidy (long) format, with the columns
#'     described in \code{\link{get_data_table}}.
#' @return The ScreenR object with the new data table.
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
