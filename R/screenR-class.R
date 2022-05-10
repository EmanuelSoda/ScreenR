#' @include  generics.R

#' @title S4 ScreenR object class
#' The screenr_object class is the main object of the package, it is passed
#' to a series of function to perform the analysis.
#'
#' @slot count_table It is used to store the count table to perform the
#'                   analysis
#' @slot annotation_table It is used to store the annotation of the shRNA
#' @slot groups It is used to store the vector of treated and untreated
#' @slot replicates It is used to store information about the replicates
#' @slot normalized_count_table It is used to store a normalized verision of
#'                              the count table
#' @slot data_table It is used to store a tidy format of the count table
#' @exportClass screenr_object
#' @concept objects
#' @rdname get_count_table
#' @examples
#' data("count_table", package = "ScreenR")
#' data("annotation_table", package = "ScreenR")
#'
#' groups <- factor(c("T1/T2", "T1/T2", "Treated", "Treated", "Treated",
#'                    "Control", "Control", "Control", "Treated", "Treated",
#'                    "Treated", "Control", "Control", "Control"))
#'
#' obj <- create_screenr_object(table = count_table,
#'                              annotation = annotation_table,
#'                              groups = groups,
#'                              replicates = c(""))
screenr_object <- setClass("screenr_object", methods::representation(
    count_table = "data.frame",
    annotation_table = "data.frame", groups = "factor", replicates = "vector",
    normalized_count_table = "data.frame", data_table = "data.frame"
))




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
        count_table <- slot(object = object, name = "count_table")
        cat(
            "ScreenR count table containing:\n",
            nrow(x = count_table), "rows\n",
            ncol(x = count_table), "columns\n"
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
        annotation_table <- slot(object = object, name = "annotation_table")
        cat(
            "ScreenR annotation table containing:\n",
            nrow(annotation_table),
            "rows\n",
            ncol(annotation_table),
            "columns\n"
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
        return(slot(object = object, name = "groups"))
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
        return(slot(object = object, name = "replicates"))
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
        normalized_count_table <-
            slot(object = object, name = "normalized_count_table")
        cat(
            "ScreenR normalized count table containing:\n",
            nrow(x = normalized_count_table), "rows\n",
            ncol(x = normalized_count_table), "columns\n"
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
        data_table <-
            slot(object = object, name = "data_table")
        cat(
            "ScreenR normalized data table containing:\n",
            nrow(x = data_table), "rows\n",
            ncol(x = data_table), "columns\n"
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
        slot(object = object, name = "count_table") <- count_table
        return(object)
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
        slot(object = object, name = "annotation_table") <- annotation_table
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
        slot(object = object, name = "groups") <- groups
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
        slot(object = object, name = "replicates") <- replicates
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
        slot(object = object,
             name = "normalized_count_table") <- normalized_count_table
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
        slot(object = object,
             name = "data_table") <- data_table
        return(object)
    }
)
