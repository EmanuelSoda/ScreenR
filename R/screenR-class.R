#' @include  S4-methods.R

#' @title S4 ScreenR object Class
#' The screenr_object class is the main object of the package, it is passed
#' to a series of function to perform the analysis. I
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
# S4 methods
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Get ScreenR count table
#' @description Get function for the count table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The count table of the ScreenR object
#' @concept objects
#' @importFrom methods slot
#' @aliases get_count_table,screenr_object
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' count_table <- get_count_table(object)
#' head(count_table)
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


# get_annotation_table
#' @title Get ScreenR annotation table
#' @description Get function for the annotation table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The annotation table of the ScreenR object
#' @export
#' @concept objects
#' @aliases get_annotation_table,screenr_object
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' annotation_table <- get_annotation_table(object)
#' head(annotation_table)
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

#' @title Get ScreenR groups
#' @description Get function for the groups of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The groups of the ScreenR object
#' @export
#' @aliases get_groups,screenr_object
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' groups <- get_groups(object)
#' groups
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


# get_replicates
#' @title Get ScreenR replicates
#' @description Get function for the replicates of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The replicates of the ScreenR object
#' @export
#' @concept objects
#' @aliases get_replicates,screenr_object
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' replicates <- get_replicates(object)
#' replicates
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

# get_normalized_count_table
#' @title Get ScreenR normalized_count_table
#' @description Get function for the normalized_count_table of
#'              the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The normalized_count_table of the ScreenR object
#' @export
#' @aliases get_normalized_count_table,screenr_object
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' normalized_count_table
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


# get_data_table
#' @title Get ScreenR data_table
#' @description Get function for the data_table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The data_table of the ScreenR object
#' @export
#' @aliases get_data_table,screenr_object
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' data_table <- get_data_table(object)
#' data_table
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


########### Set
# set_count_table
#' @title Set ScreenR count table
#' @description Set function for the count table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param count_table A count table containing in each row an shRNA and in each
#'                    column a sample
#' @return The ScreenR object with the count table
#' @concept objects
#' @importFrom methods slot
#' @aliases set_count_table,screenr_object
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' counts <- get_count_table(object)
#' set_count_table(object, counts)
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

# set_annotation_table
#' @title Set ScreenR annotation table
#' @description Set function for the annotation table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param annotation_table a table containing the annotation for each shRNA
#' @return The ScreenR object with the annotation table
#' @export
#' @aliases set_annotation_table,screenr_object
#' @concept objects
#' @importFrom methods slot<-
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' annotation <- get_annotation_table(object)
#' set_annotation_table(object, annotation)
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

# set_groups
#' @title Set ScreenR groups
#' @description Set function for the groups of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param groups The treatment and control groups
#' @return The ScreenR object containing the group field
#' @export
#' @aliases set_groups,screenr_object
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' groups <- get_groups(object)
#' set_groups(object, groups)
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


# set_replicates
#' @title Set ScreenR replicates
#' @description Set function for the replicates of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The ScreenR object with the specific replicates
#' @param  replicates The vecotr containing the replicates name
#' @export
#' @concept objects
#' @aliases set_replicates,screenr_object
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' replicates <- get_replicates(object)
#' set_replicates(object, replicates)
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

# set_normalized_count_table
#' @title Set ScreenR normalized_count_table
#' @description Set function for the normalized_count_table of
#'              the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The ScreenR object with the setted normalized_count_table
#' @param normalized_count_table A table of the normalized count table
#' @export
#' @aliases set_normalized_count_table,screenr_object
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' normalized_count_table
#' set_normalized_count_table(object, normalized_count_table)
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


# set_data_table
#' @title Set ScreenR data_table
#' @description Set function for the data_table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param data_table A count table in a tidy format
#' @return The ScreenR object with the setted data_table
#' @export
#' @aliases set_data_table,screenr_object
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' data_table <- get_data_table(object)
#' set_data_table(object, data_table)
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







