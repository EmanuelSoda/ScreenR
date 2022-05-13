###### Get Fuctions

#' @title Get ScreenR count table
#' @description Get function for the count table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The count table of the ScreenR object
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


#' @title Get ScreenR annotation table
#' @description Get function for the annotation table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The annotation table of the ScreenR object
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

#' @title Get ScreenR groups
#' @description Get function for the groups of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The groups of the ScreenR object
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


#' @title Get ScreenR replicates
#' @description Get function for the replicates of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The replicates of the ScreenR object
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

#' @title Get ScreenR normalized_count_table
#' @description Get function for the normalized_count_table of
#'              the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The normalized_count_table of the ScreenR object
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


#' @title Get ScreenR data_table
#' @description Get function for the data_table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The data_table of the ScreenR object
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

#' @title Set ScreenR count table
#' @description Set function for the count table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param count_table A count table containing in each row an shRNA and in each
#'                    column a sample
#' @return The ScreenR object with the count table
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

#' @title Set ScreenR annotation table
#' @description Set function for the annotation table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param annotation_table a table containing the annotation for each shRNA
#' @return The ScreenR object with the annotation table
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

#' @title Set ScreenR groups
#' @description Set function for the groups of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param groups The treatment and control groups
#' @return The ScreenR object containing the group field
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


#' @title Set ScreenR replicates
#' @description Set function for the replicates of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param replicates Additional parameter
#' @return The ScreenR object with the specific replicates
#' @param  replicates The vector containing the replicates name
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

#' @title Set ScreenR normalized_count_table
#' @description Set function for the normalized_count_table of
#'              the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param normalized_count_table A table of the normalized count table
#' @return The ScreenR object with the set normalized_count_table
#' @export
#' @rdname set_normalized_count_table
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' normalized_count_table
#' set_normalized_count_table(object, normalized_count_table)
setGeneric(
    "set_normalized_count_table",
    function(object, normalized_count_table) {
        standardGeneric("set_normalized_count_table")
    }
)

#' @title Set ScreenR data_table
#' @description Set function for the data_table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param data_table A count table in a tidy format
#' @return The ScreenR object with the set data_table
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
