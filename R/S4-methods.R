###### Get Fuctions

#' @title Get ScreenR count table
#' @description Get function for the count table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The count table of the ScreenR object
#' @concept objects
#' @importFrom methods slot
#' @keywords internal
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' count_table <- get_count_table(object)
#' head(count_table)
get_count_table <- function(object) {
  UseMethod(generic = 'get_count_table', object = object)
}


#' @title Get ScreenR annotation table
#' @description Get function for the annotation table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The annotation table of the ScreenR object
#' @keywords internal
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' annotation_table <- get_annotation_table(object)
#' head(annotation_table)
get_annotation_table <- function(object) {
  UseMethod(generic = 'get_annotation_table', object = object)
}


#' @title Get ScreenR groups
#' @description Get function for the groups of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The groups of the ScreenR object
#' @keywords internal
#' #' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' groups <- get_groups(object)
#' groups
get_groups <- function(object) {
  UseMethod(generic = 'get_groups', object = object)
}


#' @title Get ScreenR replicates
#' @description Get function for the replicates of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The replicates of the ScreenR object
#' @keywords internal
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' replicates <- get_replicates(object)
#' replicates
get_replicates <- function(object) {
  UseMethod(generic = 'get_replicates', object = object)
}


#' @title Get ScreenR normalized_count_table
#' @description Get function for the normalized_count_table of
#'              the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The normalized_count_table of the ScreenR object
#' @keywords internal
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' normalized_count_table
get_normalized_count_table <- function(object) {
  UseMethod(generic = 'get_normalized_count_table', object = object)
}


#' @title Get ScreenR data_table
#' @description Get function for the data_table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The data_table of the ScreenR object
#' @keywords internal
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' data_table <- get_data_table(object)
#' data_table
get_data_table <- function(object) {
  UseMethod(generic = 'get_data_table', object = object)
}





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
#' @keywords internal
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' counts <- get_count_table(object)
#' set_count_table(object, counts)
set_count_table <- function(object, count_table) {
  UseMethod(generic = 'set_count_table', object = object)
}


#' @title Set ScreenR annotation table
#' @description Set function for the annotation table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param annotation_table a table containing the annotation for each shRNA
#' @return The ScreenR object with the annotation table
#' @keywords internal
#' @concept objects
#' @importFrom methods slot<-
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' annotation <- get_annotation_table(object)
#' set_annotation_table(object, annotation)
set_annotation_table <- function(object, annotation_table) {
  UseMethod(generic = 'set_annotation_table', object = object)
}


#' @title Set ScreenR groups
#' @description Set function for the groups of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param groups The treatment and control groups
#' @return The ScreenR object containing the group field
#' @keywords internal
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' groups <- get_groups(object)
#' set_groups(object, groups)
set_groups <- function(object, groups) {
  UseMethod(generic = 'groups', object = object)
}



#' @title Set ScreenR replicates
#' @description Set function for the replicates of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The ScreenR object with the specific replicates
#' @param  replicates The vecotr containing the replicates name
#' @keywords internal
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' replicates <- get_replicates(object)
#' set_replicates(object, replicates)
set_replicates <- function(object, replicates) {
  UseMethod(generic = 'replicates', object = object)
}


#' @title Set ScreenR normalized_count_table
#' @description Set function for the normalized_count_table of
#'              the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @return The ScreenR object with the setted normalized_count_table
#' @param normalized_count_table A table of the normalized count table
#' @keywords internal
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' normalized_count_table
#' set_normalized_count_table(object, normalized_count_table)
set_normalized_count_table <- function(object, normalized_count_table) {
  UseMethod(generic = 'normalized_count_table', object = object)
}


#' @title Set ScreenR data_table
#' @description Set function for the data_table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param data_table A count table in a tidy format
#' @return The ScreenR object with the setted data_table
#' @keywords internal
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' data_table <- get_data_table(object)
#' set_data_table(object, data_table)
set_data_table <- function(object, data_table) {
  UseMethod(generic = 'data_table', object = object)
}

