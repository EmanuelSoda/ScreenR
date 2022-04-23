#' The screenr_object Class
#' The screenr_object class is the main object of the package, it is passed
#' to a series of function to perform the analysis.
#'
#' @slot count_table It is used to store the count table to perform the analysis
#' @slot annotation_table It is used to store the annotation of the shRNA
#' @slot groups It is used to store the vector of treated and untreated
#' @slot replicates It is used to store information about the replicates
#' @slot normalized_count_table It is used to store a normalized verision of the
#'                              count table
#' @slot data_table It is used to store a tidy format of the count table
#' @concept objects

screenr_object <- setClass("screenr_object", methods::representation(
    count_table = "data.frame",
    annotation_table = "data.frame", groups = "factor", replicates = "vector",
    normalized_count_table = "data.frame", data_table = "data.frame"))



###### Get Fuctions

#' @title Get ScreenR count table
#' @description Get function for the count table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @return The count table of the ScreenR object
#' @concept objects
#' @importFrom methods slot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' count_table <- get_count_table(object)
#' head(count_table)
get_count_table <- function(object){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  return(slot(object = object, name = 'count_table'))
}


#' @title Get ScreenR annotation table
#' @description Get function for the annotation table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @return The annotation table of the ScreenR object
#' @export
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' annotation_table <- get_annotation_table(object)
#' head(annotation_table)
get_annotation_table <- function(object){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  return(slot(object = object, name = 'annotation_table'))
}


#' @title Get ScreenR groups
#' @description Get function for the groups of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @return The groups of the ScreenR object
#' @export

#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' groups <- get_groups(object)
#' groups
get_groups <- function(object){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  return(slot(object = object, name = 'groups'))
}


#' @title Get ScreenR replicates
#' @description Get function for the replicates of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @return The replicates of the ScreenR object
#' @export
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' replicates <- get_replicates(object)
#' replicates
get_replicates <- function(object){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  return(slot(object = object, name = 'replicates'))
}


#' @title Get ScreenR normalized_count_table
#' @description Get function for the normalized_count_table of
#'              the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @return The normalized_count_table of the ScreenR object
#' @export
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' normalized_count_table
get_normalized_count_table <- function(object){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  return(slot(object = object, name = 'normalized_count_table'))
}


#' @title Get ScreenR data_table
#' @description Get function for the data_table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @return The data_table of the ScreenR object
#' @export
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' data_table <- get_data_table(object)
#' data_table
get_data_table <- function(object){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  return(slot(object = object, name = 'data_table'))
}





###### Set Fuctions

#' @title Set ScreenR count table
#' @description Set function for the count table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param count_table A count table containing in each row an shRNA and in each
#'                    column a sample
#' @return The ScreenR object with the count table
#' @concept objects
#' @importFrom methods slot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' counts <- get_count_table(object)
#' set_count_table(object, counts)
set_count_table <- function(object, count_table){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  if (is.null(count_table)) {
    warning("You are inputing a NULL count_table")
  }

  slot(object = object, name = 'count_table') <- count_table
  return(object)
}


#' @title Set ScreenR annotation table
#' @description Set function for the annotation table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param annotation_table a table containing the annotation for each shRNA
#' @return The ScreenR object with the annotation table
#' @export
#' @concept objects
#' @importFrom methods slot<-
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' annotation <- get_annotation_table(object)
#' set_annotation_table(object, annotation)
set_annotation_table <- function(object, annotation_table){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  if (is.null(annotation_table)) {
    warning("You are inputing a NULL annotation_table")
  }
  slot(object = object, name = 'annotation_table') <- annotation_table
  return(object)
}


#' @title Set ScreenR groups
#' @description Set function for the groups of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param groups The treatment and control groups
#' @return The ScreenR object containing the group field
#' @export

#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' groups <- get_groups(object)
#' set_groups(object, groups)
set_groups <- function(object, groups){
  if (is.null(object)) {
    stop('The object is not defined!')
    }
  if (is.null(groups)) {
    warning("You are inputing a NULL groups")
    }
  slot(object = object, name = 'groups') <- groups
  return(object)
}



#' @title Set ScreenR replicates
#' @description Set function for the replicates of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @return The ScreenR object with the specific replicates
#' @param  replicates The vecotr containing the replicates name
#' @export
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' replicates <- get_replicates(object)
#' set_replicates(object, replicates)
set_replicates <- function(object, replicates){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  if (is.null(replicates)) {
    warning("You are inputing a NULL replicates")
  }
  slot(object = object, name = 'replicates') <- replicates
  return(object)
}


#' @title Set ScreenR normalized_count_table
#' @description Set function for the normalized_count_table of
#'              the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @return The ScreenR object with the setted normalized_count_table
#' @param normalized_count_table A table of the normalized count table
#' @export
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' normalized_count_table <- get_normalized_count_table(object)
#' normalized_count_table
#' set_normalized_count_table(object, normalized_count_table)
set_normalized_count_table <- function(object, normalized_count_table){
  if (is.null(object)) {
    stop('The object is not defined!')
  }

  if (is.null(object@normalized_count_table)) {
    message <- paste("The normalized_count_table is not yet defined!\n",
                     "Please perform  normalize_data() to normalization")
    stop(message)
  }
  if (is.null(normalized_count_table)) {
    warning("You are inputing a NULL normalized_count_table")
  }

  slot(object = object, name = 'normalized_count_table') <-
    normalized_count_table

  return(object)
}


#' @title Set ScreenR data_table
#' @description Set function for the data_table of the ScreenR object
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param data_table A count table in a tidy format
#' @return The ScreenR object with the setted data_table
#' @export
#' @concept objects
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' data_table <- get_data_table(object)
#' set_data_table(object, data_table)
set_data_table <- function(object, data_table){
  if (is.null(object)) {
    stop('The object is not defined!')
  }
  if (is.null(data_table)) {
    warning("You are inputing a NULL data_table")
  }
  slot(object = object, name = 'data_table') <- data_table
  return(object)
}









