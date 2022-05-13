#' @title Create edgeR Object
#' @description Utility function that using the screenr-class
#'              object create the corresponding edgeR object.
#'              This function and other utility function enables the user to
#'              not worry abut the implementation and just focus
#'              on the analysis. The ScreenR package will take care of the rest.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @importFrom  edgeR DGEList
#' @return The edgeR object will all the needed information for the analysis.
#' @concept objects
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' create_edger_obj(object)
create_edger_obj <- function(screenR_Object) {
    # First create the Matrix of the Count table
    counts <- screenR_Object@normalized_count_table
    counts <- as.matrix(dplyr::select_if(counts, is.numeric))
    # The group for the treatment
    groups <- screenR_Object@groups

    # The annotation
    genes <- screenR_Object@annotation_table

    # Create the edgeR object
    DGEList <- edgeR::DGEList(counts = counts,
                              group = groups,
                              genes = genes)
    return(DGEList)
}
