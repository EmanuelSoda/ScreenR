#' @title Create EdgeR Object
#' @description Utility function that using the screenR-class
#'              object create the corresponding edgeR object
#' @param screenR_Object The Object of the package
#' @import edgeR

#' @return The edgeR object edgeR
#' @export
create_edgeR_obj <- function(screenR_Object){
  # First create the Matrix of the Count table
  counts <- screenR_Object@normalized_count_table
  counts <- as.matrix(counts[, 2: dim(counts)[2]])

  # The group for the treatment
  groups <- screenR_Object@groups

  # The annotation
  genes <- screenR_Object@annotation_table

  # Create the edgeR object
  DGEList <- edgeR::DGEList(counts = counts, group = groups,
                            genes = genes)
  return(DGEList)
}
