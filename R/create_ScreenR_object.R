#' @title Create the ScreenR Object
#' @description Initial function to create the Screen Object.
#' @param count_table The count table obtained from the read alignment that
#'                    contains the Barcodes as rows and samples as columns.
#' @param annotation_table The annotation table containing the information
#'                        for each Barcode and the association to the
#'                        corresponding Gene
#' @param groups A factor containing the experimental design label
#' @param replicates A vector containing the replicates label
#'
#' @return An object containing all the needed information for the analysis.
#' @export
create_screenR_object <- function(table = NULL, annotation = NULL,
                                  groups = NULL, replicates = NULL){
  if (is.null(table)) {
    stop("The table is NULL")
  } else if (is.null(annotation)) {
    stop("The annotation is NULL")
  } else if (is.null(groups)) {
    stop("The groups is NULL")
  } else if (is.null(replicates)) {
    stop("The replicates is NULL")
  }
  object <- new("screenR_object",
                count_table = table,
                annotation_table = annotation,
                groups = groups,
                replicates = replicates,
                normalized_count_table = data.frame())
  return(object)
}
