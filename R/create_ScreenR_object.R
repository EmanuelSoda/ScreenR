#' @title Create the ScreenR Object
#' @description Initial function to create the Screen Object.
#' @param table The count table obtained from the read alignment that
#'                    contains the Barcodes as rows and samples as columns.
#' @param annotation The annotation table containing the information
#'                        for each Barcode and the association to the
#'                        corresponding Gene
#' @param groups A factor containing the experimental design label
#' @param replicates A vector containing the replicates label
#' @importFrom rlang .data
#' @importFrom methods new
#' @return An object containing all the needed information for the analysis.
#' @export
create_screenR_object <- function(table = NULL, annotation = NULL,
                                  groups = NULL, replicates = c("")){
arcode <- as.factor(table$Barcode)
  annotation$Barcode <- as.factor(annotation$Barcode)
  object <- new("screenR_object",
                count_table = table,
                annotation_table = annotation,
                groups = groups,
                replicates = replicates,
                normalized_count_table = data.frame(),
                data_table = data.frame())
  return(object)
}
