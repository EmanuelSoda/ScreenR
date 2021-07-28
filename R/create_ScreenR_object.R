#' @title Create the Screen Object
#' @description Initial function to create the Screen Object.
#' @param count_table The count table obtained from the read alignment that
#'                    contains the Barcodes as rows and samples as columns.
#' @param annotation_table The annotation table containing the information
#'                        for each Barcode and the association to the
#'                        corresponding Gene
#' @param groups A factor containing the experimental design label
#' @param replicates A factor containing the replicates
#' @param color_palette a vector containing the color palette
#'
#' @return An object containing all the information for the analysis.

create_ScreenR_object <- function(count_table, annotation_table,
                                 groups, replicates, color_palette) {
  setClass("Screen_object",
           slots = list(count_table = count_table,
                        annotation_table = annotation_table,
                        groups = groups,
                        replicates = replicates,
                        color_palette =  color_palette))
}
