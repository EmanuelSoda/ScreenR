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
#' @concept objects
#' @return An object containing all the needed information for the analysis.
#' @export
create_screenR_object <- function(table = NULL, annotation = NULL,
    groups = NULL, replicates = c("")) {
    #arcode <- as.factor(table$Barcode)
    annotation$Barcode <- as.factor(annotation$Barcode)
    object <- methods::new("screenr_object",
        count_table = tibble(table),
        annotation_table = tibble(annotation), groups = groups,
        replicates = replicates, normalized_count_table = tibble(),
        data_table = tibble()
    )
    return(object)
}
