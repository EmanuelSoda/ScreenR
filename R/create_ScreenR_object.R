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
#' @examples
#' count_table <-
#'     data.frame(
#'         Barcode = c("Code_1", "Code_2", "Code_3", "Code_3"),
#'         Time_3_rep1 = c("3520", "3020", "1507", "1400"),
#'         Time_3_rep2 = c("3500", "3000", "1457", "1490"),
#'         Time_3_TRT_rep1 = c("1200", "1100", "1300", "1350"),
#'         Time_3_TRT_rep2 = c("1250", "1000", "1400", "1375")
#'     )
#' annotation_table <-
#'     data.frame(
#'         Gene = c("Gene_1", "Gene_1", "Code_2", "Code_2"),
#'         Barcode = c("Code_1", "Code_2", "Code_3", "Code_3"),
#'         Gene_ID = rep(NA, 4), Sequence = rep(NA, 4),
#'         Library = rep(NA, 4)
#'     )
#'
#' groups <- factor(c("Control", "Control", "Treated", "Treated"))
#' obj <- create_screenr_object(
#'     table = count_table,
#'     annotation = annotation_table,
#'     groups = groups, replicates = c("")
#' )
#' obj
create_screenr_object <- function(table = NULL, annotation = NULL,
    groups = NULL, replicates = c("")) {
    # arcode <- as.factor(table$Barcode)
    annotation$Barcode <- as.factor(annotation$Barcode)
    object <- methods::new("screenr_object",
        count_table = tibble(table),
        annotation_table = tibble(annotation), groups = groups,
        replicates = replicates, normalized_count_table = tibble(),
        data_table = tibble()
    )
    return(object)
}
