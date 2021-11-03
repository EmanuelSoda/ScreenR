
#' @title Create the ScreenR Object
#' @description Initial function to create the Screen Object.
#'
#' @param screenR_Object The ScreenR object
#'
#' @return screenR_Object
#' @export
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' compute_data_table(object)
#' }
#'
compute_data_table <- function(screenR_Object){
 # First the table is created with the join of the annotation and the
 # count table
  table <-
    screenR_Object@normalized_count_table %>%
    tidyr::gather(., Sample, Frequency, colnames(.)[2]:last(colnames(.))) %>%
    dplyr::mutate(Barcode = as.factor(.$Barcode)) %>%
     dplyr::left_join(screenR_Object@annotation_table,
                      by = "Barcode") %>%
     select(.data$Barcode, .data$Gene, .data$Sample,.data$Frequency,
            .data$Sequence, .data$Library, .data$Gene_ID)

  # Then the table is put into the object
  screenR_Object@data_table <- table
  return(screenR_Object)
}
