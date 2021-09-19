#' @title Compute metrics
#' @description Compute the metrics of the data_table.
#'              The metrix computed are:
#'              - Log2FC of Treated vs Control.
#'              - Z score of the Log2FC.
#'              - Robust Z score.
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}.

#' @importFrom rlang .data
#' @importFrom tidyr spread
#' @importFrom dplyr mutate filter summarise
#' @importFrom dplyr mutate
#' @importFrom stats sd median
#'
#' @return return a tibble  with all the mesures computed
#' @export

compute_metrics <- function (screenR_Object){
  # create a vector of index
  treated <- object@groups == 'Treated'
  control <- object@groups == 'Control'

  # select the name of the column that are NOT Barcode
  names <- colnames(object@count_table)[colnames(object@count_table)
                                                != 'Barcode']

  treated <- names[treated]
  control <- names[control]

  table <- object@data_table[object@data_table$Sample %in%
                               c(treated, control), ]
  table <-
    table %>%
    dplyr::mutate(Treatment = ifelse(.data$Sample %in% treated,
                              "Treated", "Control")) %>%
    dplyr::group_by(.data$Treatment, .data$Barcode) %>%
    dplyr::mutate(Mean = mean(.data$Frequency)) %>%
    dplyr::summarise(Gene = unique(.data$Gene),
                     Mean = unique(.data$Mean),.groups = "drop") %>%
    tidyr::spread(.data$Treatment, .data$Mean) %>%
    dplyr::filter(.data$Control > 0) %>%
    dplyr::mutate(Log2FC = log2(.data$Treated/.data$Control + 0.0000001))  %>%
    dplyr::mutate(Zscore = (.data$Log2FC - mean(.data$Log2FC)) / sd(.data$Log2FC))  %>%
    dplyr::mutate(ZscoreRobust = 1.4826 * (.data$Log2FC - mean(.data$Log2FC)) /
                                    median(abs(.data$Log2FC - median(.data$Log2FC))))
  return(table)
}



