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
#' @return return a tibble containing the number of mapped read for sample
#' @export

compute_metrics <- function (screenR_Object){
  # create a vector of index
  treated <- screenR_Object@groups == 'Treated'
  control <- screenR_Object@groups == 'Control'

  # select the name of the column according to the index created group_by
  treated <- screenR_Object@count_table[treated]
  control <- screenR_Object@count_table[control]

  table <-
    screenR_Object@data_table %>%
    filter(.data$Sample %in% c(treated, control)) %>%
    mutate(Treatment = ifelse(.data$Sample %in% treated,
                              "Treated", "Control")) %>%
    group_by(.data$Treatment, .data$Barcode) %>%
    mutate(Mean = mean(.data$frequency)) %>%
    summarise(Mean = unique(.data$Mean),
              Gene = unique(.data$Gene), .groups = "drop") %>%
    spread(.data$Treatment, .data$Mean) %>%
    filter(.data$Control > 0) %>%
    mutate(Log2FC = log2(.data$Treated/.data$Control + 0.0000001))  %>%
    mutate(Zscore = (.data$Log2FC - mean(.data$Log2FC)) / sd(.data$Log2FC))  %>%
    mutate(ZscoreRobust = 1.4826 * (.data$Log2FC - mean(.data$Log2FC)) /
                                    median(abs(.data$Log2FC - median(.data$Log2FC))))
}



