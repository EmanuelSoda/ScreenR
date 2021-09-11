#' @title Compute metrics
#' @description Compute the metrics of the data_table.
#'              The metrix computed are:
#'              - Log2FC of Treated vs Control.
#'              - Z score of the Log2FC.
#'              - Robust Z score.
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}.

#'
#'
#' @return return a tibble containing the number of mapped read for sample
#' @export

compute_metrics <- function (screenR_Object){
  # create a vector of index
  treated <- screenR_Object@groups == 'Treated'
  control <- screenR_Object@groups == 'Control'

  # select the name of the column according to the index created
  treated <- screenR_Object@count_table[treated]
  control <- screenR_Object@count_table[control]



  table <-
    screenR_Object@data_table %>%
    filter(Sample %in% c(treated, control)) %>%
    mutate(Treatment = ifelse(Sample %in% treated,
                              "Treated", "Control")) %>%
    group_by(Treatment, Barcode) %>%
    mutate(Mean = mean(frequency)) %>%
    summarise(Mean = unique(Mean),
              Gene = unique(Gene), .groups = "drop") %>%
    spread(Treatment, Mean) %>%
    filter(Control > 0) %>%
    mutate(Log2FC = log2(Treated/Control + 0.0000001))  %>%
    mutate(Zscore = (Log2FC - mean(Log2FC)) / sd(Log2FC))  %>%
    mutate(ZscoreRobust = 1.4826 * (Log2FC - mean(Log2FC)) /
                                    median(abs(Log2FC - median(Log2FC))))
}



