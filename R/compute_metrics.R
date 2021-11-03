#' @title Compute metrics
#' @description Compute the metrics of the data_table.
#'              The metrix computed are:
#'              - Log2FC of Treated vs Control.
#'              - Z score of the Log2FC.
#'              - Robust Z score.
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}.
#' @param treated List of Column names corresponding to treatment
#'                we want to test
#' @param control List of Column names corresponding to control
#'                we want to test

#' @importFrom rlang .data
#' @importFrom tidyr spread
#' @importFrom dplyr mutate filter summarise
#' @importFrom dplyr mutate
#' @importFrom stats sd median
#'
#' @return return a tibble  with all the mesures computed
#' @export

compute_metrics <- function (screenR_Object, treated = NULL, control = NULL){
  if(is.null(treated) && is.null(control)){
    # create a vector of index
    treated <- screenR_Object@groups == 'Treated'
    control <- screenR_Object@groups == 'Control'

    # select the name of the column that are NOT Barcode
    names <- colnames(screenR_Object@count_table)[colnames(screenR_Object@count_table)
                                          != 'Barcode']

    treated <- names[treated]
    control <- names[control]
  }

  table <- screenR_Object@data_table[screenR_Object@data_table$Sample %in%
                               c(treated, control), ]

  control <-  screenR_Object@data_table %>%
    filter(Treatment == control) %>%
    pull(Sample) %>%
    unique() %>%
    as.character()

  treated <-  screenR_Object@data_table %>%
    filter(Treatment == treatment) %>%
    pull(Sample) %>%
    unique() %>%
    as.character()


  table <- screenR_Object@data_table %>%
    filter(Sample %in% c(control, treated))

  table$Group <-
    as.factor(ifelse(table$Sample %in% treated,"Treated", "Control"))

  table <- table %>% mutate(Day = gsub("\\_.*","", Sample))

  table <-
    table %>%
    dplyr::mutate(Treatment = ifelse(.data$Sample %in% treated,
                              "Treated", "Control")) %>%
    dplyr::group_by(.data$Treatment, .data$Barcode) %>%
    dplyr::mutate(Mean = mean(.data$Frequency)) %>%
    dplyr::summarise(Gene = unique(.data$Gene),
                     Mean = unique(.data$Mean),.groups = "drop") %>%
    tidyr::spread(.data$Treatment, .data$Mean) %>%
    dplyr::filter(.data$Control != 0) %>%
    dplyr::mutate(Log2FC = log2(.data$Treated/.data$Control + 0.0000001))  %>%
    dplyr::mutate(Zscore = (.data$Log2FC - mean(.data$Log2FC)) / sd(.data$Log2FC))  %>%
    dplyr::mutate(ZscoreRobust = 1.4826 * (.data$Log2FC - mean(.data$Log2FC)) /
                                    median(abs(.data$Log2FC - median(.data$Log2FC))))
  return(table)
}


