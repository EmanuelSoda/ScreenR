#' @title Create the Z-score table
#' @description This function compute the  Z-score tablestarting from
#' the screenR object for a given treatment in a given day
#'

#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @param control The control Samples
#'
#' @param treatment The treatment Samples
#'
#' @param day The day of the treatment

#' @importFrom rlang .data
#' @importFrom tidyr spread
#' @importFrom dplyr mutate filter summarise
#' @importFrom dplyr mutate
#' @importFrom stats sd median
#'
#' @return return a tibble  with all the measure computed
#' @export

compute_metrics <- function(screenR_Object, control, treatment, day){

  control <- screenR_Object@data_table %>%
    filter(.data$Treatment %in% control) %>%
    pull(.data$Sample) %>%
    unique() %>%
    as.character()

  treated <- screenR_Object@data_table %>%
    filter(.data$Treatment %in% treatment) %>%
    pull(.data$Sample) %>%
    unique() %>%
    as.character()

  data_trt <-
    screenR_Object@data_table %>%
    filter(.data$Sample %in% c(control, treated)) %>%
    mutate(Group = as.factor(if_else(condition = .data$Sample %in% treated,
                                     true = "Treated",
                                     false = "Control")))



  data_trt <- data_trt %>%
    filter(.data$Day %in% day) %>%
    group_by(.data$Group, .data$Barcode) %>%
    mutate(Mean = mean(.data$Frequency)) %>%
    summarise(Barcode = unique(.data$Barcode), Mean = unique(.data$Mean),
              Gene = unique(.data$Gene), .groups = "drop") %>%
    spread(.data$Group, .data$Mean) %>%
    filter(.data$Control > 0) %>%
    mutate(Log2FC = log2(.data$Treated/.data$Control + 1e-07))  %>%
    mutate(Zscore = (.data$Log2FC - mean(.data$Log2FC)) / sd(.data$Log2FC))  %>%
    mutate(ZscoreRobust = 1.4826 * (.data$Log2FC - mean(.data$Log2FC)) /
             median(abs(.data$Log2FC- median(.data$Log2FC)))) %>%
    mutate(Day = day, Treatment = treatment) %>%


  return(data_trt)
}


