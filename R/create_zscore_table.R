#' @title Create the Z-score table
#' @description This function compute the  Z-score tablestarting from
#' the screenR object for a given treatment in a given day
#'
#'
#'
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @param control
#'
#' @param treatment
#'
#' @param day
#' @import dplyr
#' @importFrom  magrittr %>%
#' @importFrom rlang .data
#' @return return a tibble containing the number of barcode lost for sample
#' @export


create_zscore_table <- function(data_annotated, control, treatment, day){
  control <- data_annotated %>%
    filter(Treatment == control) %>%
    pull(Sample) %>%
    unique() %>%
    as.character()

  treated <- data_annotated %>%
    filter(Treatment == treatment) %>%
    pull(Sample) %>%
    unique() %>%
    as.character()

  data <- data_annotated %>%
    filter(Sample %in% c(control, treated))

  data$Group <-
    as.factor(ifelse(data$Sample %in% treated,"Treated", "Control"))

  data <- data %>% mutate(Day = gsub("\\_.*","", Sample))

  data <- data %>%
    filter(Day %in% day) %>%
    group_by(Group, Barcode) %>%
    mutate(Mean = mean(frequency)) %>%
    summarise(Barcode = unique(Barcode), Mean = unique(Mean),
              Gene = unique(Gene), .groups = "drop") %>%
    spread(Group, Mean) %>%
    filter(Control > 0) %>%
    mutate(Log2FC = log2(Treated/Control + 0.0000001))  %>%
    mutate(Zscore = (Log2FC - mean(Log2FC)) / sd(Log2FC))  %>%
    mutate(ZscoreRobust = 1.4826 * (Log2FC - mean(Log2FC)) /
             median(abs(Log2FC- median(Log2FC))))

  return(data)
}
