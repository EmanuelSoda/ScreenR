#' @title Plot Mapped Reads
#' @description This function perform plot the number of reads mapped for each
#'              sample.
#'
#'
#'
#'
#' @param screenR_object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @return return a ggplot object
#' @export

plot_mapped_reads <- function(screenR_Object){
  table <- ScreenR::mapped_reads(screenR_Object)
  plot <-
    table  %>%
    ggplot(., aes(x = Sample, y = Mapped, fill = Sample)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(values=screenR_Object@color_palette) +
    theme_minimal()
  return(plot)
}
