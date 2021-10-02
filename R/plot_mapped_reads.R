#' @title Plot Mapped Reads
#' @description This function perform plot the number of reads mapped for each
#'              sample.
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param palette A vector of color
#' @param alpha The alpha parameter for the opacity of the plot
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom rlang .data
#' @return return a ggplot object
#' @export

plot_mapped_reads <- function(screenR_Object, palette = NULL, alpha = 1, legende_position = "none"){

  table <- ScreenR::mapped_reads(object)
  if (is.null(palette)) {
    plot <-
      ggplot(table, aes(x = .data$Sample,
                        y = .data$Mapped,
                        fill = .data$Sample)) +
      geom_bar(stat = "identity", color = "black") +
      theme(legend.position = legende_position)
  } else {
    plot <-
      ggplot(table, aes(x = .data$Sample,
                        y = .data$Mapped,
                        fill = .data$Sample)) +
      geom_bar(alpha = alpha, stat = "identity") +
      scale_fill_manual(values=palette) +
      theme(legend.position = legende_position)
  }

  return(plot)
}
