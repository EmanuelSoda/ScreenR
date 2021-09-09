#' @title Plot the distribution of the mapped reads
#' @description Create a boxplot to show the distribution of the mapped reads
#'              in different sample
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}.
#' @param palette The color vector for the plot.
#' @param alpha The opacity of the plot.
#' @param type The type of plot. THe default is boxplot the other option is density.
#'impo
#'
#' @return return a tibble containing the number of mapped read for sample
#' @export

distribution_mapped_reads <- function(screenR_Object, palette = NULL, alpha = 1,
                                      type = "boxplot"){
  table <- count_mapped_reads(screenR_Object)
  if(type == "boxplot")
    plot <- ggplot(data = table, aes(x=Sample, y=Mapped, fill=Sample)) +
      geom_boxplot(alpha=alpha)
  else if(type == "density"){
    plot <- ggplot(data = table, aes(x=Mapped, fill=Sample)) +
      geom_density(alpha=alpha)
  }

  if (!is.null(palette)) {
    plot <- plot + scale_fill_manual(values=palette)
  }
  return(plot)
}
