#' @title Plot number of barcode lost
#' @description This fuction count the number of Barcode lost during the
#'              sequenceing. A barcode is lost if it has zero mapped read
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param palette A vector of colors
#' @param alpha A value for the opacity of the plot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return return a tibble containing the number of mapped read for sample
#' @export

plot_barcode_lost <- function(screenR_Object, palette = NULL, alpha = 1){
  table <- barcode_lost(screenR_Object)

  plot <- ggplot(table, aes(x = .data$Sample,
                            y = .data$LostBarcode,
                            fill = .data$Sample)) +
    geom_bar(alpha = alpha, stat = "identity", color = "black") +
    geom_text(aes(label=.data$LostBarcode),
              position= position_stack(vjust = 0.8),
              color = "black", size = 5) +
    theme_minimal()

  if(!is.null(palette))
      plot <- plot + scale_fill_manual(values = palette)

  return(plot)
}
