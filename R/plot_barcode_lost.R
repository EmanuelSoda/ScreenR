#' @title Plot number of barcode lost
#' @description This fuction count the number of Barcode lost during the
#'              sequenceing. A barcode is lost if it has zero mapped read
#'
#'
#'
#'
#' @param screenR_object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @importFrom magrittr %>%
#' @return return a tibble containing the number of mapped read for sample
#' @export

plot_barcode_lost<- function(screenR_Object, palette = NULL, alpha = 1){
  table <- barcode_lost(screenR_Object)
    plot <-
      table %>%
      ggplot(., aes(x = Sample, y = LostBarcode, fill = Sample)) +
      geom_bar(alpha = alpha, stat = "identity", color = "black") +
      geom_text(aes(label=LostBarcode),
                position= position_stack(vjust = 0.8),
                color = "black", size = 5)

    if(!is.null(palette)){
     plot <- plot + scale_fill_manual(values = palette)
      }
  return(plot)
}
