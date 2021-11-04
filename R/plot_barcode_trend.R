#' @title Plot the trend over time of the Barcode
#' @description Plot the log2FC over time of the barcode ein the differen
#' time point
#' @param list_data_measure A list containing the measure table of the different
#' time point
#' @param n_col The number of col to use in the facette wrap
#' @param size_line the dimension of the line
#'
#' @return The the plot of the
#' @export
#'

plot_barcode_trend <- function(list_data_measure,
                               genes,
                               n_col=2,
                               size_line =1,
                               color = NULL){

  data <- bind_rows(list_data_measure) %>%
    filter(Gene %in% genes) %>%
    mutate(Day = factor(x = Day, levels = unique(Day)))
  if (is.null(color)) {
    gg_l <- map(.x = split(data, f = as.character(data$Gene)),
                .f =  function(x) {
                  ggplot(x, aes(x=Day, y=Log2FC, group=Barcode, col = Barcode)) +
                    geom_line(size = size_line) +
                    geom_point() +
                    theme_light() +
                    theme(legend.position = "top") +
                    facet_wrap(facets = "Gene", scales = "free")
                })
  } else {
    gg_l <- map(.x = split(data, f = as.character(data$Gene)),
                .f =  function(x) {
                  ggplot(x, aes(x=Day, y=Log2FC, group=Barcode, col = Barcode)) +
                    geom_line(size = size_line) +
                    geom_point() +
                    theme_light() +
                    scale_color_manual(values = color) +
                    theme(legend.position = "top") +
                    facet_wrap(facets = "Gene", scales = "free")
                })
    }


  patchwork::wrap_plots(gg_l, ncol = n_col)

}
