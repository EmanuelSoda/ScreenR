#' @title PLot the Sample in vector Space
#' @description Plot samples on a two or three-dimensional scatterplot so that
#'              distances on the plot approximate the typical log2 fold
#'              changes between the samples.
#' @param screenR_Object The Object of the package
#' @param palette palette to use to color the groups
#' @param dimension number of plotting dimensions
#' @return The MDS Plot
#' @export
#'
#' @examples
plot_MDS <- function(screenR_Object, palette, dimension = 2){
  # We have to convert the screenR obj into an edgeR obj
  DGEList <- create_edgeR_obj(screenR_Object)

  # The Standard plotMDS
  plotMDS <- limma::plotMDS(DGEList, plot = F, ndim	= dimension)

  # Create the Updated plot MDS
  PLTdata <- data.frame(plotMDS$cmdscale.out)
  if(dimension == 2){ # 2D plot
    colnames(PLTdata) <- c("x", "y")
    PLTdata$group <- DGEList$samples$group

    plot <- ggplot(PLTdata, aes(x=x, y=y, fill = group)) +
      geom_label(aes(label = rownames(PLTdata)),
                 color = 'black', size = 2.5,
                 max.overlaps = 60, alpha = 0.8) +
      scale_fill_manual(values = unique(palette)) +
      xlab("First Dimension") +
      ylab("Second Dimension") +
      theme_minimal()

  } else if (dimension == 3){ # 3D plot
      colnames(PLTdata) <- c("x", "y", "z")
      PLTdata$group <- DGEList$samples$group
      PLTdata$names <- row.names(DGEList$samples)


      plot <- plotly::plot_ly(PLTdata,
                     x = ~x,
                     y = ~y,
                     z = ~z,
                     text = ~names,
                     color = ~group,
                     colors = ~palette,
                     alpha= 0.9,
                     size = 100)
  }

  return(plot)
}
