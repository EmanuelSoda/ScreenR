#' @title PLot the Sample in vector Space
#' @description Plot samples on a two or three-dimensional scatterplot so that
#'              distances on the plot approximate the typical log2 fold
#'              changes between the samples.
#' @param screenR_Object The Object of the package
#' @param palette palette to use to color the groups
#' @param dimension number of plotting dimensions
#' @param groups variable to fill the plot
#' @return The MDS Plot
#' @concept  plot
#' @keywords internal
#' @export

plot_MDS <- function(screenR_Object, palette, dimension = 2,
                     groups = NULL){
  # We have to convert the screenR obj into an edgeR obj
  DGEList <- create_edgeR_obj(screenR_Object)

  # The Standard plotMDS
  plotMDS <- limma::plotMDS(DGEList, plot = F, ndim	= dimension)

  # Create the Updated plot MDS
  PLTdata <- data.frame(plotMDS$cmdscale.out)
  if(dimension == 2){ # 2D plot
    colnames(PLTdata) <- c("x", "y")

  if (is.null(groups)) {
    PLTdata$group <- DGEList$samples$group

    plot <- ggplot(PLTdata, aes(x=.data$x, y=.data$y, fill = .data$group)) +
      geom_label(aes(label = rownames(PLTdata)),
                 color = 'black', size = 2.5, alpha = 0.8,
                 check_overlap = TRUE) +
      scale_fill_manual(values = unique(palette)) +
      xlab("First Dimension") +
      ylab("Second Dimension") +
      theme_minimal()
  } else {
    PLTdata$group <- groups

    plot <- ggplot(PLTdata, aes(x=.data$x, y=.data$y, fill = .data$group)) +
      geom_label(aes(label = rownames(PLTdata)),
                 color = 'black', size = 2.5, alpha = 0.8,
                 check_overlap = TRUE) +
      scale_fill_manual(values = unique(palette)) +
      xlab("First Dimension") +
      ylab("Second Dimension") +
      theme_minimal()
  }


  } else if (dimension == 3){ # 3D plot
      colnames(PLTdata) <- c("x", "y", "z")
      if (is.null(groups)) {
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
      } else{
        PLTdata$group <- groups
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
  }

  return(plot)
}

#' @title PLot the explained variance by the PC
#' @description This function plot the explained variance by the
#'              Principal Component.
#' @param screenR_Object The ScreenR object
#' @param cumulative A boolean value which indicates whether or not to plot
#'                   the cumulative variance
#' @param color The color to fill the barplot
#' @import scales
#' @return The explained variance  plot
#' @export

plot_PC_explained_variance <- function(screenR_Object,
                                       cumulative = F,
                                       color = "steelblue"){

  PC <- compute_explained_variance(screenR_Object)
  # Remove the Standard deviation row
  PC <- filter(PC, Name != "Standard deviation")

  # Get only the numeric columns which corresponds to the PCs
  numeric_col <- colnames(PC[,  unlist(lapply(PC, is.numeric))])

  # Transform the data in a longer format
  PC <- tidyr::pivot_longer(data = PC, cols = all_of(numeric_col),
                            names_to = "name")
  PC <- dplyr::mutate(PC, name = factor(x = name, levels = unique(name)))

  plot <- NULL
  if (cumulative) {
    # Select only the Cumulative Proportion
    PC <- dplyr::filter(PC, Name == "Cumulative Proportion")

    plot <- ggplot2::ggplot(PC, aes(x = name, y = value)) +
      geom_bar(stat="identity", fill=color, col="black") +
      geom_point() +
      geom_line(aes(group=Name))  +
      scale_y_continuous(labels = percent) +
      labs(x= NULL, y = "Cumulative Expressed Variance (%)")

  } else {
    # Select only the Proportion of Variance
    PC <- dplyr::filter(PC, Name == "Proportion of Variance")

    plot <- ggplot2::ggplot(PC, aes(x = name, y = value)) +
      geom_bar(stat="identity", fill=color, col="black") +
      geom_point() +
      geom_line(aes(group=Name)) +
      labs(x= NULL, y = "Expressed Variance (%)")
  }

  return(plot)
}

#' @title Compute the Explained Variance
#' @description This function compute the explained variance by each of the
#'              Principal Components
#' @param screenR_Object The Object of the package
#' @return A data.frame containing all the information of the variance expressed
#'         by the components
#' @keywords internal
#' @export

compute_explained_variance <- function(screenR_Object){
  data <- screenR_Object@count_table
  # Get only the numeric columns
  numeric_col <- unlist(lapply(data, is.numeric))

  # The data for the PC corresponds only on the numeric column
  data_PC <- data[, numeric_col]

  # To compute the PC the features has to be columns
  data_PC <- t(data_PC)

  #  Rename the columns
  colnames(data_PC) <- data$Barcode

  # Computing the PCS
  PC <- prcomp(data_PC)

  # Extract the importance ad create a data.frame
  PC <- data.frame(summary(PC)$importance)

  PC <- tibble::rownames_to_column(PC, var = "Name")

  PC <- dplyr::mutate(PC, Name = factor(x = Name, levels = unique(Name)))

  return(PC)

  }

