#' @title PLot the Sample in vector Space
#' @description Plot samples on a two or three-dimensional scatterplot so that
#'              distances on the plot approximate the typical log2 fold
#'              changes between the samples.
#' @param screenR_Object The Object of the package
#' @param groups The vector to fill the plot
#' @param alpha The opacity of the labels
#' @param size The dimension of the labels
#' @param color The color of the labels
#' @return The MDS Plot
#' @export
#' @examples
#' obj <- get0('obj', envir = asNamespace('ScreenR'))
#'
#' plot_MDS(obj)

plot_MDS <- function(screenR_Object, groups = NULL, alpha = 0.8, size = 2.5,
    color = "black") {
    # We have to convert the screenR obj into an edgeR obj
    DGEList <- create_edgeR_obj(screenR_Object)

    # The Standard plotMDS
    plotMDS <- limma::plotMDS(DGEList, plot = FALSE, ndim = 2)

    # Create the Updated plot MDS
    PLTdata <- data.frame(Sample = rownames(plotMDS$distance.matrix.squared),
        x = plotMDS$x, y = plotMDS$y)

    if (is.null(groups)) {
        PLTdata$group <- DGEList$samples$group
    } else {
        PLTdata$group <- groups
    }

    plot <- ggplot2::ggplot(PLTdata, aes(x = .data$x, y = .data$y,
        fill = .data$group)) + ggplot2::geom_label(aes(label = .data$Sample),
        color = color, size = size, alpha = alpha, check_overlap = TRUE) +
        ggplot2::labs(x = "First Dimension", y = "Second Dimension")

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
#' @examples
#' obj <- get0('obj', envir = asNamespace('ScreenR'))
#'
#' plot_PC_explained_variance(obj)
#'
#' # For the cumulative plote
#' plot_PC_explained_variance(obj, cumulative = TRUE)

plot_PC_explained_variance <- function(screenR_Object, cumulative = FALSE,
    color = "steelblue") {

    PC <- compute_explained_variance(screenR_Object)
    # Remove the Standard deviation row
    PC <- filter(PC, .data$Name != "Standard deviation")

    # Get only the numeric columns which corresponds to the PCs
    numeric_col <- colnames(PC[, unlist(lapply(PC, is.numeric))])

    # Transform the data in a longer format
    PC <- tidyr::pivot_longer(data = PC, cols = all_of(numeric_col),
        names_to = "name")
    PC <- dplyr::mutate(PC, name = factor(x = .data$name,
        levels = unique(.data$name)))
    plot <- NULL


    if (cumulative) {
        # Select only the Cumulative Proportion
        PC <- dplyr::filter(PC, .data$Name == "Cumulative Proportion")

        plot <- ggplot2::ggplot(PC, aes(x = .data$name, y = .data$value)) +
            geom_bar(stat = "identity", fill = color, col = "black") +
            geom_point() + geom_line(aes(group = .data$Name)) +
            scale_y_continuous(labels = percent) + labs(x = NULL,
            y = "Cumulative Expressed Variance (%)")

    } else {
        # Select only the Proportion of Variance
        PC <- dplyr::filter(PC, .data$Name == "Proportion of Variance")

        plot <- ggplot2::ggplot(PC, aes(x = .data$name, y = .data$value)) +
            geom_bar(stat = "identity", fill = color, col = "black") +
            geom_point() + geom_line(aes(group = .data$Name)) +
            scale_y_continuous(labels = percent) + labs(x = NULL,
            y = "Expressed Variance (%)")
    }

    return(plot)
}

#' @title Compute the Explained Variance
#' @description This function compute the explained variance by each of the
#'              Principal Components
#' @importFrom stats  prcomp
#' @param screenR_Object The Object of the package
#' @return A data.frame containing all the information of the variance
#'         expressed by the components
#' @keywords internal
#' @export
#' @examples
#' obj <- get0('obj', envir = asNamespace('ScreenR'))
#'
#' compute_explained_variance(obj)
compute_explained_variance <- function(screenR_Object) {
    data <- screenR_Object@count_table
    # Get only the numeric columns
    numeric_col <- unlist(lapply(data, is.numeric))

    # The data for the PC corresponds only on the numeric column
    data_PC <- data[, numeric_col]

    # To compute the PC the features has to be columns
    data_PC <- t(data_PC)

    # Rename the columns
    colnames(data_PC) <- data$Barcode

    # Computing the PCS
    PC <- prcomp(data_PC)

    # Extract the importance ad create a data.frame
    PC <- data.frame(summary(PC)$importance)

    PC <- tibble::rownames_to_column(PC, var = "Name")

    PC <- dplyr::mutate(PC, Name = factor(x = .data$Name,
        levels = unique(.data$Name)))

    return(PC)

}
