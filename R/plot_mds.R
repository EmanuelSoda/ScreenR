#' @title Plot a multidimensional scaling of the samples
#' @description Plots the samples on a two-dimensional scatter plot so that
#'     the distances on the plot approximate the typical log2 fold changes
#'     between the samples (\code{\link[limma]{plotMDS}} on the log2
#'     counts per million of the raw counts).
#'     Replicates should cluster together and the groups should separate.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param groups An optional vector, with one value for each sample, used to
#'     fill the labels. By default the groups of the ScreenR object are used.
#' @param alpha The opacity of the labels, between 0 and 1. Default 0.8.
#' @param size The size of the labels. Default 2.5.
#' @param color The color of the label text. Default "black".
#' @importFrom ggplot2 geom_label labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom limma plotMDS
#' @return A ggplot object.
#' @seealso \code{\link{plot_replicate_correlation}},
#'     \code{\link{plot_explained_variance}}
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_mds(object)
plot_mds <- function(screenR_Object, groups = NULL, alpha = 0.8, size = 2.5,
    color = "black") {
    # We have to convert the screenR obj into an edgeR obj
    DGEList <- create_edger_obj(screenR_Object)

    # The Standard plotMDS
    plotMDS <- limma::plotMDS(DGEList, plot = FALSE, ndim = 2)

    # Create the Updated plot MDS
    PLTdata <- data.frame(
        Sample = rownames(plotMDS$distance.matrix.squared),
        x = plotMDS$x, y = plotMDS$y
    )

    if (is.null(groups)) {
        PLTdata$group <- DGEList$samples$group
    } else {
        PLTdata$group <- groups
    }

    plot <- ggplot2::ggplot(PLTdata, aes(
        x = .data$x, y = .data$y,
        fill = .data$group
    )) +
        ggplot2::geom_label(aes(label = .data$Sample),
            color = color, size = size, alpha = alpha
        ) +
        ggplot2::labs(x = "First Dimension", y = "Second Dimension")

    return(plot)
}

#' @title Plot the variance explained by the principal components
#' @description Plots the proportion of the variance explained by each
#'     principal component of a principal component analysis (PCA) of the
#'     samples, computed on the raw counts with
#'     \code{\link{compute_explained_variance}}. It shows how many components
#'     are needed to describe the differences between the samples.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param cumulative Whether to plot the cumulative proportion of variance
#'     instead of the proportion of each component. Default FALSE.
#' @param color The fill color of the bars. Default "steelblue".
#' @importFrom  scales percent
#' @return A ggplot object.
#' @seealso \code{\link{plot_mds}}, \code{\link{plot_replicate_correlation}}
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_explained_variance(object)
#'
#' # Cumulative variance
#' plot_explained_variance(object, cumulative = TRUE)
plot_explained_variance <- function(screenR_Object, cumulative = FALSE,
    color = "steelblue") {
    PC <- compute_explained_variance(screenR_Object)
    # Remove the Standard deviation row
    PC <- filter(PC, .data$Name != "Standard deviation")

    # Get only the numeric columns which corresponds to the PCs
    numeric_col <- colnames(PC[, unlist(lapply(PC, is.numeric))])

    # Transform the data in a longer format
    PC <- tidyr::pivot_longer(
        data = PC, cols = all_of(numeric_col),
        names_to = "name"
    )
    PC <- dplyr::mutate(PC, name = factor(
        x = .data$name,
        levels = unique(.data$name)
    ))
    plot <- NULL


    if (cumulative) {
        # Select only the Cumulative Proportion
        PC <- dplyr::filter(PC, .data$Name == "Cumulative Proportion")

        plot <- ggplot2::ggplot(PC, aes(x = .data$name, y = .data$value)) +
            geom_bar(stat = "identity", fill = color, col = "black") +
            geom_point() +
            geom_line(aes(group = .data$Name)) +
            scale_y_continuous(labels = scales::percent) +
            labs(
                x = NULL,
                y = "Cumulative Expressed Variance (%)"
            )
    } else {
        # Select only the Proportion of Variance
        PC <- dplyr::filter(PC, .data$Name == "Proportion of Variance")

        plot <- ggplot2::ggplot(PC, aes(x = .data$name, y = .data$value)) +
            geom_bar(stat = "identity", fill = color, col = "black") +
            geom_point() +
            geom_line(aes(group = .data$Name)) +
            scale_y_continuous(labels = percent) +
            labs(x = NULL, y = "Expressed Variance (%)")
    }

    return(plot)
}

#' @title Compute the variance explained by the principal components
#' @description Internal function that computes a principal component
#'     analysis of the samples on the raw counts (\code{\link[stats]{prcomp}})
#'     and returns the variance explained by each component. It is used by
#'     \code{\link{plot_explained_variance}}.
#' @importFrom stats  prcomp
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A data frame with the column Name ("Standard deviation",
#'     "Proportion of Variance" and "Cumulative Proportion") and one column
#'     for each principal component.
#' @keywords internal
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' compute_explained_variance(object)
compute_explained_variance <- function(screenR_Object) {
    data <- count_table_of(screenR_Object)
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

    PC <- dplyr::mutate(PC, Name = factor(
        x = .data$Name,
        levels = unique(.data$Name)
    ))

    return(PC)
}




