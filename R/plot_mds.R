#' @title Plot a multidimensional scaling of the samples
#' @description Plots the samples on a two-dimensional scatter plot so that
#'     the distances on the plot approximate the typical log2 fold changes
#'     between the samples (\code{\link[limma]{plotMDS}} on the log2
#'     normalized counts, see \code{\link{log_normalized_counts}}: the
#'     normalization chosen in \code{\link{normalize_data}} is used).
#'     Replicates should cluster together and the groups should separate.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param groups What fills the labels: the name of a sample variable (for
#'     example "Treatment" or "Day", see \code{\link{get_sample_table}}), or
#'     a vector with one value for each sample. By default the groups of the
#'     ScreenR object (Group).
#' @param palette An optional vector of colors for the values of groups,
#'     which replaces the palette of the object. By default, when groups is
#'     the name of a sample variable (or NULL, for Group), the palette of the
#'     object for that variable is used when it is set (see
#'     \code{\link{get_palettes}}), otherwise the default colors of
#'     ggplot2.
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
    color = "black", palette = NULL) {
    object <- update_screenr_object(screenR_Object)
    # MDS of the log2 normalized counts
    plotMDS <- limma::plotMDS(log_normalized_counts(object, "plot_mds()"),
        plot = FALSE, ndim = 2)

    # Create the Updated plot MDS
    PLTdata <- data.frame(
        Sample = rownames(plotMDS$distance.matrix.squared),
        x = plotMDS$x, y = plotMDS$y
    )

    groups <- sample_groups(object, groups)
    PLTdata$group <- groups$values

    plot <- ggplot2::ggplot(PLTdata, aes(
        x = .data$x, y = .data$y,
        fill = .data$group
    )) +
        ggplot2::geom_label(aes(label = .data$Sample),
            color = color, size = size, alpha = alpha
        ) +
        ggplot2::labs(
            x = mds_axis_label(plotMDS, 1), y = mds_axis_label(plotMDS, 2),
            fill = groups$label
        ) +
        # Room for the labels of the samples at the edges
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .15)) +
        palette_scale(object, groups$variable, groups$values, palette)

    return(plot)
}

#' @title Axis label of an MDS plot
#' @description Internal function that returns the label of a dimension of
#'     the MDS plot: limma computes the distances from the leading log2 fold
#'     changes, and the label gives the proportion of the variance explained
#'     by the dimension when limma returns it.
#' @param mds The result of \code{\link[limma]{plotMDS}}.
#' @param dimension The dimension (1 or 2).
#' @return A character string.
#' @keywords internal
mds_axis_label <- function(mds, dimension) {
    label <- paste("Leading log2FC dimension", dimension)
    if (!is.null(mds$var.explained)) {
        label <- sprintf("%s (%.0f%%)", label,
            100 * mds$var.explained[dimension])
    }
    label
}

#' @title Plot the variance explained by the principal components
#' @description Plots the proportion of the variance explained by each
#'     principal component of a principal component analysis (PCA) of the
#'     samples, computed on the log2 normalized counts (with the
#'     normalization chosen in \code{\link{normalize_data}}) with
#'     \code{\link{compute_explained_variance}}. It shows how many components
#'     are needed to describe the differences between the samples. The
#'     samples on the components are shown by \code{\link{plot_pca}}.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param cumulative Whether to plot the cumulative proportion of variance
#'     instead of the proportion of each component. Default FALSE.
#' @param color The fill color of the bars. Default "steelblue".
#' @importFrom  scales percent
#' @return A ggplot object.
#' @seealso \code{\link{plot_pca}}, \code{\link{plot_mds}},
#'     \code{\link{plot_replicate_correlation}}
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
                x = "Principal component",
                y = "Cumulative explained variance"
            )
    } else {
        # Select only the Proportion of Variance
        PC <- dplyr::filter(PC, .data$Name == "Proportion of Variance")

        plot <- ggplot2::ggplot(PC, aes(x = .data$name, y = .data$value)) +
            geom_bar(stat = "identity", fill = color, col = "black") +
            geom_point() +
            geom_line(aes(group = .data$Name)) +
            scale_y_continuous(labels = percent) +
            labs(x = "Principal component", y = "Explained variance")
    }

    return(plot)
}

#' @title Compute the variance explained by the principal components
#' @description Internal function that computes a principal component
#'     analysis of the samples (\code{\link{compute_pca}}) and returns the
#'     variance explained by each component. It is used by
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
    PC <- compute_pca(screenR_Object)

    # Extract the importance ad create a data.frame
    PC <- data.frame(summary(PC)$importance)

    PC <- tibble::rownames_to_column(PC, var = "Name")

    PC <- dplyr::mutate(PC, Name = factor(
        x = .data$Name,
        levels = unique(.data$Name)
    ))

    return(PC)
}

#' @title Principal component analysis of the samples
#' @description Internal function that computes the principal component
#'     analysis (\code{\link[stats]{prcomp}}) of the samples on the log2
#'     normalized counts (\code{\link{log_normalized_counts}}).
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A prcomp object, with one row for each sample in x.
#' @keywords internal
compute_pca <- function(screenR_Object) {
    # The samples are the observations, the barcodes the variables
    stats::prcomp(t(log_normalized_counts(screenR_Object,
        "The principal component analysis")))
}

#' @title Log2 normalized counts
#' @description Internal function that returns the log2 of the normalized
#'     counts plus 1, used by \code{\link{plot_pca}},
#'     \code{\link{plot_explained_variance}} and \code{\link{plot_mds}}.
#'     The normalized counts are the raw counts divided by the effective
#'     library sizes set by \code{\link{normalize_data}} ("cpm", "tmm" or
#'     "control"), so the object must be normalized; the log and the + 1 are
#'     display transformations. On the raw counts the first principal
#'     component would mostly follow the sequencing depth of the samples.
#'     Barcodes with no reads in any sample are left out.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param what What needs the normalized counts, for the error message.
#' @return A numeric matrix with one row for each barcode and one column for
#'     each sample.
#' @keywords internal
log_normalized_counts <- function(screenR_Object, what = "This function") {
    object <- update_screenr_object(screenR_Object)
    counts <- normalized_counts(object, what)
    log2(counts[rowSums(counts) > 0, , drop = FALSE] + 1)
}

#' @title Plot a principal component analysis of the samples
#' @description Plots the samples on two principal components of a principal
#'     component analysis (PCA) of the log2 normalized counts, with the
#'     normalization chosen in \code{\link{normalize_data}} (see
#'     \code{\link{plot_explained_variance}} for the variance explained by
#'     each component). Replicates should cluster together and the groups
#'     should separate. Unlike \code{\link{plot_mds}}, which uses the most
#'     variable barcodes between each pair of samples, it uses all the
#'     barcodes.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param groups What fills the labels: the name of a sample variable (for
#'     example "Treatment" or "Day", see \code{\link{get_sample_table}}), or
#'     a vector with one value for each sample. By default the groups of the
#'     ScreenR object (Group).
#' @param palette An optional vector of colors for the values of groups,
#'     which replaces the palette of the object. By default, when groups is
#'     the name of a sample variable (or NULL, for Group), the palette of the
#'     object for that variable is used when it is set (see
#'     \code{\link{get_palettes}}), otherwise the default colors of
#'     ggplot2.
#' @param components The two principal components to plot. Default c(1, 2).
#' @param alpha The opacity of the labels, between 0 and 1. Default 0.8.
#' @param size The size of the labels. Default 2.5.
#' @param color The color of the label text. Default "black".
#' @return A ggplot object.
#' @seealso \code{\link{plot_explained_variance}}, \code{\link{plot_mds}}
#' @concept plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_pca(object)
#'
#' # Second and third components
#' plot_pca(object, components = c(2, 3))
plot_pca <- function(screenR_Object, groups = NULL, components = c(1, 2),
    alpha = 0.8, size = 2.5, color = "black", palette = NULL) {
    object <- update_screenr_object(screenR_Object)
    pca <- compute_pca(object)
    if (length(components) != 2 || !is.numeric(components) ||
        any(components < 1) || any(components > ncol(pca$x))) {
        stop("components must be two numbers between 1 and ", ncol(pca$x),
            " (the number of principal components).")
    }
    groups <- sample_groups(object, groups)
    if (length(groups$values) != nrow(pca$x)) {
        stop("groups must have one value for each sample (", nrow(pca$x),
            ").")
    }
    variance <- summary(pca)$importance["Proportion of Variance", ]
    axis_label <- function(i) {
        sprintf("PC%d (%.0f%%)", i, 100 * variance[i])
    }
    data <- data.frame(
        Sample = rownames(pca$x),
        x = pca$x[, components[1]], y = pca$x[, components[2]],
        group = groups$values
    )

    ggplot2::ggplot(data, aes(x = .data$x, y = .data$y,
        fill = .data$group)) +
        ggplot2::geom_label(aes(label = .data$Sample),
            color = color, size = size, alpha = alpha
        ) +
        ggplot2::labs(x = axis_label(components[1]),
            y = axis_label(components[2]), fill = groups$label) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .15)) +
        palette_scale(object, groups$variable, groups$values, palette)
}

#' @title Groups of the Samples of a Plot
#' @description Internal function used by \code{\link{plot_mds}} and
#'     \code{\link{plot_pca}}: it returns the values that fill the labels of
#'     the samples, and the sample variable they come from (whose palette is
#'     used).
#' @param object A ScreenR object.
#' @param groups NULL (the Group of the samples), the name of a sample
#'     variable, or a vector with one value for each sample.
#' @return A list with values, variable (NULL for a vector) and label (the
#'     title of the legend).
#' @keywords internal
sample_groups <- function(object, groups) {
    col_data <- SummarizedExperiment::colData(object)
    if (is.null(groups)) {
        return(list(values = groups_of(object), variable = "Group",
            label = "group"))
    }
    if (is.character(groups) && length(groups) == 1 && ncol(object) != 1) {
        if (!groups %in% colnames(col_data)) {
            stop("There is no sample variable '", groups, "'. The ",
                "variables are: ", paste(setdiff(colnames(col_data),
                "EffectiveLibSize"), collapse = ", "), ".", call. = FALSE)
        }
        return(list(values = col_data[[groups]], variable = groups,
            label = groups))
    }
    list(values = groups, variable = NULL, label = "group")
}




