#' @title Hit Sets of Several Methods
#' @description Internal function that turns the results of several
#'     hit-finding methods into named sets of genes, used by
#'     \code{\link{find_common_hit}} and \code{\link{plot_common_hit}}.
#' @param results A list of results: gene-level tables with a Gene column, or
#'     character vectors of gene names.
#' @param fdr NULL to use all the genes of each result, or an FDR threshold
#'     applied to the results that have an FDR column.
#' @param expressions The expressions given for the results, used to label
#'     the unnamed ones.
#' @return A named list of character vectors (unique gene names).
#' @keywords internal
hit_sets <- function(results, fdr = NULL, expressions = NULL) {
    # A single list of results can be given instead of several arguments
    if (length(results) == 1 && is.list(results[[1]]) &&
        !is.data.frame(results[[1]])) {
        results <- results[[1]]
    }
    if (length(results) == 0) {
        stop("Give the results of at least one method.", call. = FALSE)
    }
    if (!is.null(fdr) && (!is.numeric(fdr) || length(fdr) != 1 ||
        fdr < 0 || fdr > 1)) {
        stop("fdr must be NULL or a single number between 0 and 1.",
            call. = FALSE)
    }
    labels <- names(results)
    if (is.null(labels)) labels <- rep("", length(results))
    named <- labels != ""
    if (anyDuplicated(labels[named])) {
        stop("The methods must have different names.", call. = FALSE)
    }
    # Unnamed results are labelled with the expression given for them
    if (length(expressions) == length(results)) {
        labels[!named] <- vapply(expressions[!named], function(x) {
            paste(deparse(x, width.cutoff = 500L), collapse = "")
        }, character(1))
    }
    # Names of the arguments of the previous versions of ScreenR
    legacy <- c(hit_zscore = "Z-score Hits", hit_camera = "Camera Hits",
        hit_roast = "ROAST Hits")
    labels <- ifelse(labels %in% names(legacy), legacy[labels], labels)
    labels[labels == ""] <- paste("Method", which(labels == ""))
    # The same result given twice without a name gets two labels
    labels <- make.unique(labels, sep = " ")
    sets <- lapply(seq_along(results), function(i) {
        result <- results[[i]]
        if (is.factor(result)) result <- as.character(result)
        if (is.character(result)) {
            return(unique(result[!is.na(result)]))
        }
        if (!is.data.frame(result) || !"Gene" %in% colnames(result)) {
            stop("The result '", labels[i], "' must be a table with a Gene ",
                "column or a vector of gene names.", call. = FALSE)
        }
        if ("Barcode" %in% colnames(result)) {
            stop("The result '", labels[i], "' has one row for each ",
                "barcode: use the gene-level results (for example of ",
                "find_gene_zscore_hit(), find_camera_hit() or ",
                "find_roast_hit()).", call. = FALSE)
        }
        if (!is.null(fdr) && "FDR" %in% colnames(result)) {
            result <- result[!is.na(result$FDR) & result$FDR <= fdr, ,
                drop = FALSE]
        }
        genes <- as.character(result$Gene)
        unique(genes[!is.na(genes)])
    })
    stats::setNames(sets, labels)
}

#' @title Find the hits in common between methods
#' @description Returns the genes found as hits by at least common_in of the
#'     given methods. The results can come from any hit-finding method of
#'     ScreenR, and from any number of methods. To combine the methods using
#'     their FDR, see \code{\link{summarise_hits}}; to plot the overlap, see
#'     \code{\link{plot_common_hit}}.
#' @param ... The results of the methods: gene-level tables with a Gene
#'     column (for example from \code{\link{find_gene_zscore_hit}},
#'     \code{\link{find_camera_hit}}, \code{\link{find_roast_hit}},
#'     \code{\link{find_time_hit}} or \code{\link{find_zscore_hit}}) or
#'     vectors of gene names, optionally named; or a single named list of
#'     them.
#' @param common_in The minimum number of methods in which a gene has to be
#'     found. By default (NULL) all the methods.
#' @param fdr An optional FDR threshold: the genes of the results with an FDR
#'     column are kept only when their FDR is at most fdr (useful for results
#'     computed with fdr = 1). By default (NULL) all the genes of each result
#'     are used.
#' @return A character vector with the names of the common hits, ordered by
#'     the number of methods that find them.
#' @importFrom tibble tibble
#' @seealso \code{\link{plot_common_hit}}, \code{\link{summarise_hits}}
#' @export
#' @concept find
#' @examples
#' hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
#' hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
#' hit_roast <- data.frame(Gene = c("A", "L", "N"))
#'
#' # Common to all the three methods
#' find_common_hit(hit_zscore, hit_camera, hit_roast)
#'
#' # Common to at least two of the three methods
#' find_common_hit(hit_zscore, hit_camera, hit_roast, common_in = 2)
#'
#' # Any number of methods, filtered on their FDR
#' results <- list(
#'     FRY = data.frame(Gene = c("A", "B", "C"), FDR = c(0.01, 0.02, 0.3)),
#'     CAMERA = data.frame(Gene = c("A", "C"), FDR = c(0.04, 0.01)),
#'     Zscore = c("A", "B"),
#'     ROAST = data.frame(Gene = c("A", "B"), FDR = c(0.01, 0.6))
#' )
#' find_common_hit(results, common_in = 3, fdr = 0.05)
find_common_hit <- function(..., common_in = NULL, fdr = NULL) {
    sets <- hit_sets(list(...), fdr,
        as.list(substitute(list(...)))[-1])
    if (is.null(common_in)) {
        common_in <- length(sets)
    }
    if (!is.numeric(common_in) || length(common_in) != 1 || common_in < 1) {
        stop("common_in must be a single number of at least 1.",
            call. = FALSE)
    }
    genes <- unique(unlist(sets, use.names = FALSE))
    n_methods <- vapply(genes, function(gene) {
        sum(vapply(sets, function(set) gene %in% set, logical(1)))
    }, numeric(1))
    keep <- n_methods >= common_in
    genes[keep][order(-n_methods[keep])]
}

#' @title Plot the hits in common between methods
#' @description Shows how the hits of several methods overlap: a Venn
#'     diagram for up to three methods, an UpSet plot for more. The results
#'     can come from any hit-finding method of ScreenR, and from any number of
#'     methods.
#' @details The UpSet plot has one column for each combination of methods
#'     that has hits: the dots below show the methods of the combination and
#'     the bar above the number of genes found by exactly those methods. The
#'     bars on the left give the number of hits of each method. Unlike a Venn
#'     diagram, it stays readable with many methods.
#'
#'     Use fdr to plot the hits at a common threshold when the results were
#'     computed with fdr = 1 (all the genes). Results without an FDR column
#'     (for example of \code{\link{find_zscore_hit}}) and vectors of gene
#'     names are taken as lists of hits.
#'
#'     No ggplot2 theme is applied: set it with
#'     \code{\link[ggplot2]{theme_set}} or add one (with \code{&} for the
#'     UpSet plot, which is a patchwork of several plots).
#' @param ... The results of the methods: gene-level tables with a Gene
#'     column (for example from \code{\link{find_gene_zscore_hit}},
#'     \code{\link{find_camera_hit}}, \code{\link{find_roast_hit}},
#'     \code{\link{find_time_hit}} or \code{\link{find_zscore_hit}}) or
#'     vectors of gene names. Name them to label the methods (for example
#'     \code{FRY = fry_result}); unnamed results are labelled with the
#'     expression given (for example \code{fry_result}). A single named list
#'     of results can also be given. Tables with one row for each barcode (such as
#'     \code{\link{compute_barcode_statistics}}) are not accepted. For
#'     compatibility, the names hit_zscore, hit_camera and hit_roast are
#'     labelled as in previous versions.
#' @param fdr An optional FDR threshold applied to the results with an FDR
#'     column. By default (NULL) all the genes of each result are used.
#' @param type "auto" (default): a Venn diagram for up to three methods and an
#'     UpSet plot for more; "venn" (at most three methods) or "upset".
#' @param show_elements In the Venn diagram, whether to show the gene names
#'     instead of the counts (default TRUE). When a region has more than
#'     max_elements genes, its count is shown.
#' @param show_percentage In the Venn diagram, whether to add the percentage
#'     of all the hits to the counts (default TRUE).
#' @param max_elements The largest number of gene names written in a region
#'     of the Venn diagram (default 6).
#' @param n_intersections The largest number of combinations of methods shown
#'     in the UpSet plot, the largest ones first (default 20).
#' @param alpha The opacity of the circles of the Venn diagram. Default 0.5.
#' @param stroke_size The width of the circle borders.
#' @param set_name_size The text size of the method names in the Venn
#'     diagram.
#' @param text_size The text size of the counts or gene names.
#' @param text_color The color of the counts or gene names.
#' @param title The title of the plot.
#' @param color The fill colors of the circles of the Venn diagram (one for
#'     each method) or, for the UpSet plot, the color of the bars and dots.
#' @return A ggplot object (for the UpSet plot, a patchwork of ggplot
#'     objects).
#' @importFrom ggplot2 ggtitle
#' @seealso \code{\link{find_common_hit}}, \code{\link{summarise_hits}}
#' @concept plot
#' @export
#' @examples
#' hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
#' hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
#' hit_roast <- data.frame(Gene = c("A", "L", "N"))
#' plot_common_hit(hit_zscore, hit_camera, hit_roast)
#'
#' # Number of genes instead of their names
#' plot_common_hit(hit_zscore, hit_camera, hit_roast, show_elements = FALSE)
#'
#' # More than three methods: UpSet plot, at an FDR of 5%
#' results <- list(
#'     FRY = data.frame(Gene = c("A", "B", "C"), FDR = c(0.01, 0.02, 0.3)),
#'     CAMERA = data.frame(Gene = c("A", "C"), FDR = c(0.04, 0.01)),
#'     Zscore = c("A", "B", "D"),
#'     ROAST = data.frame(Gene = c("A", "B"), FDR = c(0.01, 0.6))
#' )
#' plot_common_hit(results, fdr = 0.05)
plot_common_hit <- function(..., fdr = NULL,
    type = c("auto", "venn", "upset"), show_elements = TRUE,
    show_percentage = TRUE, max_elements = 6, n_intersections = 20,
    alpha = 0.5, stroke_size = 0.5, set_name_size = 4,
    text_color = "black", text_size = 4, title = "",
    color = c("#1B9E77", "#D95F02", "#7570B3")) {
    type <- match.arg(type)
    sets <- hit_sets(list(...), fdr,
        as.list(substitute(list(...)))[-1])
    if (type == "auto") {
        type <- if (length(sets) <= 3) "venn" else "upset"
    }
    if (type == "venn") {
        if (length(sets) > 3) {
            stop("A Venn diagram can show at most three methods: use ",
                "type = \"upset\".", call. = FALSE)
        }
        plot <- venn_plot(sets, show_elements = show_elements,
            show_percentage = show_percentage, max_elements = max_elements,
            alpha = alpha, stroke_size = stroke_size,
            set_name_size = set_name_size, text_color = text_color,
            text_size = text_size, color = color)
        return(plot + ggplot2::ggtitle(title))
    }
    upset_plot(sets, n_intersections = n_intersections,
        text_size = text_size, color = color[1], title = title)
}

#' @title Membership of the Genes
#' @description Internal function that returns, for each gene found by at
#'     least one method, the combination of methods that find it.
#' @param sets A named list of gene sets.
#' @return A tibble with the columns Gene and Pattern (a string of 0 and 1,
#'     one character for each method) and one logical column per method.
#' @keywords internal
hit_membership <- function(sets) {
    genes <- unique(unlist(sets, use.names = FALSE))
    if (length(genes) == 0) {
        return(tibble::tibble(Gene = character(), Pattern = character()))
    }
    membership <- vapply(sets, function(set) genes %in% set,
        logical(length(genes)))
    if (length(genes) == 1) {
        membership <- matrix(membership, nrow = 1,
            dimnames = list(NULL, names(sets)))
    }
    pattern <- apply(membership, 1, function(row) {
        paste(as.integer(row), collapse = "")
    })
    tibble::tibble(Gene = genes, Pattern = as.character(pattern))
}

#' @title Draw a Venn Diagram
#' @description Internal function that draws a Venn diagram of one to three
#'     sets of genes with ggplot2.
#' @param sets A named list of one to three gene sets.
#' @param show_elements,show_percentage,max_elements,alpha,stroke_size,set_name_size,text_color,text_size,color
#'        See \code{\link{plot_common_hit}}.
#' @return A ggplot object.
#' @keywords internal
venn_plot <- function(sets, show_elements, show_percentage, max_elements,
    alpha, stroke_size, set_name_size, text_color, text_size, color) {
    n <- length(sets)
    centres <- list(
        `1` = data.frame(x = 0, y = 0),
        `2` = data.frame(x = c(-0.6, 0.6), y = c(0, 0)),
        `3` = data.frame(x = c(-0.6, 0.6, 0), y = c(0.35, 0.35, -0.65))
    )[[as.character(n)]]
    angle <- seq(0, 2 * pi, length.out = 200)
    circles <- do.call(rbind, lapply(seq_len(n), function(i) {
        data.frame(Method = names(sets)[i],
            x = centres$x[i] + cos(angle), y = centres$y[i] + sin(angle))
    }))
    circles$Method <- factor(circles$Method, levels = names(sets))
    # Position of the text of each region (pattern of membership)
    regions <- list(
        `1` = data.frame(Pattern = "1", x = 0, y = 0),
        `2` = data.frame(Pattern = c("10", "01", "11"),
            x = c(-1, 1, 0), y = c(0, 0, 0)),
        `3` = data.frame(
            Pattern = c("100", "010", "001", "110", "101", "011", "111"),
            x = c(-1.05, 1.05, 0, 0, -0.62, 0.62, 0),
            y = c(0.6, 0.6, -1.2, 0.8, -0.38, -0.38, 0.05))
    )[[as.character(n)]]
    labels <- list(
        `1` = data.frame(x = 0, y = 1.3),
        `2` = data.frame(x = c(-0.6, 0.6), y = c(1.3, 1.3)),
        `3` = data.frame(x = c(-0.9, 0.9, 0), y = c(1.62, 1.62, -1.95))
    )[[as.character(n)]]
    labels$Method <- names(sets)

    membership <- hit_membership(sets)
    total <- nrow(membership)
    regions$Text <- vapply(regions$Pattern, function(pattern) {
        genes <- membership$Gene[membership$Pattern == pattern]
        if (show_elements && length(genes) <= max_elements) {
            # Gene names (nothing for an empty region)
            return(paste(sort(genes), collapse = "\n"))
        }
        count <- as.character(length(genes))
        if (show_percentage && total > 0) {
            count <- sprintf("%s\n(%.0f%%)", count,
                100 * length(genes) / total)
        }
        count
    }, character(1))
    color <- rep(color, length.out = n)

    ggplot2::ggplot() +
        ggplot2::geom_polygon(data = circles, ggplot2::aes(x = .data$x,
            y = .data$y, fill = .data$Method, group = .data$Method),
            alpha = alpha, colour = "black", linewidth = stroke_size,
            show.legend = FALSE) +
        ggplot2::geom_text(data = regions, ggplot2::aes(x = .data$x,
            y = .data$y, label = .data$Text), size = text_size,
            colour = text_color, lineheight = 0.9) +
        ggplot2::geom_text(data = labels, ggplot2::aes(x = .data$x,
            y = .data$y, label = .data$Method), size = set_name_size,
            fontface = "bold") +
        ggplot2::scale_fill_manual(values = stats::setNames(color,
            names(sets))) +
        ggplot2::scale_x_continuous(breaks = NULL, name = NULL,
            expand = ggplot2::expansion(mult = 0.05)) +
        ggplot2::scale_y_continuous(breaks = NULL, name = NULL,
            expand = ggplot2::expansion(mult = 0.05)) +
        ggplot2::coord_fixed()
}

#' @title Draw an UpSet Plot
#' @description Internal function that draws an UpSet plot of several sets
#'     of genes with ggplot2 and patchwork.
#' @param sets A named list of gene sets.
#' @param n_intersections The largest number of combinations shown.
#' @param text_size The size of the counts.
#' @param color The color of the bars and dots.
#' @param title The title of the plot.
#' @return A patchwork object.
#' @keywords internal
upset_plot <- function(sets, n_intersections, text_size, color, title) {
    membership <- hit_membership(sets)
    methods <- names(sets)
    if (nrow(membership) == 0) {
        intersections <- tibble::tibble(Pattern = character(),
            Size = integer())
    } else {
        intersections <- dplyr::count(membership, .data$Pattern,
            name = "Size")
        intersections <- intersections[order(-intersections$Size,
            intersections$Pattern), ]
        intersections <- utils::head(intersections, n_intersections)
    }
    intersections$Column <- seq_len(nrow(intersections))
    # The dots: one row for each combination and method
    dots <- tibble::tibble(
        Column = rep(intersections$Column, each = length(methods)),
        Method = factor(rep(methods, times = nrow(intersections)),
            levels = rev(methods)),
        In = as.vector(vapply(intersections$Pattern, function(pattern) {
            strsplit(pattern, "")[[1]] == "1"
        }, logical(length(methods))))
    )
    present <- dots[dots$In, ]
    lines <- if (nrow(present) == 0) {
        tibble::tibble(Column = integer(), from = integer(), to = integer())
    } else {
        dplyr::summarise(dplyr::group_by(present, .data$Column),
            from = min(as.integer(.data$Method)),
            to = max(as.integer(.data$Method)), .groups = "drop")
    }
    sizes <- tibble::tibble(Method = factor(methods, levels = rev(methods)),
        Size = unname(lengths(sets)))
    columns <- ggplot2::scale_x_continuous(breaks = NULL, name = NULL,
        limits = c(0.4, max(1, nrow(intersections)) + 0.6),
        expand = c(0, 0))

    bars <- ggplot2::ggplot(intersections, ggplot2::aes(x = .data$Column,
        y = .data$Size)) +
        ggplot2::geom_col(fill = color, width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = .data$Size), vjust = -0.4,
            size = text_size * 0.8) +
        columns +
        ggplot2::scale_y_continuous(name = "Genes",
            expand = ggplot2::expansion(mult = c(0, 0.15)))
    matrix_plot <- ggplot2::ggplot(dots, ggplot2::aes(x = .data$Column,
        y = .data$Method)) +
        ggplot2::geom_point(size = 3, colour = "grey85") +
        ggplot2::geom_segment(data = lines, ggplot2::aes(x = .data$Column,
            xend = .data$Column, y = .data$from, yend = .data$to),
            colour = color, linewidth = 0.8, inherit.aes = FALSE) +
        ggplot2::geom_point(data = dots[dots$In, ], size = 3,
            colour = color) +
        columns +
        ggplot2::scale_y_discrete(name = NULL)
    set_sizes <- ggplot2::ggplot(sizes, ggplot2::aes(x = .data$Size,
        y = .data$Method)) +
        ggplot2::geom_col(fill = "grey50", width = 0.6) +
        ggplot2::scale_x_reverse(name = "Hits", n.breaks = 3,
            expand = ggplot2::expansion(mult = c(0.15, 0)),
            guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_y_discrete(breaks = NULL, name = NULL)

    patchwork::wrap_plots(A = bars, B = matrix_plot, C = set_sizes,
        design = "#A\nCB", widths = c(1.3, 3), heights = c(2, 1)) +
        patchwork::plot_annotation(title = if (nzchar(title)) title)
}
