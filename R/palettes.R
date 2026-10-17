#' @title Get or set the palettes of a ScreenR object
#' @description The palettes are the colors of the values of the sample
#'     variables (the columns of the sample table: Sample, Day, Treatment,
#'     and the Group and Replicate of each sample). They are stored in the
#'     object and used by all the plots that color the samples by one of
#'     these variables, so that, for example, the treated samples have the
#'     same color in every plot. Set them with the palettes argument of
#'     \code{\link{create_screenr_object}} or with \code{set_palettes()}.
#' @details A palette is given for each variable as a vector of colors:
#'     \itemize{
#'       \item named by the values of the variable, for example
#'             \code{Treatment = c(CTRL = "#1B9E77", TRT = "#D95F02")}. It
#'             must give a color to every value of the variable; other names
#'             are allowed (for example values of samples that were
#'             removed);
#'       \item or unnamed, with at least as many colors as the variable has
#'             values: the colors are given to the values in the order of the
#'             samples (or of the levels, for a factor such as Group).
#'     }
#'     The palettes are stored named, in \code{metadata(object)$palettes},
#'     and are kept when the object is subset.
#'
#'     The plots use them as follows: an explicit palette given to the plot
#'     (its palette argument) is used first; otherwise the palette of the
#'     object for the variable that colors the plot; otherwise the default
#'     colors of ggplot2. The plots that use them are
#'     \code{\link{plot_mapped_reads}}, \code{\link{plot_mapped_reads_distribution}},
#'     \code{\link{plot_barcode_lost}}, \code{\link{plot_barcode_lost_for_gene}},
#'     \code{\link{plot_gini}} (Sample), \code{\link{plot_mds}} and
#'     \code{\link{plot_pca}} (Group, or the variable given in groups) and
#'     \code{\link{plot_boxplot}} (the variable of fill_var). The plots that
#'     do not receive the object, such as \code{\link{plot_zscore_distribution}},
#'     take the palette as an argument, for example
#'     \code{palette = get_palettes(object)$Day}.
#' @param object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param palettes A named list with one palette for each variable (see
#'     Details), or NULL to remove all the palettes.
#' @return get_palettes(): a named list of named character vectors (empty when
#'     no palette is set). set_palettes(): the ScreenR object with the new
#'     palettes.
#' @seealso \code{\link{create_screenr_object}}
#' @concept objects
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- set_palettes(object, list(
#'     Treatment = c(T1 = "grey70", T2 = "grey40", Time3 = "#1B9E77",
#'         Time4 = "#66A61E", TRT = "#D95F02"),
#'     Day = c("#E6AB02", "#A6761D", "#7570B3", "#E7298A")
#' ))
#' get_palettes(object)
#' plot_boxplot(object, genes = "Gene_34", group_var = c("Time3", "TRT"),
#'     fill_var = "Treatment")
get_palettes <- function(object) {
    object <- update_screenr_object(object)
    palettes <- S4Vectors::metadata(object)$palettes
    if (is.null(palettes)) list() else palettes
}

#' @rdname get_palettes
#' @export
set_palettes <- function(object, palettes) {
    object <- update_screenr_object(object)
    S4Vectors::metadata(object)$palettes <- if (!is.null(palettes)) {
        check_palettes(palettes, SummarizedExperiment::colData(object))
    }
    object
}

#' @title Check the Palettes
#' @description Internal function that checks the palettes of the sample
#'     variables and returns them named by the values of each variable.
#' @param palettes A named list of color vectors.
#' @param col_data The colData of the object.
#' @return The palettes, as a named list of named character vectors.
#' @importFrom grDevices col2rgb
#' @keywords internal
check_palettes <- function(palettes, col_data) {
    if (length(palettes) == 0) {
        return(NULL)
    }
    if (!is.list(palettes) || is.null(names(palettes)) ||
        any(names(palettes) == "") || anyDuplicated(names(palettes))) {
        stop("palettes must be a list with one palette for each variable, ",
            "named by the variable, for example list(Treatment = ",
            "c(CTRL = \"#1B9E77\", TRT = \"#D95F02\")).", call. = FALSE)
    }
    variables <- setdiff(colnames(col_data), "EffectiveLibSize")
    unknown <- setdiff(names(palettes), variables)
    if (length(unknown) > 0) {
        stop("There is no sample variable ",
            paste0("'", unknown, "'", collapse = ", "), " for the palettes. ",
            "The variables are: ", paste(variables, collapse = ", "), ".",
            call. = FALSE)
    }
    stats::setNames(lapply(names(palettes), function(variable) {
        colors <- palettes[[variable]]
        if (!is.character(colors) || length(colors) == 0) {
            stop("The palette of ", variable, " must be a vector of colors.",
                call. = FALSE)
        }
        valid <- vapply(colors, function(color) {
            !inherits(tryCatch(grDevices::col2rgb(color),
                error = function(e) e), "error")
        }, logical(1))
        if (!all(valid)) {
            stop("The palette of ", variable, " has invalid colors: ",
                paste(colors[!valid], collapse = ", "), ".", call. = FALSE)
        }
        values <- variable_values(col_data[[variable]])
        if (is.null(names(colors)) || all(names(colors) == "")) {
            if (length(colors) < length(values)) {
                stop("The palette of ", variable, " has ", length(colors),
                    " colors but the variable has ", length(values),
                    " values (", paste(values, collapse = ", "), ").",
                    call. = FALSE)
            }
            return(stats::setNames(colors[seq_along(values)], values))
        }
        missing <- setdiff(values, names(colors))
        if (length(missing) > 0) {
            stop("The palette of ", variable, " has no color for: ",
                paste(missing, collapse = ", "), ".", call. = FALSE)
        }
        colors
    }), names(palettes))
}

#' @title Values of a Sample Variable
#' @description Internal function that returns the distinct values of a
#'     sample variable, in the order of the levels for a factor and of the
#'     samples otherwise.
#' @param x The variable.
#' @return A character vector.
#' @keywords internal
variable_values <- function(x) {
    if (is.factor(x)) {
        values <- levels(droplevels(x))
    } else {
        values <- unique(as.character(x))
    }
    values[!is.na(values)]
}

#' @title Color Scale of a Plot
#' @description Internal function that returns the color scale of a plot
#'     colored by a sample variable: the explicit palette when given,
#'     otherwise the palette of the object for the variable (when it gives a
#'     color to every value shown), otherwise NULL (the default colors of
#'     ggplot2).
#' @param object A ScreenR object, or NULL.
#' @param variable The sample variable that colors the plot.
#' @param values The values shown in the plot.
#' @param palette An explicit palette (vector of colors), or NULL.
#' @param aesthetic "fill" or "colour".
#' @return A ggplot2 scale, or NULL.
#' @keywords internal
palette_scale <- function(object, variable, values, palette = NULL,
    aesthetic = "fill") {
    scale <- function(colors) {
        ggplot2::scale_discrete_manual(aesthetics = aesthetic,
            values = colors)
    }
    if (!is.null(palette)) {
        return(scale(palette))
    }
    if (is.null(object) || is.null(variable)) {
        return(NULL)
    }
    colors <- S4Vectors::metadata(object)$palettes[[variable]]
    if (is.null(colors)) {
        return(NULL)
    }
    values <- unique(as.character(values[!is.na(values)]))
    missing <- setdiff(values, names(colors))
    if (length(missing) > 0) {
        warning("The palette of ", variable, " has no color for ",
            paste(missing, collapse = ", "), ": the default colors are used.",
            call. = FALSE)
        return(NULL)
    }
    scale(colors[intersect(names(colors), values)])
}
