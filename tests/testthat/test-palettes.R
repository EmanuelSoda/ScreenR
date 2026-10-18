# Palettes of the sample variables, stored in the object and used by plots

data("count_table", package = "ScreenR")
data("annotation_table", package = "ScreenR")

palette_object <- function() {
    object <- get0("object", envir = asNamespace("ScreenR"))
    set_palettes(object, list(
        Treatment = c(T1 = "grey70", T2 = "grey40", Time3 = "#1B9E77",
            Time4 = "#66A61E", TRT = "#D95F02"),
        Group = c(`T1/T2` = "grey50", Control = "#1B9E77",
            Treated = "#D95F02"),
        Day = c("#E6AB02", "#A6761D", "#7570B3", "#E7298A"),
        Sample = stats::setNames(grDevices::hcl.colors(14),
            colnames(object))
    ))
}

# The color of each value of the variable, as mapped by the color scale of
# the plot; every color drawn by the layer must come from this mapping
drawn_colors <- function(plot, layer = 1, aesthetic = "fill") {
    built <- ggplot2::ggplot_build(plot)
    scale <- built$plot$scales$get_scales(aesthetic)
    limits <- scale$get_limits()
    mapped <- stats::setNames(scale$map(limits), limits)
    stopifnot(all(built$data[[layer]][[aesthetic]] %in% mapped))
    mapped
}

test_that("palettes are checked and stored named", {
    object <- palette_object()
    palettes <- get_palettes(object)
    expect_equal(names(palettes), c("Treatment", "Group", "Day", "Sample"))
    # Unnamed colors are given to the values in the order of the samples
    expect_equal(palettes$Day, c(T1 = "#E6AB02", T2 = "#A6761D",
        Time3 = "#7570B3", Time4 = "#E7298A"))
    # Kept when the object is subset
    expect_equal(get_palettes(object[1:10, 3:8]), palettes)
    # Removed with NULL
    expect_equal(get_palettes(set_palettes(object, NULL)), list())
    # Set at creation
    counts <- count_table[count_table$Barcode != "*", ]
    created <- suppressMessages(create_screenr_object(table = counts,
        annotation = annotation_table,
        palettes = list(Day = c(Time = "black", Time3 = "red",
            Time4 = "blue"))))
    expect_equal(get_palettes(created)$Day,
        c(Time = "black", Time3 = "red", Time4 = "blue"))
})

test_that("invalid palettes give informative errors", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    expect_error(set_palettes(object, c(Day = "red")), "list")
    expect_error(set_palettes(object, list("red")), "list")
    expect_error(set_palettes(object, list(Color = "red")),
        "no sample variable 'Color'")
    expect_error(set_palettes(object, list(Day = c(T1 = "red"))),
        "no color for: T2, Time3, Time4")
    expect_error(set_palettes(object, list(Day = c("red", "blue"))),
        "2 colors but the variable has 4 values")
    expect_error(set_palettes(object,
        list(Day = c("red", "blue", "green", "notacolor"))),
        "invalid colors: notacolor")
    expect_error(set_palettes(object, list(Day = 1:4)), "vector of colors")
    # Extra values are allowed (for example removed samples)
    expect_silent(set_palettes(object, list(Day = c(T1 = "red",
        T2 = "blue", Time3 = "green", Time4 = "black", Time5 = "white"))))
})

test_that("the plots of the samples use the Sample palette", {
    object <- palette_object()
    sample_colors <- get_palettes(object)$Sample
    for (plot in list(plot_mapped_reads(object), plot_gini(object),
        plot_mapped_reads_distribution(object))) {
        colors <- drawn_colors(plot)
        expect_equal(colors, sample_colors[names(colors)])
    }
    lost <- plot_barcode_lost(object)
    colors <- drawn_colors(lost)
    expect_equal(colors, sample_colors[names(colors)])
    lost_gene <- plot_barcode_lost_for_gene(object,
        samples = c("Time3_A", "Time3_B"))
    colors <- drawn_colors(lost_gene, aesthetic = "colour")
    expect_equal(colors, sample_colors[c("Time3_A", "Time3_B")])
})

test_that("an explicit palette replaces the palette of the object", {
    object <- palette_object()
    explicit <- grDevices::rainbow(14)
    colors <- drawn_colors(plot_mapped_reads(object, palette = explicit))
    expect_equal(unname(colors), explicit)
})

test_that("plot_mds and plot_pca use the palette of the variable", {
    object <- palette_object()
    group_colors <- get_palettes(object)$Group
    for (plot_function in list(plot_mds, plot_pca)) {
        # Default: Group
        colors <- drawn_colors(plot_function(object))
        expect_equal(colors, group_colors[names(colors)])
        # The name of another sample variable
        plot <- plot_function(object, groups = "Treatment")
        colors <- drawn_colors(plot)
        expect_equal(colors,
            get_palettes(object)$Treatment[names(colors)])
        expect_equal(plot$labels$fill, "Treatment")
        # A vector of values has no palette in the object
        colors <- drawn_colors(plot_function(object,
            groups = rep(c("a", "b"), 7)))
        expect_false(any(colors %in% group_colors))
        expect_error(plot_function(object, groups = "Colour"),
            "no sample variable")
    }
})

test_that("plot_boxplot uses the palette of fill_var", {
    object <- palette_object()
    for (variable in c("Treatment", "Day", "Sample")) {
        plot <- plot_boxplot(object, genes = "Gene_34",
            group_var = c("T1", "T2", "TRT"), fill_var = variable)
        colors <- drawn_colors(plot)
        expect_equal(colors, get_palettes(object)[[variable]][names(colors)])
    }
})

test_that("plot_zscore_distribution takes the palette as an argument", {
    object <- palette_object()
    metrics <- list(
        compute_metrics(object, control = "Time3", treatment = "TRT",
            day = "Time3"),
        compute_metrics(object, control = "Time4", treatment = "TRT",
            day = "Time4"))
    day_colors <- get_palettes(object)$Day
    plot <- plot_zscore_distribution(metrics, palette = day_colors)
    colors <- drawn_colors(plot)
    expect_equal(colors, day_colors[names(colors)])
    expect_true(ggplot2::is_ggplot(plot_zscore_distribution(metrics)))
})

test_that("a palette that does not cover the values is not used", {
    object <- palette_object()
    # A new group value without a color in the palette
    groups <- as.character(get_groups(object))
    groups[1] <- "New"
    object <- suppressWarnings(set_groups(object, groups))
    expect_warning(plot <- plot_mds(object), "no color for New")
    colors <- drawn_colors(plot)
    expect_false(any(colors %in% get_palettes(object)$Group))
})
