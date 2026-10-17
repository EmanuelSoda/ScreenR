# plot_common_hit() and find_common_hit() with any number of methods

hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
hit_roast <- data.frame(Gene = c("A", "L", "N"))

# The text of the regions of a Venn diagram, by membership pattern
venn_text <- function(plot) {
    text <- ggplot2::layer_data(plot, 2)
    text$label
}

test_that("the previous calls keep working", {
    # Unnamed results are labelled with the expression given for them; the
    # argument names of the previous versions keep their labels
    plot <- plot_common_hit(hit_zscore, hit_camera, hit_roast)
    expect_true(ggplot2::is_ggplot(plot))
    labels <- ggplot2::layer_data(plot, 3)$label
    expect_equal(labels, c("Z-score Hits", "Camera Hits", "ROAST Hits"))
    plot <- plot_common_hit(hit_zscore[1:3, , drop = FALSE], hit_camera)
    expect_equal(ggplot2::layer_data(plot, 3)$label,
        c("hit_zscore[1:3, , drop = FALSE]", "Camera Hits"))
    plot <- plot_common_hit(hit_zscore = hit_zscore,
        hit_camera = hit_camera, hit_roast = hit_roast,
        show_elements = FALSE, show_percentage = FALSE, title = "T4")
    expect_equal(plot$labels$title, "T4")
    # Regions 100, 010, 001, 110, 101, 011, 111
    expect_equal(venn_text(plot), c("2", "3", "2", "2", "0", "0", "1"))

    expect_equal(find_common_hit(hit_zscore, hit_camera, hit_roast), "A")
    expect_setequal(
        find_common_hit(hit_zscore, hit_camera, hit_roast, common_in = 2),
        c("A", "B", "C"))
})

test_that("the Venn diagram shows one, two or three methods", {
    one <- plot_common_hit(Only = c("A", "B"), show_percentage = FALSE)
    expect_equal(venn_text(one), "A\nB")
    two <- plot_common_hit(X = c("A", "B", "C"), Y = c("C", "D"),
        show_elements = FALSE, show_percentage = FALSE)
    expect_equal(venn_text(two), c("2", "1", "1"))
    # Percentages of all the hits, and counts when a region has too many
    # genes to be written
    two <- plot_common_hit(X = c("A", "B", "C"), Y = c("C", "D"),
        show_elements = FALSE)
    expect_equal(venn_text(two), c("2\n(50%)", "1\n(25%)", "1\n(25%)"))
    many <- plot_common_hit(X = LETTERS[1:8], Y = "Z", max_elements = 6,
        show_percentage = FALSE)
    expect_equal(venn_text(many), c("8", "Z", ""))
})

test_that("more than three methods give an UpSet plot", {
    sets <- list(M1 = c("A", "B", "C", "D"), M2 = c("A", "B", "E"),
        M3 = c("A", "C"), M4 = c("A", "F"))
    plot <- plot_common_hit(sets)
    expect_s3_class(plot, "patchwork")
    expect_true(ggplot2::is_ggplot(plot))
    # The bars count the genes found by exactly each combination of methods
    bars <- plot$patches$plots[[1]]$data
    membership <- ScreenR:::hit_membership(sets)
    expect_equal(sum(bars$Size), length(unique(unlist(sets))))
    expect_equal(bars$Size[bars$Pattern == "1111"], 1L)
    expect_equal(bars$Size[bars$Pattern == "1000"], 1L)
    expect_true(all(bars$Size == as.integer(table(membership$Pattern)[
        bars$Pattern])))
    expect_false(is.unsorted(rev(bars$Size)))
    # The left bars are the number of hits of each method
    sizes <- plot$data
    expect_equal(sizes$Size, c(4L, 3L, 2L, 2L))
    # Only the largest combinations when there are many
    small <- plot_common_hit(sets, n_intersections = 2)
    expect_equal(nrow(small$patches$plots[[1]]$data), 2L)
    # An UpSet plot can be asked for with fewer methods, a Venn diagram not
    # with more
    expect_s3_class(plot_common_hit(hit_zscore, hit_camera, type = "upset"),
        "patchwork")
    expect_error(plot_common_hit(sets, type = "venn"), "at most three")
})

test_that("fdr selects the hits of results with an FDR column", {
    results <- list(
        FRY = data.frame(Gene = c("A", "B", "C"), FDR = c(0.01, 0.02, 0.3)),
        CAMERA = data.frame(Gene = c("A", "C"), FDR = c(0.04, 0.01)),
        Names = c("A", "D")
    )
    sets <- ScreenR:::hit_sets(list(results), fdr = 0.05)
    expect_equal(sets, list(FRY = c("A", "B"), CAMERA = c("A", "C"),
        Names = c("A", "D")))
    # Without fdr every gene of a result is a hit
    expect_equal(ScreenR:::hit_sets(list(results))$FRY, c("A", "B", "C"))
    expect_equal(find_common_hit(results, fdr = 0.05), "A")
    expect_equal(find_common_hit(results, fdr = 0.05, common_in = 2), "A")
    expect_setequal(find_common_hit(results, fdr = 0.05, common_in = 1),
        c("A", "B", "C", "D"))
    # Unnamed results are numbered
    expect_equal(names(ScreenR:::hit_sets(list(c("A"), c("B")))),
        c("Method 1", "Method 2"))
})

test_that("invalid results give informative errors", {
    expect_error(plot_common_hit(), "at least one method")
    expect_error(plot_common_hit(X = data.frame(Name = "A")), "Gene column")
    expect_error(plot_common_hit(X = 1:3), "Gene column")
    expect_error(plot_common_hit(X = data.frame(Barcode = c("b1", "b2"),
        Gene = c("A", "A"), FDR = 0.01)), "one row for each barcode")
    expect_error(plot_common_hit(list(X = "A", X = "B")), "different names")
    expect_error(plot_common_hit(X = "A", fdr = 2), "between 0 and 1")
    expect_error(find_common_hit(X = "A", common_in = 0), "at least 1")
})

test_that("results without hits are drawn", {
    expect_true(ggplot2::is_ggplot(plot_common_hit(X = character(),
        Y = character())))
    expect_no_warning(empty <- plot_common_hit(A = character(),
        B = character(), C = character(), D = character()))
    expect_s3_class(empty, "patchwork")
    expect_no_warning(print(empty))
    expect_equal(find_common_hit(X = "A", Y = "B"), character())
})

test_that("the results of every hit-finding method of ScreenR are accepted", {
    object <- get0("object", envir = asNamespace("ScreenR"))
    object <- object[SummarizedExperiment::rowData(object)$Gene %in%
        paste0("Gene_", 1:60), ]
    design <- create_design_matrix(object)
    contrast <- create_contrast(design, "Treated", "Control")
    metrics <- compute_metrics(object, control = "Time4", treatment = "TRT",
        day = "Time4")
    statistics <- compute_barcode_statistics(object, design, contrast)
    results <- list(
        Zscore = find_gene_zscore_hit(statistics, fdr = 1,
            n_permutations = 500, seed = 1),
        Empirical = suppressWarnings(find_gene_zscore_hit(statistics,
            reference = "empirical", fdr = 1)),
        CAMERA = suppressWarnings(find_camera_hit(object, design, contrast,
            fdr = 1)),
        FRY = find_roast_hit(object, design, contrast, method = "fry",
            fdr = 1),
        ROAST = find_roast_hit(object, design, contrast, nrot = 99,
            seed = 1, fdr = 1),
        Time = find_time_hit(object, treatment = "TRT",
            control = c("Time3", "Time4"), days = c("Time3", "Time4"),
            fdr = 1),
        Rule = find_zscore_hit(metrics, number_barcode = 5),
        Robust = find_robust_zscore_hit(metrics, number_barcode = 5)
    )
    plot <- plot_common_hit(results, fdr = 0.5)
    expect_s3_class(plot, "patchwork")
    sizes <- plot$data
    expected <- vapply(results, function(result) {
        if ("FDR" %in% colnames(result)) {
            length(unique(result$Gene[result$FDR <= 0.5]))
        } else {
            length(unique(result$Gene))
        }
    }, numeric(1))
    expect_equal(sizes$Size, unname(expected))
    # Barcode-level results are refused
    expect_error(plot_common_hit(statistics, results$FRY),
        "one row for each barcode")
    expect_error(plot_common_hit(metrics, results$FRY),
        "one row for each barcode")
})
