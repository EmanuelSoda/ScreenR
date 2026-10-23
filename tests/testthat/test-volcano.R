volcano_tables <- function() {
    statistics <- data.frame(
        Gene = rep(c("A", "B", "C", "D"), each = 2),
        Log2FC = c(-3, -2, 0.1, -0.1, -1, -1.5, 2, 1)
    )
    hits <- summarise_hits(list(
        one = data.frame(Gene = c("A", "B", "C", "D"),
            FDR = c(0.001, 0.8, 0.03, 0.5)),
        two = data.frame(Gene = c("A", "B", "C"), FDR = c(0.01, 0.9, 0.2))
    ), fdr = 0.05)
    list(statistics = statistics, hits = hits)
}

test_that("plot_volcano uses MaxFDR and the mean log2FC of the barcodes", {
    tables <- volcano_tables()
    plot <- plot_volcano(tables$hits, statistics = tables$statistics)
    expect_s3_class(plot, "ggplot")
    data <- plot$data
    # D has no FDR in the second method, so no MaxFDR
    expect_setequal(data$Gene, c("A", "B", "C"))
    expect_equal(data$Log2FC[data$Gene == "A"], -2.5)
    expect_equal(data$Significance[data$Gene == "C"], -log10(0.2))
    expect_equal(as.character(data$Status[data$Gene == "A"]), "Hit")
    expect_equal(as.character(data$Status[data$Gene == "C"]), "Not a hit")
    expect_match(plot$labels$subtitle, "1 hits at 5% FDR")
})

test_that("plot_volcano plots the table of a single method", {
    table <- data.frame(Gene = c("A", "B"), Log2FC = c(-2, 0.5),
        FDR = c(0, 0.4))
    plot <- plot_volcano(table, fdr = 0.1, genes = "B")
    # An FDR of 0 is drawn at the smallest positive FDR
    expect_equal(plot$data$Significance[plot$data$Gene == "A"],
        -log10(0.4))
    expect_equal(plot$layers[[3]]$data$Gene, "B")
    expect_equal(plot$labels$y, "-log10 FDR")
})

test_that("plot_volcano checks its input", {
    tables <- volcano_tables()
    expect_error(plot_volcano(tables$hits), "statistics")
    expect_error(plot_volcano(data.frame(Gene = "A", FDR = 1,
        Log2FC = 1), fdr_column = "PValue"), "PValue")
    expect_error(plot_volcano(tables$hits, tables$statistics, fdr = 2),
        "fdr")
    expect_error(plot_volcano(tables$hits, tables$statistics,
        colors = "red"), "colors")
})
