library(testthat)
library(ScreenR)
test_that("Creation of the screenR object", {
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))
  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))
  expect_s4_class(object = object, class = "screenR_object")
})
#> Test passed 🌈

test_that("Normalize Data", {
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))

  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))

  object <- normalize_data(object)
  expect_equal(dim(object@count_table), dim(object@normalized_count_table))
})
#> Test passed 🎉

test_that("Number of Barcode Lost", {
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))

  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))

  expect_equal(tibble::is_tibble(mapped_reads(object)), TRUE)

})
#> Test passed 🎉

test_that("Plot number of Barcode Lost", {
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))


  palette <- c("#1B9E77", "#1B9E77",
               "#D95F02", "#D95F02", "#D95F02",
               "#7570B3", "#7570B3", "#7570B3",
               "#E7298A", "#E7298A", "#E7298A",
               "#66A61E", "#66A61E", "#66A61E")

  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))
  plot <- plot_mapped_reads(object, palette, alpha = 0.5)
  expect_equal(class(plot)[2], "ggplot")
})
#> Test passed 🎉
