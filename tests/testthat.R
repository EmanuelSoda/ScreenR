library(testthat)
library(ScreenR)
test_that("Creation of the screenR object", {
  library(tibble)
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
  library(tibble)
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
test_that("Create counta table", {
  library(tibble)
  table <- Barcode_creation_Dataframe("")
  mapped <- mapped_reads(object)

  expect_equal(is_tibble(mapped), TRUE)
})
#> Test passed 🎉
test_that("Number mapped reads", {
  library(tibble)
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))

  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))
  mapped <- mapped_reads(object)

  expect_equal(is_tibble(mapped), TRUE)
 })
#> Test passed 🎉

test_that("Plot number mapped reads", {
  library(tibble)
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
  plot <- plot_mapped_reads(object, palette) + theme_minimal()
  plot + theme(legend.position = "bottom", legend.direction = "horizontal",
               axis.text.x = element_text(angle = 40, hjust = 1)) +
    ggtitle("Number of Mapped Reads in each sample")
  expect_equal(class(plot)[2], "ggplot")
})
#> Test passed 🎉


test_that("Boxplot mapped reads", {
  library(tibble)
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
  plot <- distribution_mapped_reads(object, palette, alpha = 0.8, type = "boxplot")
  expect_equal(class(plot)[2], "ggplot")
})
#> Test passed 🎉

test_that("Density mapped reads", {
  library(tibble)
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
  plot <- distribution_mapped_reads(object, palette, alpha = 0.8, type = "density")
  expect_equal(class(plot)[2], "ggplot")
})
#> Test passed 🎉


test_that("Number of Barcode Lost", {
  library(tibble)
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
  barcode_lost <- barcode_lost(object)
  expect_equal(is_tibble(barcode_lost), TRUE)
})
#> Test passed 🎉

test_that("Plot number of Barcode Lost", {
  library(tibble)
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

  plot <- plot_barcode_lost(screenR_Object = object, palette = palette)
  expect_equal(class(plot)[2], "ggplot")
})
#> Test passed 🎉
test_that("Plot number of barcode lost for gene", {
  library(tibble)
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
  plot <- plot_barcode_lost_for_gene(object)
  expect_equal(class(plot)[2], "ggplot")
})


test_that("Create data_table", {
  library(tibble)
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
  object <- normalize_data(object)


  object <- compute_data_table(object)
  expect_equal(class(object@data_table)[1], "tbl_df")
})
#> Test passed 🎉


test_that("Compute Metrics ", {
  library(tibble)
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
  object <- normalize_data(object)
  object <- compute_data_table(object)

  table <- compute_metrics(object)
  expect_equal(class(table)[1], "tbl_df")
})

test_that("Hit Z-score per tutti i giorni ", {
  library(tibble)
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
  object <- normalize_data(object)
  object <- compute_data_table(object)

  table <- compute_metrics(object)

  hit_table <- find_zscore_hit(table, 6)
  expect_equal(class(hit_table)[1], "tbl_df")

  # Devo verificare meglio ma secondo me sto sbagliando perché qui non credo
  # siano divisi per giorni credo faccia trattato su controllo di tutto e
  # non so come dividerli...
  # si È così e non so assolutamente come dividerli

  ############
  # Ahhhh ho capito quale è il problema, qui prendo tutti i trattatti
  # su tutti i controlli ma io voglio un Z-score per giorno!

  ### però si risolve facilmente specificando i trattamenti che voglio

})

test_that("Hit Z-score per giorno", {
  library(tibble)
  library(data.table)
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
  object <- normalize_data(object)
  object <- compute_data_table(object)

  treated <- c("Day3_Met_A", "Day3_Met_B", "Day3_Met_C")
  control <- c("Day3_A", "Day3_B", "Day3_C")
  table1 <- compute_metrics(object,treated = treated, control = control)


  hit_table <- find_zscore_hit(table, 6)
  expect_equal(class(hit_table)[1], "tbl_df")
})


test_that("Plot MDS 2D", {
  library(tibble)
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))


  palette <- c("#1B9E75", "#1B9E75",
               "#D95F02", "#D95F02", "#D95F02",
               "#7570B3", "#7570B3", "#7570B3",
               "#E7298A", "#E7298A", "#E7298A",
               "#66A61E", "#66A61E", "#66A61E")
  CountTable_THP1_CONTROL_vs_MET <-
    CountTable_THP1_CONTROL_vs_MET %>%
    dplyr::filter(Barcode != '*')
  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))
  object <- normalize_data(object)
  plot <- plot_MDS(screenR_Object = object, palette = palette)
  expect_equal(class(plot), "function")
})

test_that("Plot MDS 3D", {
  library(tibble)
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))


  palette <- c("#1B9E75", "#1B9E75",
               "#D95F02", "#D95F02", "#D95F02",
               "#7570B3", "#7570B3", "#7570B3",
               "#E7298A", "#E7298A", "#E7298A",
               "#66A61E", "#66A61E", "#66A61E")
  CountTable_THP1_CONTROL_vs_MET <-
    CountTable_THP1_CONTROL_vs_MET %>%
    dplyr::filter(Barcode != '*')
  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))
  object <- normalize_data(object)
  plot <- plot_MDS(screenR_Object = object, palette = palette, dimension = 3)
  expect_equal(class(plot)[1], "plotly")
})

test_that("Camera", {
  library(tibble)
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))


  palette <- c("#1B9E75", "#1B9E75",
               "#D95F02", "#D95F02", "#D95F02",
               "#7570B3", "#7570B3", "#7570B3",
               "#E7298A", "#E7298A", "#E7298A",
               "#66A61E", "#66A61E", "#66A61E")
  CountTable_THP1_CONTROL_vs_MET <-
    CountTable_THP1_CONTROL_vs_MET %>%
    dplyr::filter(Barcode != '*')
  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))
  object <- normalize_data(object)
  matrix <- model.matrix(~object@groups)
  colnames(matrix) <- c("Control", "T0/T48", "Treated")
  camera_hit <- find_camera_hit(screenR_Object = object,
                                matrix_model = matrix,
                                contrast = "Treated")
  expect_equal(class(camera_hit), "data.frame")
})

test_that("ROAST", {
  library(tibble)
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))


  palette <- c("#1B9E75", "#1B9E75",
               "#D95F02", "#D95F02", "#D95F02",
               "#7570B3", "#7570B3", "#7570B3",
               "#E7298A", "#E7298A", "#E7298A",
               "#66A61E", "#66A61E", "#66A61E")
  CountTable_THP1_CONTROL_vs_MET <-
    CountTable_THP1_CONTROL_vs_MET %>%
    dplyr::filter(Barcode != '*')
  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))
  object <- normalize_data(object)
  matrix <- model.matrix(~object@groups)
  colnames(matrix) <- c("Control", "T0/T48", "Treated")
  roast_hit <- find_roast_hit(screenR_Object = object,
                                matrix_model = matrix,
                                contrast = "Treated")
  expect_equal(class(roast_hit), "data.frame")
})

test_that("Plot Barcode", {
  library(tibble)
  groups <- factor(c("T0/T48", "T0/T48",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control",
                     "Treated", "Treated", "Treated",
                     "Control", "Control", "Control"))


  palette <- c("#1B9E75", "#1B9E75",
               "#D95F02", "#D95F02", "#D95F02",
               "#7570B3", "#7570B3", "#7570B3",
               "#E7298A", "#E7298A", "#E7298A",
               "#66A61E", "#66A61E", "#66A61E")

  CountTable_THP1_CONTROL_vs_MET <-
    CountTable_THP1_CONTROL_vs_MET %>%
    dplyr::filter(Barcode != '*')

  object <- create_screenR_object(table = CountTable_THP1_CONTROL_vs_MET,
                                  annotation = Table_Annotation,
                                  groups = groups,
                                  replicates = c(""))
  object <- normalize_data(object)
  matrix <- model.matrix(~object@groups)
  colnames(matrix) <- c("Control", "T0_T48", "Treated")
  hit_roast <- find_roast_hit(screenR_Object = object,
                              matrix_model = matrix,
                              contrast = "Treated")
  hit_camera <- find_camera_hit(screenR_Object = object,
                                matrix_model = matrix,
                                contrast = "Treated")

  object <- compute_data_table(object)

  treated <- c("Day3_Met_A", "Day3_Met_B", "Day3_Met_C")
  control <- c("Day3_A", "Day3_B", "Day3_C")
  table <- compute_metrics(object, treated = treated, control = control)
  hit_zscore <- find_zscore_hit(table, number_barcode = 6)

  find_common_hit <- find_common_hit(zscore_hit, camera_hit,
                                     roast_hit, common_in = 2)

  contrast <- limma::makeContrasts(Treated-Control,
                                        levels=matrix)
  plot_barcode_hit(object, matrix,
                   find_common_hit,
                   contrast = contrast,
                   gene = "ACAA1",
                   number_plot_row = 1,
                   number_plot_col = 1)
})







