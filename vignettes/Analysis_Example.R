## ----packages, message=FALSE, warning=FALSE-----------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
knitr::opts_chunk$set(fig.width=10, fig.height=7) 

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(ScreenR)

## ----read_data, message=FALSE, warning=FALSE----------------------------------
data(count_table)
data(annotation_table)

data <- count_table
colnames(data) <- c(
    "Barcode", "T1", "T2", "Time3_TRT_A", "Time3_TRT_B", "Time3_TRT_C",
    "Time3_A", "Time3_B", "Time3_C", "Time4_TRT_A", "Time4_TRT_B",
    "Time4_TRT_C", "Time4_A", "Time4_B", "Time4_c"
)
data <- data %>%
    dplyr::mutate(Barcode = as.factor(Barcode)) %>%
    dplyr::filter(Barcode != "*")


total_Annotation <-  annotation_table 

## ----Create_Object, message=FALSE, warning=FALSE------------------------------
groups <- colnames(data)[2:length(colnames(data))]
groups <- gsub("(.*)_\\w+", "\\1", groups)
groups <- factor(x = groups, levels = unique(groups))

palette <- c(
    "#66c2a5", "#fc8d62", rep("#8da0cb", 3),
    rep("#e78ac3", 3),
    rep("#a6d854", 3),
    rep("#ffd92f", 3)
)

object <- create_screenR_object(
    table = data, annotation = total_Annotation, groups = groups,
    replicates = c("")
)

## ----normalizzation, message=FALSE, warning=FALSE-----------------------------
object <- normalize_data(object)
object <- compute_data_table(object)

## ----plot_mapped_reads, message=FALSE, warning=FALSE--------------------------
plot <- plot_mapped_reads(object, palette) + 
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::number_format()) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("Number of Mapped Reads in each sample")

plot

## ----distribution_mapped_reads_boxplot, message=FALSE, warning=FALSE----------
plot <- distribution_mapped_reads(
    object, palette, alpha = 0.8,
    type = "boxplot"
) +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 40, hjust = 1)
    )

plot

## ----distribution_mapped_reads_density, message=FALSE, warning=FALSE----------
plot <- distribution_mapped_reads(
    object, palette, alpha = 0.5,
    type = "density"
) +
    ggplot2::theme(legend.position = "none")

plot

## ----plot_barcode_lost, message=FALSE, warning=FALSE--------------------------
plot <- plot_barcode_lost(screenR_Object = object, palette = palette) +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 40, hjust = 1)
    )
plot

## ----Plot_MDS_Sample, message=FALSE, warning=FALSE----------------------------
plot_MDS(screenR_Object = object) 

## ----Plot_MDS_Treatment, message=FALSE, warning=FALSE-------------------------
GGgroups <- gsub(".*_", "", groups)

plot_MDS(
    screenR_Object = object,
    groups = factor(x = GGgroups, levels = unique(GGgroups))
)

## ----Plot_MDS_Day, message=FALSE, warning=FALSE-------------------------------
GGgroups <- sub("_.*", "", groups)
plot_MDS(
    screenR_Object = object,
    groups = factor(x = GGgroups, levels = unique(GGgroups))
)

## ----compute_metrics, message=FALSE, warning=FALSE----------------------------
# 2DG
data_with_measure_TRT <- list(
    Time3 = compute_metrics(
        object, control = "Time3", treatment = "TRT",
        day = "Time3"
    ),
    Time4 = compute_metrics(
        object, control = "Time4", treatment = "TRT",
        day = "Time4"
    )
)


plot_Zscore_distribution(data_with_measure_TRT, alpha = 0.8)

## ----Z_score_hit, message=FALSE, warning=FALSE--------------------------------
zscore_hit_TRT <- list(
    Time3 = find_zscore_hit(
        table_treate_vs_control = data_with_measure_TRT$Time3,
        number_barcode = 7, metric = "median"
    ),
    Time4 = find_zscore_hit(
        table_treate_vs_control = data_with_measure_TRT$Time4,
        number_barcode = 7, metric = "median"
    )
)
zscore_hit_TRT

## ----CAMERA, message=FALSE, warning=FALSE-------------------------------------
matrix_model <- model.matrix(~0 + groups)
colnames(matrix_model) <- unique(groups)

camera_hit_TRT <- list(
    Time3 = find_camera_hit(
        screenR_Object = object, matrix_model = matrix_model,
        contrast = "Time3_TRT"
    ),
    Time4 = find_camera_hit(
        screenR_Object = object, matrix_model = matrix_model,
        contrast = "Time4_TRT"
    )
)

camera_hit_TRT

## ----ROAST, message=FALSE, warning=FALSE--------------------------------------
roast_hit_TRT <- list(
    Time3 = find_roast_hit(
        screenR_Object = object, matrix_model = matrix_model,
        contrast = "Time3_TRT"
    ),
    Time4 = find_roast_hit(
        screenR_Object = object, matrix_model = matrix_model,
        contrast = "Time4_TRT"
    )
)

roast_hit_TRT

## ----Common_Hit,  message=FALSE, warning=FALSE--------------------------------
common_hit_TRT_at_least_2 <- list(
    Time3 = find_common_hit(
        zscore_hit_TRT$Time3, camera_hit_TRT$Time3, roast_hit_TRT$Day3,
        common_in = 2
    ),
    Time4 = find_common_hit(
        zscore_hit_TRT$Time4, camera_hit_TRT$Time4, roast_hit_TRT$Day6,
        common_in = 2
    )
)

common_hit_TRT_at_least_3 <- list(
    Time3 = find_common_hit(
        zscore_hit_TRT$Time3, camera_hit_TRT$Time3, roast_hit_TRT$Time3,
        common_in = 3
    ),
    Time4 = find_common_hit(
        zscore_hit_TRT$Time4, camera_hit_TRT$Time4, roast_hit_TRT$Time4,
        common_in = 3
    )
)

## ----Venn_diagram_in_at_least_2-----------------------------------------------
plot_common_hit(
    hit_zscore = zscore_hit_TRT$Time3, hit_camera = camera_hit_TRT$Time3,
    roast_hit_TRT$Time3
)

## -----------------------------------------------------------------------------
sessionInfo()

