---
title: "Analysis_Example"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Analysis_Example}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::knitr}
editor_options: 
  chunk_output_type: console
---

# Importing Pacakge

```r
library(ScreenR)
library(tidyverse)
ggplot2::theme_set(ggplot2::theme_light())
```

# Read Data

We will use as example a Loss of Function Genetic Screening Performed on THP1
using Metforming at Day3 and Day6. First of all the data are read.

```r
data <-  tidyr::tibble(CountTable_THP1_CONTROL_vs_MET)
colnames(data) <- 
  c("Barcode", "T0", "T48_postPURO", "Day3_Met_A", "Day3_Met_B",
    "Day3_Met_C", "Day3_DMSO_A" ,"Day3_DMSO_B","Day3_DMSO_C",
    "Day6_Met_A", "Day6_Met_B","Day6_Met_C", "Day6_DMSO_A", 
    "Day6_DMSO_B", "Day6_DMSO_C")
data <-
  data %>%
  dplyr::mutate(Barcode = as.factor(Barcode)) %>%
  dplyr::filter(Barcode != "*") 


total_Annotation <-
  Table_Annotation %>% tibble::tibble() %>%
  dplyr::mutate(Barcode = as.factor(.$Barcode))
```

# Object Creation 
The ScreenR object is created using the function **create_screenR_object()**


```r
groups <- colnames(data)[2:length(colnames(data))]
groups <- gsub('(.*)_\\w+', '\\1', groups)
groups <- factor(x = groups, levels = unique(groups))
groups
```

```
##  [1] T0        T48       Day3_Met  Day3_Met  Day3_Met  Day3_DMSO Day3_DMSO
##  [8] Day3_DMSO Day6_Met  Day6_Met  Day6_Met  Day6_DMSO Day6_DMSO Day6_DMSO
## Levels: T0 T48 Day3_Met Day3_DMSO Day6_Met Day6_DMSO
```

```r
palette <-
  c("#66c2a5", "#fc8d62", rep("#8da0cb", 3), rep("#e78ac3", 3),
    rep("#a6d854", 3), rep("#ffd92f", 3))

object <- create_screenR_object(table = data,
                                annotation = total_Annotation,
                                groups = groups,
                                replicates = c(""))
```


# Start Analysis

```r
object <- normalize_data(object)
object <- compute_data_table(object)
```


## Mapped Reads

```r
plot <- plot_mapped_reads(object, palette) + 
  coord_flip() +
  scale_y_continuous(labels = scales::number_format()) +
  theme(legend.position = "none") +
  ggtitle("Number of Mapped Reads in each sample")

plot
```

![plot of chunk plot_mapped_reads](figure/plot_mapped_reads-1.png)


## Boxplot

```r
plot <-  distribution_mapped_reads(object, palette,
                                   alpha = 0.8,
                                   type = "boxplot") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 40, hjust = 1))  

plot 
```

![plot of chunk distribution_mapped_reads boxplot](figure/distribution_mapped_reads boxplot-1.png)

## Boxplot

```r
plot <- distribution_mapped_reads(object, palette,
                                   alpha = 0.5,
                                   type = "density") +
  ggplot2::theme(legend.position = "none")  

plot
```

![plot of chunk distribution_mapped_reads density](figure/distribution_mapped_reads density-1.png)

## Control Genes

```r
object@data_table %>%
  dplyr::filter(Gene %in% c("RPL30", "PSMA1", "LUC")) %>%
  
 ggplot(., aes(x=Sample, y=Frequency, fill=Treatment)) +
 geom_boxplot(alpha = 0.9, outlier.shape = NA) +
 geom_jitter(shape=16, size = 0.7, aes(colour = Treatment)) +
 scale_fill_manual(values = unique(palette)) +
 scale_color_manual(values =  unique(palette)) +
 scale_alpha_manual(values=c(1, 0.1)) +
 theme_light()  +
 ylab("Normalized Mapped Reads") +
 theme(axis.ticks = element_line(size = 0.3),
       # axis.text.x = element_text(angle = 25),
        legend.position = "none", legend.direction = "horizontal")  +
 coord_flip()  +
 scale_x_discrete(limits = rev(unique(object@data_table$Sample))) +
 facet_wrap("Gene", scales = "free")
```

![plot of chunk Control Genes](figure/Control Genes-1.png)


## Barcode Lost

```r
plot <-  plot_barcode_lost(screenR_Object = object,
                           palette = palette) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 40, hjust = 1))
plot
```

![plot of chunk plot_barcode_lost](figure/plot_barcode_lost-1.png)

## Plot MDS {.tabset}

### For Sample

```r
plot_MDS(screenR_Object = object) 
```

![plot of chunk Plot MDS Sample](figure/Plot MDS Sample-1.png)


### For Treatment

```r
GGgroups <- gsub(".*_","", groups)

plot_MDS(screenR_Object = object, 
         groups = factor(x = GGgroups, levels = unique(GGgroups)))
```

![plot of chunk Plot MDS Treatment](figure/Plot MDS Treatment-1.png)

### For Day

```r
GGgroups <- sub("_.*", "", groups)
plot_MDS(screenR_Object = object, 
         groups = factor(x = GGgroups,
                         levels = unique(GGgroups)))
```

![plot of chunk Plot MDS Day](figure/Plot MDS Day-1.png)

## Compute Metrics

```r
# 2DG
data_with_measure_Met <- list(
  Day3 = compute_metrics(object, control = "DMSO", 
                         treatment = "Met", day = "Day3"),
  Day6 = compute_metrics(object, control = "DMSO", 
                         treatment = "Met", day = "Day6"))


plot_Zscore_distribution(data_with_measure_Met,
                         alpha = 0.8) 
```

![plot of chunk compute_metrics](figure/compute_metrics-1.png)


## Z-score hit

```r
zscore_hit_Met <- list(
  Day3 = find_zscore_hit(table_treate_vs_control = data_with_measure_Met$Day3,
                number_barcode = 7,
                metric = "median"),
  Day6 = find_zscore_hit(table_treate_vs_control = data_with_measure_Met$Day6,
                number_barcode = 7,
                metric = "median"))
zscore_hit_Met
```

```
## $Day3
## # A tibble: 30 × 2
##    Gene   numberOfBarcode
##    <fct>            <int>
##  1 ASF1A                8
##  2 BRD3                 8
##  3 CBX1                 8
##  4 CBX8                 9
##  5 CHST13               8
##  6 EED                  8
##  7 ENO3                 8
##  8 FBP2                 8
##  9 HDAC1                8
## 10 HERC3                9
## # … with 20 more rows
## 
## $Day6
## # A tibble: 30 × 2
##    Gene   numberOfBarcode
##    <fct>            <int>
##  1 ACOX2                9
##  2 BAZ1B                8
##  3 BAZ2B                8
##  4 BRD9                 8
##  5 CHAF1A               8
##  6 CLOCK                9
##  7 EP300                8
##  8 GPT                  8
##  9 H2AFV                9
## 10 HECTD3               9
## # … with 20 more rows
```


## CAMERA

```r
groupss <- c(rep("T0/T48",4),as.character(groups[5:length(groups)]))


matrix_model <- model.matrix(~ 0 + groups)
colnames(matrix_model) <- unique(groups)

camera_hit_Met <- list(
  Day3 = find_camera_hit(screenR_Object = object,
                              matrix_model = matrix_model,
                              contrast = "Day3_Met"),
  Day6 = find_camera_hit(screenR_Object = object,
                              matrix_model = matrix_model,
                              contrast = "Day6_Met"))

camera_hit_Met
```

```
## $Day3
## # A tibble: 281 × 5
##    Gene    NGenes Direction PValue   FDR
##    <chr>    <dbl> <fct>      <dbl> <dbl>
##  1 INPP5E      10 Down      0.0131 0.522
##  2 LDHAL6B     10 Down      0.0189 0.674
##  3 CHD2        10 Down      0.0324 0.906
##  4 TRIM43B     10 Down      0.0414 0.906
##  5 TRIM9        9 Down      0.0447 0.906
##  6 TYMS         9 Down      0.0528 0.906
##  7 PDHA2       10 Down      0.0584 0.906
##  8 PRDM4       10 Down      0.0594 0.906
##  9 HUWE1       10 Down      0.0646 0.906
## 10 LUC         32 Down      0.0698 0.906
## # … with 271 more rows
## 
## $Day6
## # A tibble: 269 × 5
##    Gene    NGenes Direction PValue   FDR
##    <chr>    <dbl> <fct>      <dbl> <dbl>
##  1 INPP5E      10 Down      0.0131 0.779
##  2 LDHAL6B     10 Down      0.0201 0.941
##  3 CHD2        10 Down      0.0285 0.969
##  4 PDHA2       10 Down      0.0316 0.969
##  5 TRIM9        9 Down      0.0355 0.969
##  6 TYMS         9 Down      0.0370 0.969
##  7 TRIM62       9 Down      0.0437 0.969
##  8 TRIM43B     10 Down      0.0491 0.969
##  9 ME3         10 Down      0.0500 0.969
## 10 HUWE1       10 Down      0.0584 0.969
## # … with 259 more rows
```

## ROAST

```r
roast_hit_Met <- list(
  Day3 = find_roast_hit(screenR_Object = object,
                              matrix_model = matrix_model,
                              contrast = "Day3_Met"),
  Day6 = find_roast_hit(screenR_Object = object,
                              matrix_model = matrix_model,
                              contrast = "Day6_Met"))

roast_hit_Met
```

```
## $Day3
## # A tibble: 534 × 9
##    Gene   NGenes PropDown PropUp Direction PValue    FDR PValue.Mixed FDR.Mixed
##    <chr>   <int>    <dbl>  <dbl> <fct>      <dbl>  <dbl>        <dbl>     <dbl>
##  1 LUC        32        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  2 PSMA1      12        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  3 RPL30      12        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  4 SEPT5      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  5 SEPT9      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  6 ACAA1      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  7 ACAA2      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  8 ACACA      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  9 ACACB      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
## 10 ACAD11     10        1      0 Down      0.0001 0.0001       0.0001    0.0001
## # … with 524 more rows
## 
## $Day6
## # A tibble: 534 × 9
##    Gene   NGenes PropDown PropUp Direction PValue    FDR PValue.Mixed FDR.Mixed
##    <chr>   <int>    <dbl>  <dbl> <fct>      <dbl>  <dbl>        <dbl>     <dbl>
##  1 LUC        32        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  2 PSMA1      12        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  3 RPL30      12        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  4 SEPT5      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  5 SEPT9      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  6 ACAA1      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  7 ACAA2      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  8 ACACA      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
##  9 ACACB      10        1      0 Down      0.0001 0.0001       0.0001    0.0001
## 10 ACAD11     10        1      0 Down      0.0001 0.0001       0.0001    0.0001
## # … with 524 more rows
```


## Find Common Hit 

```r
common_hit_Met_at_least_2 <- list(
  Day3 = find_common_hit(zscore_hit_Met$Day3, camera_hit_Met$Day3,
                         roast_hit_Met$Day3, common_in = 2),
  Day6 = find_common_hit(zscore_hit_Met$Day6, camera_hit_Met$Day6,
                         roast_hit_Met$Day6, common_in = 2))

common_hit_Met_at_least_3  <- list(
  Day3 = find_common_hit(zscore_hit_Met$Day3, camera_hit_Met$Day3,
                         roast_hit_Met$Day3, common_in = 3),
  Day6 = find_common_hit(zscore_hit_Met$Day6, camera_hit_Met$Day6,
                         roast_hit_Met$Day6, common_in = 3))
```


```r
plot_common_hit(hit_zscore = zscore_hit_Met$Day3, 
                hit_camera = camera_hit_Met$Day3,  roast_hit_Met$Day3)
```

```
## Warning in sprintf("%d", n, 100 * n/sum(n)): un argomento non usato per il
## formato '%d'
```

![plot of chunk Venn diagram in at least 2](figure/Venn diagram in at least 2-1.png)













<!-- ## Plot Barcode Hit -->
<!-- ```{r Barcode Hit, fig.height=7, fig.width=10, message=FALSE, warning=FALSE} -->
<!-- contrast_Day21_2DG <- makeContrasts(Day21_2DG-Day21_DMSO, levels=matrix_model) -->
<!-- plot_barcode_hit(screenR_Object = object, -->
<!--                  matrix_model = matrix_model, -->
<!--                  contrast = contrast_Day21_2DG, -->
<!--                  hit_common = common_hit_2DG_at_least_3$Day21, -->
<!--                  gene = "KDM1A") -->

<!-- plot_barcode_trend(list_data_measure = data_with_measure_2DG, -->
<!--                    genes = c("KDM1A"), -->
<!--                    n_col = 1,color = ggsci::pal_jco()(10))  -->

<!-- data_with_measure_2DG$Day21  %>% arrange(Log2FC) %>%  filter(Gene == "KDM1A") -->

<!-- library(edgeR) -->
<!-- matrix <- -->
<!--   as.matrix(data[, 2: dim(data)[2]]) -->


<!-- rownames(matrix) <- data$Barcode -->

<!-- DGEList <- DGEList(counts = matrix, -->
<!--                    group = factor(groups), genes = total_Annotation) -->
<!-- xglm <- estimateDisp(DGEList, matrix_model) -->
<!-- fit <- glmFit(xglm, matrix_model) -->

<!-- contrast_Day21_2DG <- makeContrasts(Day21_2DG-Day21_DMSO, levels=matrix_model) -->
<!-- lrt_Day21_2DG <- glmLRT(fit, contrast = contrast_Day21_2DG) -->

<!-- genesymbols <- as.character(DGEList$genes[, 1]) -->
<!-- genesymbollist2 <- list() -->
<!-- unq <- unique(genesymbols) -->
<!-- unq <- unq[!is.na(unq)] -->
<!-- for(i in unq) { -->
<!--   sel <- genesymbols == i & !is.na(genesymbols) -->
<!--   if(sum(sel)>3) -->
<!--     genesymbollist2[[i]] <- which(sel) -->
<!-- } -->


<!-- barcodeplot(statistics = lrt_Day21_2DG$table$logFC, -->
<!--             index=genesymbollist2[["KDM1A"]], -->
<!--  main= paste("Day21: Barcode plot for Gene", "KDM1A", sep = " ") , -->
<!--  labels=c("Negative logFC", "Positive logFC"), -->
<!--  quantile=c(-0.5,0.5)) -->



<!-- prova <- object@annotation_table  %>%  -->
<!--   select(Gene, Barcode,Library) -->


<!-- prova <- prova %>%  -->
<!--   mutate(Barcode = as.numeric(Barcode))  %>%  -->
<!--   group_by(Gene) %>%  -->
<!--   split(.$Gene, .$Barcode)  -->

<!-- prova <- map(.x = prova,.f = function(x){x %>% pull(Barcode)}) -->

<!-- barcodeplot(lrt_Day21_2DG$table$logFC,index=prova[["KDM1A"]], -->
<!--  main= paste("Day21: Barcode plot for Gene", "KDM1A", sep = " ") , -->
<!--  labels=c("Negative logFC", "Positive logFC"), -->
<!--  quantile=c(-0.5,0.5)) -->

<!-- ``` -->








