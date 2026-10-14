# ScreenR Example Analysis

## Importing Package

``` r

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r

library(tidyr)
```

## Introduction

The R package **ScreenR** has been developed to perform analysis of
High-throughput RNA interference screening using pooled shRNA libraries
and next generation sequencing.. Nowadays several short hairpin RNA
(shRNA) libraries are commercial available, and in the last years the
interest in this type of analysis, often called barcode screening, has
greatly increased for their benefits both from a time-consuming point of
view and for the possibility of carrying out screening on a large number
of genes simultaneously. However, the bioinformatic analysis of this
type of screening still lacks a golden standard. Here, ScreenR allows
the user to carry out a preliminary quality check of the experiment,
visually inspect the data and finally identify the most significant hits
of the experiment through a series of plots and cross-statistical
analyses.

## Installation

### Bioconductor

**ScreenR** requires several CRAN and Bioconductor R packages to be
installed. Dependencies are usually handled automatically, when
installing the package using the following commands:

``` r

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("ScreenR")
```

### Github

The newest version can directly be installed from GitHub using the CRAN
package devtools:

``` r

if (!require("devtools", quietly = TRUE))
   install.packages("devtools")

devtools::install_github("EmanuelSoda/ScreenR")
```

## Analysis

Here is reported the ScreenR pipeline
![](../reference/figures/Pipeline.png)

### Loading the package

After installation, loading the package is simple as :

``` r

library(ScreenR)
#> 
#> Welcome to ScreenR
#> --------------------------
#> 
#> See ?ScreenR for a list of
#> available functions.
#> 
#> Enjoy!
#> 
```

### Read Data

The input of ScreenR is a count table obtained from barcode alignment to
the reference genome/library. A count table is usually the starting
point of an RNA-seq deferentially expressed genes analysis and consists
of a matrix containing reads count organized with:

- Genes on the rows
- Samples on the columns

For this vignette we will use as an example a Loss of Function Chemical
lethality Genetic Screening performed using shRNA libraries where each
gene is represented by ten slightly different shRNA each labeled with a
unique barcode coming from an unpublished dataset generated using the
[Cellecta](https://cellecta.com/) protocol.

First of all the data has to be read. Then another very important step
is to set up the column names in the following way:

- The first part of the string is the timepoint
- The second is the type of sample (example: treated or control or the
  name of the treatment)
- Third slot is a way to make each replicate unique (for example a
  letter or a number)

In the end we have

**`Time point_Type of sample_replicate`**

and for example we could have **h24_control_1** which mean the first
replicate of the control sample at 24 hour.

Since this dataset comes from a Chemical Synthetic Lethality experiments
the samples treated with the drug combined with the shRNAs knockdown
should present a decreased number of reads compared to the controls.

``` r

data(count_table)
data(annotation_table)

data <- count_table
colnames(data) <- c(
    "Barcode", "Time1", "Time2", "Time3_TRT_A", "Time3_TRT_B", "Time3_TRT_C",
    "Time3_CTRL_A", "Time3_CTRL_B", "Time3_CTRL_C", 
    "Time4_TRT_A", "Time4_TRT_B", "Time4_TRT_C", 
    "Time4_CTRL_A", "Time4_CTRL_B", "Time4_CTRL_C"
)
data <- data %>%
    dplyr::mutate(Barcode = as.factor(Barcode)) %>%
    dplyr::filter(Barcode != "*") %>%  
  tibble()


total_Annotation <- annotation_table
```

### Object Creation

The second needed step is to create a **ScreenR object** from the count
table. The ScreenR object is created using the function
**create_screenr_object()** and will be used to store the most important
information to perform the analysis. Most of the ScreenR function takes
as main input the ScreenR object to perform the needed operation and
return a result.

``` r

groups <- factor(c(
    "T1/T2", "T1/T2",
    "Time3_TRT", "Time3_TRT", "Time3_TRT",
    "Time3_CTRL", "Time3_CTRL", "Time3_CTRL",
    "Time4_TRT", "Time4_TRT", "Time4_TRT",
    "Time4_CTRL", "Time4_CTRL", "Time4_CTRL"
))


palette <- c("#66c2a5", "#fc8d62", rep("#8da0cb", 3), rep("#e78ac3", 3),
    rep("#a6d854", 3), rep("#ffd92f", 3))


object <- create_screenr_object(
    table = data, annotation = total_Annotation, groups = groups,
    replicates = ""
)
```

### Removing all zero rows

``` r

object <- remove_all_zero_row(object)
```

### Computing the needed tables

Once the object is created, the data must be normalized to start the
analysis. ScreenR uses *Counts Per Million* (**CPM**) normalization
which has the following mathematical expression:

``` math
CPM = \frac{Number \; of \; mapped \; reads \; to \; a \; barcode} 
             { \sum_{sample}{Number\; of \;mapped \; reads}} *10^{6}
```

The number of reads mapped for each barcode in a sample are normalized
by the number of reads in that sample and multiplied by one million.

This information is store in a *data table* which is a tidy version of
the original *count table* and will be used throughout the analysis.

``` r

object <- normalize_data(object)
object <- compute_data_table(object)
```

### Quality Check

The first step to perform when dealing with sequencing data is to check
the quality of the samples. In ScreenR this can be done using several
methods.

#### Mapped Reads

The total number of mapped reads can be displayed with a barplot with
the formula.

``` r

plot <- plot_mapped_reads(object, palette) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("Number of Mapped Reads in each sample")

plot
```

![](Analysis_Example_files/figure-html/plot_mapped_reads-1.png)

For example the distribution can be seen using both boxplots or density
plots.

\#### Boxplot Mapped Reads

``` r

plot <- plot_mapped_reads_distribution(
    object, palette,
    alpha = 0.8,
    type = "boxplot"
) +
    coord_flip() +
    theme(legend.position = "none") 

plot
```

![](Analysis_Example_files/figure-html/plot_mapped_reads_distribution_boxplot-1.png)

##### Density plot

``` r

plot <- plot_mapped_reads_distribution(
    object, palette,
    alpha = 0.5,
    type = "density"
) +
    ggplot2::theme(legend.position = "none") 

plot
```

![](Analysis_Example_files/figure-html/plot_mapped_reads_distribution_density-1.png)

#### Barcode Lost

Another very important quality check when a Genetic Screening is
performed is to check the barcode lost during the experiment, meaning
the barcode that after different time points or treatments results in
reads count equal to zero. ScreenR implements a function able to compute
and plot the number of barcodes lost for each samples.

``` r

plot <- plot_barcode_lost(screenR_Object = object, palette = palette) +
    ggplot2::coord_flip()
plot
```

![](Analysis_Example_files/figure-html/plot_barcode_lost-1.png)

Moreover it is important to check if the lost barcodes in a sample all
belong to the same gene, in order to verify that an adequate number of
barcodes per gene are still present. This can be done by visualizing the
number of barcode lost in a sample by gene.

``` r

plot <- plot_barcode_lost_for_gene(object,
    samples = c("Time4_TRT_C", "Time4_CTRL_C")
)
plot
```

![](Analysis_Example_files/figure-html/plot_barcode_lost_for_gene-1.png)

#### Plot MDS

In order to see the samples clusterization an initial MDS analysis can
be conducted. In ScreenR this can be done using the *plot_mds* function
and the user can decide the color code of the graph in order to
highlight the trend of the samples based on replicates, treatment or
timepoints simply by modifying the field levels in the plot_mds
function.

##### For Sample

``` r

plot_mds(screenR_Object = object)
```

![](Analysis_Example_files/figure-html/Plot_MDS_Sample-1.png)

##### For Treatment

``` r

group_table <- get_data_table(object)   %>%
    select(Sample, Day, Treatment) %>%
    distinct()
#> ScreenR normalized data table containing:
#>  74438 rows
#>  9 columns

group_treatment <- group_table$Treatment

plot_mds(
    screenR_Object = object,
    groups = factor(x = group_treatment, levels = unique(group_treatment))
)
```

![](Analysis_Example_files/figure-html/Plot_MDS_Treatment-1.png)

##### For Day

``` r

group_day <- group_table$Day

plot_mds(
    screenR_Object = object,
    groups = factor(x = group_day, levels = unique(group_day))
)
```

![](Analysis_Example_files/figure-html/Plot_MDS_Day-1.png)

### Statistical Analysis

Once the various steps of the quality check have been passed, the actual
statistical analysis can begin. The statistical Analysis of ScreenR is
based on three different methods:

- [Z-score filtering](https://pubmed.ncbi.nlm.nih.gov/21515799/)
- [CAMERA
  filtering](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/)
- [ROAST
  filtering](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896/)

#### Z-score hit

In order to compute the Z-score, first a list of metrics has to be
computed. In particular a *Log2FC* is computed for the treated vs
control samples in the different conditions. This can be done using the
function *compute_metrics()*. Here is reported an example of treated vs
control in different day.

Then the different distribution of the Z-score can be plotted using the
*plot_zscore_distribution* function.

``` r

# 2DG
data_with_measure_TRT <- list(
    Time3 = compute_metrics(
        object,
        control = "CTRL", treatment = "TRT",
        day = "Time3"
    ),
    Time4 = compute_metrics(
        object,
        control = "CTRL", treatment = "TRT",
        day = "Time4"
    )
)

plot_zscore_distribution(data_with_measure_TRT, alpha = 0.8) 
```

![](Analysis_Example_files/figure-html/compute_metrics-1.png)

#### Z-score hit

Based on these metrics the Z-score hit identification can be computed
using the *find_zscore_hit* function.

``` r

zscore_hit_TRT <- list(
    Time3 = find_zscore_hit(
        table_treate_vs_control = data_with_measure_TRT$Time3,
        number_barcode = 6, metric = "median"
    ),
    Time4 = find_zscore_hit(
        table_treate_vs_control = data_with_measure_TRT$Time4,
        number_barcode = 6, metric = "median"
    )
)
zscore_hit_TRT
#> $Time3
#> # A tibble: 83 × 2
#>    Gene     numberOfBarcode
#>    <chr>              <int>
#>  1 Gene_1                 7
#>  2 Gene_11                7
#>  3 Gene_116               8
#>  4 Gene_120               8
#>  5 Gene_121               7
#>  6 Gene_128               8
#>  7 Gene_15                7
#>  8 Gene_156               8
#>  9 Gene_158               7
#> 10 Gene_160               7
#> # ℹ 73 more rows
#> 
#> $Time4
#> # A tibble: 97 × 2
#>    Gene     numberOfBarcode
#>    <chr>              <int>
#>  1 Gene_107               7
#>  2 Gene_121               8
#>  3 Gene_125               7
#>  4 Gene_126               7
#>  5 Gene_134               7
#>  6 Gene_138               7
#>  7 Gene_139               7
#>  8 Gene_14                7
#>  9 Gene_147               8
#> 10 Gene_148               9
#> # ℹ 87 more rows
```

#### CAMERA

The same can be done with the CAMERA hit using the function
*find_camera_hit*.

``` r

matrix_model <- model.matrix(~ groups)
colnames(matrix_model) <- unique(groups)


camera_hit_TRT <- list(
    Time3 = find_camera_hit(
        screenR_Object = object, matrix_model = matrix_model,
        contrast = "Time3_TRT",
    ),
    Time4 = find_camera_hit(
        screenR_Object = object, matrix_model = matrix_model,
        contrast = "Time4_TRT"
    )
)

camera_hit_TRT
#> $Time3
#> # A tibble: 255 × 5
#>    Gene     NGenes Direction   PValue      FDR
#>    <chr>     <dbl> <fct>        <dbl>    <dbl>
#>  1 Gene_364     12 Down      1.24e-15 5.57e-13
#>  2 Gene_372     12 Down      2.09e-15 5.57e-13
#>  3 Gene_374     10 Down      2.31e- 7 4.11e- 5
#>  4 Gene_282     10 Down      1.55e- 4 1.66e- 2
#>  5 Gene_75      10 Down      6.76e- 4 6.02e- 2
#>  6 Gene_323     10 Down      1.35e- 3 1.03e- 1
#>  7 Gene_179     10 Down      1.96e- 3 1.28e- 1
#>  8 Gene_117     10 Down      1.11e- 2 4.45e- 1
#>  9 Gene_228     10 Down      1.52e- 2 5.06e- 1
#> 10 Gene_211     10 Down      1.82e- 2 5.70e- 1
#> # ℹ 245 more rows
#> 
#> $Time4
#> # A tibble: 241 × 5
#>    Gene     NGenes Direction   PValue      FDR
#>    <chr>     <dbl> <fct>        <dbl>    <dbl>
#>  1 Gene_364     12 Down      6.21e-26 3.32e-23
#>  2 Gene_372     12 Down      1.16e-21 3.11e-19
#>  3 Gene_374     10 Down      5.69e- 8 1.01e- 5
#>  4 Gene_323     10 Down      6.16e- 7 8.22e- 5
#>  5 Gene_282     10 Down      3.20e- 6 3.41e- 4
#>  6 Gene_75      10 Down      1.27e- 5 1.13e- 3
#>  7 Gene_296     10 Down      9.87e- 5 7.53e- 3
#>  8 Gene_80      10 Down      3.02e- 4 2.01e- 2
#>  9 Gene_54      10 Down      7.77e- 3 3.19e- 1
#> 10 Gene_521     10 Down      1.34e- 2 4.53e- 1
#> # ℹ 231 more rows
```

#### ROAST

Last but not least this is done also for the ROAST hit using the
function  
*find_roast_hit*.

``` r

roast_hit_TRT <- list(
    Time3 = find_roast_hit(
        screenR_Object = object, matrix_model = matrix_model,
        contrast = "Time3_TRT", 
    ),
    Time4 = find_roast_hit(
        screenR_Object = object, matrix_model = matrix_model,
        contrast = "Time4_TRT"
    )
)

roast_hit_TRT
#> $Time3
#> # A tibble: 31 × 9
#>    Gene    NGenes PropDown PropUp Direction PValue    FDR PValue.Mixed FDR.Mixed
#>    <chr>    <int>    <dbl>  <dbl> <fct>      <dbl>  <dbl>        <dbl>     <dbl>
#>  1 Gene_3…     12    0.917    0   Down      0.0001 0.0089       0.0001    0.0267
#>  2 Gene_3…     10    0.7      0   Down      0.0001 0.0089       0.0003    0.0445
#>  3 Gene_3…     12    0.917    0   Down      0.0002 0.0200       0.0002    0.0400
#>  4 Gene_88     10    0.5      0.1 Down      0.0006 0.0489       0.0015    0.103 
#>  5 Gene_1…     10    0.3      0   Down      0.0006 0.0489       0.0181    0.229 
#>  6 Gene_75     10    0.4      0   Down      0.001  0.0701       0.0231    0.246 
#>  7 Gene_2…     10    0.5      0   Down      0.0022 0.0957       0.0236    0.247 
#>  8 Gene_2…     10    0.2      0   Down      0.0068 0.225        0.0813    0.421 
#>  9 Gene_4…     10    0.4      0.1 Down      0.0074 0.231        0.0096    0.170 
#> 10 Gene_4…     10    0.1      0.1 Down      0.0089 0.263        0.155     0.495 
#> # ℹ 21 more rows
#> 
#> $Time4
#> # A tibble: 108 × 9
#>    Gene   NGenes PropDown PropUp Direction PValue     FDR PValue.Mixed FDR.Mixed
#>    <chr>   <int>    <dbl>  <dbl> <fct>      <dbl>   <dbl>        <dbl>     <dbl>
#>  1 Gene_…     12      1      0   Down      0.0001 0.00191       0.0001   0.00089
#>  2 Gene_…     12      1      0   Down      0.0001 0.00191       0.0001   0.00089
#>  3 Gene_…     10      0.9    0   Down      0.0001 0.00191       0.0001   0.00089
#>  4 Gene_…     10      0.9    0   Down      0.0001 0.00191       0.0002   0.00174
#>  5 Gene_…     10      0.7    0   Down      0.0001 0.00191       0.0001   0.00089
#>  6 Gene_…     10      0.7    0.1 Down      0.0001 0.00191       0.0001   0.00089
#>  7 Gene_…     10      0.7    0   Down      0.0001 0.00191       0.0001   0.00089
#>  8 Gene_…     10      0.7    0   Down      0.0001 0.00191       0.0001   0.00089
#>  9 Gene_…     10      0.6    0   Down      0.0001 0.00191       0.0012   0.00539
#> 10 Gene_…     10      0.4    0   Down      0.0001 0.00191       0.0002   0.00174
#> # ℹ 98 more rows
```

#### Find Common Hit

ScreenR considers as final hit only the one result as candidate hit in
all three statistical methods. However this is a particularly stringent
method and in some cases leads to a small number of results. For this
reason the user can also decide to opt for a less stringent method that
considers only the hits present in at least two of the statistical
methods. The two different strategies can be computed with the
function::

- **common_hit_TRT_at_least_2**: considering candidate Hits the one
  present in at least two of the three methods (less stringent)

- **common_hit_TRT_at_least_3**: considering candidate Hits the one
  present in all of the three methods

``` r

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
```

#### Plot common hit

The intersection of the hits found by the three statistical methods can
be easily visualized using the *plot_common_hit* function.

``` r

plot_common_hit(
    hit_zscore = zscore_hit_TRT$Time4, hit_camera = camera_hit_TRT$Time4,
    roast_hit_TRT$Time4, show_elements = FALSE, show_percentage = TRUE
)
```

![](Analysis_Example_files/figure-html/Venn_diagram_in_at_least_2-1.png)

As we all know, when we deal with statistical methods the is the
possibility of *type I* error also known as “false positive”. For this
reason is important to visualize the results obtained. This can be done
by visualizing the trend of the candidate hits obtained using the
function **plot_trend**.

``` r

candidate_hits <- common_hit_TRT_at_least_2$Time3

plot_trend(screenR_Object = object, 
           genes = candidate_hits[1], 
           nrow = 2, ncol = 2, 
           group_var = c("Time1", "Time2", "TRT"))
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the ScreenR package.
#>   Please report the issue at <https://github.com/EmanuelSoda/ScreenR/issues>.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

![](Analysis_Example_files/figure-html/plot_trend-1.png)

``` r


plot_trend(screenR_Object = object, 
           genes = candidate_hits[2], 
           nrow = 2, ncol = 2, 
           group_var = c("Time1", "Time2", "TRT"))
```

![](Analysis_Example_files/figure-html/plot_trend-2.png)

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.5 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] ScreenR_1.15.1   tidyr_1.3.2      dplyr_1.2.1      ggplot2_4.0.3   
#> [5] BiocStyle_2.40.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] utf8_1.2.6          sass_0.4.10         generics_0.1.4     
#>  [4] stringi_1.8.9       lattice_0.22-9      digest_0.6.39      
#>  [7] magrittr_2.0.5      evaluate_1.0.5      grid_4.6.1         
#> [10] RColorBrewer_1.1-3  bookdown_0.48       fastmap_1.2.0      
#> [13] Matrix_1.7-5        jsonlite_2.0.0      limma_3.68.5       
#> [16] BiocManager_1.30.27 mgcv_1.9-4          purrr_1.2.2        
#> [19] scales_1.4.0        textshaping_1.0.5   jquerylib_0.1.4    
#> [22] cli_3.6.6           rlang_1.3.0         splines_4.6.1      
#> [25] withr_3.0.3         cachem_1.1.0        yaml_2.3.12        
#> [28] otel_0.2.0          tools_4.6.1         locfit_1.5-9.12    
#> [31] ggvenn_0.1.19       vctrs_0.7.3         R6_2.6.1           
#> [34] lifecycle_1.0.5     stringr_1.6.0       edgeR_4.10.5       
#> [37] fs_2.1.0            ragg_1.5.2          pkgconfig_2.0.3    
#> [40] desc_1.4.3          pkgdown_2.2.1       pillar_1.11.1      
#> [43] bslib_0.12.0        gtable_0.3.6        glue_1.8.1         
#> [46] statmod_1.5.2       systemfonts_1.3.2   xfun_0.61          
#> [49] tibble_3.3.1        tidyselect_1.2.1    knitr_1.52         
#> [52] farver_2.1.2        nlme_3.1-169        htmltools_0.5.9    
#> [55] patchwork_1.3.2     rmarkdown_2.32      labeling_0.4.3     
#> [58] compiler_4.6.1      S7_0.2.2
```
