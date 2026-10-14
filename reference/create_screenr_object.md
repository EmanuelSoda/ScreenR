# Create the ScreenR Object

Initial function to create the Screen Object.

## Usage

``` r
create_screenr_object(
  table = NULL,
  annotation = NULL,
  groups = NULL,
  replicates = c("")
)
```

## Arguments

- table:

  The count table obtained from the read alignment that contains the
  Barcodes as rows and samples as columns.

- annotation:

  The annotation table containing the information for each Barcode and
  the association to the corresponding Gene

- groups:

  A factor containing the experimental design label

- replicates:

  A vector containing the replicates label

## Value

An object containing all the needed information for the analysis.

## Examples

``` r
count_table <-
    data.frame(
        Barcode = c("Code_1", "Code_2", "Code_3", "Code_3"),
        Time_3_rep1 = c("3520", "3020", "1507", "1400"),
        Time_3_rep2 = c("3500", "3000", "1457", "1490"),
        Time_3_TRT_rep1 = c("1200", "1100", "1300", "1350"),
        Time_3_TRT_rep2 = c("1250", "1000", "1400", "1375")
    )
annotation_table <-
    data.frame(
        Gene = c("Gene_1", "Gene_1", "Code_2", "Code_2"),
        Barcode = c("Code_1", "Code_2", "Code_3", "Code_3"),
        Gene_ID = rep(NA, 4), Sequence = rep(NA, 4),
        Library = rep(NA, 4)
    )

groups <- factor(c("Control", "Control", "Treated", "Treated"))
obj <- create_screenr_object(
    table = count_table,
    annotation = annotation_table,
    groups = groups, replicates = c("")
)
obj
#> An object of class "screenr_object"
#> Slot "count_table":
#> # A tibble: 4 × 5
#>   Barcode Time_3_rep1 Time_3_rep2 Time_3_TRT_rep1 Time_3_TRT_rep2
#>   <chr>   <chr>       <chr>       <chr>           <chr>          
#> 1 Code_1  3520        3500        1200            1250           
#> 2 Code_2  3020        3000        1100            1000           
#> 3 Code_3  1507        1457        1300            1400           
#> 4 Code_3  1400        1490        1350            1375           
#> 
#> Slot "annotation_table":
#> # A tibble: 4 × 5
#>   Gene   Barcode Gene_ID Sequence Library
#>   <chr>  <chr>   <lgl>   <lgl>    <lgl>  
#> 1 Gene_1 Code_1  NA      NA       NA     
#> 2 Gene_1 Code_2  NA      NA       NA     
#> 3 Code_2 Code_3  NA      NA       NA     
#> 4 Code_2 Code_3  NA      NA       NA     
#> 
#> Slot "groups":
#> [1] Control Control Treated Treated
#> Levels: Control Treated
#> 
#> Slot "replicates":
#> [1] ""
#> 
#> Slot "normalized_count_table":
#> # A tibble: 0 × 0
#> 
#> Slot "data_table":
#> # A tibble: 0 × 0
#> 
```
