# Replacement Methods of a ScreenR Object

The replacement functions of SummarizedExperiment work on ScreenR
objects, with safeguards that keep the object consistent:

- `assay<-` and `assays<-`: when the raw counts change, the
  normalization is removed (run
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  again); other assays can be added freely;

- `rowData<-`: the Barcode column must equal the row names (it is added
  when missing); the new annotation is used by all the following
  analyses;

- `colData<-` (and `$<-`): the object is checked, so for example the
  effective library sizes cannot be removed or made invalid while a
  normalization is recorded;

- `dimnames<-` (`rownames<-`, `colnames<-`): the Barcode column of the
  rowData follows the new barcode identifiers, which must be unique.

## Usage

``` r
# S4 method for class 'screenr_object,missing'
assay(x, i, withDimnames = TRUE, ...) <- value

# S4 method for class 'screenr_object,character'
assay(x, i, withDimnames = TRUE, ...) <- value

# S4 method for class 'screenr_object,numeric'
assay(x, i, withDimnames = TRUE, ...) <- value

# S4 method for class 'screenr_object,list'
assays(x, withDimnames = TRUE, ...) <- value

# S4 method for class 'screenr_object,SimpleList'
assays(x, withDimnames = TRUE, ...) <- value

# S4 method for class 'screenr_object'
rowData(x, ...) <- value

# S4 method for class 'screenr_object,DataFrame'
colData(x, ...) <- value

# S4 method for class 'screenr_object,list'
dimnames(x) <- value
```

## Arguments

- x:

  A ScreenR object.

- i:

  The assay (name or index).

- withDimnames, ...:

  Passed to the SummarizedExperiment method.

- value:

  The new value.

## Value

The modified ScreenR object.

## See also

[`screenr_object-class`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-class.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
counts <- SummarizedExperiment::assay(object, "counts")
# Changing the counts removes the normalization
SummarizedExperiment::assay(object, "counts") <- counts + 1
#> The counts have changed. The normalization has been removed: run normalize_data() again.
get_normalization(object)
#> NULL
```
