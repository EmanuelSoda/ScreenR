# Set or Remove the Normalization

Internal functions that record a normalization in the object (effective
library sizes in the colData, method and provenance in the metadata) or
remove it.

## Usage

``` r
set_normalization_state(object, sizes, state)

remove_normalization(object, reason = NULL)
```

## Arguments

- object:

  A ScreenR object.

- sizes:

  The effective library size of each sample.

- state:

  A list with the method and the provenance.

- reason:

  Why the normalization is removed, for the message; NULL for no
  message.

## Value

The ScreenR object.
