# Restore the Random State

Internal function that restores the random number state of the session
when the calling function exits, so that setting a seed inside a
function does not change the random numbers of the user.

## Usage

``` r
restore_random_state(envir = parent.frame())
```

## Arguments

- envir:

  The environment of the calling function.

## Value

NULL, invisibly.
