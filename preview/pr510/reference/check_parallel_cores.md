# Check and Adjust Core Count Based on Hardware and Environment Limits

Check and Adjust Core Count Based on Hardware and Environment Limits

## Usage

``` r
check_parallel_cores(num_cores, chk = "", verbose = FALSE)
```

## Arguments

- num_cores:

  Integer. The initial number of requested cores.

- chk:

  Character. The value of an environment variable like
  `_R_CHECK_LIMIT_CORES_`. Defaults to `""`.

- verbose:

  Logical. Whether to report parallel setup information. Defaults to
  `FALSE`.

## Value

An integer representing the safe number of cores to use.
