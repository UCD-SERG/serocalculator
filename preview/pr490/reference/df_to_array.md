# Convert a data.frame (or tibble) into a multidimensional array

Convert a data.frame (or tibble) into a multidimensional array

## Usage

``` r
df_to_array(df, dim_var_names, value_var_name = "value")
```

## Arguments

- df:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) (or
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
  in long format (each row contains one value for the intended array)

- dim_var_names:

  a [`character()`](https://rdrr.io/r/base/character.html) vector of
  variable names in `df`. All of these variables should be factors, or a
  warning will be produced.

- value_var_name:

  a [`character()`](https://rdrr.io/r/base/character.html) variable
  containing a variable name from `df` which contains the values for the
  intended array.

## Value

an [`array()`](https://rdrr.io/r/base/array.html) with dimensions
defined by the variables in `df` listed in `dim_var_names`

## Examples

``` r
library(dplyr)
library(tidyr)
#> 
#> Attaching package: ‘tidyr’
#> The following object is masked from ‘package:magrittr’:
#> 
#>     extract

df <- iris %>%
  tidyr::pivot_longer(
    names_to = "parameter",
    cols = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length")
  ) %>%
  mutate(parameter = factor(parameter, levels = unique(parameter)))
arr <- df %>%
  serocalculator:::df_to_array(
     dim_var_names = c("parameter", "Species"))
ftable(arr[,,1:5])
#>                         obs   1   2   3   4   5
#> parameter    Species                           
#> Sepal.Length setosa         5.1 4.9 4.7 4.6 5.0
#>              versicolor     7.0 6.4 6.9 5.5 6.5
#>              virginica      6.3 5.8 7.1 6.3 6.5
#> Sepal.Width  setosa         3.5 3.0 3.2 3.1 3.6
#>              versicolor     3.2 3.2 3.1 2.3 2.8
#>              virginica      3.3 2.7 3.0 2.9 3.0
#> Petal.Width  setosa         0.2 0.2 0.2 0.2 0.2
#>              versicolor     1.4 1.5 1.5 1.3 1.5
#>              virginica      2.5 1.9 2.1 1.8 2.2
#> Petal.Length setosa         1.4 1.4 1.3 1.5 1.4
#>              versicolor     4.7 4.5 4.9 4.0 4.6
#>              virginica      6.0 5.1 5.9 5.6 5.8
```
