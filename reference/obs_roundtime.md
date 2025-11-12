# round seconds from "POSIXct" "POSIXt" classes

return rounded seconds from time

## Usage

``` r
obs_roundtime(x, n = 10)
```

## Arguments

- x:

  time as "POSIXct" "POSIXt"

- n:

  factor

## Value

numeric vector

## See also

Other helpers:
[`fex()`](https://noaa-gml.github.io/rtorf/reference/fex.md),
[`obs_footname()`](https://noaa-gml.github.io/rtorf/reference/obs_footname.md),
[`obs_format()`](https://noaa-gml.github.io/rtorf/reference/obs_format.md),
[`obs_freq()`](https://noaa-gml.github.io/rtorf/reference/obs_freq.md),
[`obs_list.dt()`](https://noaa-gml.github.io/rtorf/reference/obs_list.dt.md),
[`obs_out()`](https://noaa-gml.github.io/rtorf/reference/obs_out.md),
[`obs_rbind()`](https://noaa-gml.github.io/rtorf/reference/obs_rbind.md),
[`obs_read_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_read_csvy.md),
[`obs_trunc()`](https://noaa-gml.github.io/rtorf/reference/obs_trunc.md),
[`obs_write_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_write_csvy.md),
[`rtorf-deprecated`](https://noaa-gml.github.io/rtorf/reference/rtorf-deprecated.md),
[`sr()`](https://noaa-gml.github.io/rtorf/reference/sr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
x <- Sys.time() + seq(1, 55, 1)
paste0(x,"  ",
       obs_roundtime(x), "  ",
       obs_freq(data.table::second(x),
                seq(0, 55, 10)))
} # }
```
