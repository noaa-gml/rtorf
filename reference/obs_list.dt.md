# list.dt

Some treatments for list of data.frames

## Usage

``` r
obs_list.dt(ldf, na, verbose = TRUE)
```

## Arguments

- ldf:

  list of data.frames

- na:

  common names in the final data.frame

- verbose:

  Logical to show more information

## Value

long data.table

## Note

1\. Filter out empty data.frames 2. identify common names 3. rbindlist
and return data.table

## See also

Other helpers:
[`fex()`](https://noaa-gml.github.io/rtorf/reference/fex.md),
[`obs_footname()`](https://noaa-gml.github.io/rtorf/reference/obs_footname.md),
[`obs_format()`](https://noaa-gml.github.io/rtorf/reference/obs_format.md),
[`obs_freq()`](https://noaa-gml.github.io/rtorf/reference/obs_freq.md),
[`obs_out()`](https://noaa-gml.github.io/rtorf/reference/obs_out.md),
[`obs_rbind()`](https://noaa-gml.github.io/rtorf/reference/obs_rbind.md),
[`obs_read_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_read_csvy.md),
[`obs_roundtime()`](https://noaa-gml.github.io/rtorf/reference/obs_roundtime.md),
[`obs_trunc()`](https://noaa-gml.github.io/rtorf/reference/obs_trunc.md),
[`obs_write_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_write_csvy.md),
[`rtorf-deprecated`](https://noaa-gml.github.io/rtorf/reference/rtorf-deprecated.md),
[`sr()`](https://noaa-gml.github.io/rtorf/reference/sr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
} # }
```
