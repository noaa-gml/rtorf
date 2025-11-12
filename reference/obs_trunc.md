# Trunc numbers with a desired number of decimals

Trunc numbers with a specified number of decimals.

## Usage

``` r
obs_trunc(n, dec)
```

## Arguments

- n:

  Numeric number

- dec:

  Integer, number of decimals

## Note

source https://stackoverflow.com/a/47015304/2418532

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
[`obs_roundtime()`](https://noaa-gml.github.io/rtorf/reference/obs_roundtime.md),
[`obs_write_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_write_csvy.md),
[`rtorf-deprecated`](https://noaa-gml.github.io/rtorf/reference/rtorf-deprecated.md),
[`sr()`](https://noaa-gml.github.io/rtorf/reference/sr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
# in bash:
# printf "%07.4f" 72.05785
# results in 72.0578
# but:
formatC(72.05785, digits = 4, width = 8, format = "f", flag = "0")
# results in
"072.0579"
# the goal is to obtain the same trunc number as using bash, then:
formatC(obs_trunc(72.05785, 4),
        digits = 4,
        width = 8,
        format = "f",
        flag = "0")
} # }
```
