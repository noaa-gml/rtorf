# reads CSVY

Reads CSVY, print YAML and fread file.

## Usage

``` r
obs_read_csvy(f, n = 100, ...)
```

## Arguments

- f:

  path to csvy file

- n:

  number of files to search for "—" yaml

- ...:

  extra data.table arguments

## See also

Other helpers:
[`fex()`](https://noaa-gml.github.io/rtorf/reference/fex.md),
[`obs_footname()`](https://noaa-gml.github.io/rtorf/reference/obs_footname.md),
[`obs_format()`](https://noaa-gml.github.io/rtorf/reference/obs_format.md),
[`obs_freq()`](https://noaa-gml.github.io/rtorf/reference/obs_freq.md),
[`obs_list.dt()`](https://noaa-gml.github.io/rtorf/reference/obs_list.dt.md),
[`obs_out()`](https://noaa-gml.github.io/rtorf/reference/obs_out.md),
[`obs_rbind()`](https://noaa-gml.github.io/rtorf/reference/obs_rbind.md),
[`obs_roundtime()`](https://noaa-gml.github.io/rtorf/reference/obs_roundtime.md),
[`obs_trunc()`](https://noaa-gml.github.io/rtorf/reference/obs_trunc.md),
[`obs_write_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_write_csvy.md),
[`rtorf-deprecated`](https://noaa-gml.github.io/rtorf/reference/rtorf-deprecated.md),
[`sr()`](https://noaa-gml.github.io/rtorf/reference/sr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
df <- data.frame(a = rnorm(n = 10),
                 time = Sys.time() + 1:10)

f <- paste0(tempfile(), ".csvy")
notes <- c("notes",
           "more notes")
obs_write_csvy(dt = df, notes = notes, out = f)
s <- obs_read_csvy(f)
s
# or
readLines(f)
data.table::fread(f)
} # }
```
