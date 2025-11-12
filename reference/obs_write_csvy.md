# Generates YAML and write data.frame

Add YAML header info and writes a the data.frame into disk. The YAML
section includes notes and str(dt).

## Usage

``` r
obs_write_csvy(
  dt,
  notes,
  out = paste0(tempfile(), ".csvy"),
  sep = ",",
  nchar.max = 80,
  ...
)
```

## Arguments

- dt:

  data.table

- notes:

  notes.

- out:

  outfile path.

- sep:

  The separator between columns. Default is ",".

- nchar.max:

  Max nchar for str.

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
[`obs_read_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_read_csvy.md),
[`obs_roundtime()`](https://noaa-gml.github.io/rtorf/reference/obs_roundtime.md),
[`obs_trunc()`](https://noaa-gml.github.io/rtorf/reference/obs_trunc.md),
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
readLines(f)
data.table::fread(f, h = TRUE)
} # }
```
