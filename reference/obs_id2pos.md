# obs_id2pos

return ndata.frame based on footprint/receptor id

## Usage

``` r
obs_id2pos(id, sep = "x", asdf = FALSE)
```

## Arguments

- id:

  string

- sep:

  string

- asdf:

  Logical, to return as data.frame or not

## Value

data.frame with time and location based on input

## Details

Helpers Legacy

## See also

Other helpers legacy:
[`obs_grid()`](https://noaa-gml.github.io/rtorf/reference/obs_grid.md),
[`obs_info2id()`](https://noaa-gml.github.io/rtorf/reference/obs_info2id.md),
[`obs_julian()`](https://noaa-gml.github.io/rtorf/reference/obs_julian.md),
[`obs_normalize_dmass()`](https://noaa-gml.github.io/rtorf/reference/obs_normalize_dmass.md),
[`obs_traj_foot()`](https://noaa-gml.github.io/rtorf/reference/obs_traj_foot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
id <- '2002x08x03x10x45.00Nx090.00Ex00030'
(obs_id2pos(id, asdf = TRUE) -> dx)
id <- c('2002x08x03x10x00x45.000Nx090.000Ex00030',
        '2002x08x03x10x55x45.335Sx179.884Wx00030')
(obs_id2pos(id) -> dx)
(obs_id2pos(id, asdf = TRUE) -> dx)
(obs_id2pos(rep(id, 2)) -> dx)
(obs_id2pos(rep(id, 2), asdf = TRUE) -> dx)
} # }
```
