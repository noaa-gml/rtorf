# obs_julian

return numeric

return footprint/receptor id

## Usage

``` r
obs_julian(y, m, d, origin., legacy = FALSE, verbose = TRUE)

obs_julian(y, m, d, origin., legacy = FALSE, verbose = TRUE)
```

## Arguments

- y:

  year

- m:

  month

- d:

  day

- origin.:

  string

- legacy:

  logical, to use legacy code

- verbose:

  logical, to show more messages

## Value

returns day since 1/1/1960

a string with the receptor id

## Details

Helpers Legacy

## See also

Other helpers legacy:
[`obs_grid()`](https://noaa-gml.github.io/rtorf/reference/obs_grid.md),
[`obs_id2pos()`](https://noaa-gml.github.io/rtorf/reference/obs_id2pos.md),
[`obs_info2id()`](https://noaa-gml.github.io/rtorf/reference/obs_info2id.md),
[`obs_normalize_dmass()`](https://noaa-gml.github.io/rtorf/reference/obs_normalize_dmass.md),
[`obs_traj_foot()`](https://noaa-gml.github.io/rtorf/reference/obs_traj_foot.md)

Other helpers legacy:
[`obs_grid()`](https://noaa-gml.github.io/rtorf/reference/obs_grid.md),
[`obs_id2pos()`](https://noaa-gml.github.io/rtorf/reference/obs_id2pos.md),
[`obs_info2id()`](https://noaa-gml.github.io/rtorf/reference/obs_info2id.md),
[`obs_normalize_dmass()`](https://noaa-gml.github.io/rtorf/reference/obs_normalize_dmass.md),
[`obs_traj_foot()`](https://noaa-gml.github.io/rtorf/reference/obs_traj_foot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs_julian(1, 2020, 1)
} # }
if (FALSE) { # \dontrun{
# Do not run
obs_julian(y = 2002,
            m = 8,
            d = 3,
            legacy = TRUE)
obs_julian(y = 2002,
            m = 8,
            d = 3,
            legacy = FALSE)
} # }
```
