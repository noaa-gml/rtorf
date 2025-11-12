# Read obspack metadata

Read obspack metadata

## Usage

``` r
obs_plot(
  dt,
  time,
  y = "value",
  yfactor = 1,
  colu = "site_code",
  type = "p",
  n = if (length(unique(dt[[colu]])) == 1) unique(dt[[colu]]) else
    unique(dt[[colu]])[1:2],
  pal = cptcity::find_cpt("qual")[6],
  verbose = TRUE,
  xlab = "time",
  ylab = "value",
  xlim = range(dt[[time]], na.rm = TRUE),
  ylim = range(dt[[y]], na.rm = TRUE),
  ...
)
```

## Arguments

- dt:

  data.table

- time:

  x axis column (time)

- y:

  y axis column, default "value"

- yfactor:

  factor of y

- colu:

  column to plot by color, default site_code

- type:

  type of plot, default "p"

- n:

  Character indicating \`colu\` to subset, for instance, if you want to
  plot "site_code", here include all the site_code that you want to plot

- pal:

  Color palette name see
  [cpt](https://rdrr.io/pkg/cptcity/man/cpt.html), default
  "cb_qual_Accent_08"

- verbose:

  Logical to show more information

- xlab:

  Character, xlab

- ylab:

  Character, ylab

- xlim:

  x limits

- ylim:

  y limits

- ...:

  plot arguments

## Value

Plot

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
obs_plot(dt, time = "time")
} # }
```
