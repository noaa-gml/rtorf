# obs_hysplit_control_read

This function creates a CONTROL file for HYSPLIT model. It uses inputs
from a data.frame with the receptor information.

## Usage

``` r
obs_hysplit_control_read(control = "CONTROL")
```

## Arguments

- control:

  CONTROL text file

## Value

A list with CONTROL information

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
df <- dt[1]
control_file <- tempfile()
obs_hysplit_control(df, control = control_file)
ff <- readLines(control_file)

cat(ff, sep =  "\n")
obs_hysplit_control_read(control_file)

} # }
```
