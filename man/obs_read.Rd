% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_read_obs.R
\name{obs_read}
\alias{obs_read}
\title{Read obspack (.txt)}
\usage{
obs_read(
  index,
  categories = "flask",
  expr = NULL,
  verbose = TRUE,
  n_site_code = 15,
  n_site_latitude = 18,
  n_site_longitude = 19,
  n_site_name = 15,
  n_site_country = 18,
  n_dataset_project = 21,
  n_lab = 16,
  n_scales = 31,
  n_site_elevation = 20,
  n_altitude_comment = 22,
  n_utc = 18,
  fill_value = -1e+34,
  as_list = FALSE
)
}
\arguments{
\item{index}{data.table}

\item{categories}{character; ONE category
: of c("aircraft-pfp", "aircraft-insitu",
"surface-insitu", "tower-insitu", "aircore", "surface-pfp", "shipboard-insitu",
"flask").}

\item{expr}{String expressions to filter data.table internally}

\item{verbose}{Logical to show more information}

\item{n_site_code}{number of characters extratced from metadata after search}

\item{n_site_latitude}{number of characters extracted from metadata after search}

\item{n_site_longitude}{number of characters extracted from metadata after search}

\item{n_site_name}{number of characters extracted from metadata after search}

\item{n_site_country}{number of characters extracted from metadata after search}

\item{n_dataset_project}{number of characters extracted from metadata after search}

\item{n_lab}{number of characters extracted from metadata after search}

\item{n_scales}{number of characters extracted from metadata after search}

\item{n_site_elevation}{number of characters extracted from metadata after search}

\item{n_altitude_comment}{number of characters extracted from metadata after search}

\item{n_utc}{number of characters extracted from metadata after search}

\item{fill_value}{fill value. Appeared in aoa_aircraft-flask_19_allvalid.txt}

\item{as_list}{Logical to return as list}
}
\value{
A data.frame with with an index obspack.
}
\description{
Each obspack file has a header with metadata and this
function reads selected fields from the metadata and add
them as columns. This new columns are used later to
be filtered
}
\note{
The identification of the altitude and type is critical.
The approach used here consists of:
1. Identify agl from the name of the tile.
2. If magl not present, search dill_values used in elevation and
transform them into NA (not available)
3. If magl is not present, agl = altitude - elevation
4. If there are some NA in elevation, will result some NA in agl
5. A new column is added named `altitude_final` to store agl or asl
6. Another column named `type_altitude` is added to identify "magl" or "masl"
7. If there is any case NA in `altitude_final`,
`type_altitude` is "not available"

Then, the relationship with hysplit is:
\tabular{ccc}{
  type_altitude \tab hysplit  \cr
  magl          \tab agl      \cr
  masl          \tab asl      \cr
  not available \tab f-PBL    \cr
}
}
\examples{
{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
obs_read(index, expr = "altitude_final == '5800'")
}
}
