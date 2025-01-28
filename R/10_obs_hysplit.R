#' obs_hysplit_control
#'
#' This function creates a CONTROL file for HYSPLIT model.
#' It uses inputs from a data.frame with the receptor information.
#'
#' @param df data.frame of receptor information.must include time and
#' spacial parameters
#' @param nlocations number of locations.
#' @param duration number of hours of release. (Negative is backwards in time).
#' @param vertical_motion Vertical motion option.
#' @param top_model_domain altitude above ground level (m).
#' @param met meteorological models to be used.
#' @param nmet Number of days for the meteorological files. Default is
#' number of days in duration plus two days
#' @param metpath paths for each meteorological model output.
#' @param ngases Default 1.
#' @param emissions_rate Default 0
#' @param hour_emissions hour release, depend of type of release, instantaneous 0.01,
#' continuous 1.
#' @param release_start derived from df.
#' @param nsim_grids Number of simulated grids.
#' @param center_conc_grids center coordinates for conc grid.
#' @param grid_spacing grid spacing, default is 0.1 degree.
#' @param grid_span model extension in degrees by dimension.
#' @param nconc name of the concentration file, default is "cdump"
#' @return A data.frame with with an index obspack.
#' @importFrom data.table fwrite ".N" ":=" rbindlist "%chin%"
#' @export

#' @examples {
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' df <- dt[1]
#'
#' # if hysplit20          20x01x08x22x59 x34.4790Nx116.4090Wx07983.nc
#' # release start time is 20 01 08 22 59
#' }
#
obs_hysplit_control <- function(df,
                                nlocations = 1,
                                duration = -240,
                                vertical_motion = 0,
                                top_model_domain = 20000,
                                met = c("nams", "gfs0p25"),
                                nmet =  abs(duration/24) + 2, # 10 days plus 2, defaultdays
                                metpath = c("/work/noaa/lpdm/metfiles/nams/",
                                            "/work/noaa/lpdm/metfiles/gfs0p25"),
                                ngases = 1,
                                gas = "Foot",
                                emissions_rate = 0,
                                hour_emissions = 0.01,
                                release_start = NULL,
                                nsim_grids = 1,
                                center_conc_grids = c(0,0),
                                grid_spacing = c(0.1, 0.1),
                                grid_span = c(30, 30),
                                nconc = "cdump",
                                nvert_levels = 1,
                                height_vert_levels = 50,
                                sampling_start_time  = c(0,0,0,0),
                                sampling_end_time  = c(0,0,0,0),
                                sampling_interval_type = c(0, abs(duration), 0),
                                npol_depositing = 1,
                                particle_params = c(0,0,0),
                                dmwsrdre = c(0,0,0,0,0),
                                wrhcicbc = c(0,0,0),
                                radiactive_decay = 0,
                                pol_res = 0,
                                control = "CONTROL"){
  # lat lon height
  lat <- df$latitude
  lon <- df$longitude
  agl <- df$altitude_final

  lat <- sprintf(lat, fmt = '%#.4f')

  lon <- sprintf(lon, fmt = '%#.4f')

  agl <- sprintf(agl, fmt = '%#.4f')

  start_loc <- paste(lat, lon, agl)


  yr <- df$year
  mo <- df$month
  dy <- df$day
  ho <- df$hour
  mi <- df$minute

  rel_start <- paste(substr(yr, 3, 4), #I need to confirm
                     sprintf(mo, fmt = '%02d'),
                     sprintf(dy, fmt = '%02d'),
                     sprintf(ho, fmt = '%02d'),
                     sprintf(mi, fmt = '%02d'))

  nmodels <- length(met)


  hydate <- as.Date(ISOdate(yr, mo, dy + 1, ho, mi, 0, tz = "UTC"))

  # magick ####
  sink(control)

  cat(nlocations, "\n")

  cat(start_loc, "\n")

  cat(duration, "\n")

  cat(vertical_motion, "\n")

  cat(top_model_domain, "\n")

  cat(paste(length(met), nmet),"\n")


  for(j in seq_along(met)) {
    metx <- met[j]

    if(metx == "nams") {
      for(i in  1:nmet){
        hyd <- as.Date(hydate - i)

        cat(
          paste0(metpath[j],
                 data.table::year(hyd),
                 "/"),
          "\n")

        cat(
          paste0(strftime(hyd, format = "%Y%m%d"),
                 "_hysplit.t00z.namsa"),
          "\n")
      }
    }

    if(metx == "gfs0p25") {
      for(i in  1:nmet){
        hyd <- as.Date(hydate - i)

        cat(
          paste0(metpath[j],
                 data.table::year(hyd),
                 "/"),
          "\n")

        cat(
          paste0(strftime(hyd, format = "%Y%m%d"),
                 "_gfs0p25"),
          "\n")
      }
    }
  }

  cat(ngases, "\n")

  cat(gas, "\n")

  cat(emissions_rate, "\n")

  cat(hour_emissions, "\n")

  cat(rel_start, "\n")

  cat(nsim_grids, "\n")

  cat(center_conc_grids, "\n")

  cat(grid_spacing, "\n")

  cat(grid_span, "\n")

  cat("./\n")

  cat(nconc, "\n")

  cat(nvert_levels, "\n")

  cat(height_vert_levels, "\n")

  cat(sampling_start_time, "\n")

  cat(sampling_end_time, "\n")

  cat(sampling_interval_type, "\n")

  cat(npol_depositing, "\n")

  cat(particle_params, "\n")

  cat(dmwsrdre, "\n")

  cat(wrhcicbc, "\n")

  cat(radiactive_decay, "\n")

  cat(pol_res, "\n")

  sink()

}
