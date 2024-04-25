#' Aggregates ObsPack by time
#'
#' This function add timeUTC as POSIX class,
#' local time and ending sampling time
#'
#'
#'
#' @param dt obspack data.table
#' @param gby (groupby) Character indicating the function to group the
#' data. Default is "mean" meaning that all numeric variables
#' will be averaged by the new_second column established in the `idcol`
#' @param cols Character which defines columns to be aggregated
#' @param time Character to aggregate time (solar, local, UTC)
#' @param byalt Logical, to aggregate by altitude or not (used in tower)
#' @param verbose logical to show more information
#' @return A data.frame with with an index obspack.
#' @importFrom data.table  second setDT setorderv mday
#' @importFrom lubridate decimal_date
#' @note By default add column timeUTC with parameters from time loca, solar or UTC
#' @export
#' @examples \dontrun{
#' # Do not run
#' dt <- obs_addtime(dt)
#' }
obs_agg <- function(dt,
                    gby = "mean",
                    cols = c("year",
                             "month",
                             "day",
                             "hour",
                             "minute",
                             "second",
                             "time",
                             "time_decimal",
                             "value",
                             "latitude",
                             "longitude",
                             "altitude",
                             "pressure",
                             "u",
                             "v",
                             "temperature",
                             "type_altitude",
                             "altitude_final"),
                    time = c("UTC", "solar", "local"),
                    byalt = FALSE,
                    verbose = TRUE){

  if(!inherits(dt, "data.table")){
    if(verbose) cat("Transforming your data into data.table\n")
    data.table::setDT(dt)
  }


  if(!any(names(dt) %in% "timeUTC")) stop("missing `timeUTC``")
  if(!any(names(dt) %in% "site_code")) stop("missing `site_code``")
  if(!any(names(dt) %in% "altitude_final")) stop("missing `altitude_final``")

  # Remember aircraft
  # Only aircraft need to be aggregated every 20 seconds
  # For hysplit, the ending times are not needed
  .SD <- NULL
  timecol <- NULL
  site_code <- NULL
  altitude_final <- NULL
  timeUTC <- NULL
  lab_1_abbr <- NULL
  dataset_calibration_scale <- NULL
  year_st <- NULL
  month_st  <- NULL
  day_st  <- NULL
  intake_height  <- NULL
  local_time  <- NULL
  day <- NULL
  # mean <- NULL

  # Here we need daily information
  # as this function assumes that the input is already preselected data
  # for the evening time, I need to aggregate by
  # year, month day
  # here I have two options, by solar time (default)
  # or by local time
  # solar time names ending in _st
  # while local time, the POSIXct time vector `local_time`
  # Then we have four options

  if(byalt) {
    if(verbose) cat("Selecting by alt\n")
    # Remember tower
    # A site can have different altitudes, then
    # each group must be processed differently


    if(time == "solar") {

      if(verbose) cat("Selecting time `solar`\n")

      if(any(names(dt) %in% "intake_height")) { #solar + alt
        if(verbose) cat("Identified intake_height\n")

        dt <- dt[,
                 lapply(.SD,
                        mean,
                        na.rm = T),
                 .SDcols = cols,
                 by = list(year_st,
                           month_st,
                           day_st,
                           site_code,
                           intake_height,
                           lab_1_abbr,
                           dataset_calibration_scale)]

      } else { #solar - alt
        if(verbose) cat("Identified altitude_final\n")

        dt <- dt[,
                 lapply(.SD,
                        mean,
                        na.rm = T),
                 .SDcols = cols,
                 by = list(year(local_time),
                           month(local_time ),
                           day = strftime(local_time, "%d", tz = "UTC"),
                           site_code,
                           altitude_final,
                           lab_1_abbr,
                           dataset_calibration_scale)]
      }

      dt$timeUTC <- ISOdatetime(dt$year_st,
                                   dt$month_dt,
                                   dt$day_st,
                                   0,
                                   0,
                                   0)


    } else if(time == "local") {

      if(verbose) cat("Selecting time `local`\n")

      if(any(names(dt) %in% "intake_height")) { #solar + alt
        if(verbose) cat("Identified intake_height\n")

        dt <- dt[,
                 lapply(.SD,
                        mean,
                        na.rm = T),
                 .SDcols = cols,
                 by = list(year(local_time),
                           month(year(local_time)),
                           day = strftime(local_time, "%d", tz = "UTC"),
                           site_code,
                           intake_height,
                           lab_1_abbr,
                           dataset_calibration_scale)]

      } else { #solar - alt
        if(verbose) cat("Identified altitude_final\n")

        dt <- dt[,
                 lapply(.SD,
                        mean,
                        na.rm = T),
                 .SDcols = cols,
                 by = list(year(local_time),
                           month(year(local_time)),
                           day = strftime(local_time, "%d", tz = "UTC"),
                           site_code,
                           intake_height,
                           lab_1_abbr,
                           dataset_calibration_scale)]

      }

      dt$timeUTC <- ISOdatetime(year,
                                   month,
                                   day,
                                   0,
                                   0,
                                   0)

    } else {
      if(verbose) cat("Assuming time `UTC`\n")

      if(any(names(dt) %in% "intake_height")) { #solar + alt
        if(verbose) cat("Identified intake_height\n")

        dt <- dt[,
                 lapply(.SD,
                        mean,
                        na.rm = T),
                 .SDcols = cols,
                 by = list(year,
                           month,
                           day,
                           site_code,
                           intake_height,
                           lab_1_abbr,
                           dataset_calibration_scale)]

      } else { #solar - alt
        if(verbose) cat("Identified altitude_final\n")

        dt <- dt[,
                 lapply(.SD,
                        mean,
                        na.rm = T),
                 .SDcols = cols,
                 by = list(year,
                           month,
                           day,
                           site_code,
                           intake_height,
                           lab_1_abbr,
                           dataset_calibration_scale)]

      }

      dt$timeUTC <- ISOdatetime(year,
                                month,
                                day,
                                0,
                                0,
                                0)

    }




  } else {
    if(verbose) cat("Not selecting by alt\n")

    if(time == "solar") {

      if(verbose) cat("Selecting time `solar`\n")


      dt <- dt[,
               lapply(.SD,
                      mean,
                      na.rm = T),
               .SDcols = cols,
               by = list(year(local_time),
                         month(local_time ),
                         day = strftime(local_time, "%d", tz = "UTC"),
                         site_code,
                         lab_1_abbr,
                         dataset_calibration_scale)]

      dt$timeUTC <- ISOdatetime(dt$year_st,
                                   dt$month_dt,
                                   dt$day_st,
                                   0,
                                   0,
                                   0)


    } else if(time == "local") {

      if(verbose) cat("Selecting time `local`\n")


      dt <- dt[,
               lapply(.SD,
                      mean,
                      na.rm = T),
               .SDcols = cols,
               by = list(year(local_time),
                         month(year(local_time)),
                         day = strftime(local_time, "%d", tz = "UTC"),
                         site_code,
                         lab_1_abbr,
                         dataset_calibration_scale)]


      dt$timeUTC <- ISOdatetime(year,
                                   month,
                                   day,
                                   0,
                                   0,
                                   0)

    } else {
      if(verbose) cat("Assuming time `UTC`\n")

      dt <- dt[,
               lapply(.SD,
                      mean,
                      na.rm = T),
               .SDcols = cols,
               by = list(year,
                         month,
                         day,
                         site_code,
                         intake_height,
                         lab_1_abbr,
                         dataset_calibration_scale)]

      dt$timeUTC <- ISOdatetime(year,
                                month,
                                day,
                                0,
                                0,
                                0)

    }




  }

  dt$time <- as.numeric(dt$timeUTC)
  dt$time_decimal <- lubridate::decimal_date(dt$timeUTC)

  if(any(cols %in% "dif_time")) {
    cat("Detecting dif_time. Adding ending times\n")
    dt$timeUTC_end <- dt$timeUTC + dt$dif_time
    dt$year_end <- data.table::year(dt$timeUTC_end)
    dt$month_end <- data.table::month(dt$timeUTC_end)
    dt$day_end <- data.table::mday(dt$timeUTC_end)
    dt$hour_end <- data.table::hour(dt$timeUTC_end)
    dt$minute_end <- data.table::minute(dt$timeUTC_end)
    dt$second_end <- data.table::second(dt$timeUTC_end)

  }

  data.table::setorderv(x = dt,
                        cols =  c("site_code", "timeUTC"),
                        order =  1)

  # dt$timeUTC <- as.character(dt$timeUTC)

  return(dt)
}
