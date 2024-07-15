#' Aggregates ObsPack by time
#'
#' This function add timeUTC as POSIX class,
#' local time and ending sampling time
#'
#'
#'
#' @param dt obspack data.table
#' @param cols Character which defines columns to be aggregated
#' @param byalt Logical, to aggregate by altitude or not (used in tower)
#' @param verbose logical to show more information
#' @return A data.frame with with an index obspack.
#' @importFrom data.table  second minute hour month year setDT setorderv mday
#' @importFrom lubridate decimal_date
#' @note By default add column timeUTC based on the input column `key_time`
#' @export
#' @examples \dontrun{
#' # Do not run
#' # dt <- obs_addtime(dt)
#' }
obs_agg <- function(dt,
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
                             "type_altitude"),
                    byalt = FALSE,
                    verbose = TRUE){

  if(!inherits(dt, "data.table")){
    if(verbose) cat("Transforming your data into data.table\n")
    data.table::setDT(dt)
  }


  if(!any(names(dt) %in% "key_time")) stop("missing `key_time`")
  if(!any(names(dt) %in% "site_code")) stop("missing `site_code`")
  if(!any(names(dt) %in% "altitude_final")) stop("missing `altitude_final`")

  # Remember aircraft
  # Only aircraft need to be aggregated every 20 seconds
  # For hysplit, the ending times are not needed
  .SD <- NULL
  timecol <- NULL
  site_code <- NULL
  altitude_final <- NULL
  type_altitude <- NULL
  timeUTC <- NULL
  key_time <- NULL
  lab_1_abbr <- NULL
  dataset_calibration_scale <- NULL
  year_st <- NULL
  month_st  <- NULL
  day_st  <- NULL
  intake_height  <- NULL
  local_time  <- NULL
  day <- NULL
  gby <- NULL
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
    dt <- dt[,
             lapply(.SD,
                    FUN = mean,
                    na.rm = T),
             .SDcols = cols,
             by = list(timeUTC = key_time,
                       site_code,
                       altitude_final,
                       type_altitude,
                       lab_1_abbr,
                       dataset_calibration_scale)]
  } else {
    dt <- dt[,
             lapply(.SD,
                    FUN = mean,
                    na.rm = T),
             .SDcols = cols,
             by = list(timeUTC = key_time,
                       site_code,
                       lab_1_abbr,
                       dataset_calibration_scale)]

  }


  if(verbose) cat("Adding time\n")

  dt$timeUTC <- as.POSIXct(dt$timeUTC, tz = "UTC")

  dt$year <- data.table::year(dt$timeUTC)
  dt$month <- data.table::month(dt$timeUTC)
  dt$day <- strftime(dt$timeUTC,
                     "%d",
                     tz = "UTC")
  dt$hour <- data.table::hour(dt$timeUTC)
  dt$minute <- data.table::minute(dt$timeUTC)
  dt$second <- data.table::second(dt$timeUTC)

  dt$time <- as.numeric(dt$timeUTC)
  dt$time_decimal <- lubridate::decimal_date(dt$timeUTC)

  #     if(any(cols %in% "dif_time")) {
  #       cat("Detecting dif_time. Adding ending times\n")
  #       dt$timeUTC_end <- dt$timeUTC + dt$dif_time
  #       dt$year_end <- data.table::year(dt$timeUTC_end)
  #       dt$month_end <- data.table::month(dt$timeUTC_end)
  #       dt$day_end <- data.table::mday(dt$timeUTC_end)
  #       dt$hour_end <- data.table::hour(dt$timeUTC_end)
  #       dt$minute_end <- data.table::minute(dt$timeUTC_end)
  #       dt$second_end <- data.table::second(dt$timeUTC_end)
  #
  #     }

  data.table::setorderv(x = dt,
                        cols =  c("site_code", "timeUTC"),
                        order =  1)

  # dt$timeUTC <- as.character(dt$timeUTC)

  return(dt)
}
