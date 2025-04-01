#' Aggregates ObsPack by time
#'
#' This function add timeUTC as POSIX class,
#' local time and ending sampling time
#'
#' @param dt obspack data.table
#' @param cols Character which defines columns to be aggregated
#' @param by String with columns to be aggregated
#' @param fn Function to be applied to the columns, default mean
#' @param verbose logical to show more information
#' @return A data.frame with with an index obspack.
#' @importFrom data.table  second minute hour month year setDT setorderv mday .SD
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
                    by = c("key_time",
                           "site_code",
                           "altitude_final", # useful for towers. Name may change
                           "type_altitude",
                           "lab_1_abbr",
                           "dataset_calibration_scale"),
                    fn = "mean",
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

  # Here we need daily information
  # as this function assumes that the input is already preselected data
  # for the evening time, I need to aggregate by
  # year, month day
  # here I have two options, by solar time (default)
  # or by local time
  # solar time names ending in _st
  # while local time, the POSIXct time vector `local_time`

  .SD <- NULL

  dt <- dt[,
             lapply(.SD,
                    FUN = eval(parse(text = fn)),
                    na.rm = T),
             .SDcols = cols,
             by = by]


  if(verbose) cat("Adding time\n")

  dt$timeUTC <- as.POSIXct(dt$key_time, tz = "UTC")

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
                        cols =  c("site_code",
                                  "timeUTC"),
                        order =  1)

  # dt$timeUTC <- as.character(dt$timeUTC)

  return(dt)
}
