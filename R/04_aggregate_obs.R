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
#' @param byalt Logical, to aggregate by altitude or not (used in tower)
#' @param verbose logical to show more information
#' @return A data.frame with with an index obspack.
#' @importFrom data.table  second setDT setorderv mday
#' @importFrom lubridate decimal_date

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
  # mean <- NULL
  if(byalt) {
    # Remember tower
    # A site can have different altitudes, then
    # each group must be processed differently

    dt <- dt[,
             lapply(.SD,
                    mean,
                    na.rm = T),
             .SDcols = cols,
             by = list(timeUTC,
                       site_code,
                       altitude_final,
                       lab_1_abbr, # assuming site_code matches lab_1
                       dataset_calibration_scale)] # assuming site_code matches scale

  } else {
    dt <- dt[,
             lapply(.SD,
                    mean,
                    na.rm = T),
             .SDcols = cols,
             by = list(timeUTC,
                       site_code,
                       lab_1_abbr, # assuming site_code matches lab_1
                       dataset_calibration_scale)] # assuming site_code matches scale

  }
# return(dt)

  dt$timeUTC <- as.POSIXct(dt$timeUTC, tz = "UTC")

  dt$year <- data.table::year(dt$timeUTC)
  dt$month <- data.table::month(dt$timeUTC)
  dt$day <- data.table::mday(dt$timeUTC)
  dt$hour <- data.table::hour(dt$timeUTC)
  dt$minute <- data.table::minute(dt$timeUTC)
  dt$second <- data.table::second(dt$timeUTC)

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
