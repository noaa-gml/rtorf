#' Add times into obspack
#'
#' This function add timeUTC as POSIX class,
#' local time and ending sampling time
#'
#'
#'
#' @param dt obspack data.table
#' @param verbose obspack data.table
#' @return A data.frame with with an index obspack.
#' @importFrom data.table year month hour minute second setDT fifelse uniqueN
#' @note timeUTC is calculated based on the field column start_time,
#' the timeUTC_end is calculated using this approach:
#' 1. If the column time_interval is not found, proceed
#' with the calculation using midpoint_time
#' 2. Else, use column time_interval
#'
#' @export
#' @examples {
#' # Do not run
#' obs <- system.file("data-raw", package = "robspack")
#' index <-  obs_summary(obs)
#' dt <- obs_read(index)
#' dt <- obs_addtime(dt)
#' }
obs_addtime <- function(dt,
                        verbose = TRUE){

  if(!inherits(dt, "data.table")) data.table::setDT(dt)

  if(verbose) cat("Adding timeUTC\n")
  # Time is already UTC
  dt$timeUTC <- as.POSIXct(dt$time,
                           origin = "1970-01-01",
                           tz = "UTC")

  if(verbose) cat("Adding timeUTC_start\n")
  # Time is already UTC
  dt$timeUTC_start <- as.POSIXct(dt$start_time,
                                 origin = "1970-01-01",
                                 tz = "UTC")

  if(verbose) cat("Adding timeUTC_end\n")
  # adding timeUTC_end
  midpoint_time <- NULL
  start_time <- NULL
  timeUTC <- NULL
  time_interval <- NULL
  timeUTC_start <- NULL
  if(any("time_interval" == names(dt))) {
    dt[, timeUTC_end := fifelse(
      is.na(time_interval),
      timeUTC_start + (midpoint_time - start_time)*2,
      timeUTC_start + time_interval
    )]
  } else {
    #if there is no column named time_interval
    time_inter <- (dt$midpoint_time - dt$start_time)*2
    dt$timeUTC_end <- dt$timeUTC_start + time_inter
  }

  dt$dif_time <- dt$timeUTC_end - dt$timeUTC_start
  time_warning <- NULL
  timeUTC_end <- NULL
  dt[, time_warning := ifelse(timeUTC == timeUTC_end,
                              "warning, start/end time equals",
                              "all good")]

  dt$year_end <- data.table::year(dt$timeUTC_end)
  dt$month_end <- data.table::month(dt$timeUTC_end)
  dt$day_end <- as.numeric(strftime(dt$timeUTC_end, "%d"))
  dt$hour_end <- data.table::hour(dt$timeUTC_end)
  dt$minute_end <- data.table::minute(dt$timeUTC_end)
  dt$second_end <- data.table::second(dt$timeUTC_end)


  # new_second <- NULL
  #   dt[, new_second := cut(timeUTC, breaks = freq)]
  #
  # # dt$new_second <- (( second(dt$timeUTC) + 5/2) %/% 10)*10
  # dt$new_second <- ifelse(dt$new_second == 60,
  # 0,
  # dt$new_second)

  return(dt)
}
