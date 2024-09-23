#' Helpers Legacy
#'
#'
#' @title obs_id2pos
#' @family helpers legacy
#' @name obs_id2pos
#' @description return ndata.frame based on footprint/receptor id
#' @param id string
#' @param sep string
#' @param asdf Logical, to return as data.frame or not
#' @return data.frame with time and location based on input
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' id <- '2002x08x03x10x45.00Nx090.00Ex00030'
#' (obs_id2pos(id, asdf = TRUE) -> dx)
#' id <- c('2002x08x03x10x00x45.000Nx090.000Ex00030',
#'         '2002x08x03x10x55x45.335Sx179.884Wx00030')
#' (obs_id2pos(id) -> dx)
#' (obs_id2pos(id, asdf = TRUE) -> dx)
#' (obs_id2pos(rep(id, 2)) -> dx)
#' (obs_id2pos(rep(id, 2), asdf = TRUE) -> dx)
#' }
#' }
obs_id2pos <- function(id, sep = "x", asdf = FALSE) {
  # revers of id2pos function to read identifying label for
  # multiple receptors (location&time) returns time as fractional
  # julian day since 1/1/1960 and alt as altitude above ground in
  # meters returns a 2d array for multiple receptors, a vector for
  # a single receptor examples:
  # id2pos('2002x08x03x10x45.00Nx090.00Ex00030') time lat lon alt
  # yr mon day hr min 15555.42 45.00 90.00 30.00 2002 08 03 10 00
  # id2pos(c('2002x08x03x10x00x45.000Nx090.000Ex00030','2002x08x03x10x55x45.335Sx179.884Wx00030'))
  # time lat lon alt yr mon day hr min 15555.42 45.000 90.000 30
  # 2002 08 03 10 00 15555.45 -45.335 -179.884 30 2002 08 03 10 55
  # $Id: id2pos.r,v 1.6 2012/09/24 19:34:37 mellis Exp $
  #---------------------------------------------------------------------------------------------------

  # get format using separator location (assumes same format is
  # used for all elements of id)
  cpos <- as.numeric(gregexpr("x", id[1], fixed = TRUE)[[1]])
  # Detect whether the date contains a minute string Note: this
  # will fail if the date does not include it, but there are
  # extraneous characters after the agl string that contain the
  # separator string

  encode.minutes <- length(cpos) >
    6
  yr4 <- as.numeric(substring(id, 1, cpos[1] - 1))
  mon <- as.numeric(substring(id, cpos[1] + 1, cpos[2] - 1))
  day <- as.numeric(substring(id, cpos[2] + 1, cpos[3] - 1))
  hr <- as.numeric(substring(id, cpos[3] + 1, cpos[4] - 1))
  ipos <- 4
  if (encode.minutes) {
    min <- as.numeric(substring(id, cpos[4] + 1, cpos[5] - 1))
    frac.hr <- hr + min/60
    ipos <- ipos + 1
  } else {
    frac.hr <- hr
    min <- 0
  }
  lat <- as.numeric(substring(id, cpos[ipos] + 1, cpos[ipos + 1] - 2))
  lat <- ifelse(
    substring(id, cpos[ipos + 1] - 1, cpos[ipos + 1] - 1) ==
      "S", -lat, lat
  )
  ipos <- ipos + 1
  lon <- as.numeric(substring(id, cpos[ipos] + 1, cpos[ipos + 1] - 2))
  lon <- ifelse(
    substring(id, cpos[ipos + 1] - 1, cpos[ipos + 1] - 1) ==
      "W", -lon, lon
  )
  ipos <- ipos + 1
  # strip any alpha characters (usually trailing) from agl string:
  alt <- as.numeric(sub("[a-z]+", "", substring(id, cpos[ipos] + 1)))

  time <- obs_julian(mon, day, yr4) +
    frac.hr/24

  pos <- cbind(
    time = time, lat = lat, lon = lon, alt = alt, yr = yr4, mon = mon,
    day = day, hr = hr, min = min
  )


  nx <- dimnames(pos)[[2]]


  xx <- pos[1:dim(pos)[1],
                ,
            drop = TRUE]

  if(asdf) {

    m <- matrix(xx, nrow = length(xx)/length(nx))
    df <- data.frame(m)
    names(df) <- nx
    return(df)

   } else {
    return(xx)
  }
}

#' Helpers Legacy
#'
#'
#' @title obs_julian
#' @family helpers legacy
#' @name obs_julian
#' @description return numeric
#' @param m numeric
#' @param d numeric
#' @param y numeric
#' @param origin. string
#' @return returns day since 1/1/1960
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_julian(1, 2020, 1)
#' }
#' }
obs_julian<-function(m, d, y, origin.){
  # returns day since 1/1/1960
  #
  #  $Id: julian.r,v 1.2 2007/06/27 11:54:03 skoerner Exp $
  #---------------------------------------------------------------------------------------------------

  only.origin <- all(missing(m), missing(d), missing(y))
  if(only.origin)
    m <- d <- y <- NULL
  # return days since origin
  nonnumeric.p <- !c(is.numeric(m), is.numeric(d), is.numeric(y))
  if(any(nonnumeric.p) && !only.origin) {
    badarg <- paste(c("m", "d", "y")[nonnumeric.p], collapse = ", "
    )
    stop(paste("Arguments:", badarg, "are not numeric"))
  }
  if(missing(origin.))
    if(is.null(origin. <- .Options$chron.origin))
      origin. <- c(month = 1, day = 1, year = 1960)
  nms <- names(d)
  max.len <- max(length(m), length(d), length(y))
  #
  # prepend new origin value and rep out to common max. length:
  m <- c(origin.[1], rep(m, length = max.len))
  d <- c(origin.[2], rep(d, length = max.len))
  y <- c(origin.[3], rep(y, length = max.len))
  #
  # code from julian date in the S book (p.269)
  #
  y <- y + ifelse(m > 2, 0, -1)
  m <- m + ifelse(m > 2, -3, 9)
  c <- y %/% 100
  ya <- y - 100 * c
  out <- (146097 * c) %/% 4 + (1461 * ya) %/% 4 + (153 * m + 2) %/% 5 +
    d + 1721119
  #
  # now subtract the new origin from all dates
  #
  if(!only.origin) {
    if(all(origin. == 0))
      out <- out[-1]
    else out <- out[-1] - out[1]
  }
  names(out) <- nms
  out
}
