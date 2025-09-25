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

  encode.minutes <- length(cpos) > 6
  yr4 <- as.numeric(substring(id, 1, cpos[1] - 1))
  mon <- as.numeric(substring(id, cpos[1] + 1, cpos[2] - 1))
  day <- as.numeric(substring(id, cpos[2] + 1, cpos[3] - 1))
  hr <- as.numeric(substring(id, cpos[3] + 1, cpos[4] - 1))
  ipos <- 4
  if (encode.minutes) {
    min <- as.numeric(substring(id, cpos[4] + 1, cpos[5] - 1))
    frac.hr <- hr + min / 60
    ipos <- ipos + 1
  } else {
    frac.hr <- hr
    min <- 0
  }
  lat <- as.numeric(substring(id, cpos[ipos] + 1, cpos[ipos + 1] - 2))
  lat <- ifelse(
    substring(id, cpos[ipos + 1] - 1, cpos[ipos + 1] - 1) == "S",
    -lat,
    lat
  )
  ipos <- ipos + 1
  lon <- as.numeric(substring(id, cpos[ipos] + 1, cpos[ipos + 1] - 2))
  lon <- ifelse(
    substring(id, cpos[ipos + 1] - 1, cpos[ipos + 1] - 1) == "W",
    -lon,
    lon
  )
  ipos <- ipos + 1
  # strip any alpha characters (usually trailing) from agl string:
  alt <- as.numeric(sub("[a-z]+", "", substring(id, cpos[ipos] + 1)))

  time <- obs_julian(mon, day, yr4) +
    frac.hr / 24

  pos <- cbind(
    time = time,
    lat = lat,
    lon = lon,
    alt = alt,
    yr = yr4,
    mon = mon,
    day = day,
    hr = hr,
    min = min
  )

  nx <- dimnames(pos)[[2]]

  xx <- pos[1:dim(pos)[1], , drop = TRUE]

  if (asdf) {
    m <- matrix(xx, nrow = length(xx) / length(nx))
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
obs_julian <- function(m, d, y, origin.) {
  # returns day since 1/1/1960
  #
  #  $Id: julian.r,v 1.2 2007/06/27 11:54:03 skoerner Exp $
  #---------------------------------------------------------------------------------------------------

  only.origin <- all(missing(m), missing(d), missing(y))
  if (only.origin) {
    m <- d <- y <- NULL
  }
  # return days since origin
  nonnumeric.p <- !c(is.numeric(m), is.numeric(d), is.numeric(y))
  if (any(nonnumeric.p) && !only.origin) {
    badarg <- paste(c("m", "d", "y")[nonnumeric.p], collapse = ", ")
    stop(paste("Arguments:", badarg, "are not numeric"))
  }
  if (missing(origin.)) {
    if (is.null(origin. <- .Options$chron.origin)) {
      origin. <- c(month = 1, day = 1, year = 1960)
    }
  }
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
  out <- (146097 * c) %/%
    4 +
    (1461 * ya) %/% 4 +
    (153 * m + 2) %/% 5 +
    d +
    1721119
  #
  # now subtract the new origin from all dates
  #
  if (!only.origin) {
    if (all(origin. == 0)) {
      out <- out[-1]
    } else {
      out <- out[-1] - out[1]
    }
  }
  names(out) <- nms
  out
}


#' @title obs_info2id
#' @family helpers legacy
#' @name obs_info2id
#' @description return footprint/receptor id
#' @param yr  year
#' @param mo  month
#' @param dy day
#' @param hr hour
#' @param mn minute
#' @param lat latitude
#' @param lon longitude
#' @param alt altitude above ground level
#' @param sep character, default"x"
#' @param long Logical, to add minute, and rounded with 2 decimals, instead of 4. default TRUE
#' @return a string with the receptor id
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_info2id(yr = 2002,
#'             mo = 8,
#'             dy = 3,
#'             hr = 10,
#'             mn = 0,
#'             lat = 42,
#'             lon = -90,
#'             alt = 1) [1]
#' }
#' }
obs_info2id <- function(
  yr,
  mo,
  dy,
  hr,
  mn = 0,
  lat,
  lon,
  alt,
  sep = "x",
  long = T
) {
  # function to create identifying label for single receptor
  # (location&time) expects alt as altitude above ground in meters
  # example: info2id(2002,8,3,10,45,-90,0.03) [1]
  # '2002x08x03x10x+45.00x+090.00x00030' Caution: No rounding to
  # nearest hour when long=F Cation: Arguments must be specified
  # when long=F (cannot rely on order when mn is absent).
  #---------------------------------------------------------------------------------------------------

  # 10/21/2024, Sergio
  # just to check missing arguments
  if (missing(yr)) {
    stop("Add year")
  }
  if (missing(mo)) {
    stop("Add month")
  }
  if (missing(dy)) {
    stop("Add day")
  }
  if (missing(hr)) {
    stop("Add hour")
  }
  if (missing(lat)) {
    stop("Add lat")
  }
  if (missing(lon)) {
    stop("Add lon")
  }
  if (missing(alt)) {
    stop("Add alt")
  }

  # need leading zeros
  hr <- substring(100 + hr, 2) #2 digit hr
  yr4 <- substring(10000 + yr, 2) #4 digit year
  mon <- substring(100 + mo, 2) #2 digit mon
  day <- substring(100 + dy, 2) #2 digit day
  mn <- substring(100 + mn, 2) #2 digit day

  # location
  if (!long) {
    lat <- round(lat, 2) #1 km roundoff
    lon <- round(lon, 2) #1 km roundoff
  } else {
    lat <- round(lat, 4)
    lon <- round(lon, 4)
  }

  alt <- round(alt) #1 m roundoff
  # no below ground...
  alt[alt < 0] <- 0
  # need leading zeros and sign
  lats <- rep("S", length(lat))
  lons <- rep("W", length(lat))
  lats[lat >= 0] <- "N"
  lons[lon >= 0] <- "E"
  if (!long) {
    lat <- paste(substring(100.001 + abs(lat), 2, 6), lats, sep = "") #2 dig. before decimal + sign
    lon <- paste(substring(1000.001 + abs(lon), 2, 7), lons, sep = "") #3 dig. before decimal + sign
  } else {
    lat <- paste(substring(100.00001 + abs(lat), 2, 8), lats, sep = "") #2 dig. before decimal + sign
    lon <- paste(substring(1000.00001 + abs(lon), 2, 9), lons, sep = "") #3 dig. before decimal + sign
  }
  alt <- substring(1e+05 + alt, 2)
  out <- paste(yr4, mon, day, hr, lat, lon, alt, sep = sep) #put everything together
  if (long) {
    out <- paste(yr4, mon, day, hr, mn, lat, lon, alt, sep = sep)
  }
  return(out)
}


#' @title obs_julian
#' @family helpers legacy
#' @name obs_julian
#' @description return footprint/receptor id
#' @param y  year
#' @param m  month
#' @param d day
#' @param legacy logical, to use legacy code
#' @param verbose logical, to show more messages
#' @return a string with the receptor id
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_julian(y = 2002,
#'             m = 8,
#'             d = 3,
#'             legacy = TRUE)
#' obs_julian(y = 2002,
#'             m = 8,
#'             d = 3,
#'             legacy = FALSE)
#' }
#' }
obs_julian <- function(y, m, d, origin., legacy = FALSE, verbose = TRUE) {
  if (legacy) {
    if (verbose) {
      cat("Using legacy code\n")
    }
    #returns day since 1/1/1960
    #
    #  $Id: julian.r,v 1.2 2007/06/27 11:54:03 skoerner Exp $
    #---------------------------------------------------------------------------------------------------

    only.origin <- all(missing(m), missing(d), missing(y))
    if (only.origin) {
      m <- d <- y <- NULL
    }
    # return days since origin
    nonnumeric.p <- !c(is.numeric(m), is.numeric(d), is.numeric(y))
    if (any(nonnumeric.p) && !only.origin) {
      badarg <- paste(c("m", "d", "y")[nonnumeric.p], collapse = ", ")
      stop(paste("Arguments:", badarg, "are not numeric"))
    }
    if (missing(origin.)) {
      if (is.null(origin. <- .Options$chron.origin)) {
        origin. <- c(month = 1, day = 1, year = 1960)
      }
    }
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
    out <- (146097 * c) %/%
      4 +
      (1461 * ya) %/% 4 +
      (153 * m + 2) %/% 5 +
      d +
      1721119
    #
    # now subtract the new origin from all dates
    #
    if (!only.origin) {
      if (all(origin. == 0)) {
        out <- out[-1]
      } else {
        out <- out[-1] - out[1]
      }
    }
    names(out) <- nms
    return(out)
  } else {
    day <- ISOdate(
      year = y,
      month = m,
      day = d,
      hour = 0,
      min = 0,
      sec = 0,
      tz = "UTC"
    )

    y1960 <- ISOdate(
      year = 1960,
      month = 1,
      day = 1,
      hour = 0,
      min = 0,
      sec = 0,
      tz = "UTC"
    )
    out <- day - y1960
    return(out)
  }
}


#' @title obs_load_ncdf
#' @family helpers legacy
#' @name obs_load_ncdf
#' @description NetCDF list of var and attributes
#' @param ncname pathname of NetCDF4 file
#' @param vars specify variables to retrieve otherwise all are returned
#' (NOTE: '/' indicates only return root group)
#' @param grps groups
#' @param dims return variable dimensions TRUE/FALSE
#' @param useoldfoot  old footprints have dim order: lat,lon,time but new footprints
#' (CF-1.6 convention and on) have dim order: lon,lat,time; this option flips
#' lat/lon dim order of returned footprints for backwards compatibility;
#' TRUE => flips new footprints to old convention;
#' FALSE => flips old footprints to new convention
#' @param attribs array of names of global file attributes to return (NOTE: this is for
#' backwards compatibility - current version automatically returns all
#' global file attributes)
#' @param var.attribs array of names of variable attributes to return (NOTE: this is for
#' backwards compatibility - current version automatically returns all
#' attributes for variables and dimensions)
#' @param gz netCDF file is gzipped. Note that .nc may be already compressed.
#' @param tmpdir temporary directory to unzip netCDF file
#' @param tmpfile temporary unzipped netCDF file name
#' @param clean remove temporary file after processing
#' @param lowercase return all list names in lowercase
#' @param gsubdot substitute '_' with '.' in all list variable names
#' @param verbose logical, to show more messages
#' @importFrom ncdf4 nc_open ncatt_get
#' @return list of NetCDF vars and attributes
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_load_ncdf <- function(
  ncname,
  vars = NULL,
  grps = NULL,
  dims = TRUE,
  useoldfoot = FALSE,
  attribs = 'Conventions',
  var.attribs = NULL,
  gz = FALSE,
  tmpdir = tempdir(),
  tmpfile = tempfile('load.ncdf4', tmpdir = tmpdir),
  clean = TRUE,
  lowercase = FALSE,
  gsubdot = FALSE,
  verbose = FALSE
) {
  if (verbose) {
    cat(sprintf("obs_load_ncdf Called with ncname = %s,\n", ncname))
    cat(sprintf("obs_load_ncdf and lowercase = %s,\n", lowercase))
    cat(sprintf("obs_load_ncdf and dims = %s,\n", dims))
    if (!is.null(attribs)) {
      cat(sprintf("obs_load_ncdf and attribs = %s,\n", attribs))
    }
    if (!is.null(vars)) {
      cat(sprintf("obs_load_ncdf and vars = %s.\n", vars))
    }
    if (!is.null(var.attribs)) {
      cat(sprintf("obs_load_ncdf and var.attribs = %s.\n", var.attribs))
    }
  }

  if (gz) {
    # gz not needed since nc may be already compressed
    system(paste("gunzip -c ", ncname, " > ", tmpfile, sep = ""))
    ncname <- tmpfile
    system(paste("chmod a+w ", ncname, sep = ""))
  }

  retval <- list() # this is the final objhect to return

  nc <- ncdf4::nc_open(ncname)

  # retrieve groups and/or variable names
  origvarpaths <- names(nc$var)

  # retrieve the group names & modify varpath for root variables (when there is no group)
  igrp <- 1
  grpnames <- character(0) # note: length(character(0)) = 0, length("") = 1
  varpaths <- character(0)
  root.only <- TRUE

  for (varpath in origvarpaths) {
    varstruct <- unlist(strsplit(varpath, "/"))

    if (length(varstruct) == 1) {
      # if only the variable was found then...
      grp <- "/" # ... add a character to identify the root group
      var <- sprintf("/%s", varpath)
    } else {
      grp <- varstruct[1]
      var <- varpath
      root.only <- FALSE
    }

    # skip group if grps specified and grp not requested
    if (!is.null(grps)) {
      if (!grp %in% grps) {
        next
      }
    }
    grpnames[igrp] <- grp
    varpaths[igrp] <- var
    igrp <- igrp + 1
  } # end for varpath

  if (dims) {
    #find dimension names
    dimpaths <- names(nc$dim)
    for (idim in 1:length(dimpaths)) {
      dimstruct <- unlist(strsplit(dimpaths[idim], "/"))
      if (length(dimstruct) == 1) {
        # if only the dimension was found then...
        dimpaths[idim] <- sprintf("/%s", dimpaths[idim])
      }
    } #end for idim
  }

  # read group(s)
  grpnames <- unique(grpnames)
  igrp <- 1

  for (grpname in grpnames) {
    values <- list()

    if (verbose) {
      cat(sprintf("obs_load_ncdf Reading group '%s'\n", grpname))
    }

    varnames <- grep(sprintf("^%s", grpname), varpaths, value = TRUE)
    ivar <- 1
    allnames <- varnames
    if (dims) {
      dimennames <- grep(sprintf("^%s", grpname), dimpaths, value = TRUE)
      allnames <- c(allnames, dimennames)
      if (grpname == '/') {
        dimennames <- substring(dimennames, 2, nchar(dimennames))
      }
    } else {
      dimennames <- c()
    }

    #loop over variables and optionally dimensions
    for (varname in allnames) {
      # format variable name
      basename <- unlist(strsplit(varname, "/"))[-1]
      if (grepl("^/", varname)) {
        varname <- substring(varname, 2, nchar(varname))
      }

      if (varname %in% dimennames) {
        # retrieve dimension values
        if (verbose) {
          cat(sprintf("obs_load_ncdf Reading dimension  '%s'\n", basename))
        }
        values[[ivar]] <- nc$dim[[varname]]$vals
      } else {
        # skip variable if vars specified and basename not requested
        if (!is.null(vars)) {
          if (!basename %in% vars) {
            next
          }
        }
        # retrieve variable values
        if (verbose) {
          cat(sprintf("obs_load_ncdf Reading variable '%s'\n", basename))
        }
        values[[ivar]] <- ncdf4::ncvar_get(nc, varname)
      }
      names(values)[ivar] <- basename

      # retrieve variable/dimension attributes
      attnames <- ncdf4::ncatt_get(nc, varname)

      # the following attribute names are reserved in R:
      #   ‘class’, ‘comment’, ‘dim’, ‘dimnames’, ‘names’, ‘row.names’, ‘tsp’
      if (length(attnames) > 0) {
        for (iii in 1:length(attnames)) {
          attributes(values[[ivar]])[[names(attnames)[iii]]] <- attnames[[iii]]

          # convert date/time information to POSIX time
          time.checked <- obs_interpret_udunits_time(
            vals = values[[ivar]],
            unitstring = attnames[[iii]]
          )
          if (time.checked$is.time) {
            values[[ivar]] <- time.checked$vals
            attributes(values[[iii]])$tzone <- time.checked$tz
          }
        } #end for iii
      }

      #include to keep backwards compatible
      if (gsubdot) {
        names(values)[ivar] <- gsub('_', '.', names(values)[ivar])
      }
      ivar <- ivar + 1
    } #end for varname

    if (length(values) == 0) {
      next
    }

    # concatenate variables to the user's list
    if (grpname == "/") {
      retval <- values
    } else {
      retval[[igrp]] <- values
      names(retval)[igrp] <- grpname
    }
    igrp <- igrp + 1
  } #end for grpname

  #global file attributes: return entire list
  global.att.list <- ncdf4::ncatt_get(nc, varid = 0)
  retval$global.att.list <- global.att.list
  # For backwards compatibility: also return individual specified attributes
  for (att in attribs) {
    target.name <- att
    if (gsubdot) {
      target.name <- gsub('_', '.', target.name)
    }
    # Some attribute names are reserved and we should
    # not overwrite them with values from the netCDF
    # file.  An example is "names", which occurs in
    # column3d output.  Tack on a "2" to such names.
    if (target.name %in% names(attributes(retval))) {
      target.name <- paste(target.name, "2", sep = '')
    }
    if (is.null(global.att.list[[att]])) {
      sprintf(
        "obs_load_ncdf Cannot find attribute \"%s\" in file \"%s\".",
        att,
        nc$filename
      )
    } else {
      attributes(retval)[[target.name]] <- global.att.list[[att]]
    }
  } #end for att

  #check if dimensions of any footprint need to be flipped
  if (root.only) {
    #no groups (only root group)
    footnames <- grep('^foot[[:alnum:]]*[0-9]$', names(retval), value = TRUE)
    footcount <- length(footnames)
  } else {
    #footprints in groups
    footnames <- list()
    footcount <- 0
    for (name in names(retval)) {
      footnames[[name]] <- grep(
        '^foot[[:alnum:]]*[0-9]$',
        names(retval[[name]]),
        value = TRUE
      )
      footcount <- footcount + length(footnames[[name]])
    }
  }
  footflip <- FALSE

  if (
    is.null(attributes(retval)[['Conventions']]) &
      !useoldfoot &
      footcount > 0
  ) {
    print(
      'Flipping old footprint(s) dimensions to match new convention: lon,lat,date'
    )
    footflip <- TRUE
  } else if (
    !is.null(attributes(retval)[['Conventions']]) &
      useoldfoot &
      footcount > 0
  ) {
    print(
      'Flipping new footprint(s) dimensions to match old convention: lat,lon,date'
    )
    footflip <- TRUE
  }

  if (footflip) {
    if (root.only) {
      #no groups (only root group)
      for (footname in footnames) {
        retval[[footname]] <- aperm(retval[[footname]], c(2, 1, 3))
      }
    } else {
      #footprints in groups
      for (name in names(retval)) {
        for (footname in footnames[[name]]) {
          retval[[name]][[footname]] <- aperm(
            retval[[name]][[footname]],
            c(2, 1, 3)
          )
        }
      }
    }
  }

  # for backwards compatibility retrieve attributes for specified variables
  for (var in var.attribs) {
    att.val <- ncatt_get(nc, var) #get all attributes for variable var
    retval[[paste('attrib.', var, sep = '')]] <- att.val
  }

  nc <- nc_close(nc)

  if (gz && clean) {
    system(paste("rm -f", ncname))
  }
  if (lowercase) {
    names(retval) <- tolower(names(retval))
    if (!root.only) {
      for (name in names(retval)) {
        names(retval[[name]]) <- tolower(names(retval[[name]]))
      }
    }
  }
  return(retval)
} # end


#' @title obs_interpret_udunits_time
#' @family helpers legacy
#' @name obs_interpret_udunits_time
#' @description return list of time vars
#' @param vals  time array
#' @param unitstring  month
#' @param tz  time zone, "UTC" default
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_interpret_udunits_time <- function(vals, unitstring, tz = "UTC") {
  retval <- list()
  retval$is.time <- FALSE

  if (length(grep("^DAYS since", unitstring, ignore.case = TRUE)) == 1) {
    retval$is.time <- TRUE
    # length of string is a bad parsing heuristic, but works
    # for everything so far.
    #     21 chars if "days since 1900-01-01"
    #     19 chars if "days since 1900-1-1"
    retval$vals <- as.POSIXct(
      substr(unitstring, 11, nchar(unitstring)),
      tz = tz
    ) +
      vals * 86400
    retval$tz <- tz # UTC tzone is a presumption
  }
  if (length(grep("^SECONDS since", unitstring, ignore.case = TRUE)) == 1) {
    retval$is.time <- TRUE
    retval$vals <- as.POSIXct(
      substr(unitstring, 15, nchar(unitstring)),
      tz = tz
    ) +
      vals
    retval$tz <- tz # UTC tzone is a presumption
  }
  if (length(grep("^HOURS since", unitstring, ignore.case = TRUE)) == 1) {
    retval$is.time <- TRUE
    retval$vals <- as.POSIXct(
      substr(unitstring, 12, nchar(unitstring)),
      tz = tz
    ) +
      vals * 3600
    retval$tz <- tz # UTC tzone is a presumption
  }
  if (length(grep("^decimal date", unitstring, ignore.case = TRUE)) == 1) {
    retval$is.time <- TRUE
    retval$vals <- obs_decimal_to_POSIX(vals)
    retval$tz <- tz # UTC tzone is a presumption
  }
  return(retval)
}


#' @title obs_decimal_to_POSIX
#' @family helpers legacy
#' @name obs_decimal_to_POSIX
#' @description return list of time vars
#' @param decimal.dates  vecot with format YYYY.yyyy
#' @param tz  time zone, "UTC" default
#' @param legacy logical, to use legacy code
#' @importFrom lubridate decimal_date
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_decimal_to_POSIX <- function(decimal.dates, tz = "UTC", legacy = TRUE) {
  # converts a vector of YYYY.yyyy decimal dates
  # such as those used by Arlyn into POSIXt dates.

  retval <- NULL

  if (legacy) {
    for (dd in decimal.dates) {
      year <- trunc(dd)
      y0 <- ISOdatetime(year, 1, 1, 0, 0, 0, tz = "UTC")
      y1 <- as.numeric(ISOdatetime(year + 1, 1, 1, 0, 0, 0, tz = "UTC"))
      sec.in.year <- y1 - as.numeric(y0)
      pd <- as.POSIXct(y0 + (dd - year) * sec.in.year, tz = tz)
      if (is.null(retval)) {
        retval <- pd
      } else {
        retval <- c(retval, pd)
      }
    }
    attributes(retval)$tzone <- tz

    return(retval)
  } else {
    return(lubridate::decimal_date(decimal.dates, tz = tz))
  }
}


#' @title obs_idate_to_POSIX
#' @family helpers legacy time
#' @name obs_idate_to_POSIX
#' @description return list of time vars
#' @param idate  vector of integers representing ONE date or date time
#' @param tz  time zone, "UTC" default
#' @importFrom lubridate decimal_date
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_idate_to_POSIX(c(2000, 1, 3))
#' }}
obs_idate_to_POSIX <- function(idate, tz = "UTC") {
  # takes a vector of integers representing ONE date
  # cannot now handle multiple dates
  # year month day required (00:00:00 assumed)
  # if idate has 6 elements, will use 4,5,6 as HH::MM::SS UTC
  if (length(idate) == 3) {
    return(ISOdatetime(idate[1], idate[2], idate[3], 0, 0, 0, tz = tz))
  }
  if (length(idate) == 6) {
    return(ISOdatetime(
      idate[1],
      idate[2],
      idate[3],
      idate[4],
      idate[5],
      idate[6],
      tz = tz
    ))
  }
  return(idate)
  stop(sprintf(
    "Was expecting 3 or 6 elements in the idate numeric vector, got %d instead.",
    length(idate)
  ))
}

#' @title obs_POSIX_to_idate
#' @family helpers legacy time
#' @name obs_POSIX_to_idate
#' @description return list of time vars
#' @param dates  vector of integers representing ONE date or date time
#' @param tz  time zone, "UTC" default
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_POSIX_to_idate(c(2000, 1, 3))
#' }}
obs_POSIX_to_idate <- function(dates, tz = "UTC") {
  lt <- as.POSIXlt(dates)

  return(data.frame(
    year = lt$year + 1900,
    month = lt$mon + 1,
    day = lt$mday,
    hour = lt$hour,
    minute = lt$min,
    second = lt$sec
  ))
}

#' @title obs_itau_to_POSIX
#' @family helpers legacy time
#' @name obs_itau_to_POSIX
#' @description return time vars
#' @param itaus  vector of integers representing ONE date or date time
#' @param year0  default 1999
#' @aliases itau2date
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_itau_to_POSIX(1)
#' }}
obs_itau_to_POSIX <- function(itaus, year0 = 1999) {
  if (is.na(year0)) {
    year0 <- 1999
  }
  return(ISOdatetime(year0, 1, 1, 0, 0, 0, tz = "UTC") + itaus)
}

#' @title obs_is_leap
#' @family helpers legacy time
#' @name obs_is_leap
#' @description return logical
#' @param year  integer year
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_is_leap(1)
#' }}
obs_is_leap <- function(year) {
  return(
    year %% 4 == 0 &
      (year %% 100 != 0 | year %% 400 == 0)
  )
}


#' @title obs_days_in_year
#' @family helpers legacy time
#' @name obs_days_in_year
#' @description return days
#' @param years  integer year
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_days_in_year(1)
#' }}
obs_days_in_year <- function(years) {
  retval <- numeric(0)
  for (yr in years) {
    if (obs_is_leap(yr)) {
      retval <- c(retval, 366)
    } else {
      retval <- c(retval, 365)
    }
  }
  return(retval)
}


#' @title obs_days_in_month
#' @family helpers legacy time
#' @name obs_days_in_month
#' @description return days
#' @param year  integer year
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_days_in_month(1)
#' }}
obs_days_in_month <- function(year) {
  retval <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (!is.null(year)) {
    if (obs_is_leap(year)) {
      retval[2] <- 29
    }
  }
  return(retval)
}

#' @title obs_seconds_in_year
#' @family helpers legacy time
#' @name obs_seconds_in_year
#' @description return seconds
#' @param year  integer year
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_seconds_in_year(1)
#' }}
obs_seconds_in_year <- function(year) {
  return(86400 * obs_days_in_year(year))
}


#' @title obs_length_POSIXlt
#' @family helpers legacy time
#' @name obs_length_POSIXlt
#' @description return formatted time
#' @param x  time
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_length_POSIXlt(1)
#' }}
obs_length_POSIXlt <- function(x) {
  return(length(x$wday))
}


#' @title obs_format_et
#' @family helpers legacy time
#' @name obs_format_et
#' @description return logical
#' @param elapsed.seconds  seconds
#' @param verbose  logical, to show more info
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_format_et(1)
#' }}
obs_format_et <- function(elapsed.seconds, verbose = FALSE) {
  tvec <- character(0)
  years <- 0
  weeks <- 0
  days <- 0
  hours <- 0
  minutes <- 0
  seconds <- 0

  # 365-day years
  while (elapsed.seconds > 86400 * 365) {
    elapsed.seconds <- elapsed.seconds - 86400 * 365
    years <- years + 1
  }

  # weeks
  while (elapsed.seconds > 86400 * 7) {
    elapsed.seconds <- elapsed.seconds - 86400 * 7
    weeks <- weeks + 1
  }

  # days
  while (elapsed.seconds > 86400) {
    elapsed.seconds <- elapsed.seconds - 86400
    days <- days + 1
  }

  # hours
  while (elapsed.seconds > 3600) {
    elapsed.seconds <- elapsed.seconds - 3600
    hours <- hours + 1
  }

  # minutes
  while (elapsed.seconds > 60) {
    elapsed.seconds <- elapsed.seconds - 60
    minutes <- minutes + 1
  }

  # seconds <- elapsed.seconds
  if (verbose) {
    if (years > 0) {
      tvec <- c(tvec, sprintf("%d year%s", years, ifelse(years > 1, "s", "")))
    }
    if (weeks > 0) {
      tvec <- c(tvec, sprintf("%d week%s", weeks, ifelse(weeks > 1, "s", "")))
    }
    if (days > 0) {
      tvec <- c(tvec, sprintf("%d day%s", days, ifelse(days > 1, "s", "")))
    }
    if (hours > 0) {
      tvec <- c(tvec, sprintf("%d hour%s", hours, ifelse(hours > 1, "s", "")))
    }
    if (minutes > 0) {
      tvec <- c(
        tvec,
        sprintf("%d minute%s", minutes, ifelse(minutes > 1, "s", ""))
      )
    }
    if (elapsed.seconds > 0) {
      tvec <- c(
        tvec,
        sprintf(
          "%.0f second%s",
          elapsed.seconds,
          ifelse(elapsed.seconds > 1, "s", "")
        )
      )
    }
  } else {
    if (years > 0) {
      tvec <- c(tvec, sprintf("%dy", years))
    }
    if (weeks > 0) {
      tvec <- c(tvec, sprintf("%dw", weeks))
    }
    if (days > 0) {
      tvec <- c(tvec, sprintf("%dd", days))
    }
    if (hours > 0) {
      tvec <- c(tvec, sprintf("%02dh", hours))
    }
    if (minutes > 0) {
      tvec <- c(tvec, sprintf("%02dm", minutes))
    }
    if (elapsed.seconds > 0) {
      tvec <- c(tvec, sprintf("%02.0fs", elapsed.seconds))
    }
  }
  return(paste(tvec, collapse = '-'))
}


#' @title obs_POSIX_to_decimal
#' @family helpers legacy time
#' @name obs_POSIX_to_decimal
#' @description return time decimal
#' @param posix.dates  dates+
#' @param tz  time zone
#' @param legacy logical, to use legacy or not
#' @importFrom lubridate decimal_date
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_POSIX_to_decimal(1)
#' }}
obs_POSIX_to_decimal <- function(posix.dates, tz = "UTC", legacy = TRUE) {
  # convert a vector of POSIXt dates into YYYY.yyyy decimal dates
  # (such as those used by Arlyn).  The .yyyy is the fractional part
  # of the year YYYY.  (Four decimal places shown for convenience;
  # internally this is kept as a standard numeric quantity, generally
  # R*8.)

  if (legacy) {
    if (!any(class(posix.dates) == "POSIXt")) {
      stop(
        "obs_POSIX_to_decimal Expecting one of class(posix.dates) == 'POSIXt'"
      )
    }

    #  retval <- numeric(0)
    #  ndates <- length(posix.dates)

    #  for (idate in 1:ndates) {
    #    year <- as.numeric(format(posix.dates[idate],format="%Y"))
    #    y0 <- as.numeric(ISOdatetime(year,1,1,0,0,0,tz=tz))
    #    y1 <- as.numeric(ISOdatetime(year+1,1,1,0,0,0,tz=tz))
    #    retval <- c(retval,year+(as.numeric(posix.dates[idate])-y0)/(y1-y0))
    #  }

    # alternative code as of 3 Aug 2009:  much faster, as there is no looping.

    #   There's a potential problem here in that I'm not sure how the R
    #   date-time classes handle a vector of times with different time
    #   zone attributes among the members.  It would appear that the
    #   attribute belonging to the vector overrides any such attribute
    #   assigned to a member.  This is probably because it is the vector
    #   object which can have attributes, not the member.  This code
    #   assumes that all members of the vector have the same time zone
    #   attribute.

    lt <- as.POSIXlt(posix.dates)
    y0 <- as.numeric(ISOdatetime(
      lt$year + 1900,
      1,
      1,
      0,
      0,
      0,
      tz = attributes(lt)$tzone
    ))
    y1 <- as.numeric(ISOdatetime(
      lt$year + 1901,
      1,
      1,
      0,
      0,
      0,
      tz = attributes(lt)$tzone
    ))
    return(lt$year + 1900 + (as.numeric(posix.dates) - y0) / (y1 - y0))
  } else {
    return(lubridate::decimal_date(posix.dates, tz = tz))
  }
}


#' @title obs_extract_dates
#' @family helpers legacy time
#' @name obs_extract_dates
#' @description return time
#' @param ncfile  string to ncfile
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_extract_dates(1)
#' }}
obs_extract_dates <- function(ncfile) {
  z <- obs_load_ncdf(ncfile, vars = c("decimal_date", "idate"))
  return(z)
}

#' @title obs_epoch_seconds_to_POSIX
#' @family helpers legacy time
#' @name obs_epoch_seconds_to_POSIX
#' @description return seconds
#' @param sec  seconds
#' @param tz  time zone
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_epoch_seconds_to_POSIX(1)
#' }}
obs_epoch_seconds_to_POSIX <- function(sec, tz = "UTC") {
  class(sec) <- c("POSIXt", "POSIXct")
  attributes(sec)$tzone <- tz
  return(sec)
}

#' @title obs_POSIX_to_epoch_seconds
#' @family helpers legacy time
#' @name obs_POSIX_to_epoch_seconds
#' @description return number
#' @param pt  time
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_POSIX_to_epoch_seconds(1)
#' }}
obs_POSIX_to_epoch_seconds <- function(pt) {
  return(as.numeric(pt))
}


#' @title obs_seq_midmon
#' @family helpers legacy time
#' @name obs_seq_midmon
#' @description return time
#' @param year0  year start
#' @param month0  month start
#' @param year1  year end
#' @param month1  month end
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_seq_midmon <- function(year0, month0, year1, month1) {
  retval <- NULL
  y <- year0
  m <- month0

  while (TRUE) {
    if (is.null(retval)) {
      retval <- obs_month_middle(y, m)
    } else {
      retval <- c(retval, obs_month_middle(y, m))
    }
    if (y == year1 & m == month1) {
      break
    }
    m <- m + 1
    if (m > 12) {
      y <- y + 1
      m <- 1
    }
  }
  attributes(retval)$tzone <- "UTC"
  return(retval)
}


#' @title obs_month_middle
#' @family helpers legacy time
#' @name obs_seq_midmon
#' @description return time
#' @param year  year
#' @param month  month (1 to 12)
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_month_middle <- function(year, month) {
  d.i.m <- obs_days_in_month(year)[month] # can have leap years
  if (
    (year == 1969) &
      (month == 12)
  ) {
    retval <- (mean(c(
      ISOdatetime(year, month, 1, 0, 0, 0, tz = "UTC"),
      obs_epoch_seconds_to_POSIX(-1)
    )))
  } else {
    retval <- (mean(c(
      ISOdatetime(year, month, 1, 0, 0, 0, tz = "UTC"),
      ISOdatetime(year, month, d.i.m, 23, 59, 59, tz = "UTC")
    )))
  }
  attributes(retval)$tzone <- "UTC"
  return(retval)
}


#' @title obs_seconds_in_month
#' @family helpers legacy time
#' @name obs_seconds_in_month
#' @description return time
#' @param year  year
#' @param month  month (1 to 12)
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_seconds_in_month <- function(year, month) {
  return(86400 * obs_days_in_month(year)[month]) # can have leap years
}


#' @title obs_julian_to_POSIX
#' @family helpers legacy time
#' @name obs_julian_to_POSIX
#' @description return time
#' @param julians  year
#' @param epoch  default 0001-01-01
#' @return return dates
#' @export
#' @note Convention is that the Julian date of the epoch is 1.0 (not 0!)
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_julian_to_POSIX <- function(
  julians,
  epoch = ISOdatetime(0001, 1, 1, 0, 0, 0, tz = "UTC")
) {
  # Convention is that the Julian date of the epoch is 1.0 (not 0!)
  retval <- epoch + 86400 * (julians - 1.0)
  attributes(retval)$tzone <- attributes(epoch)$tzone
  return(retval)
}


#' @title obs_POSIX_to_julian
#' @family helpers legacy time
#' @name obs_POSIX_to_julian
#' @description return time
#' @param times  time
#' @param epoch  default 0001-01-01
#' @return return dates
#' @export
#' @note Convention is that the Julian date of the epoch is 1.0 (not 0!)
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_POSIX_to_julian <- function(
  times,
  epoch = ISOdatetime(0001, 1, 1, 0, 0, 0, tz = "UTC")
) {
  # Convention is that the Julian date of the epoch is 1.0 (not 0!)
  return(as.numeric(difftime(times, epoch, units = "days") + 1.0))
}


#' @title obs_traj_foot
#' @family helpers legacy
#' @name obs_traj_foot
#' @description return trajectory
#' @param part  data.table with PARTICLE.DAT information
#' @param zlim (if not default 0,0): vertical interval for which particle distribution is looked at
#' @param foottimes vector of times between which footprint or influence will be integrated
#' @param dmassTF default TRUE  weighting by accumulated mass due to violation of mass conservation in met fields
#' @param lon.ll lower left corner of grid (longitude of southwest corner of southwest corner gridcell)
#' @param lat.ll lower left corner of grid (latitude of southwest corner of southwest corner gridcell)
#' @param numpix.x number of pixels in x directions in grid
#' @param numpix.y  number of pixels in y directions in grid
#' @param npar default 500, number of particles hysplit was run with (required in order to
#' account for those cases where a thinned particle table that may not contain all particle indices is used)
#' @return return footprint
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_traj_foot <- function(
  part = NULL,
  zlim = c(0, 0),
  foottimes = 1:240,
  dmassTF = TRUE,
  lon.ll = -145,
  lat.ll = 11,
  lon.res = 1 / 4,
  lat.res = 1 / 6,
  numpix.x = 376,
  numpix.y = 324,
  npar = 500
) {
  # Convert 'time' column to 'btime' (backtime in hours) if it's not already

  if ("time" %in% names(part)) {
    time <- NULL
    part[, btime := abs(time) / 60]
  }

  # --- Filter and prepare data based on zlim ---
  if (zlim[2] == 0) {
    # Surface influence: keep particles with positive foot values
    foot <- NULL
    part <- part[foot > 0]
  } else {
    # Volume influence: keep particles within the specified height range
    agl <- NULL
    part <- part[agl > zlim[1] & agl <= zlim[2]]

    # Recalculate 'foot' as residence time (in minutes from hours)
    index <- NULL
    part[, foot := c(diff(btime), 0) * 60, by = index]
    # Remove any rows with negative residence time (e.g. last observation for each particle)
    part <- part[foot > 0]
  }

  # Handle empty data.table after filtering
  if (nrow(part) == 0) {
    warning("No particles found within the specified filtering criteria.")
    return(part)
  }

  # --- Apply dmass weighting if requested ---
  if (dmassTF) {
    # 'ndmass' is assumed to be normalized already as per the original script's logic
    if (!("ndmass" %in% names(part))) {
      foot <- ndmass <- NULL
      cat("Adding ndmass\n")
      part <- obs_normalize_dmass(part)
    } else {
      part[, influence := foot * ndmass]
    }
  } else {
    part[, influence := foot]
  }

  # --- Grid the particles ---
  # Calculate grid indices (gitx, gity) for each particle
  lon <- lat <- NULL
  part[, gitx := floor(1 / lon.res * (lon - lon.ll) + 1)]
  part[, gity := floor(1 / lat.res * (lat - lat.ll) + 1)]

  # Initialize the final 3D array to store gridded footprints
  foot.arr <- array(0, dim = c(numpix.y, numpix.x, length(foottimes) - 1))

  # Loop over each time interval to populate the array
  for (i in 1:(length(foottimes) - 1)) {
    start_time <- foottimes[i]
    end_time <- foottimes[i + 1]

    # Subset the particles for the current time interval using data.table
    subpart <- part[
      btime > start_time &
        btime <= end_time
    ]

    if (nrow(subpart) > 0) {
      # Aggregate influence for each grid cell
      aggregated_data <- subpart[,
        .(
          total_influence = sum(influence, na.rm = TRUE)
        ),
        by = .(gity, gitx)
      ]

      # Populate the 2D slice of the footprint array for this time interval
      aggregated_data[, {
        foot.arr[gity, gitx, i] <<- total_influence / npar
      }]
    }
  }

  # Return the final populated array
  return(foot.arr)
}


#' @title obs_normalize_dmass
#' @family helpers legacy
#' @name obs_normalize_dmass
#' @description add columns of
#' @param part  particle data.table
#' @return return footprint
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_normalize_dmass <- function(
  part
) {
  part <- data.table::as.data.table(part)

  # check if btime is not presnet and add it
  if (!"btime" %in% names(part)) {
    cat("Adding btime\n")
    time <- NULL
    part[, btime := abs(time) / 60]
  }

  # must be in this order (might not be...)
  # this is from lower time to bigger time by part index
  # index from first to last repeating btime
  data.table::setorderv(part, c("btime", "index")) #data.table

  # identify particles with too strong dmass violation
  part[
    dmass < 1 / 1e3 |
      dmass > 1e3,
    unique(index)
  ] -> ind

  if (length(ind) >= length(part[, unique(index)]) / 2) {
    stop(cat(
      "More than 50% of particles have mass defect\n"
    ))
  }

  # for those particles with strong dmass violation set dmass entries to NA
  part[
    dmass < 1 / 1e3 |
      dmass > 1e3,
    dmass := NA
  ]

  part[,
    mean_dmass := mean(dmass, na.rm = T),
    by = .(btime)
  ]

  # if mean_dmass is 0

  part[mean_dmass == 0, mean_dmass := 0.0001] # legacy criteria

  part[is.na(dmass), dmass := mean_dmass]

  # normalize
  part[, ndmass := dmass / mean_dmass]
  return(part)
}
