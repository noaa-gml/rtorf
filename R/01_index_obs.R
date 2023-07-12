#' Summary of the ObsPack files
#'
#' This function returns a data.frame index of all files
#' available in obspack.
#'
#' @param obs Path to the Obspack GLOBALview txt data
#' @param categories character; default are c("aircraft-pfp", "aircraft-insitu",
#' "surface-insitu", "tower-insitu", "aircore", "surface-pfp", "shipboard-insitu",
#' "flask"). The ideia is that,as the file names include these words,
#' this function identifies which files has these words and add them as columns.
#' @param lnchar Integer; last nchar, default = 11.
#' @param out Path to the Obspack index output
#' @param verbose Logical to show more information
#' @return A data.frame with with an index of the obspack Globalview.
#' @rdname obs_summary
#' @importFrom data.table fwrite ".N" ":=" setDT data.table
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "robspack")
#' index <- obs_summary(obs)
#' }
#' }
obs_index <- function(obs,
                      categories = c("aircraft-pfp",
                                     "aircraft-insitu",
                                     "surface-insitu",
                                     "tower-insitu",
                                     "aircore",
                                     "surface-pfp",
                                     "shipboard-insitu",
                                     "flask"),
                      lnchar = 11,
                      out = paste0(tempfile(), "_index.csv"),
                      verbose = TRUE){
  cat("Please, use `obs_summary` instead")


  x <- list.files(obs,
                  full.names = T)

  na <- list.files(obs,
                   full.names = F)

  # create index ####
  # the idea
  index <- data.table::data.table(id = x, name = na)
  index$n <- 1:nrow(index)
  sector <- NULL
  for(i in seq_along(categories)) {
    index[grepl(pattern = categories[i], x = index$na),
          sector := categories[i]]
  }


  cat(paste0("Number of files of index: ", nrow(index), "\n"))
  xx <- index[, .N, by = sector]
  dx <- data.table::data.table(sector = c("Total sectors"),
                               N = sum(xx$N))
  if(verbose) print(rbind(xx, dx))


  # function to extract last n characters
  sr <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  # add the last 11 characters of each name file'
  agl <- NULL
  id <- NULL
  index[grepl(pattern = "magl", x = x),
        agl := sr(id, 11)]

  # This line replaces removes the characters magl.txt
  # for instance, remove "magl.txt" from -11magl.txt
  index$agl <- gsub("magl.txt", "", index$agl)

  # check
  index
  # Now we transform the column
  # then get the absolute number and now we have magl
  index$agl <- suppressWarnings(abs(as.numeric(index$agl)))

  if(!missing(out)) {
    data.table::fwrite(index, out)

    if(verbose) cat("index in: ", out, "\n")

  }

  if(verbose) {
    yai <- !is.na(index$agl)
    nai <- is.na(index$agl)
    cat(paste0("Detected ", sum(yai, na.rm = T), " files with agl\n"))
    cat(paste0("Detected ", sum(nai, na.rm = T), " files without agl\n"))
  }

  return(index)
}
