#example Rscript rscripts/aircraft_year.R 2020 obs/obs_aircraft-insitu.csv

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args) != 2) {
  stop("At least one argument must be supplied (year).n and obs_index", call.=FALSE)
} else {
  yy <-  as.numeric(args[1] )
  print(yy)

  ind  <- as.character(args[2])
  print(ind)

}

out <- paste0("master/master_aircraft_insitu_", yy)

message("All missing values are filled with 999999")

cat("                 _     robspackfilter              \n")
cat("               (`  ).                  _           \n")
cat("              (     ).             .:(`  )`.       \n")
cat(")           _(       '`.          :(   .    )      \n")
cat("        .=(`(      .   )     .--  `.  (    ) )      \n")
cat("       ((    (..__.:'-'   .+(   )   ` _`  ) )                 \n")
cat("`.     `(       ) )       (   .  )     (   )  ._   \n")
cat("  )      ` __.:'   )     (   (   ))     `-'.-(`  ) \n")
cat(")  )  ( )       --'       `- __.'         :(      )) \n")
cat(".-'  (_.'          .')                    `(    )  ))\n")
cat("                  (_  )                     ` __.:'          \n")
cat("\n")
cat("--..,___.--,--'`,---..-.--+--.,,-,,..._.--..-._.-a:f--.\n")

library(data.table)
library(robspack)

# 0 filters ####
north <- 80
south <- 10
west <- -170
east <- -50
max_altitude <- 8000

# evening <- 14 # for aircraft keep all the local hours

# 1 Index ####
#cat("Running obs_index(obs)\n")
#index <- obs_index(obs = obs)

# 2 Read ####
#cat("Running obs_read(index = index,
#               categories = 'aircraft-insitu',
#               verbose = T)\n")
#df <- obs_read(index = index,
#               categories = "aircraft-insitu",
#               verbose = T)
df  <- fread(ind)

# 3 ECO 2020 ####
(cat("Reading ECO 2020\n"))

if(Sys.info()[["sysname"]] == "Windows") {
  eco <- fread("../../ECO.csv.gz")
} else {
  eco <- fread("ECO.csv.gz")
}
eco$day_end <- as.numeric(eco$day_end)
eco$type_altitude <- as.numeric(eco$type_altitude)
eco$dif_time <- eco$timeUTC_end - eco$timeUTC_start

# 4 merging data ####
cat("Running obs_rbind(df, eco)\n")
df <- obs_rbind(df, eco)

# 5 adding time columns ####
cat("Running obs_addtime(df)\n")
df <- obs_addtime(df)

# 6 filtering space and time
cat("Filterinig\n")
df <- rbind(df[year == yy - 1 & month == 12],
            df[year == yy],
            df[year == yy + 1 & month == 1])

df <- df[altitude_final < max_altitude &
           latitude < north &
           latitude > south &
           longitude < east &
           longitude > west]

# 7 adding time columns ####
cat("Running obs_addtime(df)\n")
df2 <- obs_addtime(df)
df2$sec2 <- obs_freq(x = df2$second,
                     freq = seq(0, 59, 20))
df2$timeUTC_old <- df2$timeUTC
df2$timeUTC <- ISOdatetime(year = df2$year,
                           month = df2$month,
                           day = df2$day,
                           hour = df2$hour,
                           min = df2$minute,
                           sec = df2$sec2,
                           tz = "UTC")

df2[, c("timeUTC", "timeUTC_old")]

# 8 aggregating values every 20 seconds
cat("Calculating averagaes every 20 seconds\n")
df3 <- obs_agg(df2)

# 9 add local time ####
df3 <- obs_addltime(df3)
setorderv(df3, cols = c("site_code", "timeUTC"),
          order = c(-1, 1))

# 10 save txt in master ####
cat(paste0("Saving txt in master/ \n"))
master <- df3
#master[is.na(master)] <- 999999

master$timeUTC <- as.character(master$timeUTC)
master$local_time <- as.character(master$local_time)
master$latitude <- round(master$latitude, 4)
master$longitude <- round(master$longitude, 4)

fwrite(master,
       paste0(out, ".txt"),
       sep = " ")

fwrite(master,
       paste0(out, ".csv"),
       sep = ",")

# 11 save csvy in master ####
cat(paste0("Saving csvy in master/ \n"))
notes <- c("sector: Aircraft",
           "missing_values: = 999999",
           "timespan: Observations between Dec year - 1 and Jan year + 1",
           "spatial_limits: north = 80, south = 10, east = -50, west = -170",
           "data: Data averaged every 20 seconds",
           "altitude: < 8000",
           "hours: All",
           "local_time: if var `site_utc2lst` is not available, calculated as",
           "longitude/15*60*60 (John Miller)")

obs_write_csvy(dt = master,
               notes = notes,
               out = paste0(out, ".csvy"))

# 12 receptors ####
cat("Generating receptors \n")
message("Change Python/Bash to generate footprints")

receptor <- master[, c("site_code",
                       "year",
                       "month",
                       "day",
                       "hour",
                       "minute",
                       "second",
                       "latitude",
                       "longitude",
                       "altitude_final",
                       "type_altitude")]

receptor$altitude_final <- round(receptor$altitude_final)
receptor <- obs_format(receptor,
                        spf =  c("month", "day",
                                 "hour", "minute", "second"))

cat("Saving receptors in receptors/ \n")

receptor_agl <- receptor[type_altitude == 0]
receptor_asl <- receptor[type_altitude == 1]

if(nrow(receptor_agl) > 0) {
  cat(paste0(gsub("master",
                  "receptors",
                  out), "_AGL.txt"),
      "\n")

  fwrite(x = receptor_agl,
         file = paste0(gsub("master",
                            "receptors",
                            out), "_AGL.txt"),
         sep = " ")

}

if(nrow(receptor_asl) > 0) {
  cat(paste0(gsub("master",
                  "receptors",
                  out), "_ASL.txt"),
      "\n")

  fwrite(x = receptor_asl,
         file = paste0(gsub("master",
                            "receptors",
                            out), "_ASL.txt"),
         sep = " ")

}


message("Coordinates are round with 4 decimals  to match bash script")

