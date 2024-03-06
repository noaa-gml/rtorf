#example Rscript rscripts/aircore_year.R 2020 obs/obs_aircore.csv

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

out <- paste0("master/master_aircore", yy)

message("All missing values are filled with 999999")

cat("                 _     rtorffilter              \n")
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
library(rtorf)

# 0 filters ####
north <- 80
south <- 10
west <- -170
east <- -50
max_altitude <- 8000

# 1 Index ####
#cat("Running obs_index(obs)\n")
#index <- obs_index(obs = obs)

# 2 Read ####
#cat("Running obs_read(index = index,
#               categories = 'aircore',
#               verbose = T)\n")
#df <- obs_read(index = index,
#               categories = "aircore",
#               verbose = T)

df <- fread(ind)

# 3 Filtering space and time ###
cat("Filtering\n")
df <- rbind(df[year == yy - 1 & month == 12],
            df[year == yy],
            df[year == yy + 1 & month == 1])

df <- df[altitude_final < max_altitude &
           latitude < north &
           latitude > south &
           longitude < east &
           longitude > west]

names(df)

# 4 Adding time columns ####
cat("Running obs_addtime(df)\n")
df2 <- obs_addtime(df)

# 5 Add local time ####
df3 <- obs_addltime(df2)


# 6 Save txt in master ####
cat("Saving txt in master/ \n")
master <- df3
#master[is.na(master)] <- 999999
master$obspack_id <- NULL
master$altitude_comment <- NULL

master$timeUTC <- as.character(master$timeUTC)
master$timeUTC_start <- as.character(master$timeUTC_start)
master$timeUTC_end <- as.character(master$timeUTC_end)
master$local_time <- as.character(master$local_time)
master$latitude <- round(master$latitude, 4)
master$longitude <- round(master$longitude, 4)

fwrite(master,
       paste0(out, ".txt"),
       sep = " ")

fwrite(master,
       paste0(out, ".csv"),
       sep = ",")

# 7 Save csvy in master ####
cat(paste0("Saving csvy in master/ \n"))
notes <- c("sector: Aircore",
           "timespan: Observations between Dec year -1 and Jan year + 1",
           "spatial_limits: north = 80, south = 10, east = -50, west = -170",
           "data: Replace 'NOAA AirCore' by 'NOAA_AirCore'",
           "local_time: if var `site_utc2lst` is not available, calculated as",
           "longitude/15*60*60 (John Miller)")

obs_write_csvy(dt = master,
               notes = notes,
               out = paste0(out, ".csvy"))


# 8 Receptors ####
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

