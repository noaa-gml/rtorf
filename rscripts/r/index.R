wd <- paste0(getwd(), "/rscripts") #  you can change
# if you change, define a new path
# wd <- "path"
# then set working directory
setwd(wd)

cat("This is your working directory", getwd(), "\n")

library(data.table)
library(robspack)

categories <- c(
  #"aircraft-pfp",
  "aircraft-insitu",
  "surface-insitu",
  "aircore",
  #"surface-pfp",
  "tower-insitu",
  "shipboard-insitu",
  "flask"
)

# ObsPack ####
obs <- "../../../obspack_ch4_1_GLOBALVIEWplus_v4.0_2021-10-14/data/txt/"

# Create Directories ####
# master have files with all columns
dir.create("master", showWarnings = FALSE)

# receptors have files with required columns
dir.create("receptors", showWarnings = FALSE)

# receptors have files with required columns
dir.create("obs", showWarnings = FALSE)

# 1 summary (previously named index) ####
cat("Running obs_summary(obs)\n")
index <- obs_summary(obs = obs)


# 2 read ####

for(i in seq_along(categories)) {

cat(paste0("Running obs_read(index = index,
               categories = ", categories[i], "\n"))

df <- obs_read(index = index,
               categories = categories[i],
               verbose = T)

fwrite(df, paste0("obs/obs_",categories[i], ".csv.gz"))

}
