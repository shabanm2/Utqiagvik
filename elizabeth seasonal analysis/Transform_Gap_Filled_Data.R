# link to file path with gap filled data
# the files are too big to read from github (too many columns)
path <- "/Users/emvanmetre/Desktop/Arctic/Utqiagvik/elizabeth seasonal analysis/sourcedata/"

tempfile <- "Temp_data_CLEAN_TNHA_from_2022_07_15_to_2024_08_14.csv"
moistfile <- "Moist_data_CLEAN_TNHA_from_2022_07_15_to_2024_08_14.csv"

temp <- read.csv(paste0(path, tempfile))
moist <- read.csv(paste0(path, moistfile))

# ======================================
# turn into long dataframes
# ======================================
# -----------------------------
# libraries
# -----------------------------
library(dplyr)
library(tidyr)
library(splitstackshape)

# -----------------------------
# ground temperature
# -----------------------------
# rename sensors and columns first
colnames(temp)[1:4] <- c("sensor", "depthcm", "latitude", "longitude")

temp_long <- gather(temp, date, value, 5:ncol(temp))
temp_long <- cSplit(temp_long, 'sensor', sep="-", direction = "wide")
temp_long <- select(temp_long, date, sensor_1, sensor_2, depthcm, longitude, latitude, value)
colnames(temp_long)[2:3] <- c("sensor", "sensordepth")

# get rid of leading X
temp_long$date <- gsub('X', '', temp_long$date)

# --delete--
# separate date and time by a space
# rename_dates <- paste0(substring(temp_long$date, 1, 10), ' ', substring(temp_long$date, 12, 19))
# ----------

# format date and time
dates <- as.POSIXct(temp_long$date,format="%Y.%m.%d.%H.%M.%S",tz="UTC")
# -----------------------------

# -----------------------------
# volumetric water content
# -----------------------------
# rename sensors and columns first
colnames(moist)[1:4] <- c("sensor", "depthcm", "latitude", "longitude")

moist_long <- gather(moist, date, value, 5:ncol(moist))
moist_long <- cSplit(moist_long, 'sensor', sep="-", direction = "wide")
moist_long <- select(moist_long, date, sensor_1, sensor_2, depthcm, longitude, latitude, value)
colnames(moist_long)[2:3] <- c("sensor", "sensordepth")

# get rid of leading X
moist_long$date <- gsub('X', '', moist_long$date)

# format date and time
dates <- as.POSIXct(moist_long$date,format="%Y.%m.%d.%H.%M.%S",tz="UTC")
# -----------------------------

# ======================================