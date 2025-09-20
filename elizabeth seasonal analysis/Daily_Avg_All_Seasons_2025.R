# Daily Averages for All Seasons


#============================================#
# SETUP
#============================================#

# This script is automated to be able to read data provided in a specific format.
# The files need to be downloaded to a single folder, which you can provide a filepath for below.

# To run this script you need to do the following:
# 1. Update the variable 'filepath' to the folder on your local machine containing the datasets.
# 2. Update the variable 'export_to' to the folder you want to put the exported data.
#    NOTE: if you are using version control through GitHub, you should make this the
#    Analysis_Ready_Data folder in your local copy of the git repo.
# 3. Update the variable 'large_file_export' to the corresponding folder on your machine.
# 4. Make sure that the following variables are correct for your needs:
#    - years: the years for which you want to extract data
#    - seasons: of "Summer", "Fall", "Winter", and "Spring", supply the seasons
#      for your data collection
#    - sznstart: the starting season and year of your data to avoid exporting empty datasets
#    - sznend: the last season and year of your data to avoid exporting empty datasets
# 5. If there have been any changes in the sensor array or the sensor naming,
#    you will need to update the sensor variables beginning on line [XX]
#    NOTE: to find a list of all the sensors with copy-friendly text, refer to the
#    metadata sheet https://docs.google.com/spreadsheets/d/18X2YglxaPQeADujtZxG_fCtrwWgBFn8_/edit?gid=249467241#gid=249467241&range=A344

# path to the folder to load the data:
filepath = "~/Desktop/Arctic/Utqiagvik/Meteorological_Seasons_Data/RECENT_CSV_DOWNLOADS/"

# folder for the script to export cleaned data sets:
export_to = "~/Desktop/Arctic/Utqiagvik/Analysis_Ready_Data/"

# separate folder to export large data sets that you do NOT want to upload to the GitHub
# note: GitHub has a limited amount of space as well as a limit to file sizes
large_file_export = "~/Desktop/Arctic/Utqiagvik/Analysis_Ready_Data/Large_Files/"

years <- c(2022, 2023, 2024) # the years covered by the data
seasons <- c("Summer","Fall","Winter","Spring") # don't need to change this

# Date ranges (seasons) of data
# For File Naming - names the total combined file
sznstart <- "Summer2022"
sznend <- "Fall2024"

# The Data from GitHub have been uploaded here: https://github.com/shabanm2/Utqiagvik/tree/main/Meteorological_Seasons_Data/RECENT_CSV_DOWNLOADS
# the following filepath can be used to obtain the CSVs directly from GitHub,
# however this script is set up to be able to read all the files in a folder
# and that only works on your local files
# filepath: "https://raw.githubusercontent.com/shabanm2/Utqiagvik/main/Meteorological_Seasons_Data/RECENT_CSV_DOWNLOADS/"


# Sensors (for renaming serial #s):

vwcgroundsensors <- c('21393048'='BEO-B06','21398591'='BEO-B05','21393045'='BEO-B07','21398585'='BUECI-BASE','21398590'='BUECI-SA','21398583'='BUECI-SB','21393042'='BUECI-SC','21398584'='BUECI-SD','21398579'='BUECI-SE','21398578'='BUECI-SF01','21398598'='BUECI-SF02','21398588'='SSMH-BASE','21398599'='SSMH-SA','21393044'='SSMH-SB','21393049'='SSMH-SD','21398594'='SSMH-SE','21393043'='SSMH-SF','21398586'='SSMH-SG','21398580'='SSMH-SH','21398581'='SSMH-SI','21206939'='TNHA-BASE','21398593'='TNHA-SA','21398587'='TNHA-SB','21393047'='TNHA-SC','21398601'='TNHA-SD','21398577'='TNHA-SE','21398576'='BEO-B08','21393046'='TNHA-SC','21166008'='SSMH-SC','21212510'='REMOVE')
airtempsensors <- c('21398585'='BUECI-BASE','21398659'='BUECI-__','21398661'='BUECI-__','21390851'='BUECI-BASE','21397542'='BUECI-__','21398671'='BUECI-SA','21362254'='BUECI-SB','21398668'='BUECI-SC','21362256'='BUECI-SD','21187245'='BUECI-SE','21390849'='SSMH-BASE','21398670'='SSMH-SA','21398665'='SSMH-SB','21398667'='SSMH-SI','21212510'='TNHA-SF','21218018'='TNHA-BASE','21390850'='TNHA-BASE','21380919'='TNHA-SA','21398676'='TNHA-SB','21398664'='TNHA-SB','21398674'='TNHA-SC','21398666'='TNHA-SD','21981318'='BEO-BASE','21187247'='REMOVE','21397541'='REMOVE')
solarsensors <- c('21390415'='BEO-BASE','21362314'='BUECI-?','21398617'='BUECI-??','21398621'='BUECI-???','21398623'='BUECI-????','21390411'='BUECI-BASE','21390414'='BUECI-?????','21398618'='BUECI-SA','21398624'='BUECI-SB','21362313'='BUECI-SC','21362316'='BUECI-SD','21362320'='BUECI-SE','21390413'='SSMH-BASE','21398622'='SSMH-SA','21362319'='SSMH-SB','21398619'='SSMH-SI','21362318'='TNHA-SB','21176526'='TNHA-BASE','21398620'='TNHA-SA','21398616'='TNHA-SB','21362317'='TNHA-SC','21362315'='TNHA-SD','21390412'='REMOVE')

windsensors <- c('21350915'='BEO-BASE','21350894'='BUECI-BASE','21398590'='BUECI-SA','21398729'='BUECI-SB','21398724'='BUECI-SC','21398660'='BUECI-SD','21398719'='BUECI-SE','21350901'='SSMH-BASE','21350910'='SSMH-BASE','21206911'='SSMH-SA','21206912'='SSMH-SB','21398711'='SSMH-SI','21176861'='TNHA-BASE','21398709'='TNHA-SA','21181033'='TNHA-SB','21398715'='TNHA-SB','21206909'='TNHA-SC','21398712'='TNHA-SD','21390849'='REMOVE','21398665'='REMOVE','21398667'='REMOVE','21398670'='REMOVE','21981320'='REMOVE','21350819'='REMOVE','21350855'='BEO-BASE','21350820'='SSMH-BASE','21167037'='TNHA-BASE','21398716'='TNHA-SD')

winddir_ids <- c('21350855','21350894','21398590','21398729','21398724','21398660','21398719','21350820','21206911','21206912','21398711','21181033','21167037','21398709','21398715','21206909','21398712','21398716')

windspeed_ids <- c('21350915','21350894','21398590','21398729','21398724','21398660','21398719','21350901','21350910','21206911','21206912','21398711','21176861','21398709','21181033','21398715','21206909','21398712','21398716')

#===============================#
# HOW THIS SCRIPT WORKS
#===============================#

# Section 1: Loading & Cleaning Data [Lines 161-680]

# Each of the variables (ground temp, air temp, vwc, solar radiation, wind speed,
# and wind direction) are cleaned from their respective files.

# First, an empty dataframe is created, then each file for that variable is loaded,
# transposed, and cleaned, then appended onto the empty dataframe.
# The script removes unnecessary gibberish from the sensor serial numbers
# for improved readability in the end result.

# We refactor the depths of the ground temperature and vwc sensors to 
# measurements in cm.
# The final columns are Date, value, fullname, site, station, sensor, plus a
# metadata column.

# ---------------------------------#

# Section 2: Exporting Raw Data [Lines 680-751]

# Due to the nature of the size of the resulting dataframes, they are split
# by season, according to the seasons and years listed above between sznstart
# and sznend. Then they are exported into the large file folder.

# ---------------------------------#

# Section 3: Aggregating and Exporting Hourly Data [Lines 755-877]

# Each of the variables are aggregated by each hour and summarized with the min,
# max, average, and amplitude. The resulting dfs are exported by season and year.

# ---------------------------------#

# Section 4: Aggregating and Exporting Daily Data [Lines 879-979]

# Each of the variables are aggregated by each day and summarized with the min,
# max, average, and amplitude. The resulting dfs are exported by season and year.

# ---------------------------------#

# Section 5: Final Exports [Lines 981-1065]

# For both the hourly and the daily averages, each of the variables are joined together
# into a single df with columns representing the average measurement for each variable.
# The hourly and daily combined dfs are then exported.

# ************************************************
# YOU DO NOT NEED TO EDIT ANY CODE BELOW THIS LINE.
# ************************************************

# Find Files

all_files = list.files(filepath, pattern="*.csv", full.names=F)

# select ground temperature files
gtf = grepl("grnd", all_files, ignore.case=T)
gotf = grepl("ground", all_files, ignore.case=T)
gtfiles = all_files[gtf | gotf]

# select air temperature files
airf = grepl("air", all_files, ignore.case=T)
airfiles = all_files[airf]

# select solar radiation files
solf = grepl("solar", all_files, ignore.case=T)
solarfiles = all_files[solf]

# select vwc files
vwcf = grepl("vwc", all_files, ignore.case=T)
vwcfiles = all_files[vwcf]

# select wind files (windspeed or winddirection)
windf = grepl("wind", all_files, ignore.case=T)
windfiles = all_files[windf]


#============================================#
# Packages
#============================================#


#library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(gridExtra)
library(forcats)
library(splitstackshape)
library(naniar)
library(stringr)



#============================================#
# Ground Temperature
#============================================#

file = "Summer_22_grndtmp_2023_11_14_20_50_46_UTC_1.csv"

groundtemp <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(groundtemp) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in gtfiles){
  temp <- read.csv(paste0(filepath, file))
  #change sensor serial numbers to names
  names(temp) <- sub("....C..RX3000_BRW1", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW4", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW5", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW6", "", names(temp))
  names(temp) <- sub(".RXW.GP6.", "", names(temp))
  #names(temp) <- sub("Temperature.", "", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #some dupes for name change
  dates <- substr(temp$Date, 1, 10)
  if(max(dates) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  
  #there may be wind and air variables in the df
  temp <- select(temp, !contains(".RXW"))
  temp <- select(temp, !contains(".S"))
  
  #has "line" as first column
  temp <- temp[,2:ncol(temp)]
  
  # remove zeroes if zero at all depths (7-17)
  
  #start with first sensor
  colstart <- 2
  #while loop progresses through each clump of depths (.7-17) for each sensor
  while(colstart < ncol(temp)){
    colend <- colstart + 10
    if(colend > ncol(temp)){
      colend <- ncol(temp)
    }
    
    #take sum of subset of columns - if sum is zero we want to replace!
    valsums <- data.frame(rowSums(temp[,colstart:colend], na.rm=F))
    colnames(valsums) <- "sum"
    #get index number of zeroes
    valsums <- cbind(x = rownames(valsums), valsums)
    
    #select only values that are zeroes
    zeroes <- valsums %>% filter(sum==0)
    #replace zeroes with NAs
    temp[zeroes$x,c(colstart:colend)] <- NA
    
    colstart <- colend + 1
  }
  
  
  #turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[2:6] <- c("value", "variable", "site","fullname","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI",
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  # recode values from sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!vwcgroundsensors)
  # !!! lets you read from a concatenated list - allows for simpler template
  
  groundtemp <- rbind(groundtemp, temp)
}


#============================================#
# Air Temperature
#============================================#


airtemp <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(airtemp) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in airfiles){
  temp <- read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub(".RXW.THC.", "" ,names(temp))
  names(temp) <- sub(".RXW.TMB.", "" ,names(temp))
  names(temp) <- sub(".S.THC.", "" ,names(temp))
  #names(temp) <- sub("S.", "" ,names(temp))
  names(temp) <- sub("....C..RX3000_BRW1", "" ,names(temp))
  names(temp) <- sub("....C..RX3000_BRW4", "" ,names(temp))
  names(temp) <- sub("....C..RX3000_BRW5", "" ,names(temp))
  names(temp) <- sub("....C..RX3000_BRW6", "" ,names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #some dupes for name change
  dates <- substr(temp$Date, 1, 10)
  if(max(dates) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  #has "line" as first column
  temp <- temp[,2:ncol(temp)]
  
  # can't remove zeroes
  # can we assume that any zeroes in ground temp are also zeroes in air temp? same sensor?
  
  
  #turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[2:6] <- c("value", "variable", "site","fullname","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI", 
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  # recode values from sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!airtempsensors)
  # ignore sensor 21380922 (only a few random days in December 2023)
  temp <- temp %>% filter(fullname != "21380922")
  
  airtemp <- rbind(airtemp, temp)
}


#============================================#
# Volumetric Water Content
#============================================#

vwc <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(vwc) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in vwcfiles){
  temp <- read.csv(paste0(filepath, file))
  #change sensor serial numbers to names
  names(temp) <- sub("...m.3.m.3..RX3000_BRW1", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW4", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW5", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW6", "", names(temp))
  names(temp) <- sub("..RXW.GP6.", "", names(temp))
  names(temp) <- sub("Water.Content", "VWC.", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #some dupes for name change
  dates <- substr(temp$Date, 1, 10)
  if(max(dates) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  
  #there may be wind and air variables in the df
  temp <- select(temp, !contains(".RXW"))
  temp <- select(temp, !contains(".S"))
  
  #has "line" as first column
  temp <- temp[,2:ncol(temp)]
  
  # remove zeroes if zero at all depths (1-6)
  
  #start with first sensor
  colstart <- 2
  #while loop progresses through each clump of depths (.1-6) for each sensor
  while(colstart < ncol(temp)){
    colend <- colstart + 5
    if(colend > ncol(temp)){
      colend <- ncol(temp)
    }
    
    #take sum of subset of columns - if sum is zero we want to replace!
    valsums <- data.frame(rowSums(temp[,colstart:colend], na.rm=F))
    colnames(valsums) <- "sum"
    #get index number of zeroes
    valsums <- cbind(x = rownames(valsums), valsums)
    
    #select only values that are zeroes
    zeroes <- valsums %>% filter(sum==0)
    #replace zeroes with NAs
    temp[zeroes$x,c(colstart:colend)] <- NA
    
    colstart <- colend + 1
  }
  
  
  #turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[2:6] <- c("value", "variable", "site","fullname","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI", 
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  #fixed values from checking sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!vwcgroundsensors)
  
  temp <- temp %>% filter(fullname != "REMOVE")
  
  vwc <- rbind(vwc, temp)
}

#============================================#
# Solar Radiation
#============================================#

file = "Winter_solar_2023_04_06_12_55_01_UTC_1.csv"
solar <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(solar) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in solarfiles){
  temp <- read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("..RXW.LIB", "" ,names(temp))
  names(temp) <- sub("..S.LIB", "" ,names(temp))
  names(temp) <- sub("Solar.Radiation.", "Solar." ,names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW1", "" ,names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW4", "" ,names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW5", "" ,names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW6", "" ,names(temp))
  temp$Date <- as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #some dupes for name change
  dates <- substr(temp$Date, 1, 10)
  if(max(dates) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  #has "line" as first column
  temp <- temp[,2:ncol(temp)]
  
  # can't remove zeroes
  
  #turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp <- cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[2:6] <- c("value", "variable", "site","fullname","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI",
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  # recode values from sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!solarsensors)
  temp <- temp %>% filter(site != "BUECI")
  
  solar <- rbind(solar, temp)
}


#============================================#
# Wind (Speed, Direction, or both)
#============================================#

file = "Summer_24_wind_2024_09_05_18_42_55_UTC_1.csv"
windspeed <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(windspeed) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

winddir <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(winddir) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")


for(file in windfiles){
  temp <- read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("Wind.Speed.", "WindSpeed.", names(temp))
  names(temp) <- sub(".S.WSB.", "", names(temp))
  names(temp) <- sub(".RXW.WCF.", "", names(temp))
  names(temp) <- sub("...m.s..RX3000", "", names(temp))
  names(temp) <- sub("_BRW1", "", names(temp))
  names(temp) <- sub("_BRW4", "", names(temp))
  names(temp) <- sub("_BRW6", "", names(temp))
  names(temp) <- sub("Wind.Direction.", "WindDirection.", names(temp))
  names(temp) <- sub(".S.WDA.", "", names(temp))
  names(temp) <- sub("......RX3000", "", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  # get rid of Gust columns
  gust_cols = grepl("Gust", colnames(temp), ignore.case=T)
  if (length(gust_cols) > 0) {
    temp = temp %>% select(colnames(temp)[!gust_cols])
  }
  
  if(min(as.Date(temp$Date)) < as.Date("2023-03-05")){
    #The sensor names switched on March 5, 2023 to include "_SITE"
    dates <- substr(temp$Date, 1, 10)
    switch <- match("2023-03-05", dates) #Find first index of date of switch
    pre_mar <- temp[c(1:switch-1),] #select data from before switch
    post_mar <- temp[c(switch:nrow(temp)),]#select data from after switch
    
    pre_mar <- select(pre_mar, !contains("_")) #columns that don't contain "_SITE"
    post_mar <- select(post_mar, contains("_")) #columns that do contain "_SITE"
    
    #make col names match
    names(post_mar) <- sub("_TNHA", "", names(post_mar))
    names(post_mar) <- sub("_BEO", "", names(post_mar))
    names(post_mar) <- sub("_SSMH", "", names(post_mar))
    names(post_mar) <- sub("_BUECI", "", names(post_mar))
    
    temp <- temp[,c(1:2)]
    for(c in colnames(pre_mar)[c(3:ncol(pre_mar))]){
      if(c %in% colnames(post_mar)){
        pre <- pre_mar %>% select(c)
        post <- post_mar %>% select (c)
        long <- rbind(pre, post)
        temp <- data.frame(temp, long)
      }
      else{
        pre <- pre_mar %>% select(c)
        post <- data.frame(rep(NA, nrow(temp)-nrow(pre)))
        colnames(post) <- c
        long <- rbind(pre, post)
        temp <- data.frame(temp, long)
      }
    }
    
    names(temp) <- sub("X", "", names(temp))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  #has "line" as first column
  temp <- temp[,2:ncol(temp)]
  
  # can't remove zeroes
  
  #turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[2:6] <- c("value", "variable", "site","fullname","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI",
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  
  
  # recode values from sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!windsensors)

  # filter out invalid values
  temp <- temp %>% filter(value != -888.88)
  
  # separte windspeed and winddirection
  # NOTE: depths aren't matching so maybe we should double check before we recode them
  # also maybe we should keep a column with the sensor ids
  ws <- temp %>% filter(variable == "WindSpeed")
  wd <- temp %>% filter(variable == "WindDirection")
  
  windspeed <- rbind(windspeed, ws)
  winddir <- rbind(winddir, wd)
}


# recode depth values
groundtemp$depth <- recode(groundtemp$depth, "7" = "3.5cm", "8" = "10cm", "9" = "20cm", 
                           "10" = "30cm", "11" = "40cm", "12" = "50cm", "13" = "55cm", 
                           "14" = "65cm", "15" = "75cm", "16" = "85cm", "17" = "90cm")

groundtemp$depth <- factor(groundtemp$depth, c("3.5cm","10cm", "20cm", "30cm", "40cm", 
                                               "50cm", "55cm", "65cm", "75cm", "85cm", "90cm"))

vwc$depth <- recode(vwc$depth, "1" = "0-15cm", "2" = "15-30cm", "3" = "30-45cm",
                    "4" = "45-60cm", "5" = "60-75cm", "6" = "75-90cm")

vwc$depth <- factor(vwc$depth, c("0-15cm", "15-30cm", "30-45cm","45-60cm", 
                                 "60-75cm","75-90cm"))

# remove bad data
groundtemp <- groundtemp %>% filter(!value == -888.88)
airtemp <- airtemp %>% filter(!value == -888.88)
airtemp <- airtemp %>% filter(!value == 128.83)

# COLUMN NAMING CONVENTION
# [1] Date [2] fullname [3] site [4] station [5] depth [6] value [7] metadata

groundtemp <- groundtemp %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor, depth)

airtemp <- airtemp %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor)

solar <- solar %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor)

vwc <- vwc %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor, depth)

windspeed <- windspeed %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor)

winddir <- winddir %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor)


# create metadata column
# preserve BEO station ID in "metadata" but change all BEO stations to BASE

groundtemp <- groundtemp %>% mutate(metadata = case_when(fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
                                                         fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
                                                         site=="BEO"~station, T~NA))
groundtemp$fullname = as.character(groundtemp$fullname)
groundtemp$site = as.character(groundtemp$site)
groundtemp$station[groundtemp$site == "BEO"] = "BASE"
groundtemp$fullname[groundtemp$site == "BEO"] = "BEO-BASE"


airtemp <- airtemp %>% mutate(metadata = case_when(fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
                                                   fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
                                                   site=="BEO"~station, T~NA))
airtemp$fullname = as.character(airtemp$fullname)
airtemp$site = as.character(airtemp$site)
airtemp$station[airtemp$site == "BEO"] = "BASE"
airtemp$fullname[airtemp$site == "BEO"] = "BEO-BASE"


solar <- solar %>% mutate(metadata = case_when(fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
                                               fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
                                               site=="BEO"~station, T~NA))
solar$fullname = as.character(solar$fullname)
solar$site = as.character(solar$site)
solar$station[solar$site == "BEO"] = "BASE"
levels(solar$fullname)[levels(solar$site) == "BEO"] = "BEO-BASE"


vwc <- vwc %>% mutate(metadata = case_when(fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
                                           fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
                                           site=="BEO"~station, T~NA))
vwc$fullname = as.character(vwc$fullname)
vwc$site = as.character(vwc$site)
vwc$station[vwc$site == "BEO"] = "BASE"
vwc$fullname[vwc$site == "BEO"] = "BEO-BASE"


windspeed <- windspeed %>% mutate(metadata = case_when(fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
                                                       fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
                                                       site=="BEO"~station, T~NA))
windspeed$fullname = as.character(windspeed$fullname)
windspeed$site = as.character(windspeed$site)
windspeed$station[windspeed$site == "BEO"] = "BASE"
windspeed$fullname[windspeed$site == "BEO"] = "BEO-BASE"


winddir <- winddir %>% mutate(metadata = case_when(fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
                                                   fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
                                                   site=="BEO"~station, T~NA))
winddir$fullname = as.character(winddir$fullname)
winddir$site = as.character(winddir$site)
winddir$station[winddir$site == "BEO"] = "BASE"
winddir$fullname[winddir$site == "BEO"] = "BEO-BASE"




write.csv(groundtemp, file=paste(large_file_export, "Raw/", "GroundTemperature_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(airtemp, file=paste(large_file_export, "Raw/", "AirTemperature_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(solar, file=paste(large_file_export, "Raw/", "Solar_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(vwc, file=paste(large_file_export, "Raw/", "VWC_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(windspeed, file=paste(large_file_export, "Raw/", "WindSpeed_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(winddir, file=paste(large_file_export, "Raw/", "WindDirection_", sznstart, "_", sznend, "_RAW",".csv", sep=""))



#============================================#
# EXPORT RAW DATA BY SEASON
#============================================#

# Go By Each Season
# Export each variable

for(yr in years){
  for(szn in seasons){
    
    if(szn == "Summer"){
      datemin <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      
    } else if(szn=="Fall"){
      datemin <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      
    }else if(szn=="Winter"){
      datemin <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("03/01/",yr+1),format="%m/%d/%Y")
      
    } else{
      datemin <- as.Date(paste0("03/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
    }
    
    #Ground temp
    tmp <- groundtemp %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "GroundTemperature_", szn, yr, "_RAW", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- airtemp %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "AirTemperature_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "VWC_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "Solar_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- windspeed %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "WindSpeed_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    tmp <- winddir %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "WindDirection_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
  }
}



#============================================#
# HOURLY
#============================================#

gt_hourly <- groundtemp %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
gt_hourly$hour[gt_hourly$hour==''] <- "00"
gt_hourly <- gt_hourly %>% group_by(site, fullname, station, sensor, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
gt_hourly <- gt_hourly %>% select(Date, day, hour, fullname, site, station, sensor, depth, min, max, avg, amp)


at_hourly <- airtemp %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
at_hourly$hour[at_hourly$hour==''] <- "00"
at_hourly <- at_hourly %>% group_by(site, fullname, station, sensor, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
at_hourly <- at_hourly %>% select(Date, day, hour, fullname, site, station, sensor, min, max, avg, amp)


solar_hourly <- solar %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
solar_hourly$hour[solar_hourly$hour==''] <- "00"
solar_hourly <- solar_hourly %>% group_by(site, fullname, station, sensor, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
solar_hourly <- solar_hourly %>% select(Date, day, hour, fullname, site, station, sensor, min, max, avg, amp)


vwc_hourly <- vwc %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
vwc_hourly$hour[vwc_hourly$hour==''] <- "00"
vwc_hourly <- vwc_hourly %>% group_by(site, fullname, station, sensor, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
vwc_hourly <- vwc_hourly %>% select(Date, day, hour, fullname, site, station, sensor, depth, min, max, avg, amp)


ws_hourly <- windspeed %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
ws_hourly$hour[ws_hourly$hour==''] <- "00"
ws_hourly <- ws_hourly %>% group_by(site, fullname, station, sensor, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
ws_hourly <- ws_hourly %>% select(Date, day, hour, fullname, site, station, sensor, min, max, avg, amp)


wd_hourly <- winddir %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
wd_hourly$hour[wd_hourly$hour==''] <- "00"
wd_hourly <- wd_hourly %>% group_by(site, fullname, station, sensor, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
wd_hourly <- wd_hourly %>% select(Date, day, hour, fullname, site, station, sensor, min, max, avg, amp)


#============================================#
# EXPORT HOURLY BY SEASON
#============================================#

# Go By Each Season
# Export each variable

for(yr in years){
  for(szn in seasons){
    
    if(szn == "Summer"){
      datemin <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      
    } else if(szn=="Fall"){
      datemin <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      
    }else if(szn=="Winter"){
      datemin <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("03/01/",yr+1),format="%m/%d/%Y")
      
    } else{
      datemin <- as.Date(paste0("03/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
    }
    
    #Ground temp
    tmp <- gt_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "GroundTemperature_", szn, yr, "_HOURLY", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- at_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "AirTemperature_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "VWC_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "Solar_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- ws_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "WindSpeed_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    tmp <- wd_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "WindDirection_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
  }
}


#============================================#
# DAILY
#============================================#

gt_daily <- gt_hourly %>% group_by(as.Date(day), fullname, site, station, sensor, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(gt_daily)[1] <- "Date"

at_daily <- at_hourly %>% group_by(as.Date(day), fullname, site, station, sensor) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(at_daily)[1] <- "Date"

solar_daily <- solar_hourly %>% group_by(as.Date(day), fullname, site, station, sensor) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(solar_daily)[1] <- "Date"

vwc_daily <- vwc_hourly %>% group_by(as.Date(day), fullname, site, station, sensor, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(vwc_daily)[1] <- "Date"

ws_daily <- ws_hourly %>% group_by(as.Date(day), fullname, site, station, sensor) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(ws_daily)[1] <- "Date"

wd_daily <- wd_hourly %>% group_by(as.Date(day), fullname, site, station, sensor) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(wd_daily)[1] <- "Date"



#============================================#
# EXPORT DAILY BY SEASON
#============================================#

# Go By Each Season
# Export each variable for daily data

for(yr in years){
  for(szn in seasons){
    
    if(szn == "Summer"){
      datemin <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      
    } else if(szn=="Fall"){
      datemin <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      
    }else if(szn=="Winter"){
      datemin <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("03/01/",yr+1),format="%m/%d/%Y")
      
    } else{
      datemin <- as.Date(paste0("03/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
    }
    
    #Ground temp
    tmp <- gt_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "GroundTemperature_", szn, yr, "_DAILY", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- at_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "AirTemperature_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "VWC_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "Solar_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- ws_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "WindSpeed_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    tmp <- wd_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "WindDirection_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
  }
}


#============================================#
# COMBINE HOURLY
#============================================#

# Create comparable depths for groundtemp vs vwc

# Ground Temp Depths: 3.5cm 10cm 20cm 30cm 40cm 50cm 55cm 65cm 75cm 85cm 90cm
# VWC Depths: 0-15cm 15-30cm 30-45cm 45-60cm 60-75cm 75-90cm
colnames(gt_hourly)[colnames(gt_hourly) == "depth"] <- "grounddepth"
gt_hourly <- gt_hourly %>% mutate(vwcdepth = case_when(grounddepth == "3.5cm" ~ "0-15cm",
                                                       grounddepth == "10cm" ~ "0-15cm",
                                                       grounddepth == "20cm" ~ "15-30cm",
                                                       grounddepth == "30cm" ~ "30-45cm",
                                                       grounddepth == "40cm" ~ "30-45cm",
                                                       grounddepth == "50cm" ~ "45-60cm",
                                                       grounddepth == "55cm" ~ "45-60cm",
                                                       grounddepth == "65cm" ~ "60-75cm",
                                                       grounddepth == "75cm" ~ "75-90cm",
                                                       grounddepth == "85cm" ~ "75-90cm",
                                                       grounddepth == "90cm" ~ "75-90cm"))
colnames(vwc_hourly)[colnames(vwc_hourly) == "depth"] <- "vwcdepth"

colnames(gt_hourly)[colnames(gt_hourly) == "avg"] <- "groundtemp"
colnames(vwc_hourly)[colnames(vwc_hourly) == "avg"] <- "vwc"
colnames(at_hourly)[colnames(at_hourly) == "avg"] <- "airtemp"
colnames(solar_hourly)[colnames(solar_hourly) == "avg"] <- "solar"
colnames(ws_hourly)[colnames(ws_hourly) == "avg"] <- "windspeed"
colnames(wd_hourly)[colnames(wd_hourly) == "avg"] <- "winddirection"

join <- full_join(gt_hourly, vwc_hourly, by=c("Date", "fullname", "site", "station", "vwcdepth"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc) %>% unique() %>%
  full_join(at_hourly, by=c("Date", "fullname", "site", "station"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc, airtemp) %>% unique() %>%
  full_join(solar_hourly, by=c("Date", "fullname", "site", "station"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar) %>% unique() %>%
  full_join(ws_hourly, by=c("Date", "fullname", "site", "station"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed) %>% unique() %>%
  full_join(wd_hourly, by=c("Date", "fullname", "site", "station"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed, winddirection) %>% unique()


write.csv(join, file=paste(large_file_export, "All_Variables_", sznstart, "_", sznend, "_HOURLY", ".csv", sep=""))



#============================================#
# COMBINE Daily
#============================================#

colnames(gt_daily)[colnames(gt_daily) == "depth"] <- "grounddepth"
gt_daily <- gt_daily %>% mutate(vwcdepth = case_when(grounddepth == "3.5cm" ~ "0-15cm",
                                                       grounddepth == "10cm" ~ "0-15cm",
                                                       grounddepth == "20cm" ~ "15-30cm",
                                                       grounddepth == "30cm" ~ "30-45cm",
                                                       grounddepth == "40cm" ~ "30-45cm",
                                                       grounddepth == "50cm" ~ "45-60cm",
                                                       grounddepth == "55cm" ~ "45-60cm",
                                                       grounddepth == "65cm" ~ "60-75cm",
                                                       grounddepth == "75cm" ~ "75-90cm",
                                                       grounddepth == "85cm" ~ "75-90cm",
                                                       grounddepth == "90cm" ~ "75-90cm"))
colnames(vwc_daily)[colnames(vwc_daily) == "depth"] <- "vwcdepth"

colnames(gt_daily)[colnames(gt_daily) == "avg"] <- "groundtemp"
colnames(vwc_daily)[colnames(vwc_daily) == "avg"] <- "vwc"
colnames(at_daily)[colnames(at_daily) == "avg"] <- "airtemp"
colnames(solar_daily)[colnames(solar_daily) == "avg"] <- "solar"
colnames(ws_daily)[colnames(ws_daily) == "avg"] <- "windspeed"
colnames(wd_daily)[colnames(wd_daily) == "avg"] <- "winddirection"

join <- full_join(gt_daily, vwc_daily, by=c("Date", "fullname", "site", "station", "vwcdepth"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc) %>% unique() %>%
  full_join(at_daily, by=c("Date", "fullname", "site", "station"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc, airtemp) %>% unique() %>%
  full_join(solar_daily, by=c("Date", "fullname", "site", "station"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar) %>% unique() %>%
  full_join(ws_daily, by=c("Date", "fullname", "site", "station"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed) %>% unique() %>%
  full_join(wd_daily, by=c("Date", "fullname", "site", "station"), relationship = "many-to-many") %>%
  select(Date, fullname, site, station, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed, winddirection) %>% unique()


write.csv(join, file=paste(export_to, "Daily/All_Variables_", sznstart, "_", sznend, "_DAILY", ".csv", sep=""))

