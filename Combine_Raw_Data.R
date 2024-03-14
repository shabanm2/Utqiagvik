# variables:
# ground temperature
# air temperature
# volumetric water content (vwc) / ground moisture
# solar radiation
# wind speed
# wind direction


# ADD NEW DATA

# Seasons and years of current data to combine
# Used to split data into individual files for each season (smaller files)

years <- c(2022, 2023) # the years covered by the data
seasons <- c("Summer","Fall","Winter","Spring") # don't need to change this

# Date ranges (seasons) of data
# For File Naming - names the total combined file
sznstart <- "Summer2022"
sznend <- "Fall2023"

# sensor names change! update any sensor name changes here:

gtsensors <- c('21206939'='TNHA-BASE','21393042'='BUECI-SC','21393044'='SSMH-SB',
               '21393047'='TNHA-SC','21393048'='BEO-B06','21393049'='SSMH-SD',
               '21398578'='BUECI-SF01','21398579'='BUECI-SE','21398583'='BUECI-SB',
               '21398584'='BUECI-SD','21398585'='BUECI-BASE','21398588'='SSMH-BASE',
               '21398590'='BUECI-SA','21398591'='BEO-B05','21398593'='TNHA-SA',
               '21398598'='BUECI-SF02','21398599'='SSMH-SA','21398601'='TNHA-SD',
               '21398576'='SSMH-SC','21398587'='TNHA-SB','21398577'='TNHA-SE',
               '21393043'='SSMH-SE02','21398580'='SSMH-SF02','21398581'='SSMH-SG',
               '21398586'='SSMH-SF01','21398594'='SSMH-SE01')

atsensors <- c('21187245'='BUECI-SE','21218018'='TNHA-BASE','21362254'='BUECI-SB',
              '21362256'='BUECI-SD','21380919'='TNHA-SA','21390849'='SSMH-BASE',
              '21390851'='BUECI-BASE','21397541'='BEO-B02','21398665'='SSMH-SB',
              '21398666'='TNHA-SD','21398668'='BUECI-SC','21398670'='SSMH-SA',
              '21398671'='BUECI-SA','21398674'='TNHA-SC','21398676'='TNHA-SB',
              '21397542'='BUECI-BASE??','21398659'='BUECI-__','21398661'='BUECI-__',
              '21398664'='TNHA-??')

vwcsensors <- c('21206939'='TNHA-BASE','21393042'='BUECI-SC','21393044'='SSMH-SB',
               '21393047'='TNHA-SC','21393048'='BEO-B06','21393049'='SSMH-SD',
               '21398578'='BUECI-SF01','21398579'='BUECI-SE','21398583'='BUECI-SB',
               '21398584'='BUECI-SD','21398585'='BUECI-BASE','21398588'='SSMH-BASE',
               '21398590'='BUECI-SA','21398591'='BEO-B05','21398593'='TNHA-SA',
               '21398598'='BUECI-SF02','21398599'='SSMH-SA','21398601'='TNHA-SD',
               '21398576'='REMOVE','21398587'='TNHA-SB','21398577'='TNHA-SE',
               '21393043'='SSMH-SE02','21398580'='SSMH-SF02','21398581'='SSMH-SG',
               '21398586'='SSMH-SF01','21398594'='SSMH-SE01')

solarsensors <- c('21176526'='TNHA-BASE','21362313'='BUECI-SC','21362315'='TNHA-SD',
                 '21362316'='BUECI-SD','21362317'='TNHA-SC','21362319'='SSMH-SB',
                 '21362320'='BUECI-SE','21390411'='BUECI-BASE','21390413'='SSMH-BASE',
                 '21390415'='BEO-B04','21398616'='TNHA-SB','21398618'='BUECI-SA',
                 '21398620'='TNHA-SA','21398622'='SSMH-SA','21398624'='BUECI-SB',
                 '21398619'='SSMH-SG','21398623'='BUECI-???','21362314'='BUECI-__',
                 '21362318'='TNHA-??','21390414'='BUECI-BASE??','21398617'='BUECI-__',
                 '21398621'='BUECI-__')

wspeedsensors <- c('21176861'='TNHA-BASE','21206909'='TNHA-SC','21206911'='SSMH-SA',
                  '21206912'='SSMH-SB','21350910'='SSMH-BASE','21350915'='BEO-B03',
                  '21398709'='TNHA-SA','21398712'='TNHA-SD','21398711'='SSMH-SG',
                  '21398715'='TNHA-SB')

wdirsensors <- c('21167037'='TNHA-BASE','21181033'='TNHA-__','21350820'='SSMH-BASE',
                '21350855'='BEO-B03','21206909'='TNHA-SC','21206911'='SSMH-SA',
                '21206912'='SSMH-SB','21398709'='TNHA-SA','21398712'='TNHA-SD',
                '21398715'='TNHA-SB')


#============================================#
# Files
#============================================#

# link to github folder of raw data
path <- "https://raw.githubusercontent.com/shabanm2/Utqiagvik/main/Meteorological_Seasons_Data/"
export_to <- "~/Desktop/Arctic/X-Y_Plots/data/processed/Combined/" #Directory to export long dataframes

# file names by variables

#Updated filepaths: https://raw.githubusercontent.com/shabanm2/Utqiagvik/main/Meteorological_Seasons_Data/RECENT_CSV_DOWNLOADS

gtfiles <- c("RECENT_CSV_DOWNLOADS/Summer_22_grndtmp_2023_11_14_20_50_46_UTC_1.csv", # summer 2022
            "RECENT_CSV_DOWNLOADS/Fall_22_grndtmp_2023_11_14_21_31_58_UTC_1.csv", # fall 2022
            "RECENT_CSV_DOWNLOADS/Winter_grndtmp_2023_04_06_12_18_43_UTC_1.csv", # winter 2022 dec-jan
            "RECENT_CSV_DOWNLOADS/New_Spring_2023_grndtmp_2023_11_01_18_20_38_UTC_1.csv", # spring 2023
            "RECENT_CSV_DOWNLOADS/New_Summer_2023_GRNDTMP_2023_11_01_18_32_40_UTC_1.csv",
            "RECENT_CSV_DOWNLOADS/Fall_2023_grndtmp_2023_12_05_19_47_18_UTC_1.csv",
            "RECENT_CSV_DOWNLOADS/Fall_2023_grndtmp_2023_12_05_19_47_18_UTC_2.csv")

#EXCLUDE THESE SPRING FILES:
#"PLZSPRINGGRNDTMP_2023_06_20_02_45_17_UTC_1.csv", #march, june
#"Spring_grndtmp_2023_09_14_19_38_13_UTC_1.csv",
#"Spring_meteorological_grndtmp_2023_07_14_22_32_10_UTC_1.csv", #6/19-9/1


atfiles <- c("RECENT_CSV_DOWNLOADS/Summer_22_airtmp_2023_11_14_20_57_51_UTC_1.csv",# summer 2022 not updated for 2023
            "RECENT_CSV_DOWNLOADS/Fall_22_airtmp_2023_11_14_21_43_57_UTC_1.csv",
            "RECENT_CSV_DOWNLOADS/Winter_airtemp_2023_04_06_12_02_30_UTC_1.csv", # winter 2022
            "RECENT_CSV_DOWNLOADS/New_Spring_2023_AIRTMP_2023_11_01_18_30_56_UTC_1.csv",
            "RECENT_CSV_DOWNLOADS/New_Summer_2023_AIRTMP_2023_11_01_18_31_41_UTC_1.csv",
            "RECENT_CSV_DOWNLOADS/Fall_2023_airtemp_2023_12_05_20_08_48_UTC_1.csv"
            )

vwcfiles <- c("RECENT_CSV_DOWNLOADS/Summer_22_VWC_2023_11_14_21_00_08_UTC_1.csv", # summer 2022
             "RECENT_CSV_DOWNLOADS/Fall_22_VWC_2023_11_14_21_46_41_UTC_1.csv",
             "RECENT_CSV_DOWNLOADS/Winter_VWC_2023_04_06_12_36_52_UTC_1.csv",
             "RECENT_CSV_DOWNLOADS/New_Spring_2023_VWC_2023_11_01_18_26_40_UTC_1.csv",
             "RECENT_CSV_DOWNLOADS/New_Summer_2023_VWC_2023_11_01_18_34_52_UTC_1.csv",
             "RECENT_CSV_DOWNLOADS/Fall_2023_VWC_2023_12_05_20_06_01_UTC_1.csv"
             )

solfiles <- c("RECENT_CSV_DOWNLOADS/Summer_22_solar_2023_11_14_21_01_06_UTC_1.csv", # summer 2022
             "RECENT_CSV_DOWNLOADS/Fall_22_solar_2023_11_14_21_56_42_UTC_1.csv",
             "RECENT_CSV_DOWNLOADS/Winter_solar_2023_04_06_12_55_01_UTC_1.csv",
             "RECENT_CSV_DOWNLOADS/New_Spring_2023_SOLAR_2023_11_01_18_29_33_UTC_1.csv",
             "RECENT_CSV_DOWNLOADS/New_Summer_2023_SOLAR_2023_11_01_18_33_40_UTC_1.csv",
             "RECENT_CSV_DOWNLOADS/Fall_2023_SOLAR_2023_12_05_20_06_48_UTC_1.csv") 

wsfiles <- c("Spring_WindSpeed_2023_09_14_19_32_09_UTC_1.csv",
            "Summer22_Summer23_Wind_Speed_2023_09_07_19_43_04_UTC_1.csv",# summer 2022 through 2023
            "RECENT_CSV_DOWNLOADS/Fall_2023_windspeed_2023_12_05_20_07_20_UTC_1.csv")

wdfiles <- c("Spring_WindDirection_2023_09_14_19_33_07_UTC_1.csv",
            "Summer22_Summer23_Wind_Dir_2023_09_07_19_32_22_UTC_1.csv",
            "RECENT_CSV_DOWNLOADS/Fall_2023_winddirection_2023_12_05_20_07_48_UTC_1.csv")



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

file = "RECENT_CSV_DOWNLOADS/Summer_22_grndtmp_2023_11_14_20_50_46_UTC_1.csv"

groundtemp <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(groundtemp) <- c("Date", "value","variable", "site", "fullname", "depth")

for(file in gtfiles){
  temp <- read.csv(paste0(path, file))
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
  
  temp$fullname <- recode(temp$fullname, !!!gtsensors)
  # !!! lets you read from a concatenated list - allows for simpler template
  
  groundtemp <- rbind(groundtemp, temp)
}


#============================================#
# Air Temperature
#============================================#


airtemp <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(airtemp) <- c("Date", "value","variable", "site", "fullname", "depth")

for(file in atfiles){
  temp <- read.csv(paste0(path, file))
  
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
  
  #fixed values from checking sensor names
  temp$fullname <- recode(temp$fullname, !!!atsensors)
  
  airtemp <- rbind(airtemp, temp)
}


#============================================#
# Volumetric Water Content
#============================================#

vwc <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(vwc) <- c("Date", "value","variable", "site", "fullname", "depth")

for(file in vwcfiles){
  temp <- read.csv(paste0(path, file))
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
  temp$fullname <- recode(temp$fullname, !!!vwcsensors)
  
  temp <- temp %>% filter(fullname != "REMOVE")
  
  vwc <- rbind(vwc, temp)
}

#============================================#
# Solar Radiation
#============================================#

file = "Winter_solar_2023_04_06_12_55_01_UTC_1.csv"
solar <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(solar) <- c("Date", "value", "variable", "site", "fullname", "depth")

for(file in solfiles){
  temp <- read.csv(paste0(path, file))
  
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
  
  #fixed values from checking sensor names
  temp$fullname <- recode(temp$fullname, !!!solarsensors)
  temp <- temp %>% filter(site != "BUECI")
  
  solar <- rbind(solar, temp)
}


#============================================#
# Wind Speed
#============================================#

#file <- "Spring_WindSpeed_2023_09_14_19_32_09_UTC_1.csv"
windspeed <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(windspeed) <- c("Date", "value", "variable","site", "fullname", "depth")

for(file in wsfiles){
  temp <- read.csv(paste0(path, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("Wind.Speed.", "WindSpeed.", names(temp))
  names(temp) <- sub(".S.WSB.", "", names(temp))
  names(temp) <- sub(".RXW.WCF.", "", names(temp))
  names(temp) <- sub("...m.s..RX3000", "", names(temp))
  names(temp) <- sub("_BRW1", "", names(temp))
  names(temp) <- sub("_BRW4", "", names(temp))
  names(temp) <- sub("_BRW6", "", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
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
  
  #fixed values from checking sensor names
  temp$fullname <- recode(temp$fullname, !!!wspeedsensors)
  #temp <- temp %>% filter(station != "BUECI")
  
  windspeed <- rbind(windspeed, temp)
}


#============================================#
# Wind Direction
#============================================#

#file <- "Summer22_Summer23_Wind_Speed_2023_09_07_19_43_04_UTC_1.csv"
winddir <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(winddir) <- c("Date", "value","variable", "site", "fullname", "depth")

for(file in wdfiles){
  temp <- read.csv(paste0(path, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("Wind.Direction.", "WindDirection.", names(temp))
  names(temp) <- sub(".S.WDA.", "", names(temp))
  names(temp) <- sub(".RXW.WCF.", "", names(temp))
  names(temp) <- sub("......RX3000", "", names(temp))
  names(temp) <- sub("_BRW1", "", names(temp))
  names(temp) <- sub("_BRW4", "", names(temp))
  names(temp) <- sub("_BRW6", "", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
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
  
  temp <- cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[2:6] <- c("value", "variable", "site","fullname","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI",
                           "21401801" = "SSMH", "21401803" = "BEO")
  
  #fixed values from checking sensor names
  temp$fullname <- recode(temp$fullname, !!!wdirsensors)
  #temp <- temp %>% filter(station != "BUECI")
  
  winddir <- rbind(winddir, temp)
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
  select(Date, value, fullname, site, station, depth)

airtemp <- airtemp %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, depth)

solar <- solar %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, depth)

vwc <- vwc %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, depth)

windspeed <- windspeed %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, depth)

winddir <- winddir %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, depth)


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




write.csv(groundtemp, file=paste(export_to, "GroundTemperature_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(airtemp, file=paste(export_to, "AirTemperature_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(solar, file=paste(export_to, "Solar_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(vwc, file=paste(export_to, "VWC_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(windspeed, file=paste(export_to, "WindSpeed_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(winddir, file=paste(export_to, "WindDirection_", sznstart, "_", sznend, "_RAW",".csv", sep=""))



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
      write.csv(tmp, file=paste(export_to, "GroundTemperature_", szn, yr, "_RAW", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- airtemp %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "AirTemperature_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "VWC_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Solar_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- windspeed %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "WindSpeed_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    tmp <- winddir %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "WindDirection_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
  }
}



#============================================#
# HOURLY
#============================================#

gt_hourly <- groundtemp %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
gt_hourly$hour[gt_hourly$hour==''] <- "00"
gt_hourly <- gt_hourly %>% group_by(site, fullname, station, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
gt_hourly <- gt_hourly %>% select(Date, day, hour, fullname, site, station, depth, min, max, avg, amp)


at_hourly <- airtemp %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
at_hourly$hour[at_hourly$hour==''] <- "00"
at_hourly <- at_hourly %>% group_by(site, fullname, station, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
at_hourly <- at_hourly %>% select(Date, day, hour, fullname, site, station, depth, min, max, avg, amp)


solar_hourly <- solar %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
solar_hourly$hour[solar_hourly$hour==''] <- "00"
solar_hourly <- solar_hourly %>% group_by(site, fullname, station, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
solar_hourly <- solar_hourly %>% select(Date, day, hour, fullname, site, station, depth, min, max, avg, amp)


vwc_hourly <- vwc %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
vwc_hourly$hour[vwc_hourly$hour==''] <- "00"
vwc_hourly <- vwc_hourly %>% group_by(site, fullname, station, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
vwc_hourly <- vwc_hourly %>% select(Date, day, hour, fullname, site, station, depth, min, max, avg, amp)


ws_hourly <- windspeed %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
ws_hourly$hour[ws_hourly$hour==''] <- "00"
ws_hourly <- ws_hourly %>% group_by(site, fullname, station, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
ws_hourly <- ws_hourly %>% select(Date, day, hour, fullname, site, station, depth, min, max, avg, amp)


wd_hourly <- winddir %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
wd_hourly$hour[wd_hourly$hour==''] <- "00"
wd_hourly <- wd_hourly %>% group_by(site, fullname, station, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
wd_hourly <- wd_hourly %>% select(Date, day, hour, fullname, site, station, depth, min, max, avg, amp)


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
      write.csv(tmp, file=paste(export_to, "GroundTemperature_", szn, yr, "_HOURLY", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- at_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "AirTemperature_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "VWC_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Solar_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- ws_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "WindSpeed_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    tmp <- wd_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "WindDirection_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
  }
}


#============================================#
# DAILY
#============================================#

gt_daily <- gt_hourly %>% group_by(as.Date(day), fullname, site, station, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(gt_daily)[1] <- "Date"

at_daily <- at_hourly %>% group_by(as.Date(day), fullname, site, station, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(at_daily)[1] <- "Date"

solar_daily <- solar_hourly %>% group_by(as.Date(day), fullname, site, station, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(solar_daily)[1] <- "Date"

vwc_daily <- vwc_hourly %>% group_by(as.Date(day), fullname, site, station, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(vwc_daily)[1] <- "Date"

ws_daily <- ws_hourly %>% group_by(as.Date(day), fullname, site, station, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(ws_daily)[1] <- "Date"

wd_daily <- wd_hourly %>% group_by(as.Date(day), fullname, site, station, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(wd_daily)[1] <- "Date"



#============================================#
# EXPORT HOURLY BY SEASON
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
      write.csv(tmp, file=paste(export_to, "GroundTemperature_", szn, yr, "_DAILY", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- at_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "AirTemperature_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "VWC_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Solar_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- ws_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "WindSpeed_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    tmp <- wd_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "WindDirection_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
  }
}
