---
  title: "PCA_Sites_Combined_Ch2"
author: "Mirella Shaban"
date: "2026-01-20"
output: html_document
---
  
#from Elise's edits in Mirella PCA.R, for Chapter 2
  #currently needs solar added for SSMH-SA,SB, TNHA-SA --> PCA can't run b/c no solar currently added
  
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggfortify)



#load("PCA_SITES_ALL_DATA.RData")
df <- read.csv("~/Desktop/UVA/RESEARCH/Barrow/GAP_FILLED_CSV_DOWNLOADS/Sep/all_var_combined_22_25_BEO_COMP.csv")
df$site <- sub("-.*", "", df$sensor_name)
df$station <- sub(".*-", "", df$sensor_name)
#pick_site <- function(cursite){



cols <- c("sensor_name", "site", "station")
df <- df %>%
  mutate_at(cols, factor)
df <- df %>% filter(grounddepth == "10cm") %>% filter(vwcdepth == "0-15cm")
df$Date <- as.Date(df$Date, format = "%Y-%m-%d") 



## spring_dates =("-03-01","-04-01","-05-01"), end=c("-04-01","-05-01","-06-01"))
## so we are looking at march, april, may 

# summer_dates ("-06-01","-07-01","-08-01"), end=c("-07-01","-08-01","-09-01"))
### june, july, august

# fall_dates = c("-09-01","-10-01","-11-01"), end=c("-10-01","-11-01","-12-01"))
## september october november

# winter_dates =c("-12-01","-01-01","-02-01"), end=c("-01-01","-02-01","-03-01"))
##  December, January February



## what mirella is actually making a pca out of is: summer 2022 - 7/01-7/21
## and Fall 10/01 - 10/31


# pca_df <- na.omit(pca_df)
# pca_df <- unique(pca_df)

### relabeling df
df$aspect <-revalue(df$sensor_name,  c(
  "TNHA-SA" = "Residential South",
  "TNHA-SC" = "Residential North",
  "SSMH-SA" = "Hospital South",
  "SSMH-SB" = "Hospital North",
  "BEO-B05" = "Tundra-1", #2022-2023
  "BEO-B06" = "Tundra-1", #2022-2023
  "BEO-B08" = "Tundra-2" #2024-2025
))

df <- df[, c("Date", "sensor_name", "site", "station", "grounddepth", "vwcdepth", "groundtemp", "vwc", "airtemp", "solar", "windspeed", "winddirection", "aspect")] #in OG document, Date (day is first in order) comes before aspect

######################################## summer

## pulling out just summer data for hospital north and south and residential north and south
summer <- df %>%
  subset(Date >="2022-07-01" & Date <= "2022-07-31") %>%
  subset(!site == "BEO") %>%
  subset(sensor_name %in% c("TNHA-SA", "TNHA-SC", "SSMH-SA", "SSMH-SB" )) %>%
  na.omit() %>%
  unique() %>%
  droplevels()


ggplot(df%>%
         subset(Date >="2022-07-01" & Date <= "2022-07-31"), 
       aes(x=windspeed, y=winddirection)) + 
  geom_point() + 
  theme_classic()

ggplot(summer, 
       aes(x=Date, y=windspeed)) + 
  geom_point() + 
  theme_classic()


df%>%
  subset(Date >="2022-07-01" & Date <= "2022-07-31") %>% 
  
  mutate(yend = windspeed * cos(winddirection * 2 * pi / 360) * 0.1,
         xend = windspeed * sin(winddirection * 2 * pi / 360) * 0.1 + Date) %>%
  aggregate(cbind(yend, xend) ~ Date, mean) %>%
  na.omit() %>%
  ggplot(aes(x = Date, y = 0)) +
  geom_segment(aes(xend = xend, yend = yend), size = .1,
               arrow = grid::arrow(length = unit(0.06, "inches"), type = "closed")) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_x_continuous(breaks = 1:30) +
  geom_point() +
  labs(y = "") +
  coord_equal() +
  theme_classic() +
  theme(axis.text.y = element_blank())


## dropping wind direction because it too correlated with windspeed
summer$winddirection <- NULL
## also where is your time component?



summerpca <- prcomp(summer[7:11], center=TRUE, scale.=TRUE)
#need all met variables to lay between cols 8-12

#take out variables
sd <- summerpca$sdev
sd
loads <- summerpca$rotation
loads 
rownames(loads) <- colnames(summer[7:11])

scores <- summerpca$x


var <- sd^2
varPercent <- var/sum(var) * 100

summary(summerpca)
screeplot(summerpca, npcs = 15, type = "lines")
summerpca$u

## rough
autoplot(summerpca, loadings = TRUE, loadings.label = FALSE,
         loadings.colour = "black", loadings.size = .5,
         data = summer, colour = 'aspect') + theme_classic() +
  scale_colour_manual(values=c("red3","blue4", "steelblue", "goldenrod","magenta3" ), name = "site") 


## 36.58 and 27.01
## from Elise's code

loadings <- summerpca$rotation
scores <- as.data.frame(scores)
loadings <- as.data.frame(loadings)
## adding a group to the scores - rownames are presently the X column for summer
scores$aspect <-summer$aspect
loadings$variable <- colnames(summer[c(7:11)])
#scores <- full_join(scores, static %>% dplyr::select("NameID", "divergecat", "species", "correlation_post", "correlation_pre"))

cutoff <- sqrt(1/ncol((summer[c(7:11)]))) # cutoff for 'important' loadings

# cambisol, longitude, SOC, cryosol, clay, bulk density, elevation, sand, latitude, silt,
# OrgC, Nirogen

loadings <- loadings %>%
  subset(abs(PC1) >= cutoff | abs(PC2) >= cutoff)


scores$trueaspect <-  revalue(scores$aspect, c("Residential South" = "South", 
                                               "Residential North" = "North",
                                               "Hospital South" = "South",
                                               "Hospital North" = "North"))

scores$simplesite <-  revalue(scores$aspect, c("Residential South" = "Residential", 
                                               "Residential North" = "Residential",
                                               "Hospital South" = "Hospital",
                                               "Hospital North" = "Hospital"))


library(ggrepel)
group.colors <- c("Residential South" = "darkred", "Residential North" ="tomato", "Hospital North" = "skyblue", "Hospital South" = "steelblue") # do i want to change these colors?? 

#group.colors <- c("Residential South" = "darkred", "Residential North" ="tomato", "Hospital North" = "skyblue", "Hospital South" = "steelblue") ##original color palette from Elise
ggplot(scores, aes(x=PC1/sd[1], y=PC2/sd[2]))+ #sets up the plot
  geom_point(aes(color = aspect, shape = trueaspect), size=1, alpha = .7) + # add the point markers
  #add vector arrows of significant env variables
  
  geom_polygon(data = scores %>% group_by(aspect) %>% slice(chull(PC1, PC2)),
               aes(fill = aspect, color = aspect, linetype = trueaspect), alpha = 0) + 
  geom_segment(data = loadings, size = 3,
               aes(x = 0, xend=PC1*sd[1]*2, y=0, yend=PC2*sd[2]*2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "black", lwd=0.3) +
  geom_label_repel(data = loadings, 
                   aes(x=PC1*sd[1]*2, y=PC2*sd[2]*2, label = variable), size=5, fill = "white")+ #
  coord_fixed()+ 
  theme_classic() +
  scale_colour_manual(values=group.colors, name = "Site") +
  scale_fill_manual(values=group.colors, name = "Site") +
  scale_shape_manual(values=c(16, 2)) +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position = "right") +
  ylab("PC-2 (25.2%)") + xlab("PC-1 (51.4%)") + ggtitle("Summer 2022 Residential and Hospital") # CHANGE LOADINGS AND TITLE EACH TIME

## honestly normally I just put the labels on in ppt becuaes making them work in R is tedious and needs to be done individually




###################### Fall

fall <- df %>%
  subset(Date >="2024-10-01" & Date <= "2024-10-31") %>%
  subset(!site == "BEO") %>%
  subset(sensor_name %in% c("TNHA-SA", "TNHA-SC", "SSMH-SA", "SSMH-SB" )) %>%
  na.omit() %>%
  unique() %>%
  droplevels()

## also where is your time component?

fall$winddirection <- NULL

fallpca <- prcomp(fall[7:11], center=TRUE, scale.=TRUE)

#take out variables
sd <- fallpca$sdev
sd
loads <- fallpca$rotation
loads 
rownames(loads) <- colnames(fall[7:11])

scores <- fallpca$x


var <- sd^2
varPercent <- var/sum(var) * 100

summary(fallpca)
screeplot(fallpca, npcs = 15, type = "lines")
fallpca$u

## rough
autoplot(fallpca, loadings = TRUE, loadings.label = FALSE,
         loadings.colour = "black", loadings.size = .5,
         data = fall, colour = 'aspect') + theme_classic() +
  scale_colour_manual(values=c("red3","blue4", "steelblue", "goldenrod","magenta3" ), name = "site") 


## 36.58 and 27.01
## from Elise's code

loadings <- fallpca$rotation
scores <- as.data.frame(scores)
loadings <- as.data.frame(loadings)
## adding a group to the scores - rownames are presently the X column for fall
scores$aspect <-fall$aspect
loadings$variable <- colnames(fall[c(7:11)])
#scores <- full_join(scores, static %>% dplyr::select("NameID", "divergecat", "species", "correlation_post", "correlation_pre"))

cutoff <- sqrt(1/ncol((fall[c(7:11)]))) # cutoff for 'important' loadings

# cambisol, longitude, SOC, cryosol, clay, bulk density, elevation, sand, latitude, silt,
# OrgC, Nirogen

loadings <- loadings %>%
  subset(abs(PC1) >= cutoff | abs(PC2) >= cutoff)


scores$trueaspect <-  revalue(scores$aspect, c("Residential South" = "South", 
                                               "Residential North" = "North",
                                               "Hospital South" = "South",
                                               "Hospital North" = "North"))

scores$simplesite <-  revalue(scores$aspect, c("Residential South" = "Residential", 
                                               "Residential North" = "Residential",
                                               "Hospital South" = "Hospital",
                                               "Hospital North" = "Hospital"))


library(ggrepel)
group.colors <- c("Residential South" = "darkred", "Residential North" ="tomato", "Hospital North" = "skyblue", "Hospital South" = "steelblue")

ggplot(scores, aes(x=PC1/sd[1], y=PC2/sd[2]))+ #sets up the plot
  geom_point(aes(color = aspect, shape = trueaspect), size=1, alpha = .7) + # add the point markers
  #add vector arrows of significant env variables
  
  geom_polygon(data = scores %>% group_by(aspect) %>% slice(chull(PC1, PC2)),
               aes(fill = aspect, color = aspect, linetype = trueaspect), alpha = 0) + 
  geom_segment(data = loadings, size = 3,
               aes(x = 0, xend=PC1*sd[1]*2, y=0, yend=PC2*sd[2]*2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "black", lwd=0.3) +
  geom_label_repel(data = loadings, 
                   aes(x=PC1*sd[1]*2, y=PC2*sd[2]*2, label = variable), size=5, fill = "white")+ #
  coord_fixed()+ 
  theme_classic() +
  scale_colour_manual(values=group.colors, name = "Site") +
  scale_fill_manual(values=group.colors, name = "Site") +
  scale_shape_manual(values=c(16, 2)) +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position = "right") +
  ylab("PC-2 (24.2%)") + xlab("PC-1 (48.0%)") + ggtitle("Fall 2024 Residential and Hospital")

## honestly normally I just put the labels on in ppt becuaes making them work in R is tedious and needs to be done individually


