#PCA PM!

library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(ggrepel)
library(patchwork)
library(plyr) # for revalue function

# Load your data
PM_ALL <- read.csv("~/Desktop/PM_VIS_TEMP/MERGED_PM_WIND_PRECIP.csv")
PM_ALL <- PM_ALL[, 1:11]

# parse_date_time handles multiple formats automatically
PM_ALL$Date <- parse_date_time(PM_ALL$Date, orders = c("Ymd HMS", "Ymd"))
# Convert Date column
PM_ALL$Date <- as.POSIXct(PM_ALL$Date, format = "%Y-%m-%d %H:%M:%S")

##KEEP BOTH DATE FUNCTIONS

# Make site a factor
PM_ALL$site <- as.factor(PM_ALL$site)

# Create a date-only column for matching for PRECIPITATION
PM_ALL <- PM_ALL %>%
  mutate(date_only = as.Date(Date))

# Check current prcp situation
print("Precipitation NA count before fill:")
print(sum(is.na(PM_ALL$prcp)))

# Create daily precipitation lookup (take the non-NA value for each day)
daily_prcp <- PM_ALL %>%
  filter(!is.na(prcp)) %>%
  select(date_only, prcp) %>%
  distinct()

# Check how many days have precipitation data
print(paste("Days with precipitation data:", nrow(daily_prcp)))

# Join back to fill all hours with daily precipitation
PM_ALL <- PM_ALL %>%
  select(-prcp) %>%  # Remove old prcp column
  left_join(daily_prcp, by = "date_only")

PM_ALL <- PM_ALL %>%
  mutate(prcp = replace_na(prcp, 0))

# Check result
print("Precipitation NA count after fill:")
print(sum(is.na(PM_ALL$prcp)))

######################################## 
# DEFINE ALL SITES (node-03 through node-19)
########################################

sites <- c("node-03", "node-04", "node-05", "node-06", "node-07", 
           "node-08", "node-09", "node-10", "node-11", "node-12",
           "node-13", "node-14", "node-15", "node-16", "node-17",
           "node-18", "node-19")

# Set colors for ALL sites (node-03 through node-19)
group.colors <- c(
  "node-03" = "#E41A1C",
  "node-04" = "#377EB8",
  "node-05" = "#4DAF4A",
  "node-06" = "#984EA3",
  "node-07" = "#FF7F00",
  "node-08" = "#FFFF33",
  "node-09" = "#A65628",
  "node-10" = "#F781BF",
  "node-11" = "#999999",
  "node-12" = "#66C2A5",
  "node-13" = "#FC8D62",
  "node-14" = "#8DA0CB",
  "node-15" = "#E78AC3",
  "node-16" = "#A6D854",
  "node-17" = "#FFD92F",
  "node-18" = "#E5C494",
  "node-19" = "#B3B3B3"
)

######################################## 
# DEFINE PCA COLUMN NAMES (USE THROUGHOUT)
########################################

pca_cols_pm25 <- c("pm25", "Temperature", "WindSpd", "WindDir", "prcp")
pca_cols_pm10 <- c("pm10", "Temperature", "WindSpd", "WindDir", "prcp")

######################################## 
# SUMMER 2025 - PM2.5 PCA (ALL SITES)
########################################

# Start fresh
summer_pm25 <- PM_ALL %>%
  filter(Date >= "2025-06-01" & Date <= "2025-08-31") %>%
  filter(site %in% sites) %>%
  select(Date, site, pm25, Temperature, WindSpd, WindDir, prcp) %>%
  # Force all to numeric
  mutate(
    pm25 = as.numeric(as.character(pm25)),
    Temperature = as.numeric(as.character(Temperature)),
    WindSpd = as.numeric(as.character(WindSpd)),
    WindDir = as.numeric(as.character(WindDir)),
    prcp = as.numeric(as.character(prcp))
  ) %>%
  # Remove ALL problematic rows
  drop_na(pm25, Temperature, WindSpd, WindDir, prcp) %>%
  filter(
    is.finite(pm25),
    is.finite(Temperature),
    is.finite(WindSpd),
    is.finite(WindDir),
    is.finite(prcp)
  )

print(paste("Clean rows:", nrow(summer_pm25)))

# Extract PCA columns using names
pca_data_pm25 <- summer_pm25 %>% select(all_of(pca_cols_pm25))

# Verify completely clean
stopifnot(
  !any(is.na(pca_data_pm25)),
  !any(sapply(pca_data_pm25, is.infinite)),
  !any(sapply(pca_data_pm25, is.nan))
)

print("Data verified clean!")

# Check for zero variance columns
sds <- sapply(pca_data_pm25, sd)
print("Standard deviations:")
print(sds)

if(any(sds == 0)) {
  print("WARNING: Some columns have zero variance!")
  print(names(sds[sds == 0]))
}

# Run PCA
summer_pm25_pca <- prcomp(pca_data_pm25, center = TRUE, scale. = TRUE)

print("PCA Successful!")
summary(summer_pm25_pca)

# Extract PCA components
sd <- summer_pm25_pca$sdev
sd
loads <- summer_pm25_pca$rotation
loads
rownames(loads) <- pca_cols_pm25  # UPDATED: Use named columns
scores <- summer_pm25_pca$x

# Variance explained
var <- sd^2
varPercent <- var/sum(var) * 100
summary(summer_pm25_pca)
screeplot(summer_pm25_pca, npcs = 5, type = "lines")

# Prepare data for plotting
loadings <- as.data.frame(summer_pm25_pca$rotation)
scores <- as.data.frame(scores)
scores$site <- summer_pm25$site
loadings$variable <- pca_cols_pm25

# NO CUTOFF - Show all variables
# (Simply don't filter the loadings dataframe)

# Calculate range including both scores AND loadings for plot limits
x_vals <- c(scores$PC1/sd[1], loadings$PC1*sd[1]*2)
y_vals <- c(scores$PC2/sd[2], loadings$PC2*sd[2]*2)

x_range <- range(x_vals, na.rm = TRUE)

y_range <- range(y_vals, na.rm = TRUE)

x_pad <- diff(x_range) * 0.3
y_pad <- diff(y_range) * 0.3

# PM2.5 PCA Plot (ALL SITES) - Now shows ALL variables
ggplot(scores, aes(x = PC1/sd[1], y = PC2/sd[2])) +
  geom_point(aes(color = site), size = 2, alpha = 0.7) +
  geom_polygon(data = scores %>% group_by(site) %>% slice(chull(PC1, PC2)),
               aes(fill = site, color = site), alpha = 0.1) +
  geom_segment(data = loadings, size = 1,
               aes(x = 0, xend = PC1*sd[1]*2, y = 0, yend = PC2*sd[2]*2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black", lwd = 0.5) +
  geom_label_repel(data = loadings,
                   aes(x = PC1*sd[1]*2, y = PC2*sd[2]*2, label = variable), 
                   size = 5, fill = "white") +
  xlim(x_range[1] - x_pad, x_range[2] + x_pad) +
  ylim(y_range[1] - y_pad, y_range[2] + y_pad) +
  theme_classic() +
  scale_colour_manual(values = group.colors, name = "Site") +
  scale_fill_manual(values = group.colors, name = "Site") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "right") +
  ylab(paste0("PC-2 (", round(varPercent[2], 1), "%)")) + 
  xlab(paste0("PC-1 (", round(varPercent[1], 1), "%)")) + 
  ggtitle("Summer 2025 PM2.5 PCA - All Sites")


######################################## 
# SUMMER 2025 - PM10 PCA (ALL SITES)
########################################

summer_pm10 <- PM_ALL %>%
  filter(Date >= "2025-06-01" & Date <= "2025-08-31") %>%
  filter(site %in% sites) %>%
  select(Date, site, pm10, Temperature, WindSpd, WindDir, prcp) %>%
  # Force all to numeric
  mutate(
    pm10 = as.numeric(as.character(pm10)),
    Temperature = as.numeric(as.character(Temperature)),
    WindSpd = as.numeric(as.character(WindSpd)),
    WindDir = as.numeric(as.character(WindDir)),
    prcp = as.numeric(as.character(prcp))
  ) %>%
  # Remove ALL problematic rows
  drop_na(pm10, Temperature, WindSpd, WindDir, prcp) %>%
  filter(
    is.finite(pm10),
    is.finite(Temperature),
    is.finite(WindSpd),
    is.finite(WindDir),
    is.finite(prcp)
  )

# Extract PCA columns using names
pca_data_pm10 <- summer_pm10 %>% select(all_of(pca_cols_pm10))

# Run PCA
summer_pm10_pca <- prcomp(pca_data_pm10, center = TRUE, scale. = TRUE)

# Extract PCA components
sd_pm10 <- summer_pm10_pca$sdev
sd_pm10
loads_pm10 <- summer_pm10_pca$rotation
loads_pm10
rownames(loads_pm10) <- pca_cols_pm10  # UPDATED: Use named columns
scores_pm10 <- summer_pm10_pca$x

# Variance explained
var_pm10 <- sd_pm10^2
varPercent_pm10 <- var_pm10/sum(var_pm10) * 100
summary(summer_pm10_pca)
screeplot(summer_pm10_pca, npcs = 5, type = "lines")

# Prepare data for plotting
loadings_pm10 <- as.data.frame(summer_pm10_pca$rotation)
scores_pm10 <- as.data.frame(scores_pm10)
scores_pm10$site <- summer_pm10$site
loadings_pm10$variable <- pca_cols_pm10  # UPDATED: Use named columns

# Cutoff for important loadings
cutoff_pm10 <- sqrt(1/length(pca_cols_pm10))  # UPDATED: Use length of named columns

loadings_pm10 <- loadings_pm10 %>%
  subset(abs(PC1) >= cutoff_pm10 | abs(PC2) >= cutoff_pm10)

# Calculate range including both scores AND loadings for plot limits
x_vals_pm10 <- c(scores_pm10$PC1/sd_pm10[1], loadings_pm10$PC1*sd_pm10[1]*2)
y_vals_pm10 <- c(scores_pm10$PC2/sd_pm10[2], loadings_pm10$PC2*sd_pm10[2]*2)

x_range_pm10 <- range(x_vals_pm10, na.rm = TRUE)
y_range_pm10 <- range(y_vals_pm10, na.rm = TRUE)

x_pad_pm10 <- diff(x_range_pm10) * 0.3
y_pad_pm10 <- diff(y_range_pm10) * 0.3

# PM10 PCA Plot (ALL SITES)
ggplot(scores_pm10, aes(x = PC1/sd_pm10[1], y = PC2/sd_pm10[2])) +
  geom_point(aes(color = site), size = 2, alpha = 0.7) +
  geom_polygon(data = scores_pm10 %>% group_by(site) %>% slice(chull(PC1, PC2)),
               aes(fill = site, color = site), alpha = 0.1) +
  geom_segment(data = loadings_pm10, size = 1,
               aes(x = 0, xend = PC1*sd_pm10[1]*2, y = 0, yend = PC2*sd_pm10[2]*2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black", lwd = 0.5) +
  geom_label_repel(data = loadings_pm10,
                   aes(x = PC1*sd_pm10[1]*2, y = PC2*sd_pm10[2]*2, label = variable), 
                   size = 5, fill = "white") +
  xlim(x_range_pm10[1] - x_pad_pm10, x_range_pm10[2] + x_pad_pm10) +
  ylim(y_range_pm10[1] - y_pad_pm10, y_range_pm10[2] + y_pad_pm10) +
  theme_classic() +
  scale_colour_manual(values = group.colors, name = "Site") +
  scale_fill_manual(values = group.colors, name = "Site") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "right") +
  ylab(paste0("PC-2 (", round(varPercent_pm10[2], 1), "%)")) + 
  xlab(paste0("PC-1 (", round(varPercent_pm10[1], 1), "%)")) + 
  ggtitle("Summer 2025 PM10 PCA - All Sites")


########### ALL SITES SEPARATED NOW ################

######################################## 
# FUNCTION TO CREATE PCA FOR EACH SITE
########################################

create_site_pca <- function(data, site_name, pm_type = "pm25") {
  
  # Define PCA columns based on PM type
  if (pm_type == "pm25") {
    pca_cols <- c("pm25", "Temperature", "WindSpd", "WindDir", "prcp")
    pm_label <- "PM2.5"
    
    site_data <- data %>%
      filter(Date >= "2025-06-01" & Date <= "2025-08-31") %>%
      filter(site == site_name) %>%
      select(Date, site, pm25, Temperature, WindSpd, WindDir, prcp) %>%
      mutate(
        pm25 = as.numeric(as.character(pm25)),
        Temperature = as.numeric(as.character(Temperature)),
        WindSpd = as.numeric(as.character(WindSpd)),
        WindDir = as.numeric(as.character(WindDir)),
        prcp = as.numeric(as.character(prcp))
      ) %>%
      drop_na(pm25, Temperature, WindSpd, WindDir, prcp) %>%
      filter(
        is.finite(pm25),
        is.finite(Temperature),
        is.finite(WindSpd),
        is.finite(WindDir),
        is.finite(prcp)
      ) %>%
      unique() %>%
      droplevels()
    
  } else {
    pca_cols <- c("pm10", "Temperature", "WindSpd", "WindDir", "prcp")
    pm_label <- "PM10"
    
    site_data <- data %>%
      filter(Date >= "2025-06-01" & Date <= "2025-08-31") %>%
      filter(site == site_name) %>%
      select(Date, site, pm10, Temperature, WindSpd, WindDir, prcp) %>%
      mutate(
        pm10 = as.numeric(as.character(pm10)),
        Temperature = as.numeric(as.character(Temperature)),
        WindSpd = as.numeric(as.character(WindSpd)),
        WindDir = as.numeric(as.character(WindDir)),
        prcp = as.numeric(as.character(prcp))
      ) %>%
      drop_na(pm10, Temperature, WindSpd, WindDir, prcp) %>%
      filter(
        is.finite(pm10),
        is.finite(Temperature),
        is.finite(WindSpd),
        is.finite(WindDir),
        is.finite(prcp)
      ) %>%
      unique() %>%
      droplevels()
  }
  
  # Check if enough data points exist
  if (nrow(site_data) < 10) {
    message(paste("Not enough data for site:", site_name))
    return(NULL)
  }
  
  # Extract PCA data using column names
  pca_data <- site_data %>% select(all_of(pca_cols))
  
  # Run PCA
  site_pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)
  
  # Extract PCA components
  sd <- site_pca$sdev
  loads <- site_pca$rotation
  rownames(loads) <- pca_cols  # UPDATED: Use named columns
  scores <- site_pca$x
  
  # Variance explained
  var <- sd^2
  varPercent <- var/sum(var) * 100
  
  # Prepare data for plotting
  loadings <- as.data.frame(site_pca$rotation)
  scores <- as.data.frame(scores)
  scores$site <- site_data$site
  loadings$variable <- pca_cols  # UPDATED: Use named columns
  
  # Cutoff for important loadings
  cutoff <- sqrt(1/length(pca_cols))  # UPDATED: Use length of named columns
  
  loadings <- loadings %>%
    subset(abs(PC1) >= cutoff | abs(PC2) >= cutoff)
  
  # Get site-specific color
  site_color <- group.colors[site_name]
  
  # Calculate range including both scores AND loadings for plot limits
  x_vals <- c(scores$PC1/sd[1], loadings$PC1*sd[1]*2)
  y_vals <- c(scores$PC2/sd[2], loadings$PC2*sd[2]*2)
  
  x_range <- range(x_vals, na.rm = TRUE)
  y_range <- range(y_vals, na.rm = TRUE)
  
  x_pad <- diff(x_range) * 0.3
  y_pad <- diff(y_range) * 0.3
  
  # Create plot
  p <- ggplot(scores, aes(x = PC1/sd[1], y = PC2/sd[2])) +
    geom_point(size = 2, alpha = 0.7, color = site_color) +
    geom_polygon(data = scores %>% slice(chull(PC1, PC2)),
                 fill = site_color, color = site_color, alpha = 0.1) +
    geom_segment(data = loadings, size = 1,
                 aes(x = 0, xend = PC1*sd[1]*2, y = 0, yend = PC2*sd[2]*2),
                 arrow = arrow(length = unit(0.25, "cm")), colour = "black", lwd = 0.5) +
    geom_label_repel(data = loadings,
                     aes(x = PC1*sd[1]*2, y = PC2*sd[2]*2, label = variable), 
                     size = 5, fill = "white") +
    xlim(x_range[1] - x_pad, x_range[2] + x_pad) +
    ylim(y_range[1] - y_pad, y_range[2] + y_pad) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5)) +
    ylab(paste0("PC-2 (", round(varPercent[2], 1), "%)")) + 
    xlab(paste0("PC-1 (", round(varPercent[1], 1), "%)")) + 
    ggtitle(paste0("Summer 2025 ", pm_label, " PCA - ", site_name))
  
  return(p)
}

######################################## 
# GENERATE PM2.5 PCA FOR EACH SITE
########################################

pm25_plots <- list()

for (site_name in sites) {
  pm25_plots[[site_name]] <- create_site_pca(PM_ALL, site_name, pm_type = "pm25")
}

# Print each PM2.5 plot
for (site_name in sites) {
  if (!is.null(pm25_plots[[site_name]])) {
    print(pm25_plots[[site_name]])
  }
}

######################################## 
# GENERATE PM10 PCA FOR EACH SITE
########################################

pm10_plots <- list()

for (site_name in sites) {
  pm10_plots[[site_name]] <- create_site_pca(PM_ALL, site_name, pm_type = "pm10")
}

# Print each PM10 plot
for (site_name in sites) {
  if (!is.null(pm10_plots[[site_name]])) {
    print(pm10_plots[[site_name]])
  }
}

######################################## 
# COMBINE ALL PLOTS INTO ONE FIGURE
########################################

# Combine all PM2.5 plots
pm25_combined <- wrap_plots(pm25_plots[!sapply(pm25_plots, is.null)], ncol = 4) +
  plot_annotation(title = "Summer 2025 PM2.5 PCA by Site (node-03 to node-19)",
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))

print(pm25_combined)

# Combine all PM10 plots
pm10_combined <- wrap_plots(pm10_plots[!sapply(pm10_plots, is.null)], ncol = 4) +
  plot_annotation(title = "Summer 2025 PM10 PCA by Site (node-03 to node-19)",
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))

print(pm10_combined)

######################################## 
# SAVE INDIVIDUAL PLOTS
########################################

# Save PM2.5 plots
for (site_name in sites) {
  if (!is.null(pm25_plots[[site_name]])) {
    ggsave(
      filename = paste0("PM25_PCA_Summer2025_", site_name, ".png"),
      plot = pm25_plots[[site_name]],
      width = 8,
      height = 6,
      dpi = 300
    )
  }
}

# Save PM10 plots
for (site_name in sites) {
  if (!is.null(pm10_plots[[site_name]])) {
    ggsave(
      filename = paste0("PM10_PCA_Summer2025_", site_name, ".png"),
      plot = pm10_plots[[site_name]],
      width = 8,
      height = 6,
      dpi = 300
    )
  }
}

# Save combined plots (adjusted size for 17 sites)
ggsave("PM25_PCA_Summer2025_AllSites.png", pm25_combined, width = 32, height = 28, dpi = 300)
ggsave("PM10_PCA_Summer2025_AllSites.png", pm10_combined, width = 32, height = 28, dpi = 300)