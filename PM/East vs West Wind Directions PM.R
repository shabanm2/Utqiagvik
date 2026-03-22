---
title: "East vs West Wind Directions PM"
author: "Mirella Shaban"
date: "2026-03-16"
output: html_document
---
  
  library(dplyr)
library(tidyverse)
library(ggplot2)

#East vs West Wind Directions PM
#Code updated utilizing Claude 4.5

PM_ALL <- read.csv("~/Desktop/PM_VIS_TEMP/MERGED_PM_WIND_PRECIP.csv")
# Convert Date column if needed
PM_ALL$Date <- as.POSIXct(PM_ALL$Date, format = "%Y-%m-%d %H:%M:%S")

######################################## 
# DEFINE WHO THRESHOLDS
########################################

# WHO Air Quality Guidelines (2021)
# 24-hour mean thresholds:
WHO_PM25_threshold <- 15  # µg/m³ 
WHO_PM10_threshold <- 45  # µg/m³ 

######################################## 
# DEFINE WIND DIRECTION CATEGORIES
########################################

# Wind direction convention:
# North: 0° or 360°
# East: 90°
# South: 180°
# West: 270°

# East winds (coming FROM the east): 45° to 135°
# West winds (coming FROM the west): 225° to 315°

# Define weak wind threshold (adjust as needed)
weak_wind_threshold <- 2  # m/s (winds below this are considered "weak")

######################################## 
# CREATE WIND ANALYSIS DATASET
########################################

PM_wind <- PM_ALL %>%
  # Create exceedance columns based on WHO thresholds
  mutate(
    exceeds_WHO_PM25 = pm25 > WHO_PM25_threshold,
    exceeds_WHO_PM10 = pm10 > WHO_PM10_threshold
  ) %>%
  # Remove weak winds
  filter(WindSpd >= weak_wind_threshold) %>%
  # Categorize wind direction
  mutate(
    wind_category = case_when(
      WindDir >= 45 & WindDir <= 135 ~ "East",
      WindDir >= 225 & WindDir <= 315 ~ "West",
      TRUE ~ "Other"
    )
  ) %>%
  # Keep only East and West winds
  filter(wind_category %in% c("East", "West"))

######################################## 
# SUMMARY STATISTICS BY WIND DIRECTION
########################################

# Overall summary for PM2.5
pm25_wind_summary <- PM_wind %>%
  group_by(wind_category) %>%
  summarise(
    count = n(),
    mean_pm25 = mean(pm25, na.rm = TRUE),
    median_pm25 = median(pm25, na.rm = TRUE),
    sd_pm25 = sd(pm25, na.rm = TRUE),
    min_pm25 = min(pm25, na.rm = TRUE),
    max_pm25 = max(pm25, na.rm = TRUE),
    exceedances_pm25 = sum(exceeds_WHO_PM25 == TRUE, na.rm = TRUE),
    exceedance_rate_pm25 = mean(exceeds_WHO_PM25 == TRUE, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("===== PM2.5 Summary by Wind Direction =====")
print(paste0("WHO PM2.5 Threshold: ", WHO_PM25_threshold, " µg/m³"))
print(pm25_wind_summary)

# Overall summary for PM10
pm10_wind_summary <- PM_wind %>%
  group_by(wind_category) %>%
  dplyr::summarise(
    count = n(),
    mean_pm10 = mean(pm10, na.rm = TRUE),
    median_pm10 = median(pm10, na.rm = TRUE),
    sd_pm10 = sd(pm10, na.rm = TRUE),
    min_pm10 = min(pm10, na.rm = TRUE),
    max_pm10 = max(pm10, na.rm = TRUE),
    exceedances_pm10 = sum(exceeds_WHO_PM10 == TRUE, na.rm = TRUE),
    exceedance_rate_pm10 = mean(exceeds_WHO_PM10 == TRUE, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("===== PM10 Summary by Wind Direction =====")
print(paste0("WHO PM10 Threshold: ", WHO_PM10_threshold, " µg/m³"))
print(pm10_wind_summary)

# Combined PM2.5 and PM10 summary
combined_wind_summary <- PM_wind %>%
  group_by(wind_category) %>%
  dplyr::summarise(
    count = n(),
    mean_pm25 = mean(pm25, na.rm = TRUE),
    mean_pm10 = mean(pm10, na.rm = TRUE),
    exceedances_pm25 = sum(exceeds_WHO_PM25 == TRUE, na.rm = TRUE),
    exceedance_rate_pm25 = mean(exceeds_WHO_PM25 == TRUE, na.rm = TRUE) * 100,
    exceedances_pm10 = sum(exceeds_WHO_PM10 == TRUE, na.rm = TRUE),
    exceedance_rate_pm10 = mean(exceeds_WHO_PM10 == TRUE, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("===== Combined PM2.5 and PM10 Summary by Wind Direction =====")
print(combined_wind_summary)

######################################## 
# SUMMARY BY SITE AND WIND DIRECTION
########################################

# PM2.5 by site and wind direction
pm25_site_wind_summary <- PM_wind %>%
  group_by(site, wind_category) %>%
  dplyr::summarise(
    count = n(),
    mean_pm25 = mean(pm25, na.rm = TRUE),
    median_pm25 = median(pm25, na.rm = TRUE),
    sd_pm25 = sd(pm25, na.rm = TRUE),
    exceedances_pm25 = sum(exceeds_WHO_PM25 == TRUE, na.rm = TRUE),
    exceedance_rate_pm25 = mean(exceeds_WHO_PM25 == TRUE, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("===== PM2.5 Summary by Site and Wind Direction =====")
print(pm25_site_wind_summary)

# PM10 by site and wind direction
pm10_site_wind_summary <- PM_wind %>%
  group_by(site, wind_category) %>%
  dplyr::summarise(
    count = n(),
    mean_pm10 = mean(pm10, na.rm = TRUE),
    median_pm10 = median(pm10, na.rm = TRUE),
    sd_pm10 = sd(pm10, na.rm = TRUE),
    exceedances_pm10 = sum(exceeds_WHO_PM10 == TRUE, na.rm = TRUE),
    exceedance_rate_pm10 = mean(exceeds_WHO_PM10 == TRUE, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("===== PM10 Summary by Site and Wind Direction =====")
print(pm10_site_wind_summary)

######################################## 
# VISUALIZATIONS
########################################

# 1. Boxplot comparing PM2.5 by wind direction
p1 <- ggplot(PM_wind, aes(x = wind_category, y = pm25, fill = wind_category)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = WHO_PM25_threshold, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 0.5, y = WHO_PM25_threshold + 2, 
           label = paste0("WHO Threshold: ", WHO_PM25_threshold, " µg/m³"), 
           hjust = 0, color = "red", size = 4) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "PM2.5 Levels by Wind Direction",
    subtitle = paste0("Weak winds (<", weak_wind_threshold, " m/s) removed"),
    x = "Wind Direction",
    y = "PM2.5 (µg/m³)",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

print(p1)

# 2. Boxplot comparing PM10 by wind direction
p2 <- ggplot(PM_wind, aes(x = wind_category, y = pm10, fill = wind_category)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = WHO_PM10_threshold, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 0.5, y = WHO_PM10_threshold + 2, 
           label = paste0("WHO Threshold: ", WHO_PM10_threshold, " µg/m³"), 
           hjust = 0, color = "red", size = 4) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "PM10 Levels by Wind Direction",
    subtitle = paste0("Weak winds (<", weak_wind_threshold, " m/s) removed"),
    x = "Wind Direction",
    y = "PM10 (µg/m³)",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

print(p2)

# 3. Boxplot comparing PM2.5 by wind direction AND site
p3 <- ggplot(PM_wind, aes(x = site, y = pm25, fill = wind_category)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = WHO_PM25_threshold, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "PM2.5 Levels by Site and Wind Direction",
    subtitle = paste0("Weak winds (<", weak_wind_threshold, " m/s) removed | WHO threshold: ", WHO_PM25_threshold, " µg/m³"),
    x = "Site",
    y = "PM2.5 (µg/m³)",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "right"
  )

print(p3)

# 4. Boxplot comparing PM10 by wind direction AND site
p4 <- ggplot(PM_wind, aes(x = site, y = pm10, fill = wind_category)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = WHO_PM10_threshold, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "PM10 Levels by Site and Wind Direction",
    subtitle = paste0("Weak winds (<", weak_wind_threshold, " m/s) removed | WHO threshold: ", WHO_PM10_threshold, " µg/m³"),
    x = "Site",
    y = "PM10 (µg/m³)",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "right"
  )

print(p4)

# 5. Bar chart of PM2.5 exceedances by wind direction
exceedance_pm25_summary <- PM_wind %>%
  group_by(wind_category) %>%
  dplyr::summarise(
    total_observations = n(),
    exceedances = sum(exceeds_WHO_PM25 == TRUE, na.rm = TRUE),
    non_exceedances = sum(exceeds_WHO_PM25 == FALSE, na.rm = TRUE),
    exceedance_rate = exceedances / total_observations * 100,
    .groups = "drop"
  )

p5 <- ggplot(exceedance_pm25_summary, aes(x = wind_category, y = exceedances, fill = wind_category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = paste0(exceedances, "\n(", round(exceedance_rate, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "PM2.5 WHO Exceedances by Wind Direction",
    subtitle = paste0("WHO PM2.5 Threshold: ", WHO_PM25_threshold, " µg/m³ | Weak winds (<", weak_wind_threshold, " m/s) removed"),
    x = "Wind Direction",
    y = "Number of Exceedances",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  ylim(0, max(exceedance_pm25_summary$exceedances) * 1.3)

print(p5)

# 6. Bar chart of PM10 exceedances by wind direction
exceedance_pm10_summary <- PM_wind %>%
  group_by(wind_category) %>%
  dplyr::summarise(
    total_observations = n(),
    exceedances = sum(exceeds_WHO_PM10 == TRUE, na.rm = TRUE),
    non_exceedances = sum(exceeds_WHO_PM10 == FALSE, na.rm = TRUE),
    exceedance_rate = exceedances / total_observations * 100,
    .groups = "drop"
  )

p6 <- ggplot(exceedance_pm10_summary, aes(x = wind_category, y = exceedances, fill = wind_category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = paste0(exceedances, "\n(", round(exceedance_rate, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "PM10 WHO Exceedances by Wind Direction",
    subtitle = paste0("WHO PM10 Threshold: ", WHO_PM10_threshold, " µg/m³ | Weak winds (<", weak_wind_threshold, " m/s) removed"),
    x = "Wind Direction",
    y = "Number of Exceedances",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  ylim(0, max(exceedance_pm10_summary$exceedances) * 1.3)

print(p6)

# 7. Combined exceedances bar chart (PM2.5 and PM10 side by side)
exceedance_combined <- bind_rows(
  exceedance_pm25_summary %>% mutate(pollutant = "PM2.5"),
  exceedance_pm10_summary %>% mutate(pollutant = "PM10")
)

p7 <- ggplot(exceedance_combined, aes(x = wind_category, y = exceedances, fill = pollutant)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_text(aes(label = exceedances), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("PM2.5" = "purple", "PM10" = "darkgreen")) +
  theme_bw() +
  labs(
    title = "WHO Exceedances by Wind Direction",
    subtitle = paste0("PM2.5 threshold: ", WHO_PM25_threshold, " µg/m³ | PM10 threshold: ", WHO_PM10_threshold, " µg/m³"),
    x = "Wind Direction",
    y = "Number of Exceedances",
    fill = "Pollutant"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right"
  ) +
  ylim(0, max(exceedance_combined$exceedances) * 1.3)

print(p7)

# 8. Histogram of PM2.5 by wind direction with threshold line
p8 <- ggplot(PM_wind, aes(x = pm25, fill = wind_category)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  geom_vline(xintercept = WHO_PM25_threshold, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = WHO_PM25_threshold + 2, y = Inf, 
           label = paste0("WHO: ", WHO_PM25_threshold), 
           vjust = 2, hjust = 0, color = "red", size = 4) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "Distribution of PM2.5 by Wind Direction",
    subtitle = paste0("Weak winds (<", weak_wind_threshold, " m/s) removed"),
    x = "PM2.5 (µg/m³)",
    y = "Count",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right"
  )

print(p8)

# 9. Histogram of PM10 by wind direction with threshold line
p9 <- ggplot(PM_wind, aes(x = pm10, fill = wind_category)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  geom_vline(xintercept = WHO_PM10_threshold, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = WHO_PM10_threshold + 2, y = Inf, 
           label = paste0("WHO: ", WHO_PM10_threshold), 
           vjust = 2, hjust = 0, color = "red", size = 4) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "Distribution of PM10 by Wind Direction",
    subtitle = paste0("Weak winds (<", weak_wind_threshold, " m/s) removed"),
    x = "PM10 (µg/m³)",
    y = "Count",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right"
  )

print(p9)

# 10. Density plot of PM2.5 by wind direction
p10 <- ggplot(PM_wind, aes(x = pm25, fill = wind_category)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = WHO_PM25_threshold, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "Density Distribution of PM2.5 by Wind Direction",
    subtitle = paste0("Weak winds (<", weak_wind_threshold, " m/s) removed | WHO threshold: ", WHO_PM25_threshold, " µg/m³"),
    x = "PM2.5 (µg/m³)",
    y = "Density",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right"
  )

print(p10)

# 11. Density plot of PM10 by wind direction
p11 <- ggplot(PM_wind, aes(x = pm10, fill = wind_category)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = WHO_PM10_threshold, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("East" = "tomato", "West" = "steelblue")) +
  theme_bw() +
  labs(
    title = "Density Distribution of PM10 by Wind Direction",
    subtitle = paste0("Weak winds (<", weak_wind_threshold, " m/s) removed | WHO threshold: ", WHO_PM10_threshold, " µg/m³"),
    x = "PM10 (µg/m³)",
    y = "Density",
    fill = "Wind Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right"
  )

print(p11)

######################################## 
# STATISTICAL TESTS: EAST VS WEST
########################################

# T-test for PM2.5
pm25_east <- PM_wind %>% filter(wind_category == "East") %>% pull(pm25)
pm25_west <- PM_wind %>% filter(wind_category == "West") %>% pull(pm25)

pm25_ttest <- t.test(pm25_east, pm25_west)
print("===== T-test for PM2.5 (East vs West) =====")
print(pm25_ttest)

# T-test for PM10
pm10_east <- PM_wind %>% filter(wind_category == "East") %>% pull(pm10)
pm10_west <- PM_wind %>% filter(wind_category == "West") %>% pull(pm10)

pm10_ttest <- t.test(pm10_east, pm10_west)
print("===== T-test for PM10 (East vs West) =====")
print(pm10_ttest)

# Wilcoxon test (non-parametric alternative)
pm25_wilcox <- wilcox.test(pm25_east, pm25_west)
print("===== Wilcoxon test for PM2.5 (East vs West) =====")
print(pm25_wilcox)

pm10_wilcox <- wilcox.test(pm10_east, pm10_west)
print("===== Wilcoxon test for PM10 (East vs West) =====")
print(pm10_wilcox)

# Chi-square test for exceedances
print("===== Chi-square test for PM2.5 Exceedances =====")
pm25_exceedance_table <- table(PM_wind$wind_category, PM_wind$exceeds_WHO_PM25)
print(pm25_exceedance_table)
print(chisq.test(pm25_exceedance_table))

print("===== Chi-square test for PM10 Exceedances =====")
pm10_exceedance_table <- table(PM_wind$wind_category, PM_wind$exceeds_WHO_PM10)
print(pm10_exceedance_table)
print(chisq.test(pm10_exceedance_table))

######################################## 
# EXPORT SUMMARY TABLES
########################################

write.csv(pm25_wind_summary, "PM25_Wind_Direction_Summary.csv", row.names = FALSE)
write.csv(pm10_wind_summary, "PM10_Wind_Direction_Summary.csv", row.names = FALSE)
write.csv(combined_wind_summary, "Combined_PM_Wind_Direction_Summary.csv", row.names = FALSE)
write.csv(pm25_site_wind_summary, "PM25_Site_Wind_Direction_Summary.csv", row.names = FALSE)
write.csv(pm10_site_wind_summary, "PM10_Site_Wind_Direction_Summary.csv", row.names = FALSE)
write.csv(exceedance_pm25_summary, "PM25_Exceedance_Wind_Direction_Summary.csv", row.names = FALSE)
write.csv(exceedance_pm10_summary, "PM10_Exceedance_Wind_Direction_Summary.csv", row.names = FALSE)

######################################## 
# PRINT FINAL SUMMARY
########################################

print("=====================================")
print("        FINAL SUMMARY REPORT         ")
print("=====================================")
print(paste0("WHO PM2.5 Threshold: ", WHO_PM25_threshold, " µg/m³"))
print(paste0("WHO PM10 Threshold: ", WHO_PM10_threshold, " µg/m³"))
print(paste0("Weak Wind Threshold: ", weak_wind_threshold, " m/s"))
print(paste0("Total observations after filtering: ", nrow(PM_wind)))
print("-------------------------------------")
print("PM2.5 Summary:")
print(pm25_wind_summary)
print("-------------------------------------")
print("PM10 Summary:")
print(pm10_wind_summary)
print("=====================================")
