# November 8, 2023

## Combined Raw Data

**Variables:**
- Ground temperature
- Air temperature
- Solar Radiation
- Volumetric Water Content (VWC) (Ground Moisture)
- Wind Speed
- Wind Direction

**Date Range: Summer 2022 - Summer 2023**

Code File: Combine_Raw_Data.R

### Notes:
- Each observation labeled by Date/Time (UTC), sensor serial number (Station, Sensor, Depth)
- Sensor naming convention changed on March 5, 2023 to include "_SITE" at the end of the name
- Sensor names include variable indicators that are removed for cleanup
- Zeroes are removed from Ground Temperature and VWC if zeroes were measured across all sensor depths