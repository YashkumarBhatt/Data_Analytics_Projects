# 🚲 Urban Mobility Trends: Cyclistic Bike-Share Analysis (2024)

## Overview
Analysis of 12 months of real-world bike-sharing data from Cyclistic (Chicago, 2024) 
to uncover behavioral differences between casual riders and annual members. 
Built entirely in R, with automated data ingestion directly from AWS S3.

## Key Objectives
1. **Automated Data Pipeline:** Dynamically downloads and compiles 12 monthly ZIP 
archives from AWS S3 into a single dataframe using `purrr::map_dfr` — no manual 
file handling.
2. **Data Cleaning & Engineering:** Formats datetimes, calculates ride durations, 
filters invalid entries (negative times, missing stations, duplicate IDs).
3. **EDA:** Compares casual riders vs. annual members across ride volume, average 
duration, day-of-week patterns, and monthly seasonality.
4. **Visualization:** Publication-ready charts using `ggplot2`.

## Key Findings
- **Members vs. Casual:** Members dominate total ride volume; casual riders take 
significantly longer trips per ride.
- **Weekly Pattern:** Members ride consistently on weekdays (commuter behavior); 
casual riders peak on weekends.
- **Seasonality:** Both groups peak June–September, with a sharp drop in winter months.
- **Recommendation:** Target casual-to-member conversion campaigns starting May, 
before the summer peak.

## Tech Stack
`R` · `tidyverse` · `ggplot2` · `lubridate` · `purrr` · `scales`

## Dataset
12 months of 2024 Divvy trip data — publicly available via 
[Divvy/AWS S3](https://divvy-tripdata.s3.amazonaws.com). 
Dataset exceeds 1.5GB; not included in this repo. 
The notebook auto-downloads it at runtime.
