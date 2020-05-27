# climate_data
codes to download climate data (temperature and precipitation) and calculate MCWD

## 1_environment-import.R 
Import climate data from the online repositories (CRU for mean temperature and terraclimate for max and min temperature and precipitation) for points (latitude and longitude)

## 2_environment-summary.R
Uses products generated in '1_environment-import.R' to calculate summary values per year, per month and within censuses from tree inventory plots.

## 3_environment-functions.R
Functions for the other codes

## 4_environment-trends.R
Use products from '2_environment-summary.R' to calculate the linear trends per for each of the variables per plot and per cluster (group of plots based on plot codes from the ForestPlots.net database)
