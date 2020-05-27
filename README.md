# climate_data
codes to download climate data (temperature and precipitation) and calculate MCWD

## 1_environment-import.R 
Import climate data from the online repositories (CRU for mean temperature and terraclimate for max temperature, min temperature and precipitation) for points (latitude and longitude)

## 2_environment-summary.R
Uses products generated in *1_environment-import.R* to calculate summary values per year, per month and within censuses from tree inventory plots.

## 3_environment-trends.R

Use products from *2_environment-summary.R* to calculate the linear trends for each of the variables per plot and per cluster (group of plots based on plot codes from the ForestPlots.net database)

## functions/environment-functions.R
Functions for the other codes

## Input data 

### md example 
Plot coordinates in decimal degrees. 
This file can be replaced by the metadata file from ForestPlots.net 

### censusd example
File with information on each census where:
*Plot.Code* has the code for the inventory plot
*Census.Date* represents the date of the census as decimal date
*plot.census* has the code for the inventory plot plus the census number
*Census.Date0* is the date of the previous census. 
*Census.no* census number

Note that heading follow names used in ForestPlots.net

Columns: explain 
