#*********
# Code developed by Adriane Esquivel Muelbert
# Summarizing long term data from TerraClimate
# September 2018
#*********

# Objectives:

# (1) Summarize climate information for different time scales [before the census]: 
# (a) within the first year; (b) within secound year; (c) within third year;

# (2) Calculate for temperature:
# (a) mean annual temperature
# (b) max annual temperature (month)
# (c) z-score of based on mean annual temperature
# (d) mean yearly z-score, mean of z-scores for each month

# (3) Calculate for CWD:
# (a) min CWD
# (b) z-score min CWD
# (c) mean z-score CWD - during the interval

rm(list = ls())

#Libraries
library (reshape)

## Set directory ---------
setwd("/Users/esquivea/Dropbox/Leeds/TREMOR/papers/")
source ('02_Trends-plot-parameters/03.3_environment-functions.R')

#Load data =======
pd <- read.csv ('data/03_TM_plot-data_traits_2020-APR-17.csv', row.names = 1, stringsAsFactors=FALSE)
md <- read.csv ('data/02_TM_metadata_2020-APR-17.csv',row.names = 1,stringsAsFactors=FALSE)

CWD <- read.csv ('data/03d_CWD-year_TerraClim2020-APR-17.csv', row.names = 1, stringsAsFactors = FALSE)
TmaxTOplots <- read.csv ('data/03b_tmax-month_terra-clim2020-APR-17.csv')
TminTOplots <- read.csv ('data/03c_tmin-month_terra-clim2020-APR-17.csv')
TmeanTOplots <- read.csv ('data/03e_tmean-month_CRU2020-APR-17.csv')



#***********
# 1. Monthly and Annual values =======
#********

# 1.1 Temperature -----
# a. Summary for the month ------
TmaxMonth <- TEMmonth (TmaxTOplots, md = md)
TminMonth <- TEMmonth (TminTOplots, md = md)
TmeanMonth <- TEMmonth (TmeanTOplots, md = md, data.source = 'CRU')

# b. Summary per year ------
TmaxYear <- TEMyear (TmaxMonth, md = md)
TminYear <- TEMyear (TminMonth, md = md)
TmeanYear <- TEMyear (TmeanMonth, md = md)

# 1.2 CWD -----
CWDYear <- CWDYear (CWD, md = md)

# ****************
# 2 Integrate data with census information ----
# ****************
# a. Temperature data -----------
TmaxCensus <- envCensus (TmaxYear, pd = pd, var = 'TEM')
TminCensus <- envCensus (TminYear, pd = pd, var = 'TEM')
TmeanCensus <- envCensus (TmeanYear, pd = pd, var = 'TEM')

# b. CWD data ----------------
CWDCensus <- envCensus (CWDYear, pd = pd, var = 'CWD')


# OUTPUTS ----
write.csv (CWDCensus, 'data/03.1CWD_census_TerraClimate_2020-APR-17.csv')
write.csv (TmaxCensus, 'data/03.1Tmax_census_TerraClimate_2019-APR-17.csv')
write.csv (TminCensus, 'data/03.1Tmin_census_TerraClimate_2019-APR-17.csv')
write.csv (TmeanCensus, 'data/03.1Tmean_census_CRU_2020-APR-17.csv')

write.csv (TmeanYear,'data/03.2TmeanYearplot.csv')
write.csv (TminYear,'data/03.2TminYearplot.csv')
write.csv (TmaxYear,'data/03.2TmaxYearplot.csv')
write.csv (CWDYear,'data/03.2CWDYearplot.csv')

