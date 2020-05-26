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
source ('3_environment-functions.R')

#Load data =======
pd <- read.csv ('pd_example.csv',stringsAsFactors=FALSE)
md <- read.csv ('md_example.csv',stringsAsFactors=FALSE)

CWD <- read.csv ('d_CWD-year_TerraClim.csv', row.names = 1, stringsAsFactors = FALSE)
TmaxTOplots <- read.csv ('b_tmax-month_terra-clim.csv')
TminTOplots <- read.csv ('c_tmin-month_terra-clim.csv')
TmeanTOplots <- read.csv ('e_tmean-month_CRU.csv')



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
CWDYear <- CalcCWDYear (CWD, md = md)

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
write.csv (CWDCensus, '1CWD_census_TerraClimate_2020-APR-17.csv')
write.csv (TmaxCensus, '1Tmax_census_TerraClimate_2019-APR-17.csv')
write.csv (TminCensus, '1Tmin_census_TerraClimate_2019-APR-17.csv')
write.csv (TmeanCensus, '03.1Tmean_census_CRU_2020-APR-17.csv')

write.csv (TmeanYear,'2TmeanYearplot.csv')
write.csv (TminYear,'2TminYearplot.csv')
write.csv (TmaxYear,'2TmaxYearplot.csv')
write.csv (CWDYear,'2CWDYearplot.csv')

