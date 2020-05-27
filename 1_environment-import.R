#*********
# Code developed by Adriane Esquivel Muelbert
# Extract climate data using lat long information
# Importing mean (CRU), max, min (Terra climate) temperatura 
# Calculating montly values of CWD per plots using TerraClimate data
# Did not run on Windows in 2018 - ncdf4 was only working in Mac or Linus 
#*********


#Use raster library for manipulation
library(raster); library (ncdf4); library (chron); library (getCRUCLdata)
library ('R.utils'); 
library(fs)

# Upload functions file --- 
source ('functions/environment-functions.R')
# functions from this file to be used here: getClimateTerra, getCRU


# Set up -------
# URL where CRU data are stored
cru.url <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.04/cruts.2004151855.v4.04/tmp/cru_ts4.04.1901.2019.tmp.dat.nc.gz" # this goes up to 2019

# start and end of MCWD calculation
start.date <- 1959.01
end.date <- 2019.12

## Set directory ---------

#Load data =======
md <- read.csv ('md_example.csv') 
# Make coordinates very precise so it can match coordinates from NCDF
md$Longitude.Decimal<- md$Longitude.Decimal + 0.000001
md$Latitude.Decimal<- md$Latitude.Decimal + 0.000001

############
# Extracting climate data based on lat long coordinates -------
############

PreTOplots <- getClimateTerra ('ppt', md = md) # each of these take between 1-5 minutes with example data
TmaxTOplots <- getClimateTerra ('tmax', md = md)
TminTOplots <- getClimateTerra ('tmin', md = md)

write.csv (PreTOplots,'a_precip-month_terra-clim.csv')
write.csv (TmaxTOplots,'b_tmax-month_terra-clim.csv')
write.csv (TminTOplots,'c_tmin-month_terra-clim.csv')


#*********************
# CRU data for mean temperature ------
#*********************

cruTOplots <- getCRU  (url = cru.url, md) # This take between 3 to 10 minutes
write.csv (cruTOplots,'e_tmean-month_CRU.csv')


############
# Calculate MCWD -------
############

# Calculate water deficit based on 100 mm of evapotranspiration ======
Wdef <- PreTOplots - 100
Wdef <- as.matrix (Wdef)

# Start MCWD calculating from wettest month - north and south hemespheres have dry season starting at different times
# get information from wettest month - average across all years for each point
months <- rep (c(1:12),ncol (Wdef)/12)
max.W <- vector ()

for (i in 1:nrow (Wdef)) {
        a <- cbind (months, Wdef[i,])
        b <- (aggregate (a[,2] ~ months, data = a, FUN = mean))
        max.W [i] <- b[which (b[,2]==max (b[,2])),'months'] 
}

# start MCWD calculating from January when wettest month is in December, i.e. wet season in between years 
max.W[max.W==12] <- 1 # if the wettest month is DEC wet season is between years, therefore start calculating CWD from JAN
max.W_plots <- data.frame (max.W, Plot.Code = md[,'Plot.Code'], lat = md[,'Latitude.Decimal'])


# calculate CWD --- 
CWD<-as.data.frame(matrix(nrow=nrow (Wdef),ncol=ncol (Wdef)/12-1))
years.cwd <- sort (rep (seq (start.date,end.date, by=1),12)) # this will not run if end.date is not correct - pay attention to message from 'getClimateTerra'


colnames (CWD) <- unique (years.cwd)
a <- vector ()

#
for (i in 1:nrow (Wdef)){
  # This should not take long to run, a few seconds with the example data
        a <- Wdef[i,]
        j <- max.W[i]
        if (j==1){
          # remove the first year (1958)
          a <- a[-c(1:12)] 
        } else {
          # remove first 1/2 dry period (jan to the wettest month)
          # and the last 1/2 dry season (wetters month to december)
          a <- a[-c(1:(j-1), (length (a)-(12-j)):length (a))] 
        }
        
        a <- cbind (a,years.cwd)
        rownames (a) <- NULL
        CWD[i,]<- aggregate (a[,1]~a[,2], FUN = yearlyCWD)[,2]
        
}

rownames (CWD) <- md$Plot.Code

write.csv (CWD,'d_CWD-year_TerraClim.csv')

