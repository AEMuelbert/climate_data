#*********
# Code developed by Adriane Esquivel Muelbert
# Calculating montly values of CWD per plots using TerraClimate data
# September 2018
# Extract climate data using lat long information
# Would not run in Windowns - ncdf4 only works in Mac or Linus
#*********

rm(list = ls())

#Use raster library for manipulation
library(raster); library (ncdf4); library (chron)

## Set directory ---------
setwd('/Users/esquivea/Dropbox/Leeds/TREMOR/papers')
source ('02_Trends-plot-parameters/03.3_environment-functions.R')

#Load data =======
md <- read.csv ('data/02_TM_metadata_2020-APR-17.csv',  row.names = 1)
#md <- read.csv ('02_TM_metadata_2020-APR-17.csv',  row.names = 1)
# Make coordinates very precise so it can match coordinates from NCDF
md$Longitude.Decimal<- md$Longitude.Decimal + 0.000001
md$Latitude.Decimal<- md$Latitude.Decimal + 0.000001

############
# Extracting climate data based on lat long coordinates -------
############

PreTOplots <- getClimateTerra ('ppt', md = md)
TmaxTOplots <- getClimateTerra ('tmax', md = md)
TminTOplots <- getClimateTerra ('tmin', md = md)

write.csv (PreTOplots,'data/03a_precip-month_terra-clim2020-APR-17.csv')
write.csv (TmaxTOplots,'data/03b_tmax-month_terra-clim2020-APR-17.csv')
write.csv (TminTOplots,'data/03c_tmin-month_terra-clim2020-APR-17.csv')

#PreTOplots <- read.csv ('data/03c_tmin-month_terra-clim2020-APR-17.csv',row.names = 1, stringsAsFactors=FALSE)


#*********************
# CRU data for mean temperature ------
#*********************

CRU <- stack ('data/input/environment/cru_ts4.03.1901.2018.tmp.dat.nc')

nbands1<-dim (CRU)[3]
#set up dataframe to store results
cruTOplots<-as.data.frame(matrix(nrow=nrow(md),ncol=nbands1))

#Read in data from each band from path1
for(i in 1:nbands1){
  dat<-raster(CRU,layer=i)
  rain.vals<-extract(dat,cbind(md$Longitude.Decimal,md$Latitude.Decimal),method="bilinear")
  cruTOplots[,i]<-rain.vals
  colnames(cruTOplots)[i]<-names (CRU)[i]
}

colnames (cruTOplots) <- paste (sapply(strsplit(as.character(colnames (cruTOplots)), ".", fixed = T), "[", 1),
                                sapply(strsplit(as.character(colnames (cruTOplots)), ".", fixed = T), "[", 2), sep = '.')

write.csv (cruTOplots,'data/03e_tmean-month_CRU2020-APR-17.csv')


############
# Calculate CWD -------
############

# Calculate water deficit based on 100 mm of evapotranspiration ======
Wdef <- PreTOplots - 100
Wdef <- as.matrix (Wdef)

# Start CWD calculating from wettest month - north and south hemespheres have dry season starting at different times
# get information from wettest month - average across all years for each point
months <- rep (c(1:12),ncol (Wdef)/12)
max.Wdef <- vector ()

for (i in 1:nrow (Wdef)) {
        a <- cbind (months, Wdef[i,])
        b <- (aggregate (a[,2] ~ months, data = a, FUN = mean))
        max.Wdef [i] <- b[which (b[,2]==max (b[,2])),'months']        
}

# start CWD calculating from January when wettest month is in December, i.e. wet season in between years 
max.Wdef[max.Wdef==12] <- 1 # if the wettest month is DEC wet season is between years, therefore start calculating CWD from JAN
max.Wdef_plots <- data.frame (max.Wdef, Plot.Code = md[,'Plot.Code'], lat = md[,'Latitude.Decimal'])


# calculate CWD --- 
CWD<-as.data.frame(matrix(nrow=nrow (Wdef),ncol=ncol (Wdef)/12-1))
years.cwd <- sort (rep (seq (1959.01,2018.12, by=1),12))
colnames (CWD) <- unique (years.cwd)
a <- vector ()

#
for (i in 1:nrow (Wdef)){
        a <- Wdef[i,]
        j <- max.Wdef[i]
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

write.csv (CWD,'data/03d_CWD-year_TerraClim2020-APR-17.csv')

