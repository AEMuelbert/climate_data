#*********
# Code developed by Adriane Esquivel Muelbert
# Trends in climate
# November 2018
#*********

rm(list = ls())

#Libraries
library (reshape)


#Load data =======
CWD <- read.csv ('2CWDYearplot.csv', row.names = 1)
Tmean <- read.csv ('2TmeanYearplot.csv', row.names = 1)
Tmax <- read.csv ('2TmaxYearplot.csv',row.names = 1)
Tmin <- read.csv ('2TminYearplot.csv', row.names = 1)

#*************
# Climate trends ----
#*************

# calculating trends in climate for all variables 
# per plot and per census 

var <- c('Tmean','Tmax','Tmin','CWD')
climate.list <- list ()
climate.list.cluster <- list ()
data <- list (Tmean,Tmax,Tmin,CWD)
year.max <- 2015 # last year in the time series

for (j in 1:length (var)) {
  varj <- var [j]
  dataj <- data [[j]]
  dataj <- dataj [-which (dataj$year > year.max),]
  
  if (varj == c('CWD')) {
    dataj <- data.frame (plot.year = dataj$plot.year, Plot.Code = dataj$Plot.Code, mean.year =  dataj$CWD,
                         year = dataj$year, zscore.year = dataj$CWD.zscore)
    
  }
  
  
  
  if (varj %in% c('Tmean','Tmax','Tmin')) {
    dataj <- data.frame (plot.year = dataj$plot.year, Plot.Code = dataj$Plot.Code, mean.year =  dataj$Temp.mean.year,
                         year = dataj$year, zscore.year = dataj$Temp.z.year)
  }
  
  
  clim.trend <- data.frame (var = varj ,Plot.Code = unique (dataj$Plot.Code), trend = 1, zscore_trend = 1, mean = 1)
  
  for (i in 1:nrow (clim.trend)) {
    a <- dataj [which (dataj$Plot.Code == clim.trend$Plot.Code[i]),]
    clim.trend [i,3] <- coef (lm (a$mean.year ~ a$year))[2]
    clim.trend [i,4] <- coef (lm (a$zscore.year ~ a$year))[2]
    clim.trend [i,5] <- mean (a$mean.year)
    
  }
  
  climate.list [[j]] <- clim.trend 
  
  dataj$cluster <- unlist (lapply(strsplit(as.character(dataj$Plot.Code), "\\-"), "[", 1))  
  clim.trend.cluster <- data.frame (var = varj ,cluster = unique (dataj$cluster), trend = 1, zscore_trend = 1, mean = 1)
  
  for (i in 1:nrow (clim.trend.cluster)) {
    a <- dataj [which (dataj$cluster == clim.trend.cluster$cluster[i]),]
    clim.trend.cluster [i,3] <- coef (lm (a$mean.year ~ a$year))[2]
    clim.trend.cluster [i,4] <- coef (lm (a$zscore.year ~ a$year))[2]
    clim.trend.cluster [i,5] <- mean (a$mean.year)
  }
  
  climate.list.cluster [[j]] <- clim.trend.cluster 
  
}

climate.trends <- do.call("rbind",climate.list)
climate.trends.cluster <- do.call("rbind",climate.list.cluster)

write.csv (climate.trends,'3ClimatePlot.csv')
write.csv (climate.trends.cluster,'3ClimateCluster.csv')
