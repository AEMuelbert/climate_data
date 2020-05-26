getClimateTerra <- function (var, md, lat= 'Latitude.Decimal', long = 'Longitude.Decimal') {
  # enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
  baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc#fillmismatch")
  
  # get temperature data 
  nc <- nc_open(baseurlagg)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  # organizing information on time 
  t <- ncvar_get(nc, "time")
  tunits <- ncatt_get(nc, "time", "units")
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth = as.integer(unlist(tdstr)[2])
  tday = as.integer(unlist(tdstr)[3])
  tyear = as.integer(unlist(tdstr)[1])
  t2 <- chron(t, origin = c(month = tmonth, day = tday, year = tyear), out.format = 'y-m-d')
  t2 <- paste (sapply(strsplit(as.character(t2), "-", fixed = T), "[", 1),
               sapply(strsplit(as.character(t2), "-", fixed = T), "[", 2), sep = '.')
  count <- c(1, 1, -1)
  
  ClimTOplots<-as.data.frame(matrix(nrow=nrow(md),ncol=nc$dim$time$len))
  
  for (i in 1:nrow (md)) {
    flat = match(abs(lat - md[i,'Latitude.Decimal']) < 1/48, 1)
    latindex = which(flat %in% 1)
    flon = match(abs(lon - md[i,'Longitude.Decimal']) < 1/48, 1)
    lonindex = which(flon %in% 1)
    start <- c(lonindex, latindex, 1)
    ClimTOplots[i,] <- as.numeric(ncvar_get(nc, varid = var,start = start, count))
    colnames(ClimTOplots) <- t2
  }
  ClimTOplots
}

yearlyCWD<- function (x){
  #function to calculate yearly values of CWD based on monthly values of WDef
  WD <- vector ()
  for (i in 1:(length (x))){
    if (i == 1){
      WD [i] <- 0
    }else if (WD [i-1]+(x[i])<0){
      WD [i] <-  WD [i-1]+(x[i])
    } else {
      WD [i] <-  0
    }
  }
  min (WD)
}

TEMmonth <- function (TEM, md, data.source = 'TerraClimate') {
  TEM2 <- cbind (TEM, md$Plot.Code)
  colnames (TEM2) [ncol (TEM2)] <- 'Plot.Code'
  TEM2$Plot.Code <- as.factor(TEM2$Plot.Code)
  TEM2 <- melt (TEM2, id = 'Plot.Code')
  colnames (TEM2) [3] <- 'Temp'
  TEM2$year <- sapply(strsplit(as.character(TEM2$variable), ".", fixed = T), "[", 1)
  TEM2$year <- sapply(strsplit(as.character(TEM2$year), "X", fixed = T), "[", 2)
  TEM2$month <- sapply(strsplit(as.character(TEM2$variable), ".", fixed = T), "[", 2)
  TEM2$variable <- NULL 
  TEM2$plot.month <- paste (TEM2$Plot.Code, TEM2$month, sep = '.')
  TEM2$plot.year <- paste (TEM2$Plot.Code, TEM2$year, sep = '.')
  
  TEM2$year <- as.numeric (as.character (TEM2$year))
  if (data.source == 'TerraClimate') {
    TEM2$year <- ifelse (TEM2$year <= 18, TEM2$year + 2000, TEM2$year + 1900)
  }
  TEM2 <- TEM2[-which (TEM2$year < 1970),]
  TEM2$plot.year <- as.factor (paste (TEM2$Plot.Code, TEM2$year, sep = '.'))
  
  a <- aggregate(Temp~ plot.month, data = TEM2, FUN = plyr::each(mean = mean, sd = sd))
  a <- data.frame (plot.month = a$plot.month, 
                   Temp.mean.month = a$Temp[,'mean'],
                   Temp.sd.month = a$Temp[,'sd'])
  TEM2 <- merge (TEM2, a, by = 'plot.month')
  TEM2$Temp.z.month <- (TEM2$Temp-TEM2$Temp.mean.month)/TEM2$Temp.sd.month
  
  TEM2
}


TEMyear <- function (TEMmonth, md) {
  a <- aggregate(Temp~ plot.year + Plot.Code + year, data = TEMmonth, FUN = plyr::each(mean = mean, max = max))
  TEMyear <- data.frame ( Plot.Code = a$Plot.Code,
                          year = a$year,
                          plot.year = a$plot.year, 
                          Temp.mean.year = a$Temp[,'mean'],
                          Temp.max.year = a$Temp[,'max'])
  
  a <- aggregate(Temp.mean.year~ Plot.Code, data = TEMyear, FUN = plyr::each(mean = mean, sd = sd))
  a <- data.frame ( Plot.Code = a$Plot.Code,
                    Temp.mean.all = a$Temp.mean.year[,'mean'],
                    Temp.sd.all = a$Temp.mean.year[,'sd'])
  
  TEMyear <- merge (TEMyear, a, by = 'Plot.Code')
  TEMyear$Temp.z.year <- (TEMyear$Temp.mean.year-TEMyear$Temp.mean.all)/TEMyear$Temp.sd.all
  
  
  a <- aggregate(Temp.z.month~ plot.year, data = TEMmonth, FUN = mean)
  colnames (a) [2] <- 'Temp.mean.z.month'
  TEMyear <- merge (TEMyear, a, by = 'plot.year')
  
  
  # Detrended climate anomaly 
  attach(TEMyear)
  temp <- by(TEMyear, Plot.Code, function(x) lm(Temp.mean.year ~ year, data = x))
  temp2 <- sapply(temp, residuals)
  detach (TEMyear)
  temp3 <- melt (temp2)
  temp3$X1 <- NULL
  colnames (temp3) <- c('Plot.Code', 'Temp.mean.year.res')
  temp3$year <-TEMyear$year
  temp3$plot.year <- as.factor (paste (temp3$Plot.Code, temp3$year, sep = '.'))
  TEMyear <- merge (TEMyear, temp3[,c('Temp.mean.year.res','plot.year')])
  
  
  attach(TEMyear)
  temp <- by(TEMyear, Plot.Code, function(x) lm(Temp.max.year ~ year, data = x))
  temp2 <- sapply(temp, residuals)
  detach (TEMyear)
  temp3 <- melt (temp2)
  temp3$X1 <- NULL
  colnames (temp3) <- c('Plot.Code', 'Temp.max.year.res')
  temp3$year <-TEMyear$year
  temp3$plot.year <- as.factor (paste (temp3$Plot.Code, temp3$year, sep = '.'))
  TEMyear <- merge (TEMyear, temp3[,c('Temp.max.year.res','plot.year')])
  TEMyear
}

CalcCWDYear <- function (CWD, md) {
  CWD2 <- cbind (CWD, md$Plot.Code)
  colnames (CWD2) [ncol (CWD2)] <- 'Plot.Code'
  CWD2$Plot.Code <- as.factor(CWD2$Plot.Code)
  CWD2 <- melt (CWD2, id = 'Plot.Code')
  colnames (CWD2) [3] <- 'CWD'
  CWD2$year <- sapply(strsplit(as.character(CWD2$variable), ".", fixed = T), "[", 1)
  CWD2$year <- sapply(strsplit(as.character(CWD2$variable), "X", fixed = T), "[", 2)
  CWD2$variable <- NULL 
  CWD2$plot.year <- paste (CWD2$Plot.Code, CWD2$year, sep = '.')
  CWD2 <- CWD2[-which (CWD2$year<1970),]
  
  a <- aggregate(CWD~ Plot.Code, data = CWD2, FUN = plyr::each(mean = mean, sd = sd))
  a <- data.frame ( Plot.Code = a$Plot.Code,
                    CWD.mean.all = a$CWD[,'mean'],
                    CWD.sd.all = a$CWD[,'sd'])
  
  CWD2 <- merge (CWD2, a, by = 'Plot.Code')
  CWD2$CWD.zscore <- (CWD2$CWD-CWD2$CWD.mean.all)/CWD2$CWD.sd.all
  
  # Detrended CWD anomaly 
  CWD2$year <- as.numeric (CWD2$year)
  attach(CWD2)
  temp <- by(CWD2, Plot.Code, function(x) lm(CWD ~ year, data = x))
  temp2 <- sapply(temp, residuals)
  detach (CWD2)
  temp3 <- melt (temp2)
  temp3$X1 <- NULL
  colnames (temp3) <- c('Plot.Code', 'CWD.res')
  temp3$year <- CWD2$year
  temp3$plot.year <- CWD2$plot.year
  CWD2 <- merge (CWD2, temp3[,c('CWD.res','plot.year')])
  CWD2
}

envCensus <- function (env.data, pd, start.date = 1985, var) {
  census <- unique (pd[,c('Plot.Code', 'Census.Date', 'plot.census','Census.No', 'Census.Date0')])
  census$Census.No <- as.numeric (census$Census.No)
  census <- census[order (census$Plot.Code, census$Census.No),]
  census$idate <- c(NA,census$Census.Date[c(1:(nrow (census)-1))])
  # for the first census of each plot use the last 3 years as census interval
  census$idate <- ifelse (is.na (census$Census.Date0), census$Census.Date - 3, census$idate)
  # some censuses are shorter than on year. In that case use last three years before the census
  census$idate2 <- ifelse (census$Census.Date - census$idate < 1, census$Census.Date - 3, census$idate)
  if (unique (census$Census.Date < start.date) == F) {
    
  } else {
    census <- census [-which (census$Census.Date < start.date),]  
  }
  
  # The max temperature within the census interval
  
  temp <- merge (census, env.data, by = 'Plot.Code', all.x = T)
  temp <- temp[order (temp$plot.census),]
  
  if (var == 'TEM') {
    tempi <- temp [which (temp$year >= (temp$Census.Date-3) & temp$year < temp$Census.Date), 
                   c('plot.census','Temp.mean.year','Temp.max.year',
                     'Temp.z.year','Temp.mean.z.month','Temp.mean.year.res','Temp.max.year.res', 'year', 'Census.Date', 'Plot.Code')]
    
    tempi <- aggregate(cbind (Temp.mean.year,Temp.max.year,Temp.z.year,Temp.mean.z.month,Temp.mean.year.res,Temp.max.year.res)~ plot.census,
                       data = tempi, FUN = max)
    colnames (tempi) [c(2:7)] <- c ('Temp.mean.yeari','Temp.max.yeari','Temp.z.yeari','Temp.mean.z.monthi','Temp.mean.year.resi','Temp.max.year.res')
    
  } else {
    if (var == 'CWD') {
      tempi <- temp [which (temp$year >= (temp$Census.Date-3) & temp$year < temp$Census.Date), 
                     c('plot.census','CWD','CWD.zscore','CWD.res','year', 'Census.Date', 'Plot.Code')]
      tempi <- aggregate(cbind (CWD,CWD.zscore,CWD.res)~ plot.census,
                         data = tempi, FUN = min)
      colnames (tempi) [c(2:4)] <- c ('CWDi','CWD.zscorei','CWD.resi')
      
    }
  }
  
  tempi
  
} 
  

