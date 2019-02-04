library(TSclust)
library(lubridate)
library(xts)
library(readr)
library(data.table)


#MyPathCode
os<-.Platform$OS.type
path<-ifelse(os == "unix","~/Desktop/GitHub/Drive Sync","C:\\Users\\Bismuth\\Desktop\\GitHub\\Drive Sync\\")
#paste0(path,"RestOfPath"
.repath <- function(fullpath) {
  h <- gsub("/","\\",fullpath, fixed = TRUE)
}

fullpath <-paste0(path,"/Datasets/transplant/")
if(os != "unix"){fullpath <- .repath(fullpath)}











#Import all needed files
AHiC <- read_csv(paste0(fullpath,"AHiC.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BHiE <- read_csv(paste0(fullpath,"BHiE.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BHiC <- read_csv(paste0(fullpath,"BHiC.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BMidE <- read_csv(paste0(fullpath,"BMidE.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BMidC <- read_csv(paste0(fullpath,"BMidC.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BMid2E <- read_csv(paste0(fullpath,"BMid2E.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BMid2C <- read_csv(paste0(fullpath,"BMid2C.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
AMidE <- read_csv(paste0(fullpath,"AMidE.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
AMidC <- read_csv(paste0(fullpath,"AMidC.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
AMid2E <- read_csv(paste0(fullpath,"AMid2E.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
AMid2C <- read_csv(paste0(fullpath,"AMid2C.csv"), col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))

#Setting up buoy data
wl <- read_csv(paste0(fullpath,"buoy.csv"),  col_types = cols(EST = col_datetime(format = "%m/%d/%y %H:%M")))
wl$ROUNDED <- round_date(wl$EST,"15 minutes")
wl <- wl[-c(1,2,5095,5096),]
wl <- xts(wl$`Water Level`,as.POSIXct(wl$ROUNDED))
wl <- period.apply(wl,endpoints(wl,on = "mins",k=15),mean)
wl <- as.data.frame(wl)
plot(y=wl$`Water Level`,x=wl$EST)

#Trim to correct length
AHiC <- AHiC[-1,]
AMidC <- AMidC[-2038,]
AMidE <- AMidE[-2038,]
AMid2C <- AMid2C[-2038,]
AMid2E <- AMid2E[-2038,]
BHiC <- BHiC[-2038,]
BHiE <- BHiE[-2038,]
BMidC <- BMidC[-1,]
BMidE <- BMidE[-1,]
BMid2C <- BMid2C[-1,]
BMid2E <- BMid2E[-1,]

#Take only exposed, daytime records
Map <- read.csv(paste0(fullpath,"Map.csv"))
Map <- Map[order(Map$AB,Map$Desig),]
DayOrNite <-function(zone,Sunrise, Sunset, maxht){
  zone$Date <- as.Date(zone$`Date/Time`)
  zone$Time <- format(zone$`Date/Time`,"%H:%M:%S")
  zone$Counter <- seq(1,2037, by = 1)
  zone$period <- "day"
  n=1
  x=nrow(zone)
  while (x > 0){
    
    if (wl$V1[x] >= maxht){zone <-zone[-x,]}
    x=x-1
  }
  while (n <= nrow(zone)){
    if (zone$Time[n] <= Sunrise | zone$Time[n] >= Sunset ){zone$period[n] = "night"}
    
    n= n+1
  }
  
  z=nrow(zone)
  while (z > 0){
    if(zone$period[z] == "night"){zone <-zone[-z,]}
    # if (wl$V1[z] >= maxht){zone <-zone[-z,]}
    z=z-1
  }
  
  
  
  return(zone)
  
}

plot(BMidE$Rounded,BMidE$Value,type = 'l', xlim = c())
points(BMidE$Rounded,BMidE$Value,col = "red")
AHiC_2 <-DayOrNite(AHiC,"05:00:00","20:15:00",Map[1,5])
AMidC_2 <-DayOrNite(AMidC,"05:00:00","20:15:00",Map[2,5])
AMidE_2 <-DayOrNite(AMidE,"05:00:00","20:15:00",Map[3,5])
AMid2C_2 <-DayOrNite(AMid2C,"05:00:00","20:15:00",Map[4,5])
AMid2E_2 <-DayOrNite(AMid2E,"05:00:00","20:15:00",Map[5,5])
BHiC_2 <-DayOrNite(BHiC,"05:00:00","20:15:00",Map[6,5])
BHiE_2 <-DayOrNite(BHiE,"05:00:00","20:15:00",Map[7,5])
BMidC_2 <-DayOrNite(BMidC,"05:00:00","20:15:00",Map[8,5])
BMidE_2 <-DayOrNite(BMidE,"05:00:00","20:15:00",Map[9,5])
BMid2C_2 <-DayOrNite(BMid2C,"05:00:00","20:15:00",Map[10,5])
BMid2E_2 <-DayOrNite(BMid2E,"05:00:00","20:15:00",Map[11,5])


plot(x=AHiC$Rounded,y=AHiC$Value,type="l")
points(x=AHiC_2$Rounded,y=AHiC_2$Value)

plot(x=AMidC$Rounded,y=AMidC$Value,pch=2,col="red")
points(x=AMidC_2$Rounded,y=AMidC_2$Value)

#Some descriptive stats
agg_wind <- do.call(data.frame, aggregate(wind$X17.2236, list(AHiC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))[- c(1,22),]
max(BMidC$Value)

   
agg_AHiC <- do.call(data.frame, aggregate(AHiC_2$Value, list(AHiC_2$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) )) [- c(1,22),]
mean(agg_AHiC$x.daily_exp_hr)
sd(agg_AHiC$x.daily_exp_h)
mean(agg_AHiC$x.mn)
sd(agg_AHiC$x.mn)
mean(agg_AHiC$x.range)
sd(agg_AHiC$x.range)
min(AHiC_2$Value)
max(AHiC_2$Value)


agg_BHiC <- do.call(data.frame, aggregate(BHiC_2$Value, list(BHiC_2$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) )) [- c(1,22),]
mean(agg_BHiC$x.daily_exp_hr)
sd(agg_BHiC$x.daily_exp_h)
mean(agg_BHiC$x.mn)
sd(agg_BHiC$x.mn)
mean(agg_BHiC$x.range)
sd(agg_BHiC$x.range)
min(BHiC_2$Value)
max(BHiC_2$Value)

agg_BHiE <- do.call(data.frame, aggregate(BHiE_2$Value, list(BHiE_2$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) )) [- c(1,22),]
mean(agg_BHiE$x.daily_exp_hr)
sd(agg_BHiE$x.daily_exp_h)
mean(agg_BHiE$x.mn)
sd(agg_BHiE$x.mn)
mean(agg_BHiE$x.range)
sd(agg_BHiE$x.range)
min(BHiE_2$Value)
max(BHiE_2$Value)


agg_BMidC <- do.call(data.frame, aggregate(BMidC_2$Value, list(BMidC_2$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) )) [- c(1,22),]
mean(agg_BMidC$x.daily_exp_hr)
sd(agg_BMidC$x.daily_exp_h)
mean(agg_BMidC$x.mn)
sd(agg_BMidC$x.mn)
mean(agg_BMidC$x.range)
sd(agg_BMidC$x.range)
min(BMidC_2$Value)
max(BMidC_2$Value)

agg_AMidE <- do.call(data.frame, aggregate(AMidE_2$Value, list(AMidE_2$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) )) [- c(1,22),]
mean(agg_AMidE$x.daily_exp_hr)
sd(agg_AMidE$x.daily_exp_h)
mean(agg_AMidE$x.mn)
sd(agg_AMidE$x.mn)
mean(agg_AMidE$x.range)
sd(agg_AMidE$x.range)
min(AMidE_2$Value)
max(AMidE_2$Value)



agg_AMidC <- do.call(data.frame, aggregate(AMidC_2$Value, list(AMidC_2$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) )) [- c(1,22),]
mean(agg_AMidC$x.daily_exp_hr)
sd(agg_AMidC$x.daily_exp_h)
mean(agg_AMidC$x.mn)
sd(agg_AMidC$x.mn)
mean(agg_AMidC$x.range)
sd(agg_AMidC$x.range)
min(AMidC_2$Value)
max(AMidC_2$Value)

agg_BMidE <- do.call(data.frame, aggregate(BMidE_2$Value, list(BMidE_2$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) )) [- c(1,22),]
mean(agg_BMidE$x.daily_exp_hr)
sd(agg_BMidE$x.daily_exp_h)
mean(agg_BMidE$x.mn)
sd(agg_BMidE$x.mn)
mean(agg_BMidE$x.range)
sd(agg_BMidE$x.range)
min(BMidE_2$Value)
max(BMidE_2$Value)


























#IGNORE AFTER ALL THIS
test <- rbind(agg_AHiC,agg_BHiC,agg_BHiE,agg_AMidC,agg_AMidE,agg_BMidC,agg_BMidE)
summary(lm(x.mn~.+.,data=test ))




#5DayMeansTemp
colMeans(matrix(agg_BHiC$x.mn,nrow=5))
colMeans(matrix(agg_BHiE$x.mn,nrow=5))
colMeans(matrix(agg_BMidC$x.mn,nrow=5))
colMeans(matrix(agg_BMidE$x.mn,nrow=5))
colMeans(matrix(agg_AHiC$x.mn,nrow=5))
colMeans(matrix(agg_AMidC$x.mn,nrow=5))
colMeans(matrix(agg_AMidE$x.mn,nrow=5))

#5DayMeansExposure


colMeans(matrix(agg_BHiC$x.daily_exp_hr,nrow=5))
colMeans(matrix(agg_BHiE$x.daily_exp_hr,nrow=5))
colMeans(matrix(agg_BMidC$x.daily_exp_hr,nrow=5))
colMeans(matrix(agg_BMidE$x.daily_exp_hr,nrow=5))
colMeans(matrix(agg_AHiC$x.daily_exp_hr,nrow=5))
colMeans(matrix(agg_AMidC$x.daily_exp_hr,nrow=5))
colMeans(matrix(agg_AMidE$x.daily_exp_hr,nrow=5))


#DaysWeCareAbout
agg_AHiC <- do.call(data.frame, aggregate(AHiC$Value, list(AHiC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x) ) ))[c(2,14,18,21),]
agg_BHiC <- do.call(data.frame, aggregate(BHiC$Value, list(BHiC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x) ) ))[c(2,14,18,21),]
agg_BHiE <- do.call(data.frame, aggregate(BHiE$Value, list(BHiE$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x) ) ))[c(2,14,18,21),]
agg_BMidC <- do.call(data.frame, aggregate(BMidC$Value, list(BMidC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x) ) ))[c(2,14,18,21),]
agg_BMidE <- do.call(data.frame, aggregate(BMidE$Value, list(BMidE$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x) ) ))[c(2,14,18,21),]
agg_AMidC <- do.call(data.frame, aggregate(AMidC$Value, list(AMidC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x) ) ))[c(2,14,18,21),]
agg_AMidE <- do.call(data.frame, aggregate(AMidE$Value, list(AMidE$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x) ) ))[c(2,14,18,21),]

#Before or after noon
noontime <- function(x){
  x$timer <- "1"
  for (i in 1:nrow(x)){
    if (x$Time[i] > "12:00:00"){x$timer[i] <- "afternoon"}
    if (x$Time[i] <= "12:00:00"){x$timer[i] <- "day"}
  }
  return(x)
}

AHiC <-noontime(AHiC)
AMidC <-noontime(AMidC)
AMidE <-noontime(AMidE)
AMid2C <-noontime(AMid2C)
AMid2E <-noontime(AMid2E)
BHiC <-noontime(BHiC)
BHiE <-noontime(BHiE)
BMidC <-noontime(BMidC)
BMidE <-noontime(BMidE)
BMid2C <-noontime(BMid2C)
BMid2E <-noontime(BMid2E)


mean(agg_AHiC$x.mn)

#Some Descriptive Statistics For Day/Afternoon
agg_AHiC <- do.call(data.frame, aggregate(AHiC$Value, list(AHiC$Date,AHiC$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_BHiC <- do.call(data.frame, aggregate(BHiC$Value, list(BHiC$Date,BHiC$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_BHiE <- do.call(data.frame, aggregate(BHiE$Value, list(BHiE$Date,BHiE$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_BMidC <- do.call(data.frame, aggregate(BMidC$Value, list(BMidC$Date,BMidC$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_BMidE <- do.call(data.frame, aggregate(BMidE$Value, list(BMidE$Date,BMidC$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_BMid2C <- do.call(data.frame, aggregate(BMid2C$Value, list(BMid2C$Date, BMid2C$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_BMid2E <- do.call(data.frame, aggregate(BMid2E$Value, list(BMid2E$Date, BMid2E$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_AMidC <- do.call(data.frame, aggregate(AMidC$Value, list(AMidC$Date,AMidC$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_AMidE <- do.call(data.frame, aggregate(AMidE$Value, list(AMidE$Date,AMidE$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_AMid2C <- do.call(data.frame, aggregate(AMid2C$Value, list(AMid2C$Date, AMid2C$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))
agg_AMid2E <- do.call(data.frame, aggregate(AMid2E$Value, list(AMid2E$Date, AMid2E$timer), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))

rm(list = ls(pattern = "^A"))
rm(list = ls(pattern = "^B"))
rm(list = ls(pattern = "^M"))
rm(list = ls(pattern = "^w"))
rm(list = ls(pattern = "^t"))

#Comparing Before/After noon 
test <- rbind(agg_AHiC,agg_BHiC,agg_BHiE)
test3 <- rbind(agg_AMidC,agg_AMidE,agg_BMidC,agg_BMidE)


test2 <- subset(test3,`Group.2` == "day")
mean(test2$x.mn)
sd(test2$x.mn)
test2 <- subset(test3,`Group.2` == "afternoon")
mean(test2$x.mn)
sd(test2$x.mn)
#


#Compare exposure
mean(c(agg_AHiC$x.daily_exp_hr[11],agg_BHiE$x.daily_exp_hr[11],agg_BHiC$x.daily_exp_hr[11]))
mean(c(agg_AHiC$x.daily_exp_hr[21],agg_BHiE$x.daily_exp_hr[21],agg_BHiC$x.daily_exp_hr[21]))
mean(c(agg_AMidC$x.daily_exp_hr[11],agg_AMidE$x.daily_exp_hr[11],agg_BMidE$x.daily_exp_hr[11],agg_BMidC$x.daily_exp_hr[11]))
mean(c(agg_AMidC$x.daily_exp_hr[21],agg_AMidE$x.daily_exp_hr[21],agg_BMidE$x.daily_exp_hr[21],agg_BMidC$x.daily_exp_hr[21]))

