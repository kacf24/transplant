library(TSclust)
library(lubridate)
library(xts)
library(readr)
library(data.table)


#MyPathCode
os<-.Platform$OS.type
path<-ifelse(os == "unix","~/Desktop/GitHub/","C:\\Users\\Bismuth\\Desktop\\GitHub\\")
#paste0(path,"RestOfPath"
.repath <- function(fullpath) {
  h <- gsub("/","\\",fullpath, fixed = TRUE)
}

fullpath <-paste0(path,"Datasets/transplant/")
if(os != "unix"){fullpath <- .repath(fullpath)}


files <- list.files(fullpath,pattern = "*.csv")

for(i in 1:length(files)){
  filename=files[i]
  data=read_csv(paste0(fullpath,filename))
  assign(x = filename,value = data)
  rm(data)
}








#Import all needed files
AHiC <- read_csv("AHiC.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BHiE <- read_csv("BHiE.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BHiC <- read_csv("BHiC.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BMidE <- read_csv("BMidE.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BMidC <- read_csv("BMidC.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BMid2E <- read_csv("BMid2E.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
BMid2C <- read_csv("BMid2C.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
AMidE <- read_csv("AMidE.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
AMidC <- read_csv("AMidC.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
AMid2E <- read_csv("AMid2E.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))
AMid2C <- read_csv("AMid2C.csv", col_types = cols(`Date/Time` = col_datetime(format = "%m/%d/%y %H:%M")))

#Setting up buoy data
wl <- read_csv("CO-OPS__8413320__wl (2).csv",  col_types = cols(EST = col_datetime(format = "%m/%d/%Y %H:%M")))
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
Map <- read.csv("Map.csv")
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
AHiC <-DayOrNite(AHiC,"05:00:00","20:15:00",Map[1,4])
AMidC <-DayOrNite(AMidC,"05:00:00","20:15:00",Map[2,4])
AMidE <-DayOrNite(AMidE,"05:00:00","20:15:00",Map[3,4])
AMid2C <-DayOrNite(AMid2C,"05:00:00","20:15:00",Map[4,4])
AMid2E <-DayOrNite(AMid2E,"05:00:00","20:15:00",Map[5,4])
BHiC <-DayOrNite(BHiC,"05:00:00","20:15:00",Map[6,4])
BHiE <-DayOrNite(BHiE,"05:00:00","20:15:00",Map[7,4])
BMidC <-DayOrNite(BMidC,"05:00:00","20:15:00",Map[8,4])
BMidE <-DayOrNite(BMidE,"05:00:00","20:15:00",Map[9,4])
BMid2C <-DayOrNite(BMid2C,"05:00:00","20:15:00",Map[10,4])
BMid2E <-DayOrNite(BMid2E,"05:00:00","20:15:00",Map[11,4])







#Some descriptive stats
agg_wind <- do.call(data.frame, aggregate(wind$X17.2236, list(AHiC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) ))[- c(1,22),]
max(BMidC$Value)

   
agg_AHiC <- do.call(data.frame, aggregate(AHiC$Value, list(AHiC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x) ) )) [- c(1,22),]
agg_BHiC <- do.call(data.frame, aggregate(BHiC$Value, list(BHiC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_BHiE <- do.call(data.frame, aggregate(BHiE$Value, list(BHiE$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_BMidC <- do.call(data.frame, aggregate(BMidC$Value, list(BMidC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_BMidE <- do.call(data.frame, aggregate(BMidE$Value, list(BMidE$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_BMid2C <- do.call(data.frame, aggregate(BMid2C$Value, list(BMid2C$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_BMid2E <- do.call(data.frame, aggregate(BMid2E$Value, list(BMid2E$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_AMidC <- do.call(data.frame, aggregate(AMidC$Value, list(AMidC$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_AMidE <- do.call(data.frame, aggregate(AMidE$Value, list(AMidE$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_AMid2C <- do.call(data.frame, aggregate(AMid2C$Value, list(AMid2C$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]
agg_AMid2E <- do.call(data.frame, aggregate(AMid2E$Value, list(AMid2E$Date), FUN = function(x) c(mn = mean(x), daily_exp_hr = length(x)*15/60, `sd`=sd(x), `min` = min(x), `max` = max(x), `range` = max(x)-min(x)  ) ))[- c(1,22),]





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

