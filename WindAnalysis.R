library(readr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

#MyPathCode
os<-.Platform$OS.type
path<-ifelse(os == "unix","~/Desktop/GitHub/","C:\\Users\\Bismuth\\Desktop\\GitHub\\")
#paste0(path,"RestOfPath"
.repath <- function(fullpath) {
  h <- gsub("/","\\",fullpath, fixed = TRUE)
}

fullpath <-paste0(path,"Datasets/transplant/wind.csv")
if(os != "unix"){fullpath <- .repath(fullpath)}

wind <- read_csv(fullpath, col_types = cols(Time = col_time(format = "%H:%M")))
wind$DN <- "moo"
wind$Time <- format(wind$Time,"%H:%M:%S")
wind$DN <- ifelse(wind$Time >= "4:00:00" | wind$Time <= "15:00:00","day","night" )

SetWindSpeedThreshold <- function(wind){
  
wind$windspeedthresh <- 1
wind$`Wind speed (mph)` <- as.numeric (wind$`Wind speed (mph)`)
  
  n = 1
  while (n <= nrow(wind)){
    if (wind$`Wind speed (mph)`[n] >= 1  & wind$`Wind speed (mph)`[n] < 4){wind$windspeedthresh[n]<-2}
    if (wind$`Wind speed (mph)`[n] >= 4 & wind$`Wind speed (mph)`[n] < 6){wind$windspeedthresh[n]<-3}
    if (wind$`Wind speed (mph)`[n] >=7  & wind$`Wind speed (mph)`[n] < 11){wind$windspeedthresh[n]<-4}
    if (wind$`Wind speed (mph)`[n] >= 11){wind$windspeedthresh[n]<-5}
    n=n+1
  } 
  wind$windspeedthresh <- as.factor(wind$windspeedthresh)
  return(wind)
}
wind <- SetWindSpeedThreshold(wind)


mround <- function(x,base){ 
  base*round(x/base) 
} 
wind$CorTime<-mround(wind$`Wind direction (degrees)`,90)
#GottaMakeSomePlots
#Plot Wind Speed
#group.colors = c(day = "white",night = "black")
#ggplot(wind, aes(x=wind$`Wind speed (mph)`, fill=wind$period)) +
# geom_histogram(breaks = seq(0,15, by = 2.5),col="black")+
#  xlim(c(0,15))+
#scale_fill_manual(values=group.colors, name = "Period")+
#xlab("Wind Speed (mph)")+
#theme_bw()

#Plot Wind Direction
#ggplot(wind, aes(x=wind$WindDirection, fill=wind$period)) +
# geom_histogram(breaks = seq(0,180, by = 90),col="black")+
#scale_x_continuous(breaks = seq(0,180, by = 45), labels = c("","North","", "South",""))  +
# scale_fill_manual(values=group.colors, name = "Period")+
#  xlab("Wind Direction")+
#  theme_bw()+
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
