library(pvclust)
library(readr)


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


#Custom Graphics for pvclust
#Custom Graphics
hc2axes<- function (x) {
  A <- x$merge
  n <- nrow(A) + 1
  x.axis <- c()
  y.axis <- x$height
  x.tmp <- rep(0, 2)
  zz <- match(1:length(x$order), x$order)
  for (i in 1:(n - 1)) {
    ai <- A[i, 1]
    if (ai < 0) 
      x.tmp[1] <- zz[-ai]
    else x.tmp[1] <- x.axis[ai]
    ai <- A[i, 2]
    if (ai < 0) 
      x.tmp[2] <- zz[-ai]
    else x.tmp[2] <- x.axis[ai]
    x.axis[i] <- mean(x.tmp)
  }
  return(data.frame(x.axis = x.axis, y.axis = y.axis))
}
text.pvclust <- function(x, col=c("gray0","gray50",8), print.num=FALSE,  float=0.02, cex=NULL, font=NULL, ...){
  axes <- hc2axes(x$hclust)
  usr  <- par()$usr; wid <- usr[4] - usr[3]
  au <- as.character(round(x$edges[,"au"]*100))
  bp <- as.character(round(x$edges[,"bp"]*100))
  rn <- as.character(row.names(x$edges))
  au[length(au)] <- "AU"
  bp[length(bp)] <- "BP"
  rn[length(rn)] <- "edge #"
  a <- text(x=axes[,1], y=axes[,2] + float * wid, au,
            col=col[1], pos=2, offset=.3, cex=cex, font=2)
  a <- text(x=axes[,1], y=axes[,2] + float * wid, bp,
            col=col[2], pos=4, offset=.3, cex=cex, font=font)
  if(print.num)
    a <- text(x=axes[,1], y=axes[,2], rn,
              col=col[3], pos=1, offset=.3, cex=cex, font=font)
}






#Trim to correct length
rm(Map.csv)
AHiC <- AHiC[-1,]
AMidC <- AMidC[-2038,]
AMidE <- AMidE[-2038,]
BHiC <- BHiC[-2038,]
BHiE <- BHiE[-2038,]
BMidC <- BMidC[-1,]
BMidE <- BMidE[-1,]

#Z-ScoreNormalize

BHiC$Value<-(BHiC$Value - mean(BHiC$Value)) / sd(BHiC$Value)
BHiE$Value<-(BHiE$Value - mean(BHiE$Value)) / sd(BHiE$Value)
BMidC$Value<-(BMidC$Value - mean(BMidC$Value)) / sd(BMidC$Value)
BMidE$Value<-(BMidE$Value - mean(BMidE$Value)) / sd(BMidE$Value)
AHiC$Value<-(AHiC$Value - mean(AHiC$Value)) / sd(AHiC$Value)
AMidC$Value<-(AMidC$Value - mean(AMidC$Value)) / sd(AMidC$Value)
AMidE$Value<-(AMidE$Value - mean(AMidE$Value)) / sd(AMidE$Value)
wind$X17.2236 <- (wind$X17.2236 - mean(wind$X17.2236)) / sd(wind$X17.2236)

#CreateTheTree
CombinedFrame <- data.frame(A.High.C = AHiC$Value,A.Mid.C = AMidC$Value,A.Mid.E = AMidE$Value,B.High.C = BHiC$Value,B.High.E = BHiE$Value,B.Mid.C = BMidC$Value,B.Mid.E = BMidE$Value, CAMNET.Station = wind$X17.2236)
IP.dis <- diss(CombinedFrame, "EUCL")
plot((hclust(IP.dis, method ="ward.D2")))

fit <- pvclust(CombinedFrame, method.hclust="ward.D2",method.dist="euclidean")
plot(fit,main = "",xlab = '', sub = '', col.pv = c("gray","gray0"), float = 0.025, print.num = F)