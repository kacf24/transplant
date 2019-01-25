library(pvclust)
library(readr)




#Custom Graphics for pvclust
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
AHiC.csv <- AHiC.csv[-1,]
AMidC.csv <- AMidC.csv[-2038,]
AMidE.csv <- AMidE.csv[-2038,]
BHiC.csv <- BHiC.csv[-2038,]
BHiE.csv <- BHiE.csv[-2038,]
BMidC.csv <- BMidC.csv[-1,]
BMidE.csv <- BMidE.csv[-1,]
wind.csv <- wind.csv[-1,]
#Z-ScoreNormalize

BHiC<-(BHiC.csv$Value - mean(BHiC.csv$Value)) / sd(BHiC.csv$Value)
BHiE<-(BHiE.csv$Value - mean(BHiE.csv$Value)) / sd(BHiE.csv$Value)
BMidC<-(BMidC.csv$Value - mean(BMidC.csv$Value)) / sd(BMidC.csv$Value)
BMidE<-(BMidE.csv$Value - mean(BMidE.csv$Value)) / sd(BMidE.csv$Value)
AHiC<-(AHiC.csv$Value - mean(AHiC.csv$Value)) / sd(AHiC.csv$Value)
AMidC<-(AMidC.csv$Value - mean(AMidC.csv$Value)) / sd(AMidC.csv$Value)
AMidE<-(AMidE.csv$Value - mean(AMidE.csv$Value)) / sd(AMidE.csv$Value)
wind<- (wind.csv$Temp - mean(wind.csv$Temp)) / sd(wind.csv$Temp)

#CreateTheTree
CombinedFrame <- data.frame(A.High.C = AHiC,A.Mid.C = AMidC,A.Mid.E = AMidE,B.High.C = BHiC,B.High.E = BHiE,B.Mid.C = BMidC,B.Mid.E = BMidE, CAMNET.Station = wind)

fit <- pvclust(CombinedFrame, method.hclust="ward.D2",method.dist="euclidean")
plot(fit,main = "",xlab = '', sub = '', col.pv = c("gray","gray0"), float = 0.025, print.num = F)
