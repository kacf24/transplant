library(plyr)

#Choose a cutoff proportion between 0 and 1
cutoff <- 1 
#Read in your ASV table
ASVTablet<-read.csv("ASVtablet.csv")
metadata<-read.csv("metadata.csv")
taxonomy<-read.csv("taxonomy.csv")


ASVTable <- data.frame(t(ASVTablet[-1]))
colnames(ASVTable) <- ASVTablet[, 1]
rm(ASVTablet)

ASVTable$samples <- metadata$samples
TableandMetadata <- join(ASVTable,metadata,by="samples",type="inner")
Sample_1 <- subset(TableandMetadata,TableandMetadata$treatment=="watered"&TableandMetadata$day =="4"&TableandMetadata$tissue=="holdfast")

#Switch the commenting if you're trying to compare more than one thing, and uncomment the sections at end of code
Sample_2 <- Sample_1
#Sample_2 <- subset(TableandMetadata,TableandMetadata$treatment=="control"&TableandMetadata$day =="4"&TableandMetadata$tissue=="vegetative")
pA_Sample_1 <- ifelse(Sample_1[,1:(ncol(Sample_1)-ncol(metadata))] > 0,1,0)
pA_Sample_2 <- ifelse(Sample_2[,1:(ncol(Sample_2)-ncol(metadata))] > 0,1,0)
colMeans_Sample_1 <-colMeans(pA_Sample_1)
colMeans_Sample_2 <-colMeans(pA_Sample_2)

thresh_Sample_1 <- colMeans_Sample_1
thresh_Sample_2 <- colMeans_Sample_2


for(i in 1:length(colMeans_Sample_1)){
  
  
  
  if(colMeans_Sample_1[i] < cutoff){
    thresh_Sample_1[i] <- 0 }
  if(colMeans_Sample_2[i] < cutoff){
    thresh_Sample_2[i] <- 0 }
  
  
}



ASV <- NULL
ASV <- data.frame(t(names(thresh_Sample_1)))
colnames(ASV) <- names(thresh_Sample_1)

results <- NULL
results <-ifelse(thresh_Sample_2 != 0 & thresh_Sample_1 != 0, "both", ifelse(thresh_Sample_2 != 0 & thresh_Sample_1 == 0,"Sample_2",ifelse(thresh_Sample_1 != 0 & thresh_Sample_2 == 0,"Sample_1","none")))  

results_both <- results
for(j in length(results):1){
  if(results[j] == "none" |results[j] == "Sample_1" | results[j] == "Sample_2"){
    results_both<- results_both[-j]
  } 
  
}

results_Sample_1 <- results
for(j in length(results):1){
  if(results[j] == "none" |results[j] == "both" | results[j] == "Sample_2"){
    results_Sample_1<- results_Sample_1[-j]
  } 
  
}

results_Sample_2 <- results
for(j in length(results):1){
  if(results[j] == "none" |results[j] == "both" | results[j] == "Sample_1"){
    results_Sample_2<- results_Sample_2[-j]
  } 
  
}



df_r_both <- data.frame(ASVid =results_both)
df_r_both$ASVid <- row.names(df_r_both)
output_both <- join(df_r_both,taxonomy,by="ASVid",type="inner")


write.csv(output_both, file = "controlReproductive.csv")

#df_r_Sample_1 <- data.frame(ASVid =results_Sample_1)
#df_r_Sample_1$ASVid <- row.names(df_r_Sample_1)
#output_Sample_1 <- join(df_r_Sample_1,taxonomy,by="ASVid",type="inner")
#write.csv(output_Sample_1, file = "controlDryVegetativecontrol.csv")

#df_r_Sample_2 <- data.frame(ASVid =results_Sample_2)
#df_r_Sample_2$ASVid <- row.names(df_r_Sample_2)
#output_Sample_2 <- join(df_r_Sample_2,taxonomy,by="ASVid",type="inner")
#write.csv(output_Sample_2, file = "controlDryVegetativeDry.csv")

