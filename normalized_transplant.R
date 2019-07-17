library(stringr)
library(phyloseq)
library(tidyverse)
library(gridExtra)
library(DESeq2)
library(vegan)


#Read in your csv files
taxa_table <- read.csv(file = "~/Desktop/post-phd/TRANSPLANT/rewrites/rarefaction/just the taxa.csv")
ASV_tablet <- read.csv(file = "~/Desktop/post-phd/TRANSPLANT/rewrites/rarefaction/normalizedASVtablet.csv")
sampleData <- read.csv(file = "~/Desktop/post-phd/TRANSPLANT/rewrites/rarefaction/metadata.csv")

#Set ASV as characters instead of numbers
taxa_table$ASVid <- as.character(taxa_table$ASVid)
ASV_tablet$samples<-as.character(ASV_tablet$samples)

#Saves names/ASVs for later
names <- ASV_tablet$samples
sampleIDs<-colnames(ASV_tablet)[-1]

#Transposes the Matrix
ASV_table<-t(ASV_tablet)
colnames(ASV_table)<-ASV_table[1,]
ASV_table <- ASV_table[-1,]
ASV_table<-data.frame(unlist(ASV_table))
colnames(ASV_table)<-names
for (i in 1:ncol(ASV_table)){
  ASV_table[,i]<-as.numeric(as.character(ASV_table[,i]))
}
ASV_table$samples <- sampleIDs


#Stuff that makes your phyloseq object
ASVIDs <- taxa_table$ASVid
ASV2 <- as.matrix(dplyr::select(ASV_table, -samples))
#dim(ASV2)
row.names(ASV2) <- sampleIDs
#make the official OTU table object
ASVs <- otu_table(ASV2, taxa_are_rows = FALSE)
taxmat <- as.matrix(dplyr::select(taxa_table, -ASVid))
row.names(taxmat) <- ASVIDs
#make the official taxa table object
taxa <- tax_table(taxmat)
metaData <- dplyr::select(sampleData, -samples, -sample_ID)

row.names(metaData) <- sampleIDs
#make the official phyloseq object
ps <- phyloseq(ASVs, tax_table(taxa), sample_data(metaData))