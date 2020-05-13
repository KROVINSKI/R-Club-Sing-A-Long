library(pheatmap)
library(RColorBrewer)
library(gplots)

setwd("//Users/katherinerovinski/GIT/R-Club-Sing-A-Long/R club graphics share 2020.05.13/drive-download-20200513T191728Z-001/MG_graphics_heatmaps")
####Heatmap with pheatmap package####
#simple heatmap to visualize relatedness matrix
#read in relatedness matrix (from StAMMP package)
CSH_relate<-read.table("CSH_Gmatrix_StAMMP",sep="\t",header=T,row.names=1)
head(CSH_relate)
pheatmap(CSH_relate, cluster_rows=T, cluster_cols=T, clustering_distance_rows='euclidean', clustering_distance_cols='euclidean', clustering_method='ward.D2', show_rownames=T, show_colnames=F)


####Heatmap with heatmap.2####
#DNA methylation data (differentially methylated cytosines)in this example
#but good for gene expression data as well
DMC_Liv1<-read.table("DMC_Liv1",sep="\t",header=T,row.names=1)
head(DMC_Liv1)
#make variable for IDs of sample
Liv1_heatmap<-c("Stream","Stream","Stream","Stream","Stream","Stream","Stream","Stream","Stream","Stream","hatchery","hatchery","hatchery","hatchery","hatchery","hatchery","hatchery","hatchery","hatchery","hatchery")
#if else statement to specify colors for treatments 
col_anno <- as.matrix(ifelse(Liv1_heatmap == "Stream", "#e41a1c", "#377eb8"))
#plot it
heatmap<-heatmap.2(as.matrix(DMC_Liv1[,c(-1)]),na.color="grey",dendrogram = 'col',labRow=NA,col=brewer.pal(9,"Reds"),trace=c("none"),ColSideColors=col_anno)


