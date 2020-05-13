library(tidyverse)

#################################################
####### using ggplot manhattan plot function #####
# customizable function for making manhattan plots 
# `https://github.com/pcgoddard/Burchardlab_Tutorials/wiki/GGplot2-Manhattan-Plot-Function`

# source my ggplot function 
source(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/ggplot_manhattan_plot_GL.R")

#####################
##### outliers ######

# load list of outlier loci
# identified with Bayescan and PCAdapt

load(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/final_outlier_loci.Rdata")
final_outlier_loci

##########################################
##### load RADseq Fst and alignment data ##### 

# load data for Sebastes schlegelli reference guided alignment
load(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/ling_ssc_ref_rag_-i0.5_fst_chrom_pos_qqman.Rdata")

# color palette for plotted SNPs 
# colors alternate so neighboring chromosomes are distinct

mypalette <- c("grey", "black")
mysnps <- final_outlier_loci

r1 <- gg.manhattan(ling_ssc_ref_fst_chrom_pos_qqman, threshold=NA, hlight=mysnps, col=mypalette, ylims=c(0,10), title = "")

#  print PDF
pdf(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/ling_2_ling_ssc_ref_rag_-i0.5.pdf",
    width = 12, height = 5)
r1
dev.off()

# load data for gasterosteus reference guided alignment
load(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/ling_gas_ref_fst_chrom_pos_qqman.Rdata")

r2 <- gg.manhattan(ling_gas_ref_fst_chrom_pos_qqman, threshold=NA, hlight=mysnps, col=mypalette, ylims=c(0,10), title = "")

# print pdf
pdf(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/ling_2_ling_gas_ref_HiC.pdf",
    width = 13, height = 5)
r2
dev.off()

# load data for larimichthys reference guided alignment
load(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/ling2_larmich_ref_fst_chrom_pos_qqman.Rdata")

r3 <- gg.manhattan(ling2_larmich_ref_fst_chrom_pos_qqman, threshold=NA, hlight=mysnps, col=mypalette, ylims=c(0,10), title = "")

#### using pdf ####
pdf(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/ling_2_larmich_ref.pdf",
    width = 13, height = 5)
r3
dev.off()

#### plot manhattan plots together with cowplot #### 
library(cowplot)

# plotted in order of alignment scores
pdf(file = "/Volumes/HOME/Documents/scripts/r_club/2020-05-13_manhattan_plots/sebastes_gasterosteus_larimichthys_ragoo_manhattans.pdf",
    width = 13, height = 9)
plot_grid(r1, r2, r3, labels = c("A","B", "C"), align = 'v', ncol = 1)
dev.off()

