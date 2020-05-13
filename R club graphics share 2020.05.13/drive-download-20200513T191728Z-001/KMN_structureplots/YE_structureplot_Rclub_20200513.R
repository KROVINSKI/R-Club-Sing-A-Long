#### R script to read output files from Structure and plot ####

#### first read in the structure data for each K of interest, and each individual's admixture proportion to K ####
# uses loop for reformatting the structure data
# read in the files for structure plotting
setwd("~/Documents/statistics_resources/Rclub/ggplot_show&tell")

YE_allPOP_NOPOPLOCPRIOR_K2 <- read.table(file = "./YE_allpop_structureoutput/K2.outfile", header = F)
YE_allPOP_NOPOPLOCPRIOR_K3 <- read.table(file = "./YE_allpop_structureoutput/K3.outfile", header = F)
YE_allPOP_NOPOPLOCPRIOR_K4 <- read.table(file = "./YE_allpop_structureoutput/K4.outfile", header = F)
YE_allPOP_NOPOPLOCPRIOR_K5 <- read.table(file = "./YE_allpop_structureoutput/K5.outfile", header = F)

# column names defined in new import, and make new variable, in a loop/lapply
YE_allPOP_NOPOPLOCPRIOR_dflist <- mget(ls(pattern = "^YE_allPOP_NOPOPLOCPRIOR_K*"))
YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL <- lapply(YE_allPOP_NOPOPLOCPRIOR_dflist, `[`,-c(2,3,5)) 
YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL_renameCOL <- lapply(YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL, function(x) {
  names(x)[c(1,2)] <- c("sampleID", "popNum")
  names(x)[c(3:length(colnames(x)))] <- paste("Q", 1:(length(colnames(x))-2), sep = "")
  x$popName <- ifelse(x$popNum == 1, "A",
                              ifelse(x$popNum == 2, "VI",
                                     ifelse(x$popNum == 3, "GC",
                                            ifelse(x$popNum == 4, "WA coast",
                                                   ifelse(x$popNum == 5, "Neah", 
                                                          ifelse(x$popNum == 6, "Sekiu",
                                                                 ifelse(x$popNum == 7, "Hood Canal",
                                                                        ifelse(x$popNum == 8, "CPS",
                                                                               ifelse(x$popNum == 9, "San Juan Islands",
                                                                                      ifelse(x$popNum == 10, "SSI",
                                                                                             ifelse(x$popNum == 11, "MI",
                                                                                                    ifelse(x$popNum == 12, "DS",
                                                                                                           ifelse(x$popNum == 13, "UJS",
                                                                                                                  ifelse(x$popNum == 14, "OR coast",
                                                                                                                         ifelse(x$popNum == 15, "CA", NA)))))))))))))))
  
  # order population names by designating popName as an ordered factor
  x$popName <- factor(x$popName, ordered = T, 
                              levels = c("A", "VI", "GC", "WA coast", "Neah", 
                                         "Sekiu", "Hood Canal", "CPS", "San Juan Islands",
                                         "SSI", "MI", "DS", "UJS",
                                         "OR coast", "CA"))
  
  x # x calls to return otherwise this won't produce anything
})  

# now reshape the dataframe into vertical format with gather
library(tidyverse)
YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL_renameCOL_stacked <- 
  lapply(YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL_renameCOL, function(x) {
    gather(x, "Q", "proportion", 3:(length(colnames(x))-1))
  })



# loop for plotting the structure data, now that it's read in and organized
YEallPOP_STRUC_NOLOCPRIOR_K2toK5_RdYlBu_Klabelonly <- lapply(seq_along(YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL_renameCOL_stacked), function(y) {
  Kvalonly <- paste("K = ", gsub(pattern = "YE_allPOP_NOPOPLOCPRIOR_K(\\d+)", replacement = "\\1", 
                                 names(YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL_renameCOL_stacked)[[y]], perl = T),
                    sep = "")
  ggplot(YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL_renameCOL_stacked[[y]], aes(x = factor(sampleID), y = proportion, fill = Q)) + 
    geom_bar(stat = "identity", width = 1.1) + 
    scale_fill_brewer(palette = "RdYlBu") +
    #scale_fill_viridis(discrete = T, option = "inferno") + 
    xlab(NULL) + 
    ylab(paste("Q for ", Kvalonly, sep = "")) + 
    theme(axis.title.y = element_text(size = 8)) +
    theme_bw() +
    theme(legend.position = "none", axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
    theme(strip.text.x = element_blank()) + 
    scale_y_continuous(limits = c(0,1.001), expand = c(0, 0)) + # 1.01 because some of the sums are slightly >1
    facet_grid(. ~ popName, drop = T, space = "free", scales = "free", switch = "x") 
})

YEallPOP_STRUC_NOLOCPRIOR_K5plot <- ggplot(YE_allPOP_NOPOPLOCPRIOR_dflist_delCOL_renameCOL_stacked$YE_allPOP_NOPOPLOCPRIOR_K5, aes(x = factor(sampleID), y = proportion, fill = Q)) + 
  geom_bar(stat = "identity", width = 1.1) + 
  scale_fill_brewer(palette = "RdYlBu") +
  #scale_fill_viridis(discrete = T, option = "inferno") + 
  xlab(NULL) + 
  ylab("Q for K = 5") + 
  theme(axis.title.y = element_text(size = 8)) +
  theme_bw() +
  theme(legend.position = "none", axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
  theme(strip.text.x = element_text(size = 8)) + 
  #theme(panel.background = element_rect(fill = "white")) +
  #geom_vline(xintercept = c(0.5,19.5,20.5,41.5,64.5,88.5)) +# draw vertical lines to separate populations
  scale_y_continuous(limits = c(0,1.001), expand = c(0, 0)) + # 1.01 because some of the sums are slightly >1 
  facet_grid(. ~ popName, drop = T, space = "free", scales = "free", switch = "x") +
  theme(strip.background = element_blank()) 

# put all the plots together in one plot
library(grid)
library(gridExtra)
YEallPOP_STRUC_NOlocprior_K2toK5 <- grid.arrange(YEallPOP_STRUC_NOLOCPRIOR_K2toK5_RdYlBu_Klabelonly[[1]], # this is K = 2
                                                 YEallPOP_STRUC_NOLOCPRIOR_K2toK5_RdYlBu_Klabelonly[[2]], # this is K = 3
                                                 YEallPOP_STRUC_NOLOCPRIOR_K2toK5_RdYlBu_Klabelonly[[3]], # this is K = 4
                                       YEallPOP_STRUC_NOLOCPRIOR_K5plot, # K = 10
                                       ncol = 1, 
                                       heights = c(1,1,1,1.15)) #NOTE: to get these panels to look equal in size, I changed the distance between panels for the last plot to be like the others, and used this heights parameter

setwd("~/Desktop/rockfish/RAD/dataprocess_FINAL0416")
save.image("CanYe_summaryplotsanddata_forConsGenet0917.RData")
