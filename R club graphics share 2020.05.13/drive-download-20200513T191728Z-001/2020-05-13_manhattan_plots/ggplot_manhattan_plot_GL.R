# ggplot2 function for manhattan plots

# visit https://github.com/pcgoddard/Burchardlab_Tutorials/wiki/GGplot2-Manhattan-Plot-Function

# Libraries ====
library(readr)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

gg.manhattan <- function(df, threshold, hlight, col, ylims, title){
  # format df
  df.tmp <- df %>% 
    
    # Compute chromosome size
    group_by(CHR) %>% 
    summarise(chr_len=max(BP)) %>% 
    
    # Calculate cumulative position of each chromosome
    mutate(tot=cumsum(chr_len)-chr_len) %>%
    select(-chr_len) %>%
    
    # Add this info to the initial dataset
    left_join(df, ., by=c("CHR"="CHR")) %>%
    
    # Add a cumulative position of each SNP
    arrange(CHR, BP) %>%
    mutate( BPcum=BP+tot) %>%
    
    # Add highlight and annotation information
    mutate( is_highlight=ifelse(SNP %in% hlight, "yes", "no")) %>%
    mutate( is_annotate=ifelse(P < threshold, "yes", "no"))
  
  # get chromosome center positions for x-axis
  axisdf <- df.tmp %>% group_by(CHR) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )
  
  ggplot(df.tmp, aes(x=BPcum, y=P)) +
    # Show all points
    geom_point(aes(color=as.factor(CHR)), alpha=0.8, size=2) +
    scale_color_manual(values = rep(col, 22 )) +
    
    # custom X axis:
    ### longo messing
    ## use the following for full genome
     scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
    ## for single chromosome chromosome
    # scale function for megabases
    #scale_x_continuous(labels=function(x)x/1000000) +
   
    # scale_y_continuous(label = "Fst" ) + # expand=c(0,0)removes space between plot area and x axis 
    ylim(NA,1) +
    
    # add plot and axis titles
    # ggtitle(paste0(title)) +
    # labs(x = "Chromosome", y = expression('F'[ST])) +
    # labs(x = "CHR3 position (Mb)", y = expression('F'[ST])) +
     labs(x = "Chromosome Scale Pseudomolecule", y = expression('F'[ST])) +
    
    # add genome-wide sig and sugg lines
    # geom_hline(yintercept = -log10(sig)) +
    #geom_hline(yintercept = -log10(sugg), linetype="dashed") +
    
    # Add highlighted points
    geom_point(data=subset(df.tmp, is_highlight=="yes"), color="cyan3", size=2) +
    
    # Add label using ggrepel to avoid overlapping
    geom_label_repel(data=df.tmp[df.tmp$is_annotate=="yes",], aes(label=as.factor(SNP), alpha=0.7), size=5, force=1.3) +
    
    # Customize the theme:
    ### change axis text to smaller font for chromsome specific
    ## something like 14
    # default is 22, no axis.text defined
    theme_bw(base_size = 22) +
    theme( 
      plot.title = element_text(hjust = 0.5),
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text = element_text(size = 16)
    )
}