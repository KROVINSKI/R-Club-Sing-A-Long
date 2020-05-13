########################################################
## R Club - ggplot day
## Created by Becca Maher
#######################################################

rm(list=ls())

# libraries
library("phyloseq")
library("ggplot2")
library("microbiome")
library("dplyr")

# Figure 1 - Taxa plot
#######################################################
# load the rarefied data table with all species
load(file = "./data/ps_rar8663.RData")
ps
sample_sums(ps)
# transform to relative abundance
ps_rel <- transform(ps, "compositional")
ps_rel
sample_sums(ps_rel)
# melt the data at the Genus level
ps_rel_genus_melt <- ps_rel %>%
  tax_glom(taxrank = "Genus") %>%
  psmelt()
head(ps_rel_genus_melt)

levels(ps_rel_genus_melt$bleach) <- c("Apparently Healthy", "Bleached")
levels(ps_rel_genus_melt$type) <- c("Resistant","Susceptible")
# plot
bar_species = ggplot(ps_rel_genus_melt, aes(x = reorder(geno, geno.num), y=Abundance)) + 
  geom_bar(stat="identity", position="fill", aes(fill = reorder(Genus, Abundance))) +
  facet_grid(bleach~type, scales = "free_x", space = "free_x") +
  theme_bw() +
  ylab("Relative Abundance") +
  xlab("Genotype") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  labs(fill = "Genus", size = "Genus")
bar_species


# Get genera with mean realtive abundance >0.01 across all samples 
genus_sum <- ps_rel_genus_melt %>% group_by(Genus) %>% dplyr::summarise(Aver = mean(Abundance))
genus_sub <- genus_sum[which(genus_sum$Aver > 0.01),]
names <- genus_sub$Genus
names
# Replace genera with <0.01 abundance with "NA"
ps_rel_genus_melt$genus <- ps_rel_genus_melt$Genus

ps_rel_genus_melt$genus[ps_rel_genus_melt$genus != "MD3-55" & 
                          ps_rel_genus_melt$genus != "Vibrio" &
                          ps_rel_genus_melt$genus != "Acinetobacter" &
                          ps_rel_genus_melt$genus != "Corynebacterium" &
                          ps_rel_genus_melt$genus != "Pseudoalteromonas" &
                          ps_rel_genus_melt$genus != "Cloacibacterium" &
                          ps_rel_genus_melt$genus != "Staphylococcus" &
                          ps_rel_genus_melt$genus != "Alteromonas" &
                          ps_rel_genus_melt$genus != "Aestuariibacter"] <- NA

# plot
bar_species = ggplot(ps_rel_genus_melt, aes(x = reorder(geno, geno.num), y=Abundance)) + 
  geom_bar(stat="identity", position="fill", aes(fill = reorder(genus, Abundance))) +
  scale_fill_manual(values=c("#999999", "#FFFF00",  "#CC3300","#0072B2","#660066", "#E69F00", "#56B4E9","#CC79A7",  "#009E73"), 
                    breaks = c("Pseudoalteromonas","Acinetobacter","Staphylococcus","Aestuariibacter",
                               "Cloacibacterium","Corynebacterium","Vibrio","Alteromonas","MD3-55"),
                    labels = c("Pseudoalteromonas","Acinetobacter","Staphylococcus","Aestuariibacter",
                               "Cloacibacterium","Corynebacterium","Vibrio","Alteromonas","Aquarickettsia")) +
  facet_grid(bleach~type, scales = "free_x", space = "free_x") +
  theme_bw() +
  ylab("Relative Abundance") +
  xlab("Genotype") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(fill = "Genus", size = "Genus")
bar_species
