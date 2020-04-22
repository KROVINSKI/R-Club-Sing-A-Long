################################################################
## Multivariate community analysis tutorial using vegan 
## and phyloseq
## Created by Becca maher
##############################################################

rm(list = ls())
# install packages
#install.packages("vegan")
# learn more about vegan: https://cran.r-project.org/web/packages/vegan/vegan.pdf


# load libraries
# Vegan: Community Ecology Package
# Ordination methods, diversity analysis and other functions for community and vegetation ecologists.
library("vegan")
library("dplyr")
library("ggplot2")



# load species data that comes with the vegan package
data(package = "vegan") ## names of data sets in the package

# This dataset was collected as a part of the project focused on the effect 
# of management on dune meadows. It contains 20 plots of 2×2 m2, 
# sampled in 1982 following Braun-Blanquet method estimating 
# plant cover in each plot (using 9-grade ordinal cover scale). 
# Total of 30 species (28 vascular and 2 bryophytes) were recorded.

data(dune) # Vegetation and Environment in Dutch Dune Meadows

dim(dune) #a data frame of observations of 30 species at 20 sites
# load environmental data


data("dune.env")


dim(dune.env)



## Alpha diversity ##
# calculate alpha diversity measures
diversity(dune, index = "simpson")
diversity(dune, index = "shannon")
specnumber(dune)
# make dataframe
simpson <- as.data.frame(diversity(dune, index = "simpson"))
shannon <- as.data.frame(diversity(dune, index = "shannon"))
richness <- as.data.frame(specnumber(dune))
# rename columns
colnames(simpson)[1] <- "simpson"
colnames(shannon)[1] <- "shannon"
colnames(richness)[1] <- "richness"
# combine into a dataframe
alphadiv <- cbind(dune.env, simpson, shannon, richness)
# exploratory plots
ggplot(alphadiv, aes(x = Management, y = simpson, fill = Management)) +
  geom_boxplot()
ggplot(alphadiv, aes(x = Management, y = shannon, fill = Management)) +
  geom_boxplot()
ggplot(alphadiv, aes(x = Management, y = richness, fill = Management)) +
  geom_boxplot()
# fit linear models
mod.simpson <- lm(simpson ~ Management, data = alphadiv)
anova(mod.simpson)
mod.shannon <- lm(shannon ~ Management, data = alphadiv)
anova(mod.shannon)
mod.richness <- lm(richness ~ Management, data = alphadiv)
anova(mod.richness)




## Beta diversity ##

#1# Basic sites-species ordination plot
# ordinate the data using the Bray-Curtis distance and NMDS
# Nonmetric Multidiminsional Scaling
dune.mds <- metaMDS(dune, distance = "bray")
dune.mds
plot(dune.mds)

# Ordinations
#- The ordination graphs shows the main features of similarities and differences in the data
#- If two SUs are close to each other, they have similar communities
#- If two SUs are far away from each other, the communities differ
#- If two species are close to each other, they have similar occurrence patterns, 
#  and occur most abundantly in SUs that are close to them
#- It can be assumed that environmental conditions are behind these differences and 
#  we can identify the external variables that explain  the ordination structure



#2# Site ordination colored by metadata
# add metadata to sites
site.scrs <- as.data.frame(scores(dune.mds, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Management = dune.env$Management) #add grouping variable "Management" to dataframe
site.scrs <- cbind(site.scrs, Landuse = dune.env$Use) #add grouping variable of cluster grouping to dataframe

head(site.scrs)
# plot
nmds.plot.dune <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Management), shape = factor(site.scrs$Landuse)), size = 2)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Management", shape = "Landuse")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

nmds.plot.dune + labs(title = "Basic ordination plot") #displays plot



# Fits vectors into an ordiantion: looks for maximum correlation  of variables with projection of points
dune.envfit <- envfit(dune.mds, dune.env, permutations = 999) # this fits environmental vectors
dune.spp.fit <- envfit(dune.mds, dune, permutations = 999) # this fits species vectors



#3# Site ordination colored by metadata with significant environmental vectors overlay
# isolate significant environmental vectors
env.scores.dune <- as.data.frame(scores(dune.envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.dune <- cbind(env.scores.dune, env.variables = rownames(env.scores.dune)) #and then gives them their names
env.scores.dune <- cbind(env.scores.dune, pval = dune.envfit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores.dune, pval<=0.05) #subset data to show variables significant at 0.05

head(env.scores.dune)
# plot ordination with environmental vector
nmds.plot.dune+
  geom_segment(data = sig.env.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = sig.env.scrs, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", segment.size = 0.25)+ #add labels for env variables
  labs(title="Ordination with environmental vectors")


#4# Site ordination colored by metadata with significant species vectors overlay
spp.scrs <- as.data.frame(scores(dune.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = dune.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

head(sig.spp.scrs)
# plot ordination with species vectors
nmds.plot.dune+
  geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with species vectors")



#5#  Stats
# function to compute dissimilarity indices
BC_dist <- vegdist(dune, method = "bray")
# PERMANOVA: permutational multivariate analysis of variance using distance matrices
adonis2(BC_dist ~ Management*A1, data=dune.env)
# PERMDISP: multivariate homogeneity of group dispsersions (variances)
anova(betadisper(BC_dist, dune.env$Management))




## Extra
# Ordination with ellipses
# function for ellipses

veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#data for ellipse, in this case using the management factor
df_ell.dune.management <- data.frame() #sets up a data frame before running the function.
for(g in levels(site.scrs$Management)){
  df_ell.dune.management <- rbind(df_ell.dune.management, cbind(as.data.frame(with(site.scrs [site.scrs$Management==g,],
                                                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2))))) ,Management=g))
}

# data for labelling the ellipse
NMDS.mean.dune=aggregate(site.scrs[ ,c("NMDS1", "NMDS2")], 
                         list(group = site.scrs$Management), mean)

# data for labelling the ellipse
NMDS.mean=aggregate(site.scrs[,c("NMDS1", "NMDS2")], 
                    list(group = site.scrs$Management), mean)

nmds.plot.dune+ 
  geom_path(data = df_ell.dune.management, aes(x = NMDS1, y = NMDS2, group = Management, color = Management)) #this is the ellipse, seperate ones by Site.
