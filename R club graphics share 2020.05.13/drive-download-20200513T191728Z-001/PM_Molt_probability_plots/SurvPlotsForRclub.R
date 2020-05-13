
# detach all previously loaded packages to prevent package conflicts
library(ggplot2) #at least one library needs to loaded for the next line to work
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

#load libraries for this project
library(survival) 
library(survminer)

#set the working directory
setwd("/Users/paul.mcelhany/Documents/Github Projects/juvAnalysis")

#The original date in format compatible with physical data sheets to reduce data entry error
# The data frame is NOT tidy (Hadley would be appalled)
d <- read.csv("Master juv log.csv", 
              stringsAsFactors=FALSE,  header = TRUE)

## Look at stacked bar plot of the data

###########
### Basic survival plot

# Some dark magic happens (not shown) to make a data frame for survival plots
dsub2 <- read.csv("dsub.csv", stringsAsFactors = FALSE)

# Make survival object from survival package
# Specify time, event and type of data censoring
# The survival object is the response variable in a survival analysis model
surv <- Surv(time = dsub2$day, event = dsub2$isDead, type = "right")
# The survival object is basically the days column with desinations for right censored observations
surv
# a survival curve (Kaplan-Meier) fit from survival package
sf <- survfit(surv ~ Treatment, dsub2)
summary(sf)

#set the font size for the plots
fontSize <- 20
#the survival plot from the survminer package
pSurv <- ggsurvplot(survfit(surv ~ Treatment, dsub2), risk.table = FALSE, pval = FALSE, conf.int = TRUE,
                    font.main = fontSize, font.x =  fontSize, font.y = fontSize, 
                    font.tickslab = fontSize, font.legend = fontSize,
                    break.time.by = 40, legend = c(0.3, 0.3), palette = c("red","blue"),
                    legend.title = "pH Treatment", 
                    legend.labs = c("pH = 7.15 (7.13-7.18)", "pH = 7.77 (7.76-7.79)"))
pSurv +  xlab("Time (Days)") 

### Look at CI + jitter plot of molt timing

##################
## Molt probability curves (like survial curve, but event is molting instead of death)

#More dark magic (not shown) to make a data frame for molt probability curves
dMoltEvents2 <- read.csv("dMoltEvents.csv", stringsAsFactors = FALSE)

#
#Make a survival fit for each molt transition
# the J1 to J2 fit
s1 <- survfit(Surv(day, J1toJ2event, type = "right") ~ Treatment, data = dMoltEvents2)
#plot of J1 to J2 
p1 <- ggsurvplot(s1, data = dMoltEvents2, risk.table = FALSE, pval = FALSE, conf.int = TRUE,
                 font.main = 24, font.x =  24, font.y = 24, font.tickslab = 24, font.legend = 10,
                 break.time.by = 3, legend.title = "Treatment", xlab = "Time in days")
p1
# make the survial fits for all the other transitions
s2 <- survfit(Surv(day, J2toJ3event, type = "right") ~ Treatment, data = dMoltEvents2)
s3 <- survfit(Surv(day, J3toJ4event, type = "right") ~ Treatment, data = dMoltEvents2)
s4 <- survfit(Surv(day, J4toJ5event, type = "right") ~ Treatment, data = dMoltEvents2)
s5 <- survfit(Surv(day, J5toJ6event, type = "right") ~ Treatment, data = dMoltEvents2)
s6 <- survfit(Surv(day, J6toJ7event, type = "right") ~ Treatment, data = dMoltEvents2)

#combine all of the individual transition fits into a list so we can plot them all on same graph
fit <- list(S1 = s1, S2 = s2, S3 = s3, S4 = s4, S5 = s5, S6 = s6)
# the plot with all transition fits 
# combine = TRUE combines all the fits on one plot
# fun = "event" reverses y axis from survival to probabiliy of the event
# linetype = "strata" uses different lineypes for each treatment
# censor = FALSE suppresses showing censored points (they clutter the graph, which is already pretty busy)
gp <- ggsurvplot(fit, data = dMoltEvents2, combine = TRUE,
                 risk.table = FALSE, pval = FALSE, conf.int = TRUE, fun = "event", linetype = "strata",
                 font.main = 24, font.x =  24, font.y = 24, font.tickslab = 24, font.legend = 16,
                 break.time.by = 40, censor = FALSE, size = 1.5, legend = "none",
                 xlab = "Time in days",ylab = "Molt Probability")
gp
#Vector of line types for graph to replace defaults
lines <-  c(rep(c("dotted", "solid"), times = 6))
# Plot beautification (set linetype, add horizontal line at median, add manual text legend)
# Can add ggplot calls to gp$plot, which is a ggplot object 
gp$plot <- gp$plot + scale_linetype_manual(values = lines) + 
  geom_hline(yintercept = 0.5, linetype = 2) +
  annotate("text",x = 120, y = 1.05, label = "Solid = low CO2; Dashed = high CO2; Color group by molt stage")
gp
