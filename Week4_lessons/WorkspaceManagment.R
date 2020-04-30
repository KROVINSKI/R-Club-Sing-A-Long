# Hello World
# Title: 
# Author:
# Date: 


#***************************************
# Libraries
#***************************************








#***************************************
# 1.0) File Management 
#***************************************
# write R code that tells R exactly where to find critical files
# Separate raw data, analysis/scripts and outputs

# Setting the working directory
# Print my current working directory

# example from the lesson

getwd()

setwd("/cloud/project")

library(here)
library(ggplot2)

read.csv(here("Data", "mt.cars.csv))
             
My.plot <- ggplot(mt.cars,
              aes(x= mpg, y=cyl )) + 
              geom_point()
              
              ggsave(plot = My.plot,
              filename = here("Figures", "Fig.1.png"),
              width = 4,
              dpi = "retina")
              
              here()






#***************************************
# End of Script | End of Document
#***************************************