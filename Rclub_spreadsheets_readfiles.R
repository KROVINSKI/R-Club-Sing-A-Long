#### 2020-05-06 R CLUB -- best practices for spreadsheets and reading data into R ####

# install and load packages
# install.packages("tidyverse") -- remove comment if you need to install it

library(tidyverse)
library(tidyr)

# set working directory
setwd("/Users/katherinerovinski/GIT/R-Club-Sing-A-Long") # set the working directory

# read steelhead return data from Sashin Creek, Alaska, 2019 -- in three flavors
#### read as an excel file ####
# Note that despite readxl being part of the tidyverse, it's not core tidyverse and needs to be explicitly loaded
library(readxl) 
steelhead_19_xl <- read_excel("2019 Sashin Omykiss data & weir notes R club (1).xlsx", sheet = "WILD adults - weir - R")
# now click on the dataframe and see what it looks like after import

# format dates
steelhead_19_xl$Date <- as.Date(steelhead_19_xl$Date, "%Y-%m-%d") # make this recognizable as a date to R
steelhead_19_xl$`Kelt Out (Date/Time)` <- as.Date(steelhead_19_xl$`Kelt Out (Date/Time)`, "%Y-%m-%d") # make this recognizable as a date to R

# make a separate variable for year, by extracting the year from the date
steelhead_19_xl$Year <- format(as.Date(steelhead_19_xl$Date, "%Y-%m-%d"), "%Y")

# add year designation to the sample ID
steelhead_19_xl$sampleID <- paste(as.character(steelhead_19_xl$Date,format = "%y"), steelhead_19_xl$`Genetics ID`, sep = "-")

# remove extra rows
steelhead_19_xl <- steelhead_19_xl[-c(41:46),]
colnames(steelhead_19_xl) <- c("number", "adult.return.date", "Time", "sex", "adult.return.FL", "Weight (Kg)", "Genetics.ID", 
                                       "PITtag", "adult.kelt.outmigration.date", "RBC.gametes.collected", "scale.card", "Comments", "Year",
                                       "Sample.ID")

# summarize steelhead mean FL, weight, numbers
mean(steelhead_19_xl$adult.return.FL, na.rm = T) # 682 mm
mean(steelhead_19_xl$`Weight (Kg)`, na.rm = T) # 3334 g


#### read as a csv file ####
# can be done in base R with read.csv, or tidyverse read_csv
# save as in Excel as CSV file then import
steelhead_19_csv <- read_csv(file = "2019 Sashin Omykiss data & weir notes R club.csv")
# now click on the dataframe and see what it looks like after import

# format dates
steelhead_19_csv$Date <- as.Date(steelhead_19_csv$Date, "%m/%d/%Y") # make this recognizable as a date to R
steelhead_19_csv$`Kelt Out (Date/Time)` <- as.Date(steelhead_19_csv$`Kelt Out (Date/Time)`, "%m/%d/%y") # make this recognizable as a date to R

# make a separate variable for year, by extracting the year from the date
steelhead_19_csv$Year <- format(as.Date(steelhead_19_csv$Date, "%m/%d/%Y"), "%Y")

# add year designation to the sample ID
steelhead_19_csv$sampleID <- paste(as.character(steelhead_19_csv$Date,format = "%y"), steelhead_19_csv$`Genetics ID`, sep = "-")

# remove extra rows -- look at datafile imported to see that there are extra rows and columns
steelhead_19_csv <- steelhead_19_csv[c(1:40), -c(13:18)]
colnames(steelhead_19_csv) <- c("number", "adult.return.date", "Time", "sex", "adult.return.FL", "Weight (Kg)", "Genetics.ID", 
                               "PITtag", "adult.kelt.outmigration.date", "RBC.gametes.collected", "scale.card", "Comments", "Year",
                               "Sample.ID")

# summarize steelhead mean FL, weight, numbers
mean(steelhead_19_csv$adult.return.FL, na.rm = T) # 682 mm
mean(steelhead_19_csv$`Weight (Kg)`, na.rm = T) # 3334 g

#### read as a csv file ####
# can be done in base R with read.csv, or tidyverse read_tsv
# save as in Excel as CSV file then import
steelhead_19_tsv <- read_tsv(file = "2019 Sashin Omykiss data & weir notes R club.txt")
# now click on the dataframe and see what it looks like after import

# format dates
steelhead_19_tsv$Date <- as.Date(steelhead_19_tsv$Date, "%m/%d/%Y") # make this recognizable as a date to R
steelhead_19_tsv$`Kelt Out (Date/Time)` <- as.Date(steelhead_19_tsv$`Kelt Out (Date/Time)`, "%m/%d/%y") # make this recognizable as a date to R

# make a separate variable for year, by extracting the year from the date
steelhead_19_tsv$Year <- format(as.Date(steelhead_19_tsv$Date, "%m/%d/%Y"), "%Y")

# add year designation to the sample ID
steelhead_19_tsv$sampleID <- paste(as.character(steelhead_19_tsv$Date,format = "%y"), steelhead_19_tsv$`Genetics ID`, sep = "-")

# remove extra rows -- look at datafile imported to see that there are extra rows and columns
steelhead_19_tsv <- steelhead_19_tsv[c(1:40), -c(13:18)]
colnames(steelhead_19_tsv) <- c("number", "adult.return.date", "Time", "sex", "adult.return.FL", "Weight (Kg)", "Genetics.ID", 
                                "PITtag", "adult.kelt.outmigration.date", "RBC.gametes.collected", "scale.card", "Comments", "Year",
                                "Sample.ID")

# summarize steelhead mean FL, weight, numbers
mean(steelhead_19_tsv$adult.return.FL, na.rm = T) # 682 mm
mean(steelhead_19_tsv$`Weight (Kg)`, na.rm = T) # 3334 g



#### try to read from the GDrive ####
# see https://googlesheets4.tidyverse.org/ for a reference, but cannot get this to work with NOAA Google
# install.packages("devtools")
devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
url <- 'drive.google.com/open?id=1KvzqiLA9xxSA7NFWpr28x3D6PTe6BCOgbJHqbwqC1II' # only works with a publicly viewable URL
steelhead_19_gsheet4 <- range_read(url, sheet = 2)

install.packages("gsheet")
library(gsheet)
steelhead_19_gsheet <- gsheet2tbl(url, sheetid = 2) # sheetID not behaving as it should
