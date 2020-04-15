##Hello world- this is Kate Rovinski following along with R club example
## "Rclub_tidyverse.R" is the "clean" example


library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)

#**********************************
## 1.) Data Background Information
#**********************************
#some basics about this data:
#These are a subset of Chinook salmon samples from one of my projects

#Samples were genotyped either using RADseq or GT-seq
#with the goal of assigning Y-chromosome haplogroups to each sample
#GT-seq samples were also assigned a genetic sex by sequencing the sex determining gene
#Some samples were sequenced with both RADseq and GTseq to test for concordance of haplogroup assignment

#Phenotypic data includes morphological sex(assigned with various methods),
#fork length (cm), weight (kg), and freshwater and salwater age
#There is a lot of missing data for phenotypes

#I'll walk you through using the tidyverse to manipulate, filter, and summarize the data
#in ways that were useful for my project


#set working directory to wherever you have saved the example data and the code
##changed the working directory to where I locally have saved the tidyverse example files

#**********************************
## 2.0) Working Directory
#**********************************
setwd("/Users/katherinerovinski/GIT/R-Club-Sing-A-Long/R club tidyverse")


#**********************************
## 3.0) Bringing in Data 
#**********************************
#load example data
chinData <- read_xlsx("tidyverseExampleData.xlsx", sheet="Sheet1")

#look at example data
head(chinData)
#hopefully the column names are self-explanatory, the two whose names may need some explanation are
#FWage which is freshwater age and SWage which is saltwater age


#**********************************
## 4.0) Introducing Functions - Separate 
#**********************************

#Separate and unite functions

#the separate function splits one column into multiple columns while the unite column combines multiple columns
#into a single column.

#separate splits based on non-alphanumeric characters by default, but a delimiter can be specified with
#the sep option

#both commands remove the original column(s) by default.  This can be changed with the remove=FALSE option


#Lets add a population column to the data. 
#The sample names use a standardized format where the population ID is separated from the sample ID 
#using an underscore
#we can use the separate command to split the sample column into a population and sample number column

#look at example data again
head(chinData)

chinData %>% separate(Sample,into=c("pop","sampleID"))

#you'll notice that the original Sample column is gone, we can keep this using the remove=FALSE option if we want
chinData %>% separate(Sample,into=c("pop","sampleID"),remove=FALSE)

#let's pretend that the population and sample ID columns were separate to begin with, and that we need
#to combine them into the full sample name

#we'll start by separating the Sample name and saving as a new object
chinData_sampleSplit<-chinData %>% separate(Sample,into=c("pop","sampleID"))

#double check that this looks correct
chinData_sampleSplit
#note that we're not using the head command anymore since manipulating the data with tidy functions
#has converted this into a tibble

#make an new Sample column using the unite function
chinData_sampleSplit %>% unite(Sample,pop,sampleID,sep="_")
#you can see we now have the Sample column as it was in the original dataset



#**********************************
## 5.0) Introducing Functions - Select 
#**********************************
#Select

#The select function is used to select or remove columns from the dataset.
#This website is useful for more comprehensive details of select:
#https://dplyr.tidyverse.org/reference/select.html

#look at data again
head(chinData)

#Giving the select function a list of columns means those are the columns to keep.
# Seperating the sample and source ID

chinData %>% separate(Sample,into=c("pop","sampleID"),remove=FALSE) %>%
  select(c("Sample","Source"))
#this keeps only the Sample and Source columns

##opposite

#adding a - in from of the column names means these are columns to remove
chinData %>% separate(Sample,into=c("pop","sampleID"),remove=FALSE) %>%
  select(c(-"Sample",-"Source"))
#this keeps all columns except Sample and Source

#lets remove the sampleID column from our dataset going forward because it isn't really useful
chinData %>% separate(Sample,into=c("pop","sampleID"),remove=FALSE) %>%
  select(-sampleID)

#This is fine, but what if we want the population column to be first
#we can also reorder columns using select.
chinData %>% separate(Sample,into=c("pop","sampleID"),remove=FALSE) %>%
  select(-sampleID) %>% select(pop,everything())
#now the population column is first


#**********************************
## 6.0) Introducing Functions - Mutate 
#**********************************

#Mutate

#the mutate function can be used to add new columns, or update an existing column in a dataframe
#This website is useful for more comprehensive details of mutate:
#https://dplyr.tidyverse.org/reference/mutate.html

#lets use mutate to add a total age column that is the sum of the freswater age and saltwater age columns
chinData %>% separate(Sample,into=c("pop","sampleID"),remove=FALSE) %>%
  select(-sampleID) %>% select(pop,everything()) %>%
  mutate(totAge=FWage+SWage)

#at this step I'm going to save the manipulated data as a new R object
#This isn't necessary but will keep the code in the following examples cleaner
chinData_processed<-chinData %>% separate(Sample,into=c("pop","sampleID"),remove=FALSE) %>%
  select(-sampleID) %>% select(pop,everything()) %>%
  mutate(totAge=FWage+SWage)

chinData_processed

#***********************************
## 7.0) Introducing Functions - Filter Function 
#***********************************

#Filter function

#The filter function returns rows based on a specified criteria
#Lets try a few different ways to filter
#This website is useful for more comprehensive details of filter:
#https://dplyr.tidyverse.org/reference/filter.html

chinData_processed
#you can see there are 651 rows in the original dataset (first 10 shown with 641 more)

unique(chinData_processed$phenoSex)

#these samples were genotyped with RADseq and with GT-seq data
#lets filter the data to just ghe GT-seq genotyped samples
chinData_processed %>% filter(Source=="GTseq")
#now there are 395 rows

#we can filter to samples that were assigned a phenotypic sex
chinData_processed %>% filter(phenoSex %in% c("M","F"))
#not that the %in% operator lets you identify samples that matched either M or F for phenoSex

#we can filter to samples that were not assigned a phenotypic sex
chinData_processed %>% filter(!phenoSex %in% c("M","F"))
#adding the ! in front of the variable means you are keeping samples that do not match this criteria

#lets do a numeric filter on the RADseq genotype rate
#we'll identify really bad samples with less than 70% of the loci genotyped
chinData_processed %>% filter(RADgenoRate<=0.7)
#there are none, that's a good sign

#lets find samples with less than 90%
chinData_processed %>% filter(RADgenoRate<=0.9)

#Finally, lets exclude all samples that are missing length data
chinData_processed %>% filter(!is.na(forkLength))
#the is.na function tests if cells in this column are NA
#combining this with ! means wer are keeping samples that are not NA


#***********************************
## 8.0) Introducing Functions - Arrange Function 
#***********************************

#Arrange function

#The arrange function lets you sort data based on one or more columns
#the order matters, rows will be arranged by each variable sequentially
#sorting is in ascending order by default, can be changed to descending oder using desc()
#This website is useful for more comprehensive details of arrange: 
#https://dplyr.tidyverse.org/reference/arrange.html

chinData_processed

#let's arrange samples by state and data source
chinData_processed %>% arrange(state, Source)

#how about by state and population
chinData_processed %>% arrange(state, pop)

#we can keep adding sorting variables as desired
chinData_processed %>% arrange(state, pop, Source)

#we can revers the sort order by using desc
chinData_processed %>% arrange(desc(state), pop, Source)
#now state is sorted in reverse alphabetical order


#***********************************
## 8.0) Introducing Functions - Group_by & Summarize Function 
#***********************************

# 8.1 Group_by and summarize

#group_by is useful for grouping observations by variables, but by itself it doesn't really do anything
#I use it in combination with the summarize function to give useful data summaries
#This website is useful for more comprehensive details of group_by: 
#https://dplyr.tidyverse.org/reference/group_by.html

#see what happens when using group_by
chinData_processed %>%
  group_by(pop,phenoSex)
#nothing really looks different, let's combine it with summarize to do something useful


# 8.2 Summarize

#the summarize command gives data summaries for specified groupings, it does remove other columns though
#This website is useful for more comprehensive details of summarize: 
#https://dplyr.tidyverse.org/reference/summarise.html

#get the number of individuals of each sex in each population
chinData_processed %>%
  group_by(pop,phenoSex) %>% 
  summarize(count=n())

#get the proportion of individuals of each sex in each population
chinData_processed %>%
  group_by(pop,phenoSex) %>% 
  summarize(count=n()) %>%
  mutate(freq=count/sum(count))
#mutate operated on the grouped variables, giving the frequency of each sex within the populations

#get the average length by sex in each population
chinData_processed %>%
  group_by(pop,phenoSex) %>% 
  summarize(avgLength=mean(forkLength,na.rm=TRUE))
#we need to include na.rm=TRUE to remove NA values, 
#otherwise populations with any missing values will have an NA for the mean


#the order of the grouping variable matters, this is what happens when we reverse the grouping order
chinData_processed %>%
  group_by(phenoSex,pop) %>% 
  summarize(count=n())



#***********************************
## 9.0) Introducing Functions - Pivoting Function 
#***********************************

#Pivoting (pivot wider, pivot longer)

## 9.1 Wider
## 9.2 Longer

#pivoting makes long datasets wider (pivot_wider) or wide datasets longer (pivot_longer)
#this can be used to make untidy data tidy, or in this example it can be used to format
#data into tables for publication

#we'll start with the summary of average length by sex per population that we did in the last example
#This is a long dataset, it has many rows and few columns
#This format is tidy but in this case it isn't useful for publication
#lets make thsi wider, splitting the phenoSex column by variable into new columns
chinData_processed %>%
  group_by(pop,phenoSex) %>% 
  summarize(avgLength=mean(forkLength,na.rm=TRUE)) %>%
  pivot_wider(names_from=phenoSex, values_from=avgLength)
#this format is much better for publication





#***********************************
## 10.0) Functions of the Tidyverse - RIGHT INTO PLOTS  
#***********************************

#plotting results of commands

#one of the nice things with tidyr is that the manipulated data can be easily fed in to ggplot
#this makes it very easy to make plots, and to plot results of different filters etc..

chinData_processed

#let's look at the distribution of fork length by haplogroup
chinData_processed %>%
  ggplot(data=.) + geom_boxplot(aes(x=haplogroup,y=forkLength))



#this are commands that I commonly use int he tidyverse but this really just scratches the surface.

