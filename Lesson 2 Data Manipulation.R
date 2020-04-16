## Hello World
## 2020.04.16 R Club 

#*********************************
##Lesson 2 Data Manipulation
#*********************************


#*********************************
##Lesson 2 | Section 1- types of Data
#*********************************

# Scalars, Lists, Vectors, Dataframes
# Scalars- [elements] 
# Vectors- [elements] collection of the same elements 
# Lists- - [elements] any collection of different types of elements

#Rmarkdown: Combines text with code and plots and calculations all in one place


#*********************************
##Lesson 2 | Section 2- Windows- "Console"
#*********************************
#Console the space where code/commands are executed


#*********************************
##Lesson 2 | Section 3- Packages and Datasets (different functions/tools)
#*********************************
# packages extensions for R 
# re - install packages for different versions of R 
# use functions enough and you might just stumble into being a library author


#*********************************
##Lesson 2 | Section 4- Functions and Arguments
#*********************************

#exmaple
library(tidyverse)
library(yarrr)

AirPassengers
str(AirPassengers)
summary(AirPassengers)

CO2

summary(CO2)

#*********************************
##Lesson 2 | Section 5- Subsetting and Indexing
#*********************************

##subsetting with brackets

y <- data.frame(A= LETTERS[1:5],
                B= 1:5,
                C= sqrt (6:10))
y[1,2]

# To change one element we need to use the <- (or ->) operator
# if you name the vector then you have more confidence that you can select the correct one


#*********************************
##Lesson 2 | Section 6- Subsetting based on name with dollar sign
#*********************************

# In many contexts, the dollar sign is the most useful way of subsetting. When you want to take a single column of a dataframe, for example, and use it as a vector, this is the way to do it.

names(pirates)
dim(pirates)
pirates
#you will reach the max- [ reached 'max' / getOption("max.print") -- omitted 942 rows ]
#use the function view
View(pirates)
#object should display in the global environment
colnames(pirates)
> colnames(pirates)
#[1] "id"              "sex"             "age"            
#[4] "height"          "weight"          "headband"       
#[7] "college"         "tattoos"         "tchests"        
#[10] "parrots"         "favorite.pirate" "sword.type"     
#[13] "eyepatch"        "sword.time"      "beard.length"   
#[16] "fav.pixar"       "grogg"

pirates$tchests

hist(pirates$tattoos)

#*********************************
##Lesson 2 | Section 7- Subsetting based on name with brackets  
#*********************************

colnames(WorldPhones)

Amers <- WorldPhones[,c("N.Amer", "S.Amer", "Mid.Amer")]  
totals <- rowsum(Amers)

#error will occur- we need to make them numeric
is.numeric(row.names(Amers))
years <- as.numeric(row.names(Amers))
totals <- rowsum(Amers)

plot(totals~years)

#*********************************
##Lesson 2 | Section 8- Invoking the Data Viewer 
#*********************************
?View
#Invoke a Data Viewer
#Description
#Invoke a spreadsheet-style data viewer on a matrix-like R object.

#Usage
#View(x, title)
#Arguments

#*********************************
##Lesson 2 | Section 9- Reading-in and Writing-out data 
#*********************************
#If you have a .txt file that you want to read into R, use the read_delim() function. Many of the data files use commas to separate files.

#If you want to know how to use a particular function, type ?<function.name> in the Console
read_csv(file = "Data/mtcars.csv)
         my.cars <-read_delim(file = "Data/mtcars.csv", delim = ',')
         
         #Reading in a text file titled mtcars.csv, which is in the Data folder.
         ?read_delim
         
         #Writing out
         #Some times the file you want to save is more complex than a dataframe. You can use write_rds() to create an R object that can be imported into another R session with read_rds()
         
         #exmaple of how to write out save(study1.df, score.by.sex, study1.htest,
         file = "data/study1.RData")
         
         