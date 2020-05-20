#load libraries needed later on
library(adegenet)
library(ggplot2)
library(tibble)
library(dplyr)
library(stringr)

#Rclub functions

#benefits of functions:
# functions are great for repetitive tasks
# you can write a function to do the work once instead of the same lines of code over and over
# this streamlines the code in your projects but also has multiple other benefits
#   Functions reduce copy/paste errors
#   If you need to  change the code, you can update the function in one place
#   rather than every place the commands have been copied and pasted in your project

#basic anatomy of a function
# myfunction <- function(arg1, arg2, ... ){
#   statements
#   return(object)
# }


#There are three key steps to creating a new function, according to Hadley Wickham:
  
#1) You need to pick a name for the function. 
#   This name should make sense and relate to the purpose of the function
#   Also, avoid names that are already used for other functions (ie min, max).
#   In the function above, myFunction is the name


#2) You list the inputs, or arguments, to the function inside function. 
#   The function above has two arguments (arg1 and arg2).  
#   The ... means other unspecified arguments can be passed in.  
#   These arguments can be used as input to other functions within this function.
#   For example, you could add xlim(0,20) to set xlimits if a plot is generated within this function

#3) You place the code you have developed in body of the function, 
#   a { block that immediately follows function(...).
#   In the code above, this is the following lines:
#   statements                                  
#   return(object)
# }

#the return() statement specifies what will be the final output from the function

#simple example function
#these examples taken from: https://swcarpentry.github.io/r-novice-inflammation/02-func-R/

#convert fahrenheit to celcius
fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

#freezing point of water in Celcius
fahrenheit_to_celsius(32)

#boiling point of water in Celcius
fahrenheit_to_celsius(212)

#extra bits- Celcius Conversion
fahrenheit_to_celsius(65)

#extra bits- Celcius Conversion
fahrenheit_to_celsius(55)

#convert celcius to kelvin
celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

# freezing point of water in Kelvin
celsius_to_kelvin(0)


#convert fahrenheit to kelvin by combining functions within another function
fahrenheit_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}

# freezing point of water in Kelvin
fahrenheit_to_kelvin(32.0)


#some things to remember:
# Scope is important for functions
#   variables that are created within functions are only used within the function and not accessible outside of it
#   this is a big advantage for naming objects, otherwise you would have to worry about objects with the same
#   names inside and outside functions
# 
# If a function requires an input and you don't provide it, it will fail
#   You can tell functions that certain inputs are optional, or give them default values to allow flexibility
#   I will have examples of this below



#let's make a more complicated function

#the goal of this function is to take a set of genotypes as input, plot a PCA, and return the PCA results as output

#let's start by setting the working directory and loading the genotype data.
#The genotype data is the Chinook salmon baseline published in Clemento et al. 2014.

#set working directory
setwd("/Users/katherinerovinski/GIT/R-Club-Sing-A-Long/Functions/drive-download-20200520T190521Z-001")
#load some genotypes
genoData<-read.delim("2010_Baseline_SNPset_genepop_table.txt",header=TRUE,row.names=1,
                     stringsAsFactors=FALSE,colClasses="character")


#if I was doing it once, these are the steps I would take:

#save genotype data as new object for purposes of example
exampleGenos<-genoData

#convert missing genotypes to NA
exampleGenos[exampleGenos=="000000"]<-NA

#convert genotypes table to genind file
exampleGenos_genind<-df2genind(exampleGenos, ncode=3)

#do pca
exampleGenos_scaleGen<-scaleGen(exampleGenos_genind, NA.method="mean")
exampleGenos_PCA<-dudi.pca(exampleGenos_scaleGen, cent=FALSE, scale = FALSE, scannf=FALSE, nf=4)

#save pca results
exampleGenos_PCA_results<-exampleGenos_PCA$li

#get population from sample name
exampleGenos_PCA_results<-rownames_to_column(exampleGenos_PCA_results,var="sample")
exampleGenos_PCA_results<-exampleGenos_PCA_results %>% mutate(population=str_split_fixed(sample,"--",2)[,1])

#plot results
plot<-ggplot()+geom_point(data = exampleGenos_PCA_results, aes(x=Axis1,y=Axis2))

#this works fine but if I want to try PCAs using different sets of samples or loci
#then I have to either rewrite this whole block of code each time or I have to change the input (exampleGenos)
#and overwrite or save as a new object at the end each time.  Either way is tedious and prone to mistakes.

#an easy solution is to convert the block of code into a function.
#to make the function below I've just copied and pasted the code block above, wrapped in {} 
#and then added the line specifying the function name and arguments.  
#I also changed "exampleGenos" to "genotypes" in the object names 
#because this follows the normal naming scheme I would use.

#make function to plot the PCA
plotPCA<-function(genotypes,...){
  
  #convert missing genotypes to NA
  genotypes[genotypes=="000000"]<-NA
  
  #convert genotypes table to genind file
  genotypes_genind<-df2genind(genotypes, ncode=3)
  
  #do pca
  genotypes_scaleGen<-scaleGen(genotypes_genind, NA.method="mean")
  genotypes_PCA<-dudi.pca(genotypes_scaleGen, cent=FALSE, scale = FALSE, scannf=FALSE, nf=4)
  
  #save pca results
  genotypes_PCA_results<-genotypes_PCA$li
  
  #get population from sample name
  genotypes_PCA_results<-rownames_to_column(genotypes_PCA_results,var="sample")
  genotypes_PCA_results<-genotypes_PCA_results %>% mutate(population=str_split_fixed(sample,"--",2)[,1])
  
  #plot results
  plot<-ggplot()+geom_point(data = genotypes_PCA_results, aes(x=Axis1,y=Axis2,...))
  print(plot)
  
  #return results
  return(genotypes_PCA_results)
}

#now we can run all that code with just this command
plotPCA(genoData)

#remember the ...?  I've set up the function so that passes arguments into the ggplot statement
#let's tell it to color code samples by population
plotPCA(genoData,color=population)



#this only plots axis 1 and 2, what if we want the options to plot others?

plotPCA2<-function(genotypes,xAxis="Axis1",yAxis="Axis2",...){

  #convert missing genotypes to NA
  genotypes[genotypes=="000000"]<-NA
  
  #convert genotypes table to genind file
  genotypes_genind<-df2genind(genotypes, ncode=3)
  
  #do pca
  genotypes_scaleGen<-scaleGen(genotypes_genind, NA.method="mean")
  genotypes_PCA<-dudi.pca(genotypes_scaleGen, cent=FALSE, scale = FALSE, scannf=FALSE, nf=4)
  
  #save pca results
  genotypes_PCA_results<-genotypes_PCA$li
  
  #get population from sample name
  genotypes_PCA_results<-rownames_to_column(genotypes_PCA_results,var="sample")
  genotypes_PCA_results<-genotypes_PCA_results %>% mutate(population=str_split_fixed(sample,"--",2)[,1])
  
  #plot results
  #note that I am using aes_string instead of aes to allow the axes to be specified from the function arguments
  plot<-ggplot()+geom_point(data = genotypes_PCA_results, aes_string(x=xAxis,y=yAxis,...))
  print(plot)
  
  #return results
  return(genotypes_PCA_results)
}

plotPCA2(genoData, xAxis="Axis1", yAxis="Axis2",color="population")
plotPCA2(genoData, xAxis="Axis2", yAxis="Axis3",color="population")


#let's modify the function to allow filtering by population

plotPCA3<-function(genotypes,xAxis="Axis1",yAxis="Axis2",excludePops,...){
  
  #convert missing genotypes to NA
  genotypes[genotypes=="000000"]<-NA
  
  #filter samples out if their population is listed in excludePops
  genotypes <- genotypes %>% rownames_to_column(var="sample") %>%
    mutate(population=str_split_fixed(sample,"--",2)[,1]) %>% 
    filter(!population %in% excludePops) %>% select(-population) %>%
    column_to_rownames(var="sample")
    
  
  #convert genotypes table to genind file
  genotypes_genind<-df2genind(genotypes, ncode=3)
  
  #do pca
  genotypes_scaleGen<-scaleGen(genotypes_genind, NA.method="mean")
  genotypes_PCA<-dudi.pca(genotypes_scaleGen, cent=FALSE, scale = FALSE, scannf=FALSE, nf=4)
  
  #save pca results
  genotypes_PCA_results<-genotypes_PCA$li
  
  #get population from sample name
  genotypes_PCA_results<-rownames_to_column(genotypes_PCA_results,var="sample")
  genotypes_PCA_results<-genotypes_PCA_results %>% mutate(population=str_split_fixed(sample,"--",2)[,1])
  
  #plot results
  #note that I am using aes_string instead of aes to allow the axes to be specified from the function arguments
  plot<-ggplot()+geom_point(data = genotypes_PCA_results, aes_string(x=xAxis,y=yAxis,...))
  print(plot)
  
  #return results
  return(genotypes_PCA_results)
}

plotPCA3(genoData, xAxis="Axis1", yAxis="Axis2",color="population",excludePops="CentralValleyfa")

#what happens now if we don't tell it to exclude any populations?
plotPCA3(genoData, xAxis="Axis1", yAxis="Axis2",color="population")


#let's fix the function so that we have the option of specifying populations to exclude
plotPCA4<-function(genotypes,xAxis="Axis1",yAxis="Axis2",excludePops=NULL,...){
  
  #convert missing genotypes to NA
  genotypes[genotypes=="000000"]<-NA
  
  #filter samples out if their population is listed in excludePops
  genotypes <- genotypes %>% rownames_to_column(var="sample") %>%
    mutate(population=str_split_fixed(sample,"--",2)[,1]) %>% 
    filter(!population %in% excludePops) %>% select(-population) %>%
    column_to_rownames(var="sample")
  
  
  #convert genotypes table to genind file
  genotypes_genind<-df2genind(genotypes, ncode=3)
  
  #do pca
  genotypes_scaleGen<-scaleGen(genotypes_genind, NA.method="mean")
  genotypes_PCA<-dudi.pca(genotypes_scaleGen, cent=FALSE, scale = FALSE, scannf=FALSE, nf=4)
  
  #save pca results
  genotypes_PCA_results<-genotypes_PCA$li
  
  #get population from sample name
  genotypes_PCA_results<-rownames_to_column(genotypes_PCA_results,var="sample")
  genotypes_PCA_results<-genotypes_PCA_results %>% mutate(population=str_split_fixed(sample,"--",2)[,1])
  
  #plot results
  #note that I am using aes_string instead of aes to allow the axes to be specified from the function arguments
  plot<-ggplot()+geom_point(data = genotypes_PCA_results, aes_string(x=xAxis,y=yAxis,...))
  print(plot)
  
  #return results
  return(genotypes_PCA_results)
}

plotPCA4(genoData, xAxis="Axis1", yAxis="Axis2",color="population",excludePops="CentralValleyfa")

#what happens now if we don't tell it to exclude any populations?
plotPCA4(genoData, xAxis="Axis1", yAxis="Axis2",color="population")
