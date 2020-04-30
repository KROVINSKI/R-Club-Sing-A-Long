## Hello World
# Title:Lecture 5: Loops and Related Iterations
# Author: R Club Lectures
# Date: April 2020

#***************************************
# Libraries
#***************************************

library(yarrr)
library(schoolmath)


#***************************************
# Creating loops
#***************************************

# example from lecture of a loop 
set.seed(108)
AreaSampled <-    round(runif(1200, min = 1, max = 100), 2)
Barnacles <-      round(rnorm(n = 1200, mean = 1000, sd = 100)*AreaSampled, 0)
Month <-          rep(1:12, each = 100)
Continent <-      sample(c("Africa", "Asia", "Europe", "NAmerica", "SAmerica", "Australia", "Antarctica"), 1200, replace =T) 
myData <- tibble(AreaSampled, Barnacles, Month, Continent)
kable(myData) %>% 
  kable_styling(full_width = T, fixed_thead = T) %>% 
  scroll_box( height = "500px")


# strucutre of a for loop 

results.vector <- c()                               #create an empty container in which to store results
for (i in 1:length(unique(myData$Month))){          #set up the loop; for each unique Month...
  x <- myData$AreaSampled[myData$Month == i]        #create a subset of AreaSampled in that Month...
  y <- myData$Barnacles[myData$Month == i]          #create a subset of Barnacles counted in that Month...
  results.vector[i] <- cor(x, y)
}
plot(y = results.vector, x = 1:12)  #no, doesn't look like it




# The basic idea of a for loop is to do the same calculation (above, cor()) many times, 
# while changing something about the data in each iteration. It can be as simple as:


for(i in 1:5){    #for each value of an iteration variable
  print(paste0("Number ", i))        #do this thing 
}                 


#and when you hit the closure bracket, 
# go back and change the iteration variable to the next value in line, 
# and start over.

# Example: 

for (YARRRR in 1:5){
  print(YARRRR * 10)
}


# Exercise
# The function is.prim(x) from the package 'schoolmath' returns a logical result (TRUE / FALSE) 
# telling you whether the number x is a prime number.
# Create a for loop that returns a vector telling you whether 
# numbers from 1 to 20 are prime or not, like:
#  
# the number 1 is prime - TRUE . . . the number 4 is prime - FALSE


for (i in 1:20){
  print (paste("The number ", i, "is prime", is.prim(i)))
}







