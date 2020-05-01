## Hello World
# Title:Lecture 5: Loops and Related Iterations
# Author: R Club Lectures
# Date: April 2020

#***************************************
# Libraries
#***************************************

library(yarrr)
library(schoolmath)
library(ggplot2)
library(nycflights13)


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



par(mfrow = c(2, 2))  

# Set up a 2 x 2 plotting space. Because you set up the space with mfrow, the plotting space will be filled
# row by row, left to right. Try out what happens if you use mfcol = c(2,2)
# Create the loop.vector (all the columns)


loop.vector <- 1:4
for (i in loop.vector)  # Loop over loop.vector

# store data in column.i as x

    x <- examscores[,i]
  
  # Plot histogram of x
  hist(x,
       main = paste("Question", i),
       xlab = "Scores",
       xlim = c(0, 100))



# 2-Dimensional Matrix with a loop
  
  results <- matrix(NA, nrow=length(unique(myData$Continent)), ncol=length(unique(myData$Month)))
  #create a matrix of the correct size
  for (i in 1: length(unique(myData$Continent))){  #indexing by name
    for (j in 1:length(unique(myData$Month))){  #indexing by numerical index
      x <- myData$AreaSampled[myData$Continent == unique(myData$Continent)[i] & myData$Month == j]
      y <- myData$Barnacles[myData$Continent == unique(myData$Continent)[i] & myData$Month == j]
      results[i,j] <- cor(x, y)
    } 
  }  
  #This gives you a matrix of correlation coefficients; it's easiest to look at that in the form of a heatmap
  heatmap(results, Rowv = NA, Colv = NA, labRow = unique(myData$Continent), xlab = "Month")
  
  

  
  # Creating loops for plots
  
  # For loops and nyc flights as exmaple
  #
  # Remember the `for` loop we asked for in the Problem Set2? Let's get back to it.
  # Now let's try again the task originally assigned: make a `for` loop that creates an histogram of the departure times from each NY airport 
  n.flights <- with (flights, table(dest)) # Get the number of flights to each airport
  n.flights <- sort(desc(n.flights))    # Sort it
  top10 <- names (n.flights)[1:10]      # Keep the names of the top10
  orgin.airports <- unique(flights$origin)   # The departure airports, conveniently in a vector
  
  
  
# FOR LOOPs & WHILE LOOPs the difference 
  
# while - in case you don't know how many times you want to iterate


  i <- 1
  while (i^2 < 200) { 
    print (paste("The square of", i, "is", i^2))
    i <- i+1
  }

  
#***************************************
# Creating loops
#***************************************
  
# apply and friends: lapply, sapply
#   A way of performing loops without actually writing the loops is using the apply and related functions. These functions use:
    
#     An input dataset (upon which you want to perform the action)
#   A function, which can either be built-in or customized
#   The apply function works when your input dataset is a dataframe or a matrix, and you want to perform row-wise or column-wise operations.
  
  


