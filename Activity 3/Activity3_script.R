###################
# Activity 3 Script
# Tyler Pantle
###################

# Installing Packages: Step 1
# install lubridate package, then change it to a comment
## install.packages(c("lubridate"))
# step 1 only has to be done...
# ...any time you're on a new computer, updating r, you have to re-install your packages

# Installing Packages: Step 2
# step 2 has to be done every time!
library(lubridate)
# this should return a warning about 'date'
# 'date' is a function in both the lubridate package and base R
# using the 'library' code attaches the lubridate function

# Create a function 
# The names of the arguements for your function will be in parentheses
# Everything in curly brackets will be run each time the function is run
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message) # if the statement is false, it prints the error message...if it's true, it prints nothing
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

# evaluate a true statement
assert(2 == 2, "error: unequal values")

# set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")
