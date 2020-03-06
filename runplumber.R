library(plumber)
library(jsonlite)
library(dplyr)
r <- plumb("plumber.R")  # Where 'plumber.R' is the location of the file shown above
r$run(port=8000)
    
