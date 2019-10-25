#code for predictive analytics course

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

#specify the packages of interest
packages = c("tidyverse","topicmodels","tidytext","pdftools", "tm", "rvest", "stm", "SnowballC", "ggwordcloud", "gutenbergr", "quanteda", "furrr", "purrr")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded

problematic.packages <<- ""
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    loaded.correctly = FALSE
    try({
      install.packages(x, dependencies = TRUE)
      loaded.correctly = require(x,  character.only = TRUE)
      detach_package(x, character.only = T)
    })
    if (loaded.correctly == FALSE)
    {
      problematic.packages <<- paste(x, problematic.packages, sep=", ")
    }
  }
  else
  {
    detach_package(x, character.only = T)
  }
})

#print out those packages which require more work
cat("------------------------------------------------------------\n")
cat(paste("These packages could not be installed:",
          problematic.packages,
          "Remember this does not include tensorflow or kears.",
          "We use a separate script to install tensorflow and keras", sep="\n"))

cat(paste("If any packages are listed as problematic:", "Please restart your R session and then run the script again by clicking the 'Source' button.", sep="\n"))
cat(paste("If that does not work please try typing:", "install.packages('PROBLEMATIC_PACKAGE_NAME')", "Any Warning messages can be ignored.", sep="\n"))