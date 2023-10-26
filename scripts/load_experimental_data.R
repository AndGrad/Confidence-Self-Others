###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###                              LOAD DATA                              ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------

packages = c("tidyverse")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# This imports the cleaned experimental data, prior to any data wrangling. 
# read study one data (BEAST)
beast_data <- read.csv('data/beast/clean_data/clean_data.csv') ## @andrea change filename to something unique!!

# read study two data (Elections)
elections_data <- read.csv('data/elections/clean_data/clean_data.csv') ## @andrea change filename to something unique!!

