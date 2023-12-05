## check and install packages. code by Vikram B. Baliga https://vbaliga.github.io/posts/2019-04-28-verify-that-r-packages-are-installed-and-loaded/

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest

## packages are sorted by domain. if installation is not working
## smoothly you can uncomment individual packages that may not be necessary for the current analysis 

packages = c(
  
  ## utils
  "here",
  "jtools",
  
  ## data wrangling
  "tidyverse",
  "broom",
  "memisc",
  
  ## plotting
  "gghalves",
  "ggthemes",
  "RColorBrewer",
  "scales",
  "showtext",
  "grid",
  "patchwork",
  "ggdist",
  "cowplot",
  "sjPlot",
  "ggstance",
  "ggsignif",
  
  ## stats
  "lme4",
  "lmerTest",
  "kableExtra",
  
  ## bayesian analyses 
  "rstan",
  "rstanarm",
  "brms",
  "bayesplot",
  "repmod",
  "posterior",
  "tidybayes"
)


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