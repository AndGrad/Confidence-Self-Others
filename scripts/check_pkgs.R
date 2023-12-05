## check and install packages.

## check if package manager is needed, if yes install
if (!require("pacman")) install.packages("pacman")

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


pacman::p_load(char = packages)
