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

## this is to circumvent a current issue with gghalves. should be removed in the futures if the packaged is updated to fit with ggplot
remotes::install_github("teunbrand/gghalves@compat_ggplot2_400")

options(contrasts  = c("contr.treatment" , "contr.treatment" ))
