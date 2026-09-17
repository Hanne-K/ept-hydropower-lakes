#############################################

# 1 Setup

#############################################

# Description: Setup of packages and other settings

# Setup data handling ----------------------------------------------------------

## Install & load function
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
  require(x, character.only = TRUE)
}

## names of packages to be installed (if not installed yet) and loaded
package_vec <- c(
  "here", # package for locating and storing files with relative paths
  "dplyr", # data manipulation
  "knitr", # for rmarkdown table visualisations
  "forcats", # solving common problems with categorical variables
  "rgbif",
  "sf", # an alternative spatial object library
  "geosphere", # compute spatial distances
  "mapview",
  "ggplot2", # for visualistion
  "ggpubr", # publication ready plots
  "cowplot",
  "scales", # adjusting ggplot scales
  "ggExtra", # add marginal density plot to ggplot
  "report", # for citing packages including versions
  "stringr", # for string operations
  "readxl",
  "flextable",
  "RColorBrewer",
  "tidyr",
  "MASS", # modern applied statistics with S
  "lme4", # Mixed-effects models
  "glmmTMB", # modelling package
  "MuMIn", # Model selection, AICc, dredge
  "effects", # plot and view model parameters
  "emmeans",  # get predicted values
  "lattice", # diagnostic plots
  "fitdistrplus", # fit distributions
  "DHARMa", # residual diagnostics package, required for performance package
  "performance", # check model performance
  "COUNT",
  "msme",
  "gridExtra",
  "grid",
  "sads",
  "vegan",
  "betapart",
  "indicspecies" # package for indicator species analysis
)

## executing install & load for each package
sapply(package_vec, install.load.package)
