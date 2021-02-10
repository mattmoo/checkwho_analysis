## library() calls go here
library(conflicted)
library(dotenv)
library(drake)


#Load required packages
required.cran.packages = list(
  "data.table",
  # For installing non-CRAN packages
  "devtools",
  # For Drake
  "dflow",
  "visNetwork",
  "fst",
  # For date manipulation
  "fasttime",
  "lubridate",
  # For figures
  "ggplot2",
  "ggpubr",
  "gghighlight",
  "gganimate",
  "scico",
  "forcats",
  
  "Hmisc",
  "stringr",
  # "snakecase",
  "coin",
  "dummies",
  "DescTools",
  "plyr",
  "quantileCI",
  "quantreg",
  "lmtest",
  # For generating tables
  "jtools",
  "huxtable",
  "gtsummary",
  # Need version 3.2.0-3 for perm function
  "bda",
  # For change point detection
  "mcp",
  "rjags",
  "coda",
  # Custom package for ICD-10
  "icd10amachi",
  # For the report
  "officer",
  
  'survey'
)

conflict_prefer("%like%", "data.table")
conflict_prefer("View", "utils")

options(gtsummary.tbl_summary.percent_fun = function(x) style_number(x * 100, digits = 1))


new.packages <- unlist(required.cran.packages[!(required.cran.packages %in% installed.packages()[,"Package"])])
if(length(new.packages)) install.packages(new.packages) 
lapply(required.cran.packages, require, character.only=T)

# options(mc.cores = 3)

# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# devtools::install_github("hoehleatsu/quantileCI", dependencies=FALSE)

# For permutation test with custom function
# install_version("bda", version = "3.2.0-3", repos ="http://cran.us.r-project.org")

# daoh.tools.path = 'P:/FMHSfiles/SCIENCE/packages/R/daohtools'
# daoh.tools.path = '../daohtools'
# # devtools::document(daoh.tools.path)
# devtools::build(daoh.tools.path)
# devtools::install(daoh.tools.path, dependencies = T, reload = T)
# devtools::install('"P:/FMHSfiles/SCIENCE/MOH_general/icd10amachi"', dependencies = T, reload = T)
# devtools::install_github('mattmoo/daohtools')
# devtools::install_github('milesmcbain/dflow')

library(daohtools)