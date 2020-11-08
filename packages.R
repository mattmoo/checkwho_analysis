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
  "scico",
  
  "Hmisc",
  "stringr",
  # "snakecase",
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
  "officer"
)

conflict_prefer("%like%", "data.table")

options(gtsummary.tbl_summary.percent_fun = function(x) style_number(x * 100, digits = 1))


new.packages <- unlist(required.cran.packages[!(required.cran.packages %in% installed.packages()[,"Package"])])
if(length(new.packages)) install.packages(new.packages) 
lapply(required.cran.packages, require, character.only=T)

# options(mc.cores = 3)

# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# devtools::install_github("hoehleatsu/quantileCI", dependencies=FALSE)

# For permutation test with custom function
# install_version("bda", version = "3.2.0-3", repos ="http://cran.us.r-project.org")

# devtools::document('../../../packages/daohtools')
# devtools::build('../../../packages/daohtools')
# devtools::install('../../../packages/daohtools', dependencies = T, reload = T)
# devtools::install('"P:/FMHSfiles/SCIENCE/MOH_general/icd10amachi"', dependencies = T, reload = T)
# devtools::install_github('mattmoo/daohtools')
# devtools::install_github('milesmcbain/dflow')

library(daohtools)