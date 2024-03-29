#######################################################################

# MANY LEGAL LABS -- Central Executive

#######################################################################

# This script will run the entire project. When executed, it will call all relevant R scripts and render the reports for each replication study.

# Set up environment --------------------------------------------------

## Set parameters

# Should the project save data output when built?

## When set to TRUE, scripts will save calculated data files, including wrangled data and effect size calculations
## Note that if this parameter is set to TRUE, the scripts will overwrite any existing data files from previous builds of the project.

write_data <- TRUE

# Should the project read calculated data from stored files?

## When set to TRUE, scripts will import data files, rather than relying on the objects produced by previous source files
## The functionality of this parameter is currently very limited.
## Only change this to TRUE if you know what you are doing.

read_data  <- FALSE

## Check and install necessary packages for the project

### Packages required by the Many Legal Labs project



dependencies <- c("dplyr", "tidyr", "stringr", "metafor", "lavaan", "ggplot2", "cowplot", "rmarkdown", "ggstance", "flextable", "officer", "ggbeeswarm")

### Check whether packages are installed locally and get list of what needs to be installed

installation_list <- dependencies[!(dependencies %in% rownames(installed.packages()))]

### Install necessary packages

if (length(installation_list) > 0) {
  
  install.packages(installation_list, dependencies = TRUE)
  
}

## Load packages

lapply(dependencies, library, character.only = TRUE)

## Functions

### Render report

# This function is used to knit the R Markdown reports using the global environment.

mll_report <- function(input) {
  
  require(rmarkdown)
  
  render(
    input = input,
    output_format = "html_document",
    output_dir = "./reports/",
    clean = TRUE,
    envir = globalenv(),
    run_pandoc = TRUE,
    quiet = FALSE
  )
  
}

# R Scripts -----------------------------------------------------------

## Correll et al  (2002, Study 1)

source("./scripts/simulate/correll_simulation.R") # REPLACE WITH DATA IMPORTATION
source("./scripts/wrangle/correll_prelim.R")
source("./scripts/calculate/correll_core.R")
source("./scripts/analyze/correll_analysis.R")

## Darley et al (2000, Study 2)

source("./scripts/simulate/darley_simulation.R") # REPLACE WITH DATA IMPORTATION
source("./scripts/wrangle/darley_prelim.R")
source("./scripts/calculate/darley_core.R")
source("./scripts/analyze/darley_analysis.R")

## Loftus & Palmer (1974, Study 2)

source("./scripts/simulate/loftus_simulation.R") # REPLACE WITH DATA IMPORTATION
source("./scripts/wrangle/loftus_prelim.R")
source("./scripts/calculate/loftus_core.R")
source("./scripts/analyze/loftus_analysis.R")

## Serota et al (2010, Study 3)

source("./scripts/simulate/serota_simulation.R") # REPLACE WITH DATA IMPORTATION
source("./scripts/wrangle/serota_prelim.R")
source("./scripts/calculate/serota_core.R")
source("./scripts/analyze/serota_analysis.R")

## Summarize

source("./scripts/analyze/MLL_summary.R")
source("./scripts/plots/plot_layout.R")

# Markdown reports ----------------------------------------------------

## Set up directory

if (!file.exists("./reports/")) {
  
  dir.create("./reports/")
  
} 

## Correll et al  (2002, Study 1)

mll_report("report_correll.Rmd")

## Darley et al (2000, Study 2)

mll_report("report_darley.Rmd")

## Loftus & Palmer (1974, Study 2)

mll_report("report_loftus.Rmd")

## Serota et al (2010, Study 3)

mll_report("report_serota.Rmd")

## Main report

mll_report("MLL_main_report.Rmd")
