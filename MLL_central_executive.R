#######################################################################

# MANY LEGAL LABS -- Central Executive

#######################################################################

# This script will run the entire project. When executed, it will call all relevant R scripts and render the reports for each replication study.

# Set up environment --------------------------------------------------

packages_central <- c("rmarkdown")

lapply(packages_central, library, character.only = TRUE)

## Functions

### Render report

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


