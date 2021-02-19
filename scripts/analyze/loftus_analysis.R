
#######################################################################

# Loftus & Palmer (1974) [Study 2] -- Meta-Analysis

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("metafor", "dplyr", "ggplot2")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(1621)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

# Import calculated effect data ---------------------------------------

loftus_h1_smd  <- read.csv("./data/loftus_effects/loftus_h1_smd.csv")
loftus_h2_lor  <- read.csv("./data/loftus_effects/loftus_h2_lor.csv")
loftus_h3_lor  <- read.csv("./data/loftus_effects/loftus_h3_lor.csv")

# Import original effects

loftus_org <- read.csv("./data/original/darley_original.csv")

# Set study color

loftus_color_1 <- "#6D98BA"

# HYPOTHESIS 1 --------------------------------------------------------

## Random effects meta-analysis

loftus_h1_meta <- rma(
  yi = d, 
  vi = var,
  data = loftus_h1_smd,
  method = "REML"
)

## Forest plot

loftus_h1_forest <- 
  forest_plot_smd(
    meta_analysis = loftus_h1_meta, 
    replication_data = loftus_h1_smd, 
    title = "Loftus & Palmer (1974), Hypothesis 1",
    study_color = loftus_color_1,
    org_d = loftus_org[loftus_org$hypothesis == "h1", ]$d, 
    org_ci_lower = loftus_org[loftus_org$hypothesis == "h1", ]$ci_lower, 
    org_ci_upper = loftus_org[loftus_org$hypothesis == "h1", ]$ci_upper
  )

# HYPOTHESIS 2 --------------------------------------------------------

## Random effects meta-analysis

loftus_h2_meta <- rma(
  yi = d, 
  vi = var,
  data = loftus_h2_lor,
  method = "REML"
)

## Forest plot

loftus_h2_forest <- 
  forest_plot_lor(
    meta_analysis = loftus_h2_meta, 
    replication_data = loftus_h2_lor, 
    title = "Loftus & Palmer (1974), Hypothesis 2",
    study_color = loftus_color_1,
    org_lor = loftus_org[loftus_org$hypothesis == "h2", ]$lor, 
    org_ci_lower = loftus_org[loftus_org$hypothesis == "h2", ]$ci_lower, 
    org_ci_upper = loftus_org[loftus_org$hypothesis == "h2", ]$ci_upper
  )

# HYPOTHESIS 3 --------------------------------------------------------

## Random effects meta-analysis

loftus_h3_meta <- rma(
  yi = d, 
  vi = var,
  data = loftus_h3_lor,
  method = "REML"
)

## Forest plot

loftus_h3_forest <- 
  forest_plot_lor(
    meta_analysis = loftus_h2_meta, 
    replication_data = loftus_h2_lor, 
    title = "Loftus & Palmer (1974), Hypothesis 3",
    study_color = loftus_color_1,
    org_lor = loftus_org[loftus_org$hypothesis == "h3", ]$lor, 
    org_ci_lower = loftus_org[loftus_org$hypothesis == "h3", ]$ci_lower, 
    org_ci_upper = loftus_org[loftus_org$hypothesis == "h3", ]$ci_upper
  )

