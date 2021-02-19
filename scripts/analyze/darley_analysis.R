
#######################################################################

# Darley, Carlsmith, & Robinson (2000) [Study 2] -- Meta-Analysis

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("metafor", "dplyr", "ggplot2")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(540)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

# Import calculated effect data ---------------------------------------

darley_h1_smd  <- read.csv("./data/darley_effects/darley_h1_smd.csv")
darley_h2_smd  <- read.csv("./data/darley_effects/darley_h2_smd.csv")
darley_h3_smd  <- read.csv("./data/darley_effects/darley_h3_smd.csv")

darley_mc1_lor <- read.csv("./data/darley_effects/darley_mc1_lor.csv")
darley_mc2_lor <- read.csv("./data/darley_effects/darley_mc2_lor.csv")
darley_mc3_lor <- read.csv("./data/darley_effects/darley_mc3_lor.csv")

# Import original effects

darley_org <- read.csv("./data/original/darley_original.csv")

# Set study color

darley_color_1 <- "#8E4585"

# HYPOTHESIS 1 --------------------------------------------------------

## Random effects meta-analysis

darley_h1_meta <- rma(
  yi = d, 
  vi = var,
  data = darley_h1_smd,
  method = "REML"
  )

## Forest plot

darley_h1_forest <- 
  forest_plot_smd(
    meta_analysis = darley_h1_meta, 
    replication_data = darley_h1_smd, 
    title = "Darley et al (2000), Hypothesis 1",
    study_color = darley_color_1,
    org_d = darley_org[darley_org$hypothesis == "h1", ]$d, 
    org_ci_lower = darley_org[darley_org$hypothesis == "h1", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "h1", ]$ci_upper
  )

# HYPOTHESIS 2 --------------------------------------------------------

## Random effects meta-analysis

darley_h2_meta <- rma(
  yi = d, 
  vi = var,
  data = darley_h2_smd,
  method = "REML"
)

## Forest plot

darley_h2_forest <- 
  forest_plot_smd(
    meta_analysis = darley_h2_meta, 
    replication_data = darley_h2_smd, 
    title = "Darley et al (2000), Hypothesis 2",
    study_color = darley_color_1,
    org_d = darley_org[darley_org$hypothesis == "h2", ]$d, 
    org_ci_lower = darley_org[darley_org$hypothesis == "h2", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "h2", ]$ci_upper
  )

# HYPOTHESIS 3 --------------------------------------------------------

## Random effects meta-analysis

darley_h3_meta <- rma(
  yi = d, 
  vi = var,
  data = darley_h3_smd,
  method = "REML"
)

## Forest plot

darley_h3_forest <- 
  forest_plot_smd(
    meta_analysis = darley_h3_meta, 
    replication_data = darley_h3_smd, 
    title = "Darley et al (2000), Hypothesis 3",
    study_color = darley_color_1,
    org_d = darley_org[darley_org$hypothesis == "h3", ]$d, 
    org_ci_lower = darley_org[darley_org$hypothesis == "h3", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "h3", ]$ci_upper
  )

# MANIPULATION CHECK 1 ------------------------------------------------

## Random effects meta-analysis

darley_mc1_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc1_lor,
  method = "REML"
)

## Forest plot

darley_mc1_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc1_meta, 
    replication_data = darley_mc1_lor, 
    title = "Darley et al (2000), Manipulation Check 1",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc1", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc1", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc1", ]$ci_upper
  )

# MANIPULATION CHECK 2 ------------------------------------------------

## Random effects meta-analysis

darley_mc2_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc2_lor,
  method = "REML"
)

## Forest plot

darley_mc2_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc2_meta, 
    replication_data = darley_mc2_lor, 
    title = "Darley et al (2000), Manipulation Check 2",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc2", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc2", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc2", ]$ci_upper
  )

# MANIPULATION CHECK 3 ------------------------------------------------

## Random effects meta-analysis

darley_mc3_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc3_lor,
  method = "REML"
)

## Forest plot

darley_mc3_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc3_meta, 
    replication_data = darley_mc3_lor, 
    title = "Darley et al (2000), Manipulation Check 3",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc3", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc3", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc3", ]$ci_upper
  )

