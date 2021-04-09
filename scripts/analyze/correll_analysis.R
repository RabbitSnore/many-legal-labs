
#######################################################################

# Correll, Park, Judd, & Wittenbrink (2002) [Study 1] -- Meta-Analysis

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("metafor", "dplyr", "ggplot2")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(666)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

# Import calculated effect data ---------------------------------------

correll_h1_smd <- read.csv("./data/correll_effects/correll_h1_smd.csv")
correll_h2_smd <- read.csv("./data/correll_effects/correll_h2_smd.csv")
correll_h3_smd <- read.csv("./data/correll_effects/correll_h3_smd.csv")
correll_h4_smd <- read.csv("./data/correll_effects/correll_h4_smd.csv")
correll_h5_smd <- read.csv("./data/correll_effects/correll_h5_smd.csv")

# Import original effects

correll_org <- read.csv("./data/original/correll_original.csv")

# Set study color

correll_color_1 <- "#FF7F50"

# HYPOTHESIS 1 --------------------------------------------------------

## Random effects meta-analysis

correll_h1_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h1_smd,
  method = "REML"
  )

## Forest plot

correll_h1_forest <- 
  forest_plot_smd(
    meta_analysis = correll_h1_meta, 
    replication_data = correll_h1_smd, 
    title = "Correll et al (2002), Hypothesis 1",
    study_color = correll_color_1,
    org_d = correll_org[correll_org$hypothesis == "h1", ]$d, 
    org_ci_lower = correll_org[correll_org$hypothesis == "h1", ]$ci_lower, 
    org_ci_upper = correll_org[correll_org$hypothesis == "h1", ]$ci_upper
    )

# HYPOTHESIS 2 --------------------------------------------------------

## Random effects meta-analysis

correll_h2_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h2_smd,
  method = "REML"
)

## Forest plot

correll_h2_forest <- 
  forest_plot_smd(
    meta_analysis = correll_h2_meta, 
    replication_data = correll_h2_smd, 
    title = "Correll et al (2002), Hypothesis 2",
    study_color = correll_color_1,
    org_d = correll_org[correll_org$hypothesis == "h2", ]$d, 
    org_ci_lower = correll_org[correll_org$hypothesis == "h2", ]$ci_lower, 
    org_ci_upper = correll_org[correll_org$hypothesis == "h2", ]$ci_upper
  )

# HYPOTHESIS 3 --------------------------------------------------------

## Random effects meta-analysis

correll_h3_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h3_smd,
  method = "REML"
)

## Forest plot

correll_h3_forest <- 
  forest_plot_smd(
    meta_analysis = correll_h3_meta, 
    replication_data = correll_h3_smd, 
    title = "Correll et al (2002), Hypothesis 3",
    study_color = correll_color_1,
    org_d = correll_org[correll_org$hypothesis == "h3", ]$d, 
    org_ci_lower = correll_org[correll_org$hypothesis == "h3", ]$ci_lower, 
    org_ci_upper = correll_org[correll_org$hypothesis == "h3", ]$ci_upper
  )

# HYPOTHESIS 4 --------------------------------------------------------

## Random effects meta-analysis

correll_h4_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h4_smd,
  method = "REML"
)

## Forest plot

correll_h4_forest <- 
  forest_plot_smd(
    meta_analysis = correll_h4_meta, 
    replication_data = correll_h4_smd, 
    title = "Correll et al (2002), Hypothesis 4",
    study_color = correll_color_1,
    org_d = correll_org[correll_org$hypothesis == "h4", ]$d, 
    org_ci_lower = correll_org[correll_org$hypothesis == "h4", ]$ci_lower, 
    org_ci_upper = correll_org[correll_org$hypothesis == "h4", ]$ci_upper
  )

# HYPOTHESIS 5 --------------------------------------------------------

## Random effects meta-analysis

correll_h5_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h5_smd,
  method = "REML"
)

## Forest plot

correll_h5_forest <- 
  forest_plot_smd(
    meta_analysis = correll_h5_meta, 
    replication_data = correll_h5_smd, 
    title = "Correll et al (2002), Hypothesis 5",
    study_color = correll_color_1,
    org_d = correll_org[correll_org$hypothesis == "h5", ]$d, 
    org_ci_lower = correll_org[correll_org$hypothesis == "h5", ]$ci_lower, 
    org_ci_upper = correll_org[correll_org$hypothesis == "h5", ]$ci_upper
  )
