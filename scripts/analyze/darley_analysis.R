
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

## Recommended punishment

darley_h1_smd  <- read.csv("./data/darley_effects/darley_h1_smd.csv")
darley_h2_smd  <- read.csv("./data/darley_effects/darley_h2_smd.csv")
darley_h3_smd  <- read.csv("./data/darley_effects/darley_h3_smd.csv")

## Attributions of responsibility

darley_mc1_lor <- read.csv("./data/darley_effects/darley_mc1_lor.csv")
darley_mc2_lor <- read.csv("./data/darley_effects/darley_mc2_lor.csv")
darley_mc3_lor <- read.csv("./data/darley_effects/darley_mc3_lor.csv")

## Recommended outcomes

darley_mc4_lor  <- read.csv("./data/darley_effects/darley_mc4_lor.csv")
darley_mc5_lor  <- read.csv("./data/darley_effects/darley_mc5_lor.csv")
darley_mc6_lor  <- read.csv("./data/darley_effects/darley_mc6_lor.csv")
darley_mc7_lor  <- read.csv("./data/darley_effects/darley_mc7_lor.csv")
darley_mc8_lor  <- read.csv("./data/darley_effects/darley_mc8_lor.csv")
darley_mc9_lor  <- read.csv("./data/darley_effects/darley_mc9_lor.csv")
darley_mc10_lor <- read.csv("./data/darley_effects/darley_mc10_lor.csv")
darley_mc11_lor <- read.csv("./data/darley_effects/darley_mc11_lor.csv")
darley_mc12_lor <- read.csv("./data/darley_effects/darley_mc12_lor.csv")

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

# MANIPULATION CHECK 4 ------------------------------------------------

## Random effects meta-analysis

darley_mc4_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc4_lor,
  method = "REML"
)

## Forest plot

darley_mc4_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc4_meta, 
    replication_data = darley_mc4_lor, 
    title = "Darley et al (2000), Manipulation Check 4",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc4", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc4", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc4", ]$ci_upper
  )

# MANIPULATION CHECK 5 ------------------------------------------------

## Random effects meta-analysis

darley_mc5_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc5_lor,
  method = "REML"
)

## Forest plot

darley_mc5_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc5_meta, 
    replication_data = darley_mc5_lor, 
    title = "Darley et al (2000), Manipulation Check 5",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc5", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc5", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc5", ]$ci_upper
  )

# MANIPULATION CHECK 6 ------------------------------------------------

## Random effects meta-analysis

darley_mc6_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc6_lor,
  method = "REML"
)

## Forest plot

darley_mc6_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc6_meta, 
    replication_data = darley_mc6_lor, 
    title = "Darley et al (2000), Manipulation Check 6",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc6", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc6", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc6", ]$ci_upper
  )

# MANIPULATION CHECK 7 ------------------------------------------------

## Random effects meta-analysis

darley_mc7_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc7_lor,
  method = "REML"
)

## Forest plot

darley_mc7_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc7_meta, 
    replication_data = darley_mc7_lor, 
    title = "Darley et al (2000), Manipulation Check 7",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc7", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc7", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc7", ]$ci_upper
  )

# MANIPULATION CHECK 8 ------------------------------------------------

## Random effects meta-analysis

darley_mc8_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc8_lor,
  method = "REML"
)

## Forest plot

darley_mc8_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc8_meta, 
    replication_data = darley_mc8_lor, 
    title = "Darley et al (2000), Manipulation Check 8",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc8", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc8", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc8", ]$ci_upper
  )

# MANIPULATION CHECK 9 ------------------------------------------------

## Random effects meta-analysis

darley_mc9_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc9_lor,
  method = "REML"
)

## Forest plot

darley_mc9_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc9_meta, 
    replication_data = darley_mc9_lor, 
    title = "Darley et al (2000), Manipulation Check 9",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc9", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc9", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc9", ]$ci_upper
  )

# MANIPULATION CHECK 10 -----------------------------------------------

## Random effects meta-analysis

darley_mc10_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc10_lor,
  method = "REML"
)

## Forest plot

darley_mc10_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc10_meta, 
    replication_data = darley_mc10_lor, 
    title = "Darley et al (2000), Manipulation Check 10",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc10", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc10", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc10", ]$ci_upper
  )

# MANIPULATION CHECK 11 -----------------------------------------------

## Random effects meta-analysis

darley_mc11_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc11_lor,
  method = "REML"
)

## Forest plot

darley_mc11_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc11_meta, 
    replication_data = darley_mc11_lor, 
    title = "Darley et al (2000), Manipulation Check 11",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc11", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc11", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc11", ]$ci_upper
  )

# MANIPULATION CHECK 12 -----------------------------------------------

## Random effects meta-analysis

darley_mc12_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = darley_mc12_lor,
  method = "REML"
)

## Forest plot

darley_mc12_forest <- 
  forest_plot_lor(
    meta_analysis = darley_mc12_meta, 
    replication_data = darley_mc12_lor, 
    title = "Darley et al (2000), Manipulation Check 12",
    study_color = darley_color_1,
    org_lor = darley_org[darley_org$hypothesis == "mc12", ]$lor,
    org_ci_lower = darley_org[darley_org$hypothesis == "mc12", ]$ci_lower, 
    org_ci_upper = darley_org[darley_org$hypothesis == "mc12", ]$ci_upper
  )

