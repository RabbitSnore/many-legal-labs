
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
loftus_h4_med  <- read.csv("./data/loftus_effects/loftus_h4_med.csv")

# Import original effects

loftus_org <- read.csv("./data/original/loftus_original.csv")

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
  yi = log_odds, 
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
  yi = log_odds, 
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

# HYPOTHESIS 4 --------------------------------------------------------

## Data wrangle

loftus_h4_med_long <- loftus_h4_med %>% 
  pivot_longer(
    cols = c(ends_with("est")),
    names_to = c("type", "parameter"),
    values_to = "estimate",
    names_pattern = "(.*)_(.*)"
  ) %>% 
  mutate(
    var = case_when(
      type == "direct"   ~ direct_var,
      type == "indirect" ~ indirect_var
    ),
    ci_upper = case_when(
      type == "direct"   ~ direct_ci_upper,
      type == "indirect" ~ indirect_ci_upper
    ),
    ci_lower = case_when(
      type == "direct"   ~ direct_ci_lower,
      type == "indirect" ~ indirect_ci_lower
    )
  ) %>% 
  select(-starts_with("direct"), -starts_with("indirect"), -parameter)

## Multivariate meta-analysis

# Multivariate meta-analysis simultaneously synthesizing direct and indirect effects, as recommended by Cheung (2020, 10.31234/osf.io/df6jp) and Cheung and Cheung (2016, 10.1002/jrsm.1166).

loftus_h4_meta <- rma.mv(
  yi = estimate, 
  V = var,
  mods = ~ type - 1,
  random = ~ type|ID,
  data = loftus_h4_med_long,
  method = "REML"
)

## Forest plot (indirect effect)

## Forest plot (direct effect)

