
#######################################################################

# Correll, Park, Judd, & Wittenbrink (2002) [Study 1] -- Effect Size Calculation

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr", "tidyr")

lapply(packages, library, character.only = TRUE)

## Functions

source("./scripts/calculate/effect_size_functions.R")

# Import wrangled data ------------------------------------------------

# correll <- read.csv("./data/correll_wrangled.csv") # THIS WILL NOT WORK YET

# Set up basic information --------------------------------------------

lab_count <- length(unique(correll$lab))

latency_cols <- c("black_armed_latency", "black_unarmed_latency", "white_armed_latency", "white_unarmed_latency")

# HYPOTHESIS 1 --------------------------------------------------------

# Participants are faster at making a correct decision to shoot, when the target holds a gun, than the correct decision to not shoot, when the target does not hold a gun.

# Standardized mean difference in Mean Response Latency for Object Type, across Ethnicity

## Set up data

correll_h1_data <- correll %>% 
  pivot_longer(
    cols = latency_cols,
    names_to = "trial_type",
    values_to = "latency"
  ) %>% 
  extract(col = "trial_type", into = c("race", "object", "variable"), regex = "(.*)_(.*)_(.*)")

## Calculate effect sizes

### Set up empty data frame for effects

correll_h1_smd <- data.frame(
  ID = 1:lab_count,
  d = rep(NA, lab_count),
  var = rep(NA, lab_count),
  ci_lower = rep(NA, lab_count),
  ci_upper = rep(NA, lab_count)
)

### Standardized mean differences for each lab

for (i in 1:lab_count) {
  
  correll_h1_smd[i, ] <- d_calc(
    
    ID = correll_h1_data$lab[i], 
    
    x = correll_h1_data$object[correll_h1_data$lab == i], 
    y = correll_h1_data$latency[correll_h1_data$lab == i], 
    
    cond_1 = "armed", 
    cond_2 = "unarmed"
    
    )
  
}

# HYPOTHESIS 2 --------------------------------------------------------

# Participants are faster to fire at an armed target if the target is African American than if the target is White.

# Standardized mean difference in Mean Response Latency for Ethnicity, for Gun trials

# HYPOTHESIS 3 --------------------------------------------------------

# Participants are faster to not fire at an unarmed target if the target is White than if the target is African American.

# Standardized mean difference in Mean Response Latency for Ethnicity, for No-Gun trials

# HYPOTHESIS 4 --------------------------------------------------------

# Participants are more likely to shoot an unarmed target (i.e., false alarms) than to not-shoot an armed target (i.e., misses).

#  Standardized mean difference in Errors for Object Type, across Ethnicity
