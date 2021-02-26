
#######################################################################

# Correll, Park, Judd, & Wittenbrink (2002) [Study 1] -- Effect Size Calculation

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr", "tidyr")

lapply(packages, library, character.only = TRUE)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

# Import wrangled data ------------------------------------------------

correll <- read.csv("./data/correll_wrangle.csv")

# Set up basic information --------------------------------------------

lab_count_correll <- length(unique(correll$lab)) # Number of labs providing data

## Names of columns

latency_cols  <- c("black_armed_latency", 
                   "black_unarmed_latency", 
                   "white_armed_latency", 
                   "white_unarmed_latency")

accuracy_cols <- c("black_armed_accuracy", 
                   "black_unarmed_accuracy", 
                   "white_armed_accuracy", 
                   "white_unarmed_accuracy")

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
  extract(
    col = "trial_type", 
    into = c("race", "object", "variable"), 
    regex = "(.*)_(.*)_(.*)"
  )

## Calculate effect sizes

### Set up empty data frame for effects

correll_h1_smd <- empty_smd_data(lab_count_correll)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_correll) {
  
  correll_h1_smd[i, ] <- d_calc(
    
    ID = unique(correll_h1_data$lab)[i], 
    
    x = correll_h1_data$object[correll_h1_data$lab == unique(correll_h1_data$lab)[i]], 
    y = correll_h1_data$latency[correll_h1_data$lab == unique(correll_h1_data$lab)[i]], 
    
    cond_1 = "armed", 
    cond_2 = "unarmed"
    
    )
  
}

# HYPOTHESIS 2 --------------------------------------------------------

# Participants are faster to fire at an armed target if the target is African American than if the target is White.

# Standardized mean difference in Mean Response Latency for Ethnicity, for Gun trials

## Set up data

correll_h2_data <- correll %>% 
  pivot_longer(
    cols = latency_cols,
    names_to = "trial_type",
    values_to = "latency"
  ) %>% 
  extract(
    col = "trial_type", 
    into = c("race", "object", "variable"), 
    regex = "(.*)_(.*)_(.*)"
  ) %>% 
  filter(object == "armed")

## Calculate effect sizes

### Set up empty data frame for effects

correll_h2_smd <- empty_smd_data(lab_count_correll)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_correll) {
  
  correll_h2_smd[i, ] <- d_calc(
    
    ID = unique(correll_h2_data$lab)[i], 
    
    x = correll_h2_data$race[correll_h2_data$lab == unique(correll_h2_data$lab)[i]], 
    y = correll_h2_data$latency[correll_h2_data$lab == unique(correll_h2_data$lab)[i]], 
    
    cond_1 = "black", 
    cond_2 = "white"
    
  )
  
}

# HYPOTHESIS 3 --------------------------------------------------------

# Participants are faster to not fire at an unarmed target if the target is White than if the target is African American.

# Standardized mean difference in Mean Response Latency for Ethnicity, for No-Gun trials

## Set up data

correll_h3_data <- correll %>% 
  pivot_longer(
    cols = all_of(latency_cols),
    names_to = "trial_type",
    values_to = "latency"
  ) %>% 
  extract(
    col = "trial_type", 
    into = c("race", "object", "variable"), 
    regex = "(.*)_(.*)_(.*)"
  ) %>% 
  filter(object == "unarmed")

## Calculate effect sizes

### Set up empty data frame for effects

correll_h3_smd <- empty_smd_data(lab_count_correll)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_correll) {
  
  correll_h3_smd[i, ] <- d_calc(
    
    ID = unique(correll_h3_data$lab)[i], 
    
    x = correll_h3_data$race[correll_h3_data$lab == unique(correll_h3_data$lab)[i]], 
    y = correll_h3_data$latency[correll_h3_data$lab == unique(correll_h3_data$lab)[i]], 
    
    cond_1 = "black", 
    cond_2 = "white"
    
  )
  
}

# HYPOTHESIS 4 --------------------------------------------------------

# Participants are more likely to shoot an unarmed target (i.e., false alarms) than to not-shoot an armed target (i.e., misses).

#  Standardized mean difference in Errors for Object Type, across Ethnicity

## Set up data

correll_h4_data <- correll %>% 
  pivot_longer(
    cols = all_of(accuracy_cols),
    names_to = "trial_type",
    values_to = "accuracy"
  ) %>% 
  extract(
    col = "trial_type", 
    into = c("race", "object", "variable"), 
    regex = "(.*)_(.*)_(.*)"
  )

## Calculate effect sizes

### Set up empty data frame for effects

correll_h4_smd <- empty_smd_data(lab_count_correll)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_correll) {
  
  correll_h4_smd[i, ] <- d_calc(
    
    ID = unique(correll_h4_data$lab)[i], 
    
    x = correll_h4_data$object[correll_h4_data$lab == unique(correll_h4_data$lab)[i]], 
    y = correll_h4_data$accuracy[correll_h4_data$lab == unique(correll_h4_data$lab)[i]], 
    
    cond_1 = "armed", 
    cond_2 = "unarmed"
    
  )
  
}

# Export Calculated Effect Sizes --------------------------------------

## If the data directory does not exist, it will be necessary to create it

if (!file.exists("./data/correll_effects/")) {
  
  dir.create("./data/correll_effects/")
  
} 

## Hypothesis 1

if (!file.exists("./data/correll_effects/correll_h1_smd.csv")) {
  
  write.csv(
    correll_h1_smd,
    "./data/correll_effects/correll_h1_smd.csv",
    row.names = FALSE
  )
  
}

## Hypothesis 2

if (!file.exists("./data/correll_effects/correll_h2_smd.csv")) {
  
  write.csv(
    correll_h2_smd,
    "./data/correll_effects/correll_h2_smd.csv",
    row.names = FALSE
  )
  
}

## Hypothesis 3

if (!file.exists("./data/correll_effects/correll_h3_smd.csv")) {
  
  write.csv(
    correll_h3_smd,
    "./data/correll_effects/correll_h3_smd.csv",
    row.names = FALSE
  )
  
}

## Hypothesis 4

if (!file.exists("./data/correll_effects/correll_h4_smd.csv")) {
  
  write.csv(
    correll_h4_smd,
    "./data/correll_effects/correll_h4_smd.csv",
    row.names = FALSE
  )
  
}

