
#######################################################################

# Loftus & Palmer (1974) [Study 2] -- Effect Size Calculation

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr", "tidyr")

lapply(packages, library, character.only = TRUE)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

# Import wrangled data ------------------------------------------------

loftus <- read.csv("./data/loftus_wrangle.csv")

# Set up basic information --------------------------------------------

lab_count_loftus <- length(unique(loftus$lab)) # Number of labs providing data

# HYPOTHESIS 1 --------------------------------------------------------

# Participants will provide higher speed estimates in the "smashed" condition compared to the "hit" condition.

# Standardized mean difference of speed estimates between "smashed" and "hit"

## Calculate effect sizes 

### Set up empty data frame for effects

loftus_h1_smd <- empty_smd_data(lab_count_loftus)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_loftus) {
  
  loftus_h1_smd[i, ] <- d_calc(
    
    ID = unique(loftus$lab)[i], 
    
    x = loftus$verb_condition[loftus$lab == i], 
    y = loftus$speed_estimate[loftus$lab == i], 
    
    cond_1 = "smash", 
    cond_2 = "hit"
    
  )
  
}

# HYPOTHESIS 2 --------------------------------------------------------

# Participants will more frequently report seeing broken glass in the "smashed" condition compared to the "hit" condition.

# Proportion difference (odds ratios) in reporting broken glass between "smashed" and "hit"

## Calculate effect sizes 

### Set up empty data frame for effects

loftus_h2_lor <- empty_lor_data(lab_count_loftus)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_loftus) {
  
  loftus_h2_lor[i, ] <- odds_calc(
    
    ID = unique(loftus$lab)[i], 
    
    x = loftus$verb_condition[loftus$lab == i], 
    y = loftus$broken_glass[loftus$lab == i], 
    
    cond_1 = "smash", 
    cond_2 = "hit"
    
  )
  
}

# HYPOTHESIS 3 --------------------------------------------------------

# Participants will more frequently report seeing broken glass in the "smashed" condition compared to the "control" condition.

# Proportion difference (odds ratios) in reporting broken glass between "smashed" and "control"

## Calculate effect sizes 

### Set up empty data frame for effects

loftus_h3_lor <- empty_lor_data(lab_count_loftus)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_loftus) {
  
  loftus_h3_lor[i, ] <- odds_calc(
    
    ID = unique(loftus$lab)[i], 
    
    x = loftus$verb_condition[loftus$lab == i], 
    y = loftus$broken_glass[loftus$lab == i], 
    
    cond_1 = "smash", 
    cond_2 = "control"
    
  )
  
}

# Export Calculated Effect Sizes --------------------------------------

## If the data directory does not exist, it will be necessary to create it

if (!file.exists("./data/loftus_effects/")) {
  
  dir.create("./data/loftus_effects/")
  
} 

## Hypothesis 1

if (!file.exists("./data/loftus_effects/darley_h1_smd.csv")) {
  
  write.csv(
    loftus_h1_smd,
    "./data/loftus_effects/loftus_h1_smd.csv",
    row.names = FALSE
  )
  
}

## Hypothesis 2

if (!file.exists("./data/loftus_effects/loftus_h2_lor.csv")) {
  
  write.csv(
    loftus_h2_lor,
    "./data/loftus_effects/loftus_h2_lor.csv",
    row.names = FALSE
  )
  
}

## Hypothesis 3

if (!file.exists("./data/loftus_effects/loftus_h3_lor.csv")) {
  
  write.csv(
    loftus_h3_lor,
    "./data/loftus_effects/loftus_h3_lor.csv",
    row.names = FALSE
  )
  
}