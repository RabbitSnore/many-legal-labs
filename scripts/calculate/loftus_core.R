
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

### Mediation analysis functions

#### A function to fit mediation models and extract parameters of interest

mediation_calc <- function(ID, x, m, y) {
  
  require(lavaan)
  
  # Specification
  
  model_spec <- 
  '
  # Regressions
  
  m ~ a*x
  
  y ~ c*x + b*m
  
  # Defined values
  
  indirect := a*b
  direct   := c
  
  '
  
  # Fitting
  
  med_fit <- sem(
    model = model_spec,
    data = data.frame(x, m, y),
    ordered = "y" # Set the outcome variable as categorical
    )
  
  med_standard <- standardizedsolution(med_fit) # Create a data frame with standardized values
  
  # Extract values
  
  direct            <- med_standard[med_standard$lhs == "direct", ]$est.std
  direct_var        <- med_standard[med_standard$lhs == "direct", ]$se^2
  direct_ci_upper   <- med_standard[med_standard$lhs == "direct", ]$ci.upper
  direct_ci_lower   <- med_standard[med_standard$lhs == "direct", ]$ci.lower
  indirect          <- med_standard[med_standard$lhs == "indirect", ]$est.std
  indirect_var      <- med_standard[med_standard$lhs == "indirect", ]$se^2
  indirect_ci_upper <- med_standard[med_standard$lhs == "indirect", ]$ci.upper
  indirect_ci_lower <- med_standard[med_standard$lhs == "indirect", ]$ci.lower
  
  # Construct data frame
  
  out <- data.frame(
    ID                = ID, 
    direct            = direct, 
    direct_var        = direct_var, 
    direct_ci_upper   = direct_ci_upper, 
    direct_ci_lower   = direct_ci_lower, 
    indirect          = indirect, 
    indirect_var      = indirect_var, 
    indirect_ci_upper = indirect_ci_upper, 
    indirect_ci_lower = indirect_ci_lower
    )
  
  return(out)
  
}

#### Create an empty data frame for use with mediation_calc()

empty_med_data <- function(n) {
  
  out <- data.frame(
    ID                = rep(NA, n),
    direct            = rep(NA, n),
    direct_var        = rep(NA, n),
    direct_ci_upper   = rep(NA, n),
    direct_ci_lower   = rep(NA, n),
    indirect          = rep(NA, n),
    indirect_var      = rep(NA, n),
    indirect_ci_upper = rep(NA, n), 
    indirect_ci_lower = rep(NA, n)
  )
  
  return(out)
  
}

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
    
    x = loftus$verb_condition[loftus$lab == unique(loftus$lab)[i]], 
    y = loftus$speed_estimate[loftus$lab == unique(loftus$lab)[i]], 
    
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
    
    x = loftus$verb_condition[loftus$lab == unique(loftus$lab)[i]], 
    y = loftus$broken_glass[loftus$lab == unique(loftus$lab)[i]], 
    
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
    
    x = loftus$verb_condition[loftus$lab == unique(loftus$lab)[i]], 
    y = loftus$broken_glass[loftus$lab == unique(loftus$lab)[i]], 
    
    cond_1 = "smash", 
    cond_2 = "control"
    
  )
  
}

# HYPOTHESIS 4 --------------------------------------------------------

# The effect of smashed (vs. hit) on reporting broken glass will be partially (not fully) mediated by increased speed estimates

## Set up data

loftus_med <- loftus %>% 
  mutate(
    verb_condition = case_when(
      verb_condition == "smash"   ~ 1,
      verb_condition == "hit"     ~ 0,
      verb_condition == "control" ~ NA_real_
    )
  )

## Calculate effect sizes

### Set up empty data frame for effects

loftus_h4_med <- empty_med_data(lab_count_loftus)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_loftus) {
  
  loftus_h4_med[i, ] <- mediation_calc(
    
    ID = unique(loftus$lab)[i], 
    
    x = loftus_med$verb_condition[loftus_med$lab == unique(loftus$lab)[i]], 
    m = loftus_med$speed_estimate[loftus_med$lab == unique(loftus$lab)[i]], 
    y = loftus_med$broken_glass[loftus_med$lab == unique(loftus$lab)[i]]
    
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

## Hypothesis 4

if (!file.exists("./data/loftus_effects/loftus_h4_med.csv")) {
  
  write.csv(
    loftus_h4_medr,
    "./data/loftus_effects/loftus_h4_med.csv",
    row.names = FALSE
  )
  
}