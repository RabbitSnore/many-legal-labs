
#######################################################################

# Darley, Carlsmith, & Robinson (2000) [Study 2] -- Effect Size Calculation

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr", "tidyr")

lapply(packages, library, character.only = TRUE)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

# Import wrangled data ------------------------------------------------

darley <- read.csv("./data/darley_wrangle.csv")

# Set up basic information --------------------------------------------

lab_count_darley <- length(unique(darley$lab)) # Number of labs providing data

# HYPOTHESIS 1 --------------------------------------------------------

# Participants will recommend higher punishment when the perpetrator acted in a jealous rage, compared to when the behavior was caused by an inoperable tumor.

# Standardized mean difference for punishment recommendation, for jealous rage vs. inoperable tumor

## Calculate effect sizes 

### Set up empty data frame for effects

darley_h1_smd <- empty_smd_data(lab_count_darley)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_darley) {
  
  darley_h1_smd[i, ] <- d_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$punishment[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "inoperable"
    
  )
  
}

# HYPOTHESIS 2 --------------------------------------------------------

# Participants will recommend higher punishment when the perpetrator acted in a jealous rage, compared to when the behavior was caused by an operable tumor.

# Standardized mean difference for punishment recommendation, for jealous rage vs. operable tumor

## Calculate effect sizes 

### Set up empty data frame for effects

darley_h2_smd <- empty_smd_data(lab_count_darley)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_darley) {
  
  darley_h2_smd[i, ] <- d_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$punishment[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "operable"
    
  )
  
}

# HYPOTHESIS 3 --------------------------------------------------------

# Participants will recommend higher punishment when the perpetrator acted because of an inoperable tumor, compared to when the behavior was caused by an operable tumor.

# Standardized mean difference for punishment recommendation, for operable tumor vs. inoperable tumor

## Calculate effect sizes 

### Set up empty data frame for effects

darley_h3_smd <- empty_smd_data(lab_count_darley)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_darley) {
  
  darley_h3_smd[i, ] <- d_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$punishment[darley$lab == i], 
    
    cond_1 = "inoperable", 
    cond_2 = "operable"
    
  )
  
}

# MANIPULATION CHECK 1 ------------------------------------------------

# Participants will attribute responsibility to the actor more frequently in the jealous rage condition, compared to when the behavior was caused by an inoperable tumor.

# Proportion differences (odds ratios) in attributing responsibility to the actor, for Scenario (jealous rage vs. inoperable).

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc1_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc1_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$attribution[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "inoperable"
    
  )
  
}

# MANIPULATION CHECK 2 ------------------------------------------------

# Participants will attribute responsibility to the actor more frequently in the jealous rage condition, compared to when the behavior was caused by an inoperable tumor.

# Proportion differences (odds ratios) in attributing responsibility to the actor, for Scenario (jealous rage vs. operable).

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc2_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc2_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$attribution[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "operable"
    
  )
  
}


# MANIPULATION CHECK 3 -----------------------------------------------

# Participants will attribute responsibility to the actor more frequently in the inoperable tumor condition compared to the operable tumor condition.
# Darley et al. found a nonsignificant difference for this comparison, but the direction of proportions was such that more people attributed responsibility to the actor when the tumor was inoperable.

# Proportion difference (odds ratios) in attributing responsibility to the actor, for Scenario (operable vs. inoperable)

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc3_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc3_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$attribution[darley$lab == i], 
    
    cond_1 = "inoperable", 
    cond_2 = "operable"
    
  )
  
}


# MANIPULATION CHECK 4 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc4_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc4_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$prison[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "inoperable"
    
  )
  
}

# MANIPULATION CHECK 5 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc5_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc5_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$prison[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "operable"
    
  )
  
}

# MANIPULATION CHECK 6 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc6_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc6_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$prison[darley$lab == i], 
    
    cond_1 = "inoperable", 
    cond_2 = "operable"
    
  )
  
}

# MANIPULATION CHECK 7 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc7_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc7_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$mental_inst[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "inoperable"
    
  )
  
}

# MANIPULATION CHECK 8 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc8_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc8_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$mental_inst[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "operable"
    
  )
  
}

# MANIPULATION CHECK 9 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc9_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc9_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$mental_inst[darley$lab == i], 
    
    cond_1 = "inoperable", 
    cond_2 = "operable"
    
  )
  
}

# MANIPULATION CHECK 10 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc10_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc10_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$free[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "inoperable"
    
  )
  
}

# MANIPULATION CHECK 11 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc11_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc11_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$free[darley$lab == i], 
    
    cond_1 = "jealous_rage", 
    cond_2 = "operable"
    
  )
  
}

# MANIPULATION CHECK 12 -----------------------------------------------

## Calculate effect sizes 

### Set up empty data frames for effects

darley_mc12_lor <- empty_lor_data(lab_count_darley)

### Compute log odds ratios for each lab

for (i in 1:lab_count_darley) {
  
  darley_mc12_lor[i, ] <- odds_calc(
    
    ID = unique(darley$lab)[i], 
    
    x = darley$scenario[darley$lab == i], 
    y = darley$free[darley$lab == i], 
    
    cond_1 = "inoperable", 
    cond_2 = "operable"
    
  )
  
}

# Export Calculated Effect Sizes --------------------------------------

## If the data directory does not exist, it will be necessary to create it

if (!file.exists("./data/darley_effects/")) {
  
  dir.create("./data/darley_effects/")
  
} 

## Hypothesis 1

if (!file.exists("./data/darley_effects/darley_h1_smd.csv")) {
  
  write.csv(
    darley_h1_smd,
    "./data/darley_effects/darley_h1_smd.csv",
    row.names = FALSE
  )
  
}

## Hypothesis 2

if (!file.exists("./data/darley_effects/darley_h2_smd.csv")) {
  
  write.csv(
    darley_h2_smd,
    "./data/darley_effects/darley_h2_smd.csv",
    row.names = FALSE
  )
  
}

## Hypothesis 3

if (!file.exists("./data/darley_effects/darley_h3_smd.csv")) {
  
  write.csv(
    darley_h3_smd,
    "./data/darley_effects/darley_h3_smd.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 1

if (!file.exists("./data/darley_effects/darley_mc1_lor.csv")) {
  
  write.csv(
    darley_mc1_lor,
    "./data/darley_effects/darley_mc1_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 2

if (!file.exists("./data/darley_effects/darley_mc2_lor.csv")) {
  
  write.csv(
    darley_mc2_lor,
    "./data/darley_effects/darley_mc2_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 3

if (!file.exists("./data/darley_effects/darley_mc3_lor.csv")) {
  
  write.csv(
    darley_mc3_lor,
    "./data/darley_effects/darley_mc3_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 4

if (!file.exists("./data/darley_effects/darley_mc4_lor.csv")) {
  
  write.csv(
    darley_mc4_lor,
    "./data/darley_effects/darley_mc4_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 5

if (!file.exists("./data/darley_effects/darley_mc5_lor.csv")) {
  
  write.csv(
    darley_mc5_lor,
    "./data/darley_effects/darley_mc5_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 6

if (!file.exists("./data/darley_effects/darley_mc6_lor.csv")) {
  
  write.csv(
    darley_mc6_lor,
    "./data/darley_effects/darley_mc6_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 7

if (!file.exists("./data/darley_effects/darley_mc7_lor.csv")) {
  
  write.csv(
    darley_mc7_lor,
    "./data/darley_effects/darley_mc7_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 8

if (!file.exists("./data/darley_effects/darley_mc8_lor.csv")) {
  
  write.csv(
    darley_mc8_lor,
    "./data/darley_effects/darley_mc8_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 9

if (!file.exists("./data/darley_effects/darley_mc9_lor.csv")) {
  
  write.csv(
    darley_mc9_lor,
    "./data/darley_effects/darley_mc9_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 10

if (!file.exists("./data/darley_effects/darley_mc10_lor.csv")) {
  
  write.csv(
    darley_mc10_lor,
    "./data/darley_effects/darley_mc10_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 11

if (!file.exists("./data/darley_effects/darley_mc11_lor.csv")) {
  
  write.csv(
    darley_mc11_lor,
    "./data/darley_effects/darley_mc11_lor.csv",
    row.names = FALSE
  )
  
}

## Manipulation check 12

if (!file.exists("./data/darley_effects/darley_mc12_lor.csv")) {
  
  write.csv(
    darley_mc12_lor,
    "./data/darley_effects/darley_mc12_lor.csv",
    row.names = FALSE
  )
  
}