
#######################################################################

# Serota, Levine, & Boster (2010) [Study 3] -- Effect Size Calculation

#######################################################################

# Set up environment --------------------------------------------------

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

### Serota et al effect size functions

#### A function to fit a power function model using nonlinear least squares regression

power_calc <- function(ID, x, y) {
  
  # The model has the form y ~ a * x^k, where a is the intercept and k is the exponent (i.e., the coefficient of interest)
  
  # x is a vector of values representing the number of lies told in the last 24 hours
  # y is a vector of values representing the number of participants telling a given number of lies (i.e., the frequency)
  # start is a vector with a length of 2, with the first value being the starting value for the intercept and the second value being the starting value for the exponent
  
  #### A function to guess reasonable starting values for the nonlinear regression
  
    # Specifying this as a function might be overkill right now
    # But if we have to make the starting value selection more sophisticated it will be easier to do so with this basic foundation in place
  
  starting_values <- function(ID, x, y) {
    
    # Select starting values for the intercept and exponent based on the data
    
    start <- c(NA, NA)
    
    start[1] <- round(max(y), 2)
    
    start[2] <- -1 # This will always set the starting k value at -1
    # Unless we have some good justification for changing this, we'll just hold this starting value constant
    # But if we decide to change this later, this is the part of the code that will need to be adjusted
    
    return(start)
    
  }
  
  start <- starting_values(ID, x, y)
  
  model <- nls(y ~ a * x^k, start = list(a = start[1], k = start[2]))
  
  k   <- summary(model)$parameters[2, 1]
  var <- summary(model)$parameters[2, 2]^2
  
  ci_upper <- k + sqrt(var)*qt(.975, summary(model)$df[2])
  ci_lower <- k - sqrt(var)*qt(.975, summary(model)$df[2])
  
  a       <- summary(model)$parameters[1, 1]
  var_a   <- summary(model)$parameters[1, 2]^2
  
  out <- data.frame(ID = ID, k = k, var = var, a = a, var_a = var_a,  ci_lower = ci_lower, ci_upper = ci_upper)
  
  return(out)
  
}

#### The data frame produced by this function is designed to work with the power_calc() function

empty_k_data <- function(n) {
  
  out <- data.frame(
    ID       = rep(NA, n),
    k        = rep(NA, n),
    var      = rep(NA, n),
    a        = rep(NA, n),
    var_a    = rep(NA, n),
    ci_lower = rep(NA, n),
    ci_upper = rep(NA, n)
  )
  
  return(out)
  
}

# Import wrangled data ------------------------------------------------

if (read_data == TRUE) {
  
  serota <- read.csv("./data/serota_frequencies.csv")
  
  serota_summary <- read.csv("./data/serota_summary.csv")
  
} else {
  
  serota <- prop_lies
  
  serota_summary <- serota_summary
  
}

# Set up basic information --------------------------------------------

lab_count_serota <- length(unique(serota$lab)) # Number of labs providing data

# DESCRIPTIVES _-------------------------------------------------------

# Estimate the mean number of lies told in the last 24 hours

## Calculate standard errors of the means

serota_desc <- serota_summary %>% 
  mutate(
    sem       = sd/sqrt(N),
    var_m     = sem^2,
    ci_upper  = mean + sem*qnorm(.975),
    ci_lower  = mean - sem*qnorm(.975),
    sd_weight = 1/sqrt(N)
  )

# HYPOTHESIS 1 --------------------------------------------------------

# The distribution of the number of lies told in the last 24 hours can be characterized by a power function with a negative exponent.

# Coefficient (i.e., exponent in a power function) for a non-linear least squares regression predicting frequency from the number of lies

## Calculate coefficients

### Set up empty data frame for effects

serota_h1_k <- empty_k_data(lab_count_serota)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_serota) {
  
  serota_h1_k[i, ] <- power_calc(
    
    ID = unique(serota$lab)[i], 
    
    x = serota$lies[serota$lab == unique(serota$lab)[i]], 
    y = serota$freq[serota$lab == unique(serota$lab)[i]] 
  
  )
  
}

### Link moderators with effects

serota_mods <- serota_raw %>% 
  select(ID = lab, country) %>% 
  unique()

serota_h1_k <- serota_h1_k %>% 
  left_join(serota_mods, by = "ID")

serota_h1_k <- serota_h1_k %>% 
  mutate(
    usa = case_when(
      country == "United States" ~ "US",
      !is.na(country)            ~ "Non-US"
    )
  )

# Export Calculated Effect Sizes --------------------------------------

## If the data directory does not exist, it will be necessary to create it

if (write_data == TRUE) {
  
  if (!dir.exists("./data/serota_effects/")) {
    
    dir.create("./data/serota_effects/")
    
  }
  
} 

## Descriptives

if (write_data == TRUE) {
  
  write.csv(
    serota_desc,
    "./data/serota_effects/serota_desc.csv",
    row.names = FALSE
  )
  
}

## Hypothesis 1

if (write_data == TRUE) {
  
  write.csv(
    serota_h1_k,
    "./data/serota_effects/serota_h1_k.csv",
    row.names = FALSE
  )
  
}

