
#######################################################################

# Serota, Levine, & Boster (2010) [Study 3] -- Core Analyses

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr", "tidyr")

lapply(packages, library, character.only = TRUE)

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
  
  start <- starting_values(x, y)
  
  model <- nls(y ~ a * x^k, start = list(a = start[1], k = start[2]))
  
  k   <- summary(model)$parameters[2, 1]
  var <- sqrt(summary(model)$parameters[2, 2])
  
  cis <- confint(model)
  
  ci_upper <- cis[2, 2]
  ci_lower <- cis[2, 1]
  
  a   <- summary(model)$parameters[1, 1]
  
  out <- data.frame(ID = ID, k = k, var = var, a = a, ci_lower = ci_lower, ci_upper = ci_upper)
  
  return(out)
  
}

# Import wrangled data ------------------------------------------------

serota <- read.csv("./data/serota_wrangle.csv")

# Set up basic information --------------------------------------------

lab_count_serota <- length(unique(serota$lab)) # Number of labs providing data

# HYPOTHESIS 1 --------------------------------------------------------

# The distribution of the number of lies told in the last 24 hours can be characterized by a power function with a negative exponent.

# Coefficient (i.e., exponent in a power function) for a non-linear least squares regression predicting frequency from the number of lies

## Calculate coefficients




