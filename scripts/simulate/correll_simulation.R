
#######################################################################

# Correll, Park, Judd, & Wittenbrink (2002) [Study 1] -- Data Simulation

#######################################################################

# Overview ------------------------------------------------------------

## Each participant performs 80 trials, with 20 allocated per cell in the 2 x 2 design
## The factors are race (White/Black) and object (Armed/Unarmed)
## Each trial has a response latency (in milliseconds) and an accuracy indicator (correct/incorrect)

## This script simulates data of a structure similar to what we expect will be produced by the actual experiment, but it does not simulate any effects.

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr", "stringr")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(42069)

# Set up design--------------------------------------------------------

## Basics of multi-lab design and study design

n_labs    <- 55 # Number of labs
n_per     <- 80 # Number of participants per lab

race      <- c("white", "black")    # Race variable labels
object    <- c("armed", "unarmed")  # Object variable labels
n_trials  <- 20                     # Number of trials per cell

trials    <- str_pad(1:n_trials, nchar(n_trials), pad = 0) # Labels for trials
variables <- c("latency", "accuracy")                      # Labels for variables

## Set up participants

correll_wide <- 
  expand.grid(
    lab     = 1:n_labs,
    subject = 1:n_per
    ) %>% 
  arrange(by = lab) %>% 
  mutate(
    subject = paste(lab, subject, sep = "_")
  )

## Set up data column names

trial_combn <- 
  expand.grid(
    race   = race, 
    object = object, 
    trial  = trials,
    variable = variables
    )

trial_cols <- apply(trial_combn, 1, paste, collapse = "_")

# Simulate wide-form data ---------------------------------------------

## Generate response latencies from a Poisson distribution

latencies  <- replicate(n_trials * nrow(expand.grid(race, object)), rpois(nrow(correll_wide), 500)) %>% 
  as.data.frame()

## Generate accuracy indicators from a binomial distribution

accuracies <- replicate(n_trials * nrow(expand.grid(race, object)), rbinom(nrow(correll_wide), 1, .95)) %>% 
  as.data.frame()

## Bind the simulated data and rename the columns

response_data <- bind_cols(latencies, accuracies)

colnames(response_data) <- trial_cols

correll_wide <- bind_cols(correll_wide, response_data)

# Save data simulated data file ---------------------------------------

if (!file.exists("./data/correll_sim.csv")) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    correll_wide,
    "./data/correll_sim.csv",
    row.names = FALSE
  )
  
}
