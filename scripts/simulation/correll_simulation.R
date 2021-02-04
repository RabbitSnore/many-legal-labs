
#######################################################################

# Correll, Park, Judd, & Wittenbrink (2002) [Study 1] -- Data Simulation

#######################################################################

# Overview ------------------------------------------------------------

## Each participant performs 80 trials, with 20 allocated per cell in the 2 x 2 design
## The factors are race (White/Black) and object (Gun/No Gun)
## Each trial has a reaction time (in milliseconds) and an accuracy indicator (correct/incorrect)

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr")

lapply(packages, library, character.only = TRUE)

# Set up design--------------------------------------------------------

## Basics of multi-lab design and study design

n_labs   <- 55 # Number of labs
n_per    <- 80 # Number of participants per lab

k_cells  <- 4  # Number of cells in the design
n_trials <- 20 # Number of trials per cell

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

# Simulate wide-form data ---------------------------------------------



# Wrangle to long-form ------------------------------------------------


