#######################################################################

# Loftus & Palmer (1974) [Study 2] -- Data simulation

#######################################################################

# Set up environment --------------------------------------------------

## Seed

set.seed(007)

# Set up design--------------------------------------------------------

## Basics of multi-lab design and study design

n_labs    <- 55 # Number of labs
n_per     <- 80 # Number of participants per lab

verb_condition     <- c("smash", "hit", "control")
##speed_estimate     <-      # Positive int? (NA for control condition) 
broken_glass       <- c(0,1) # 0 = No, 1 = Yes
speed_unit         <- c("mph","kph")

## Set up participants

loftus_wide <- 
  expand.grid(
    lab     = 1:n_labs,
    subject = 1:n_per
  ) %>% 
  arrange(by = lab) %>% 
  mutate(
    subject = paste(lab, subject, sep = "_")
  )

## Set up data column names

loftus_wide$verb_condition <- sample(verb_condition, nrow(loftus_wide), replace = TRUE)

loftus_wide$speed_estimate  <- rpois(nrow(loftus_wide), 30) 

loftus_wide$speed_estimate[loftus_wide$verb_condition == "control"] <- NA

lab_speed <-  
  expand.grid(lab = 1:n_labs)

lab_speed$speed_unit <- sample(speed_unit, nrow(lab_speed), replace = TRUE)

loftus_wide <- left_join(loftus_wide, lab_speed, by = "lab", "lab")

loftus_wide$broken_glass <- broken_glass

# Save data simulated data file ---------------------------------------

if (!file.exists("./data/loftus_sim.csv")) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    loftus_wide,
    "./data/loftus_sim.csv",
    row.names = FALSE
  )
  
}

