#######################################################################

# Darley, Carlsmith, & Robinson (2000) [Study 2] -- Data Simulation

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr", "stringr")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(666)

# Set up design--------------------------------------------------------

## Basics of multi-lab design and study design

n_labs    <- 55 # Number of labs
n_per     <- 80 # Number of participants per lab

scenario     <- c("jealous_rage", "inoperable", "operable")
punishment   <- c(1:7)                     # O Scale of 1-7
attribution  <- c(0,1)                     # 0 = Tumor, 1 = Actor

## Set up participants

darley_wide <- 
  expand.grid(
    lab     = 1:n_labs,
    subject = 1:n_per
  ) %>% 
  arrange(by = lab) %>% 
  mutate(
    subject = paste(lab, subject, sep = "_")
  )

## Set up data column names

darley_wide$scenario <- sample (scenario, nrow(darley_wide), replace = TRUE)

darley_wide$attribution <- attribution
attribution_count <- count(darley_wide, attribution)

darley_wide$punishment <- sample(punishment, nrow(darley_wide), replace = TRUE)

# Save data simulated data file ---------------------------------------

if (!file.exists("./data/darley_sim.csv")) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    darley_wide,
    "./data/darley_sim.csv",
    row.names = FALSE
  )
  
}
