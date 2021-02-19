
#######################################################################

# Loftus & Palmer (1974) [Study 2] -- Cleaning and Preliminary Work

#######################################################################

## Import

loftus_sim <- read.csv("./data/loftus_sim.csv")

## Wrangle

loftus_wrangle <- loftus_sim # Right now this is very silly. But we might have to do more wrangling with the real data.

## Save data

if (!file.exists("./data/loftus_wrangle.csv")) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    darley_wrangle,
    "./data/loftus_wrangle.csv",
    row.names = FALSE
  )
  
}
