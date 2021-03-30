
#######################################################################

# Loftus & Palmer (1974) [Study 2] -- Cleaning and Preliminary Work

#######################################################################

# Set up environment --------------------------------------------------

## Import

if (read_data == TRUE) {
  
  loftus_sim <- read.csv("./data/loftus_sim.csv")
  
} else {
  
  loftus_sim <- loftus_wide
  
}

# Wrangle -------------------------------------------------------------

## Convert kph to mph

loftus_wrangle <- loftus_sim %>% 
  mutate(
    speed_estimate = case_when(
      speed_unit == "mph" ~ as.numeric(speed_estimate),
      speed_unit == "kph" ~ as.numeric(speed_estimate * 0.621371192)
    )
  )

# Save data -----------------------------------------------------------

if (write_data == TRUE) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    loftus_wrangle,
    "./data/loftus_wrangle.csv",
    row.names = FALSE
  )
  
}
