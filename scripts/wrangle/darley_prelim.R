
#######################################################################

# Darley, Carlsmith, & Robinson (2000) [Study 2] -- Cleaning and Preliminary Work

#######################################################################

## Import

if (read_data == TRUE) {
  
  darley_sim <- read.csv("./data/darley_sim.csv")
  
} else {
  
  darley_sim <- darley_wide
  
}

## Wrangle

darley_sim <- darley_sim %>% 
  mutate(
  prison = case_when(
    outcome == "prison" ~ 1,
    outcome == "mental_inst" ~ 0,
    outcome == "free" ~ 0,
    TRUE ~ NA_real_
  ))

darley_sim <- darley_sim %>% 
  mutate(
    mental_inst = case_when(
      outcome == "prison" ~ 0,
      outcome == "mental_inst" ~ 1,
      outcome == "free" ~ 0,
      TRUE ~ NA_real_
    ))

darley_sim <- darley_sim %>% 
  mutate(
    free = case_when(
      outcome == "prison" ~ 0,
      outcome == "mental_inst" ~ 0,
      outcome == "free" ~ 1,
      TRUE ~ NA_real_
    ))

darley_wrangle <- darley_sim # Right now this is very silly. But we might have to do more wrangling with the real data.

## Save data

if (write_data == TRUE) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    darley_wrangle,
    "./data/darley_wrangle.csv",
    row.names = FALSE
  )
  
}