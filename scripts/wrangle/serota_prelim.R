
#######################################################################

# Serota, Levine, & Boster (2010) [Study 3] -- Cleaning and Preliminary Work

#######################################################################

# Create summary tables for each replication

## Means, SDs, medians, modes, mimimum, maximum for number of lies in the last 24 hours, along with sample size

# Create frequency tables for each replication

# Standardize frequency counts into proportions

## Standardizing the frequencies will ensure that the intercepts of the nonlinear regressions will be comparable across studies

# The output should be a series of frequency tables appended to each other, with identifiers (lab IDs)

## Packages

packages <- c("dplyr", "tidyr")

lapply(packages, library, character.only = TRUE)

## Import

serota_sim <- read.csv("./data/serota_sim.csv")

# Wrangle -------------------------------------------------------------

serota_summary <- serota_sim %>% 
  group_by(lab) %>% 
  summarise(mean = mean(total_lies),
            sd = sd(total_lies),
            median = median(total_lies),
            min = min(total_lies),
            max = max(total_lies),
            N = sum(!is.na(total_lies)),
            mode = which.max(table(total_lies)))


freq_lies <- serota_sim %>% 
  group_by(lab) %>% 
  count(lies = serota_sim$total_lies)

freq_lies <- freq_lies %>% 
  rename(freq = n)

prop_lies <- freq_lies %>%
  group_by(lab) %>%
  mutate(prop = freq / sum(freq))

lab_country <- serota_sim[c("lab","country")]

lab_country <- lab_country %>% 
  distinct()

serota_summary <- left_join(serota_summary, lab_country, by = "lab")

# Save data simulated data file ---------------------------------------

if (!file.exists("./data/serota_summary.csv")) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    serota_summary,
    "./data/serota_summary.csv",
    row.names = FALSE
  )
  
}