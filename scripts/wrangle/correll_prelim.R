
#######################################################################

# Correll, Park, Judd, & Wittenbrink (2002) [Study 1] -- Cleaning and Preliminary Work

#######################################################################

# DATA EXCLUSIONS -----------------------------------------------------

# Remove trials that exceed 850ms response window

# Remove trials with incorrect responses

## Check exclusion rate

### Between subjects

### Within subjects

# TRANSFORMATIONS -----------------------------------------------------

# Log transform response latencies

# Calculate within participant cell means for response latencies

# Calculate errors for each cell, within participants



## Packages

packages <- c("dplyr", "stringr")

lapply(packages, library, character.only = TRUE)

## Import

correll_wrangle <- read.csv("./data/correll_sim.csv")

## Exclusions

correll_latencies <- select(correll_wrangle, contains("latency"))

correll_latencies[correll_latencies > 850] <- NA

white_armed <- select(correll_latencies, contains("white_armed"))
black_armed <- select(correll_latencies, contains("black_armed"))
white_unarmed <- select(correll_latencies, contains("white_unarmed"))
black_unarmed <- select(correll_latencies, contains("black_unarmed"))


## Save data

write.csv(
  correll_wrangle,
  "./data/correll_wrangle.csv",
  row.names = FALSE
)
