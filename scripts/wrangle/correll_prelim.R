
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


correll_sim <- read.csv("./data/correll_sim.csv")


## Basic study design

trials      <- 20   # number of trials
time_limit  <- 850  # time limit in ms


## Latency exclusions
  
  
correll_latencies <- select(correll_sim, contains("latency"))


## Number of latancies per trial

exclusion_lat_wa <- sum(select(correll_latencies, contains ("white_armed")) > time_limit)

exclusion_lat_ba <- sum(select(correll_latencies, contains ("black_armed")) > time_limit)

exclusion_lat_wu <- sum(select(correll_latencies, contains("white_unarmed")) > time_limit)

exclusion_lat_bu <- sum(select(correll_latencies, contains("black_unarmed")) > time_limit)

## Removing trials exceeding 850ms. Sum of exclusions stemming from exceeding 850ms.

correll_latencies[correll_latencies > time_limit] <- NA

exclusion_lat <- sum(is.na(correll_latencies))

sum_lat_exclusions <- rowSums(is.na(correll_latencies))

## Creating separate dataframes for each trial

white_armed <- select(correll_latencies, contains ("white_armed"))
black_armed <- select(correll_latencies, contains ("black_armed"))
white_unarmed <- select(correll_latencies, contains ("white_unarmed"))
black_unarmed <- select(correll_latencies, contains ("black_unarmed"))

## Number of latency errors per trial

error_lat_wa <- rowSums(is.na(white_armed))
error_lat_ba <- rowSums(is.na(black_armed))
error_lat_wu <- rowSums(is.na(white_unarmed))
error_lat_bu <- rowSums(is.na(black_unarmed))

## Accuracy exclusions

correll_accuracy <- select(correll_sim, contains("accuracy"))

correll_latencies[correll_accuracy == 0] <- NA

## Sum of exclusions stemming from inaccurate responses.

exclusion_acc <- sum(correll_accuracy == 0)

sum_acc_exclusions <- rowSums((correll_accuracy == 0))

## Number of inaccurate responses per trial

exclusion_acc_wa <- sum(select(correll_accuracy, contains ("white_armed")) == 0)
exclusion_acc_ba <- sum(select(correll_accuracy, contains ("black_armed")) == 0)
exclusion_acc_wu <- sum(select(correll_accuracy, contains ("white_unarmed")) == 0)
exclusion_acc_bu <- sum(select(correll_accuracy, contains ("black_unarmed")) == 0)

## Log transform response latencies

correll_latencies <- log(correll_latencies)

## Sum of both types of exclusion

sum_exclusions <- rowSums(is.na(correll_latencies))

## Overwriting dataframes, accounting for all errors and with log-transformed latencies

white_armed <- select(correll_latencies, contains ("white_armed"))
black_armed <- select(correll_latencies, contains ("black_armed"))
white_unarmed <- select(correll_latencies, contains ("white_unarmed"))
black_unarmed <- select(correll_latencies, contains ("black_unarmed"))

## Number of all errors per trial

error_wa <- rowSums(is.na(white_armed))
error_ba <- rowSums(is.na(black_armed))
error_wu <- rowSums(is.na(white_unarmed))
error_bu <- rowSums(is.na(black_unarmed))

## Number of accuracy errors per trial

error_acc_wa <- error_wa - error_lat_wa
error_acc_ba <- error_ba - error_lat_ba
error_acc_wu <- error_wu - error_lat_wu
error_acc_bu <- error_bu - error_lat_bu

## Proportion of total errors per trial

white_armed_accuracy <- error_wa / trials
black_armed_accuracy <- error_ba / trials
white_unarmed_accuracy <- error_wu / trials
black_unarmed_accuracy <- error_bu / trials

## Proportion of latency errors per trial

error_prop_lat_wa <- error_lat_wa / trials
error_prop_lat_ba <- error_lat_ba / trials
error_prop_lat_wu <- error_lat_wu / trials
error_prop_lat_bu <- error_lat_bu / trials

## Proportion of accuracy errors per trial

error_prop_acc_wa <- error_acc_wa / trials
error_prop_acc_ba <- error_acc_ba / trials
error_prop_acc_wu <- error_acc_wu / trials
error_prop_acc_bu <- error_acc_bu / trials

## Mean Latency per trial

white_armed <- white_armed %>% mutate(white_armed_latency = rowMeans(white_armed, na.rm = TRUE))
black_armed <- black_armed %>% mutate(black_armed_latency = rowMeans(black_armed, na.rm = TRUE))
white_unarmed <- white_unarmed %>% mutate(white_unarmed_latency = rowMeans(white_unarmed, na.rm = TRUE))
black_unarmed <- black_unarmed %>% mutate(black_unarmed_latency = rowMeans(black_unarmed, na.rm = TRUE))


## Smushing data

correll_latencies$white_armed_accuracy <- white_armed_accuracy

correll_latencies$black_armed_accuracy <- black_armed_accuracy

correll_latencies$white_unarmed_accuracy <- white_unarmed_accuracy

correll_latencies$black_unarmed_accuracy <- black_unarmed_accuracy

correll_wrangle <- bind_cols (lab = correll_sim$lab, subject = correll_sim$subject, correll_latencies, white_armed_latency = white_armed$white_armed_latency, black_armed_latency = black_armed$black_armed_latency, white_unarmed_latency = white_unarmed$white_unarmed_latency, black_unarmed_latency = black_unarmed$black_unarmed_latency)

## Save data

if (!file.exists("./data/correll_wrangle.csv")) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    correll_wrangle,
    "./data/correll_wrangle.csv",
    row.names = FALSE
  )
  
}
