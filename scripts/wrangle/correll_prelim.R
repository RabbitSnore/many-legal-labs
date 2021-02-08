
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


## Exclusions

trials <- 20
time_limit <- 850


correll_latencies <- select(correll_sim, contains("latency"))


exclusion_lat_wa <- sum(select(correll_latencies, contains ("white_armed")) > time_limit)

exclusion_lat_ba <- sum(select(correll_latencies, contains ("black_armed")) > time_limit)

exclusion_lat_wu <- sum(select(correll_latencies, contains("white_unarmed")) > time_limit)

exclusion_lat_bu <- sum(select(correll_latencies, contains("black_unarmed")) > time_limit)


correll_latencies[correll_latencies > time_limit] <- NA
exclusion_lat <- sum(is.na(correll_latencies))
sum_lat_exclusions <- rowSums(is.na(correll_latencies))

white_armed <- select(correll_latencies, contains ("white_armed"))
black_armed <- select(correll_latencies, contains ("black_armed"))
white_unarmed <- select(correll_latencies, contains ("white_unarmed"))
black_unarmed <- select(correll_latencies, contains ("black_unarmed"))

error_lat_wa <- rowSums(is.na(white_armed))
error_lat_ba <- rowSums(is.na(black_armed))
error_lat_wu <- rowSums(is.na(white_unarmed))
error_lat_bu <- rowSums(is.na(black_unarmed))


correll_accuracy <- select(correll_sim, contains("accuracy"))
correll_latencies[correll_accuracy == 0] <- NA


correll_latencies <- log(correll_latencies)

exclusion_acc <- sum(correll_accuracy == 0)
sum_acc_exclusions <- rowSums((correll_accuracy == 0))


sum_exclusions <- rowSums(is.na(correll_latencies))


exclusion_acc_wa <- sum(select(correll_accuracy, contains ("white_armed")) == 0)
exclusion_acc_ba <- sum(select(correll_accuracy, contains ("black_armed")) == 0)
exclusion_acc_wu <- sum(select(correll_accuracy, contains ("white_unarmed")) == 0)
exclusion_acc_bu <- sum(select(correll_accuracy, contains ("black_unarmed")) == 0)


white_armed <- select(correll_latencies, contains ("white_armed"))
black_armed <- select(correll_latencies, contains ("black_armed"))
white_unarmed <- select(correll_latencies, contains ("white_unarmed"))
black_unarmed <- select(correll_latencies, contains ("black_unarmed"))

error_wa <- rowSums(is.na(white_armed))
error_ba <- rowSums(is.na(black_armed))
error_wu <- rowSums(is.na(white_unarmed))
error_bu <- rowSums(is.na(black_unarmed))

error_acc_wa <- error_wa - error_lat_wa
error_acc_ba <- error_ba - error_lat_ba
error_acc_wu <- error_wu - error_lat_wu
error_acc_bu <- error_bu - error_lat_bu

error_prop_wa <- error_wa / trials
error_prop_ba <- error_ba / trials
error_prop_wu <- error_wu / trials
error_prop_bu <- error_bu / trials

error_prop_lat_wa <- error_lat_wa / trials
error_prop_lat_ba <- error_lat_ba / trials
error_prop_lat_wu <- error_lat_wu / trials
error_prop_lat_bu <- error_lat_bu / trials

error_prop_acc_wa <- error_acc_wa / trials
error_prop_acc_ba <- error_acc_ba / trials
error_prop_acc_wu <- error_acc_wu / trials
error_prop_acc_bu <- error_acc_bu / trials

white_armed <- white_armed %>% mutate(white_armed_lat_mean = rowMeans(white_armed, na.rm = TRUE))
black_armed <- black_armed %>% mutate(black_armed_lat_mean = rowMeans(black_armed, na.rm = TRUE))
white_unarmed <- white_unarmed %>% mutate(white_unarmed_lat_mean = rowMeans(white_unarmed, na.rm = TRUE))
black_unarmed <- black_unarmed %>% mutate(black_unarmed_lat_mean = rowMeans(black_unarmed, na.rm = TRUE))


## Smushing data

##correll_latencies$sum_lat_exclusions <- sum_lat_exclusions
##correll_latencies$sum_acc_exclusions <- sum_acc_exclusions
##correll_latencies$sum_exclusions <- sum_exclusions
correll_latencies$error_prop_wa <- error_prop_wa
correll_latencies$error_prop_ba <- error_prop_ba
correll_latencies$error_prop_wu <- error_prop_wu
correll_latencies$error_prop_bu <- error_prop_bu


correll_wrangle <- bind_cols (lab = correll_sim$lab, subject = correll_sim$subject, correll_latencies, wa_mean_lat = white_armed$white_armed_lat_mean, ba_mean_lat = black_armed$black_armed_lat_mean, wu_mean_lat = white_unarmed$white_unarmed_lat_mean, bu_mean_lat = black_unarmed$black_unarmed_lat_mean)


## Save data

write.csv(
  correll_wrangle,
  "./data/correll_wrangle.csv",
  row.names = FALSE
)
