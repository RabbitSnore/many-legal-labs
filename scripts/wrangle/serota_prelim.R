
#######################################################################

# Serota, Levine, & Boster (2010) [Study 3] -- Cleaning and Preliminary Work

#######################################################################

# Create summary tables for each replication

## Means, SDs, medians, modes, minimum, maximum for number of lies in the last 24 hours, along with sample size

# Create frequency tables for each replication

# Standardize frequency counts into proportions

## Standardizing the frequencies will ensure that the intercepts of the nonlinear regressions will be comparable across studies

# The output should be a series of frequency tables appended to each other, with identifiers (lab IDs)

## Import

if (read_data == TRUE) {
  
  serota_raw <- read.csv("./data/serota_raw.csv")
  
} else {
  
  serota_raw <- serota_wide
  
}

# Wrangle -------------------------------------------------------------

## Getting min and max amount of lies for whole dataset

min_lies <- min(serota_raw$total_lies)

max_lies <- max(serota_raw$total_lies)

## Summary Serota

serota_summary <- serota_raw %>% 
  group_by(lab) %>% 
  summarise(mean = mean(total_lies),
            sd = sd(total_lies),
            median = median(total_lies),
            min = min(total_lies),
            max = max(total_lies),
            N = sum(!is.na(total_lies)),
            mode = which.max(table(total_lies)),
            country = unique(country))


##Create frequency table for lies told in last 24 hours with lab IDs

table_lies <- table(serota_raw$lab, serota_raw$total_lies)

df_lies <- as.data.frame.matrix(table_lies)

df_lies$lab = serota_summary$lab


## Change frequencies to long form

long_lies <- reshape(df_lies, 
                     direction = "long",
                     varying = list(names(df_lies)[1:(ncol(df_lies)-1)]), 
                     v.names = "freq",
                     idvar = "lab",
                     timevar = "lies",
                     times = min_lies:max_lies
                      )

## Sort by lab

long_lies <- long_lies[order(long_lies$lab),]

## Removing columns for 0 lies in last 24 hours

filter_lies <- filter(long_lies, lies != 0)


## Getting proportion of lies per lab

prop_lies <- filter_lies %>%
  group_by(lab) %>%
  mutate(prop = freq / sum(freq))


# Save data simulated data file ---------------------------------------

if (write_data == TRUE) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    serota_summary,
    "./data/serota_summary.csv",
    row.names = FALSE
  )
  
}

if (write_data == TRUE) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    prop_lies,
    "./data/serota_frequencies.csv",
    row.names = FALSE
  )
  
}

