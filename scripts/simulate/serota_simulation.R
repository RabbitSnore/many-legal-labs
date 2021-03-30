####################################################################

# Serota, Levine, & Boster (2010) [Study 3] -- Simulation

####################################################################

# Set up environment --------------------------------------------------

## Seed

set.seed(5318008)

# Set up design-----------------------------------------------------

## Basics of multi-lab design and study design

n_labs    <- 55 # Number of labs
n_per     <- 80 # Number of participants per lab
n_mode    <- 2  # Number of modes
n_person  <- 5  # Number of types of relationships
n_lies    <- 10

mode      <- c("face", "mediated")
person    <- c("family", "friend", "business", "acquaintance", "stranger")
country   <- c("Basutoland", "Ceylon", "DDR", "Neutral Moresnet", "Prussia", "Roman empire", "Sikkim", "Tavolara", "Vermont", "Yugoslavia", "Zanzibar", "United States")

last_lie <- c("more than 24 hours ago but within the last 2 days", "more than 2 days ago but within the last week", "more than a week ago but within the last month", "more than a month ago", "never")

## Set up participants

serota_wide <- 
  expand.grid(
    lab     = 1:n_labs,
    subject = 1:n_per
  ) %>% 
  arrange(by = lab) %>% 
  mutate(
    subject = paste(lab, subject, sep = "_")
  )

## Set up data columns

lab_country <-  
  expand.grid(lab = 1:n_labs)

lab_country$country <- sample(country, nrow(lab_country), replace = TRUE)

serota_wide <- left_join(serota_wide, lab_country, by = "lab", "lab")

lies  <- replicate( n_mode * nrow(expand.grid(person)), rpois(nrow(serota_wide), 1)) %>% 
  as.data.frame()

lie_combn <- 
  expand.grid(
    mode   = mode, 
    person = person 
  )

lie_cols <- apply(lie_combn, 1, paste, collapse = "_")

colnames(lies) <- lie_cols

serota_wide <- bind_cols(serota_wide, lies)


## Calculate total lies

serota_wide$mediated_total <- rowSums(select(serota_wide, contains ("mediated")))

serota_wide$face_total <- rowSums(select(serota_wide, contains ("face")))

serota_wide$total_lies <- rowSums(select(serota_wide, contains("total")))

serota_wide$family_lies <- rowSums(select(serota_wide, contains ("family")))
serota_wide$friends_lies <- rowSums(select(serota_wide, contains ("friend")))
serota_wide$business_lies <- rowSums(select(serota_wide, contains ("business")))
serota_wide$acquaintance_lies <- rowSums(select(serota_wide, contains ("acquaintance")))
serota_wide$stranger_lies <- rowSums(select(serota_wide, contains ("stranger")))


serota_wide$last_lie[serota_wide$total_lies == 0] <- sample(last_lie, nrow(serota_wide), replace = TRUE)

# Save data simulated data file ---------------------------------------

if (!file.exists("./data/serota_raw.csv")) {
  
  if (!file.exists("./data/")) {
    
    dir.create("./data/")
    
  }  
  
  write.csv(
    serota_wide,
    "./data/serota_raw.csv",
    row.names = FALSE
  )
  
}
