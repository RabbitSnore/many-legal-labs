#######################################################################

# example Plot Data

#######################################################################

# Set up environment --------------------------------------------------

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

## Seed

set.seed(666)

# Set up design--------------------------------------------------------

## Basics of multi-lab design and study design

n_labs    <- 55 # Number of labs
n_per     <- 80 # Number of participants per lab

scenario     <- c("jealous_rage", "inoperable", "operable")
punishment   <- c(1:7)                     # O Scale of 1-7
attribution  <- c(0,1)                     # 0 = Tumor, 1 = Actor
outcome      <- c("prison","mental_inst","free") #Recommended outcome

## Set up participants

example_sim <- 
  expand.grid(
    lab     = 1:n_labs,
    subject = 1:n_per
  ) %>% 
  arrange(by = lab) %>% 
  mutate(
    subject = paste(lab, subject, sep = "_")
  )

## Set up data column names

example_sim$scenario <- sample(scenario, nrow(example_wide), replace = TRUE)

example_sim$attribution <- attribution
attribution_count <- count(example_sim, attribution)

example_sim$punishment <- sample(punishment, nrow(example_sim), replace = TRUE)

example_sim$outcome <-sample(outcome, nrow(example_sim), replace = TRUE)

## Wrangle

example_sim <- example_sim %>% 
  mutate(
    prison = case_when(
      outcome == "prison" ~ 1,
      outcome == "mental_inst" ~ 0,
      outcome == "free" ~ 0,
      TRUE ~ NA_real_
    ))

example_sim <- example_sim %>% 
  mutate(
    mental_inst = case_when(
      outcome == "prison" ~ 0,
      outcome == "mental_inst" ~ 1,
      outcome == "free" ~ 0,
      TRUE ~ NA_real_
    ))

example_sim <- example_sim %>% 
  mutate(
    free = case_when(
      outcome == "prison" ~ 0,
      outcome == "mental_inst" ~ 0,
      outcome == "free" ~ 1,
      TRUE ~ NA_real_
    ))

# Calculations --------------------------------------------------------

lab_count_example <- length(unique(example_sim$lab)) # Number of labs providing data

## Calculate effect sizes 

### Set up empty data frame for effects

example_smd <- empty_smd_data(lab_count_example)

### Compute standardized mean differences for each lab

for (i in 1:lab_count_example) {
  
  example_smd[i, ] <- d_calc(
    
    ID = unique(example_sim$lab)[i], 
    
    x = example_sim$scenario[example_sim$lab == unique(example_sim$lab)[i]], 
    y = example_sim$punishment[example_sim$lab == unique(example_sim$lab)[i]], 
    
    cond_1 = "inoperable", 
    cond_2 = "operable"
    
  )
  
}

# Analysis ------------------------------------------------------------

## Random effects meta-analysis

example_meta <- rma(
  yi = d, 
  vi = var,
  data = example_smd,
  method = "REML"
)

# Export objects ------------------------------------------------------

if (!dir.exists("./data/plot_data/")) {
  
  dir.create("./data/plot_data/")
  
}

write.csv(example_sim, "./data/plot_data/example_sim.csv", row.names = FALSE)
write.csv(example_smd, "./data/plot_data/example_smd.csv", row.names = FALSE)
saveRDS(example_meta, "./data/plot_data/example_meta.rds")


