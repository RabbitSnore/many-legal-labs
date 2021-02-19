
#######################################################################

# Darley, Carlsmith, & Robinson (2000) [Study 2] -- Effect Size Calculation

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("dplyr", "tidyr")

lapply(packages, library, character.only = TRUE)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

# Import wrangled data ------------------------------------------------

darley <- read.csv("./data/darley_wrangle.csv")

# Set up basic information --------------------------------------------

lab_count <- length(unique(darley$lab)) # Number of labs providing data

# HYPOTHESIS 1 --------------------------------------------------------

# Participants will recommend higher punishment when the perpetrator acted in a jealous rage, compared to when the behavior was caused by an inoperable tumor .

# Standardized mean difference for punishment recommendation, for jealous rage vs. inoperable tumor

# HYPOTHESIS 2 --------------------------------------------------------

# Participants will recommend higher punishment when the perpetrator acted in a jealous rage, compared to when the behavior was caused by an operable tumor.

# Standardized mean difference for punishment recommendation, for jealous rage vs. operable tumor

# HYPOTHESIS 3 --------------------------------------------------------

# Participants will recommend higher punishment when the perpetrator acted because of an inoperable tumor, compared to when the behavior was caused by an operable tumor.

# Standardized mean difference for punishment recommendation, for operable tumor vs. inoperable tumor

# MANIPULATION CHECK 1 ------------------------------------------------

# Participants will attribute responsibility to the actor more frequently in the jealous rage condition, compared to when the behavior was caused by (1) an inoperable tumor or (2) an operable tumor.

# Proportion differences (odds ratios) in attributing responsibility to the actor, for Scenario (jealous rage vs. inoperable and operable.)

# MANIPULATION CHECK 2 -----------------------------------------------

# Participants will attribute responsibility to the actor more frequently in the inoperable tumor condition compared to the operable tumor condition.
# Darley et al. found a nonsignificant difference for this comparison, but the direction of proportions was such that more people attributed responsibility to the actor when the tumor was inoperable.

# Proportion difference (odds ratios) in attributing responsibility to the actor, for Scenario (operable vs. inoperable)
