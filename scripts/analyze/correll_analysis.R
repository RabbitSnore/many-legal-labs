
#######################################################################

# Correll, Park, Judd, & Wittenbrink (2002) [Study 1] -- Meta-Analysis

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("metafor", "dplyr", "ggplot2")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(666)

## Functions

### Forest plot

forest_plot <- function(meta_analysis, replication_data, org_d, org_ci_lower, org_ci_upper, title, study_color = "black", boundary_pad = 0.20, breaks = 0.10) {
  
  ### Set up original and meta-analytic estimates
  
  forest_estimates <- data.frame(
    ID = c("Replication", "Original"),
    d = c(meta_analysis$beta[[1]], org_d),
    var = c(NA, NA),
    ci_lower = c(meta_analysis$ci.lb, org_ci_lower),
    ci_upper = c(meta_analysis$ci.ub, org_ci_upper)
  )
  
  ### Bind the original and meta-analytic estimates to the calculated effects from each lab
  
  forest_estimates <- bind_rows(forest_estimates, replication_data)
  
  #### Set up the effect IDs with the proper order for the forest plot
  
  forest_estimates$ID <- factor(forest_estimates$ID, 
                                    levels = c(
                                      "Original",  
                                      "Replication", 
                                      arrange(replication_data, d)$ID
                                    )
  )
  
  #### Set up an indicator for whether the estimate is from the meta-analysis, the original, or a replication
  
  forest_estimates <- forest_estimates %>% 
    mutate(
      estim = as.factor(
        case_when(
          ID == "Replication" ~ "meta",
          ID == "Original" ~ "original",
          !is.na(ID) ~ "study"
        ))
    )
  
  ### Set up plot boundaries
  
  effect_max <- round(max(forest_estimates$d, na.rm = TRUE), 1) + boundary_pad
  effect_min <- round(min(forest_estimates$d, na.rm = TRUE), 1) - boundary_pad
  
  ### Draw the forest plot
  
  forest <- 
    ggplot(forest_estimates,
           aes(
             x = d,
             y = ID,
             xmin = ci_lower,
             xmax = ci_upper
           )) +
    geom_point(
      aes(
        shape = estim,
        size = estim
      ),
      color = study_color
    ) +
    scale_size_manual(
      values = c(4, 4, 1.2)
    ) +
    scale_shape_manual(
      values = c(18, 18, 15)
    ) +
    geom_errorbarh(
      height = .50,
      color = study_color
    ) +
    geom_vline(
      xintercept = meta_analysis$beta[[1]],
      linetype = "dashed",
      color = "black",
      size = 1
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dotted"
    ) +
    geom_hline(
      yintercept = 2.5
    ) +
    scale_x_continuous(
      breaks = seq(effect_min, effect_max + 0.20, breaks)
    ) +
    coord_cartesian(
      xlim = c(effect_min, effect_max)
    ) +
    labs(
      title = title,
      x = expression(paste("Effect size (", italic("d"), ")", sep = "")),
      y = ""
    ) +
    guides(
      shape = FALSE,
      group = FALSE,
      size = FALSE
    ) +
    theme_classic() +
    theme(
      axis.text = element_text(color = "black"),
      legend.position = c(.8, .2),
      legend.background = element_rect( 
        size = 0.5,
        linetype = "solid",
        color= "black"),
      plot.title = element_text(hjust = 0.5)
    )
  
  return(forest)
  
}

# Import calculated effect data ---------------------------------------

correll_h1_smd <- read.csv("./data/correll_effects/correll_h1_smd.csv") # THIS WILL NOT WORK YET
correll_h2_smd <- read.csv("./data/correll_effects/correll_h2_smd.csv") # THIS WILL NOT WORK YET
correll_h3_smd <- read.csv("./data/correll_effects/correll_h3_smd.csv") # THIS WILL NOT WORK YET
correll_h4_smd <- read.csv("./data/correll_effects/correll_h4_smd.csv") # THIS WILL NOT WORK YET

# Import original effects

correll_org <- read.csv(".data/original/correll_original.csv") # THIS WILL NOT WORK YET

# Set study color

correll_color_1 <- "#FF7F50"

# HYPOTHESIS 1 --------------------------------------------------------

## Random effects meta-analysis

correll_h1_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h1_smd,
  method = "REML"
  )

## Forest plot

correll_h1_forest <- 
  forest_plot(
    meta_analysis = correll_h1_meta, 
    replication_data = correll_h1_smd, 
    title = "Correll et al (2002), Hypothesis 1",
    study_color = correll_color_1,
    org_d = correll_org[hypothesis == "h1", ]$d, 
    org_ci_lower = correll_org[hypothesis == "h1", ]$ci_lower, 
    org_ci_upper = correll_org[hypothesis == "h1", ]$ci_upper
    )

# HYPOTHESIS 2 --------------------------------------------------------

## Random effects meta-analysis

correll_h2_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h2_smd,
  method = "REML"
)

## Forest plot

correll_h2_forest <- 
  forest_plot(
    meta_analysis = correll_h2_meta, 
    replication_data = correll_h2_smd, 
    title = "Correll et al (2002), Hypothesis 2",
    study_color = correll_color_1,
    org_d = correll_org[hypothesis == "h2", ]$d, 
    org_ci_lower = correll_org[hypothesis == "h2", ]$ci_lower, 
    org_ci_upper = correll_org[hypothesis == "h2", ]$ci_upper
  )

# HYPOTHESIS 3 --------------------------------------------------------

## Random effects meta-analysis

correll_h3_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h3_smd,
  method = "REML"
)

## Forest plot

correll_h2_forest <- 
  forest_plot(
    meta_analysis = correll_h2_meta, 
    replication_data = correll_h2_smd, 
    title = "Correll et al (2002), Hypothesis 3",
    study_color = correll_color_1,
    org_d = correll_org[hypothesis == "h3", ]$d, 
    org_ci_lower = correll_org[hypothesis == "h3", ]$ci_lower, 
    org_ci_upper = correll_org[hypothesis == "h3", ]$ci_upper
  )

# HYPOTHESIS 4 --------------------------------------------------------

## Random effects meta-analysis

correll_h4_meta <- rma(
  yi = d, 
  vi = var,
  data = correll_h4_smd,
  method = "REML"
)

## Forest plot

correll_h4_forest <- 
  forest_plot(
    meta_analysis = correll_h4_meta, 
    replication_data = correll_h4_smd, 
    title = "Correll et al (2002), Hypothesis 4",
    study_color = correll_color_1,
    org_d = correll_org[hypothesis == "h4", ]$d, 
    org_ci_lower = correll_org[hypothesis == "h4", ]$ci_lower, 
    org_ci_upper = correll_org[hypothesis == "h4", ]$ci_upper
  )