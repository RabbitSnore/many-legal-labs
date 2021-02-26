
#######################################################################

# Serota, Levine, & Boster (2010) [Study 3] -- Meta-Analysis

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("metafor", "dplyr", "ggplot2")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(1919)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

### Forest plot for exponential coefficients

forest_plot_k <- function(meta_analysis, replication_data, org_k, org_ci_lower, org_ci_upper, title, study_color = "black", boundary_pad = 0.20, breaks = 0.10) {
  
  ### Set up original and meta-analytic estimates
  
  forest_estimates <- data.frame(
    ID = c("Replication", "Original"),
    k = c(meta_analysis$beta[[1]], org_k),
    var = c(NA, NA),
    ci_lower = c(meta_analysis$ci.lb, org_ci_lower),
    ci_upper = c(meta_analysis$ci.ub, org_ci_upper)
  )
  
  ### Bind the original and meta-analytic estimates to the calculated effects from each lab
  
  replication_data$ID <- as.character(replication_data$ID)
  
  forest_estimates <- bind_rows(forest_estimates, replication_data)
  
  #### Set up the effect IDs with the proper order for the forest plot
  
  forest_estimates$ID <- factor(forest_estimates$ID, 
                                levels = c(
                                  "Original",  
                                  "Replication", 
                                  as.character(arrange(replication_data, k)$ID)
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
  
  effect_max <- round(max(forest_estimates$ci_upper, na.rm = TRUE), 1) + boundary_pad
  effect_min <- round(min(forest_estimates$ci_lower, na.rm = TRUE), 1) - boundary_pad
  
  ### Draw the forest plot
  
  forest <- 
    ggplot(forest_estimates,
           aes(
             x = k,
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
      x = expression(paste("Effect size (", italic("power function coefficient"), ")", sep = "")),
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

### Forest plot for means

forest_plot_mean <- function(meta_analysis, replication_data, org_mean, org_ci_lower, org_ci_upper, title, study_color = "black", boundary_pad = 0.20, breaks = 0.10) {
  
  ### Set up original and meta-analytic estimates
  
  forest_estimates <- data.frame(
    ID = c("Replication", "Original"),
    k = c(meta_analysis$beta[[1]], org_mean),
    var = c(NA, NA),
    ci_lower = c(meta_analysis$ci.lb, org_ci_lower),
    ci_upper = c(meta_analysis$ci.ub, org_ci_upper)
  )
  
  ### Bind the original and meta-analytic estimates to the calculated effects from each lab
  
  replication_data$ID <- as.character(replication_data$ID)
  
  forest_estimates <- bind_rows(forest_estimates, replication_data)
  
  #### Set up the effect IDs with the proper order for the forest plot
  
  forest_estimates$ID <- factor(forest_estimates$ID, 
                                levels = c(
                                  "Original",  
                                  "Replication", 
                                  as.character(arrange(replication_data, mean)$ID)
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
  
  effect_max <- round(max(forest_estimates$ci_upper, na.rm = TRUE), 1) + boundary_pad
  effect_min <- round(min(forest_estimates$ci_lower, na.rm = TRUE), 1) - boundary_pad
  
  ### Draw the forest plot
  
  forest <- 
    ggplot(forest_estimates,
           aes(
             x = k,
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
      x = "Mean",
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

serota_h1_k <- read.csv("./data/serota_effects/serota_h1_k.csv")

serota_desc <- read.csv("./data/serota_effects/serota_desc.csv")

# Import original effects

serota_org <- read.csv("./data/original/serota_original.csv")

# Set study color

serota_color_1 <- "#C3941D"

# DESCRIPTIVES --------------------------------------------------------

## Mean

### Random effects meta-analysis

serota_mean_meta <- rma(
  yi = mean, 
  vi = var_m,
  data = serota_desc,
  method = "REML"
)

### Forest plot

serota_mean_forest <- 
  forest_plot_mean(
    meta_analysis = serota_mean_meta, 
    replication_data = serota_mean_meta, 
    title = "Serota et al (2010), Average Lies per Day",
    study_color = serota_color_1,
    org_mean = serota_org[serota_org$hypothesis == "mean", ]$mean, 
    org_ci_lower = serota_org[serota_org$hypothesis == "mean", ]$ci_lower, 
    org_ci_upper = serota_org[serota_org$hypothesis == "mean", ]$ci_upper
  )

## Standard deviation

### Meta-analysis

# This approach is based on the equation provided by Bond & DePaulo (2008, 10.1037/0033-2909.134.4.477)
# Test this to make sure it works reasonably...

serota_sd_meta <- rma(
  yi = sd,
  mods = ~ sd_weight,
  data = serota_desc,
  weighted = FALSE,
  method = "REML"
)

# HYPOTHESIS 1 --------------------------------------------------------

## Random effects meta-analysis

serota_h1_meta <- rma(
  yi = k, 
  vi = var,
  data = serota_h1_k,
  method = "REML"
)

## Forest plot

serota_h1_forest <- 
  forest_plot_k(
    meta_analysis = serota_h1_meta, 
    replication_data = serota_h1_meta, 
    title = "Serota et al (2010), Hypothesis 1",
    study_color = serota_color_1,
    org_k = serota_org[serota_org$hypothesis == "h1", ]$k, 
    org_ci_lower = serota_org[serota_org$hypothesis == "h1", ]$ci_lower, 
    org_ci_upper = serota_org[serota_org$hypothesis == "h1", ]$ci_upper
  )