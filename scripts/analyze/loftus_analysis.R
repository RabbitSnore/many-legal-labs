
#######################################################################

# Loftus & Palmer (1974) [Study 2] -- Meta-Analysis

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("metafor", "dplyr", "ggplot2")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(1621)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

### Forest plot for mediation coefficients

forest_plot_med <- function(meta_analysis, replication_data, org_med, org_ci_lower, org_ci_upper, title, study_color = "black", boundary_pad = 0.20, breaks = 0.10) {
  
  ### Set up original and meta-analytic estimates
  
  forest_estimates <- data.frame(
    ID       = c(rep("Replication", 2), rep("Original", length(org_med))),
    estimate = c(meta_analysis$beta[[1]], meta_analysis$beta[[2]], org_med),
    var      = c(NA, NA, rep(NA, length(org_med))),
    ci_lower = c(meta_analysis$ci.lb[[1]], meta_analysis$ci.lb[[2]], org_ci_lower),
    ci_upper = c(meta_analysis$ci.ub[[1]], meta_analysis$ci.ub[[2]], org_ci_upper)
  )
  
  ### Bind the original and meta-analytic estimates to the calculated effects from each lab
  
  replication_data$ID <- as.character(replication_data$ID)
  
  forest_estimates <- bind_rows(forest_estimates, replication_data)
  
  #### Set up the effect IDs with the proper order for the forest plot
  
  forest_estimates$ID <- factor(forest_estimates$ID, 
                                levels = c(
                                  "Original",  
                                  "Replication", 
                                  as.character(unique(arrange(replication_data, estimate)$ID))
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
             x = estimate,
             y = ID,
             xmin = ci_lower,
             xmax = ci_upper
           )) +
    facet_wrap(~ type, nrow = 1) +
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
    # geom_vline(
    #   xintercept = meta_analysis$beta[[1]],
    #   linetype = "dashed",
    #   color = "black",
    #   size = 1
    # ) +
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
      x = expression(paste("Effect size (", italic("standardized coefficient"), ")", sep = "")),
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

loftus_h1_smd  <- read.csv("./data/loftus_effects/loftus_h1_smd.csv")
loftus_h2_lor  <- read.csv("./data/loftus_effects/loftus_h2_lor.csv")
loftus_h3_lor  <- read.csv("./data/loftus_effects/loftus_h3_lor.csv")
loftus_h4_med  <- read.csv("./data/loftus_effects/loftus_h4_med.csv")

# Import original effects

loftus_org <- read.csv("./data/original/loftus_original.csv")

# Set study color

loftus_color_1 <- "#6D98BA"

# HYPOTHESIS 1 --------------------------------------------------------

## Random effects meta-analysis

loftus_h1_meta <- rma(
  yi = d, 
  vi = var,
  data = loftus_h1_smd,
  method = "REML"
)

## Forest plot

loftus_h1_forest <- 
  forest_plot_smd(
    meta_analysis = loftus_h1_meta, 
    replication_data = loftus_h1_smd, 
    title = "Loftus & Palmer (1974), Hypothesis 1",
    study_color = loftus_color_1,
    org_d = loftus_org[loftus_org$hypothesis == "h1", ]$d, 
    org_ci_lower = loftus_org[loftus_org$hypothesis == "h1", ]$ci_lower, 
    org_ci_upper = loftus_org[loftus_org$hypothesis == "h1", ]$ci_upper
  )

# HYPOTHESIS 2 --------------------------------------------------------

## Random effects meta-analysis

loftus_h2_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = loftus_h2_lor,
  method = "REML"
)

## Forest plot

loftus_h2_forest <- 
  forest_plot_lor(
    meta_analysis = loftus_h2_meta, 
    replication_data = loftus_h2_lor, 
    title = "Loftus & Palmer (1974), Hypothesis 2",
    study_color = loftus_color_1,
    org_lor = loftus_org[loftus_org$hypothesis == "h2", ]$lor, 
    org_ci_lower = loftus_org[loftus_org$hypothesis == "h2", ]$ci_lower, 
    org_ci_upper = loftus_org[loftus_org$hypothesis == "h2", ]$ci_upper
  )

# HYPOTHESIS 3 --------------------------------------------------------

## Random effects meta-analysis

loftus_h3_meta <- rma(
  yi = log_odds, 
  vi = var,
  data = loftus_h3_lor,
  method = "REML"
)

## Forest plot

loftus_h3_forest <- 
  forest_plot_lor(
    meta_analysis = loftus_h2_meta, 
    replication_data = loftus_h2_lor, 
    title = "Loftus & Palmer (1974), Hypothesis 3",
    study_color = loftus_color_1,
    org_lor = loftus_org[loftus_org$hypothesis == "h3", ]$lor, 
    org_ci_lower = loftus_org[loftus_org$hypothesis == "h3", ]$ci_lower, 
    org_ci_upper = loftus_org[loftus_org$hypothesis == "h3", ]$ci_upper
  )

# HYPOTHESIS 4 --------------------------------------------------------

## Data wrangle

loftus_h4_med_long <- loftus_h4_med %>% 
  pivot_longer(
    cols = c(ends_with("est")),
    names_to = c("type", "parameter"),
    values_to = "estimate",
    names_pattern = "(.*)_(.*)"
  ) %>% 
  mutate(
    var = case_when(
      type == "direct"   ~ direct_var,
      type == "indirect" ~ indirect_var
    ),
    ci_upper = case_when(
      type == "direct"   ~ direct_ci_upper,
      type == "indirect" ~ indirect_ci_upper
    ),
    ci_lower = case_when(
      type == "direct"   ~ direct_ci_lower,
      type == "indirect" ~ indirect_ci_lower
    )
  ) %>% 
  select(-starts_with("direct"), -starts_with("indirect"), -parameter)

## Multivariate meta-analysis

# Multivariate meta-analysis simultaneously synthesizing direct and indirect effects, as recommended by Cheung (2020, 10.31234/osf.io/df6jp) and Cheung and Cheung (2016, 10.1002/jrsm.1166).

loftus_h4_meta <- rma.mv(
  yi = estimate, 
  V = var,
  mods = ~ type - 1,
  random = ~ type|ID,
  data = loftus_h4_med_long,
  method = "REML"
)

## Forest plot 

# THE DIRECT AND INDIRECT EFFECTS ARE PROBABLY BEST PLOTTED SEPARATELY AND THEN COMBINED INTO ONE PLOT

loftus_h4_forest <- 
  forest_plot_med(
    meta_analysis = loftus_h4_meta, 
    replication_data = loftus_h4_med_long, 
    title = "Loftus & Palmer (1974), Hypothesis 4",
    study_color = loftus_color_1,
    org_med = loftus_org[loftus_org$hypothesis == "h4", ]$lor, 
    org_ci_lower = loftus_org[loftus_org$hypothesis == "h4", ]$ci_lower, 
    org_ci_upper = loftus_org[loftus_org$hypothesis == "h4", ]$ci_upper
  )

