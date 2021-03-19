
#######################################################################

# Loftus & Palmer (1974) [Study 2] -- Meta-Analysis

#######################################################################

# Set up environment --------------------------------------------------

## Packages

packages <- c("metafor", "dplyr", "tidyr", "ggplot2", "cowplot")

lapply(packages, library, character.only = TRUE)

## Seed

set.seed(1621)

## Functions

### General effect size functions

source("./scripts/calculate/effect_size_functions.R")

### Forest plot for mediation coefficients

forest_plot_med <- function(meta_analysis, replication_data, org_med, org_ci_lower, org_ci_upper, meta_effect, meta_ci_lower, meta_ci_upper, title, study_color = "black", boundary_pad = 0.25, multiple = .50) {
  
  ### Set up original and meta-analytic estimates
  
  forest_estimates <- data.frame(
    ID       = c("Replication", rep("Original", length(org_med))),
    estimate = c(meta_effect, org_med),
    var      = c(NA, rep(NA, length(org_med))),
    ci_lower = c(meta_ci_lower, org_ci_lower),
    ci_upper = c(meta_ci_lower, org_ci_upper)
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
  
  effect_max <- round(max(forest_estimates$ci_upper, na.rm = TRUE) / multiple) * multiple
  effect_min <- round(min(forest_estimates$ci_lower, na.rm = TRUE) / multiple) * multiple
  
  ### Draw the forest plot
  
  forest <- 
    ggplot(forest_estimates,
           aes(
             x = estimate,
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
      xintercept = meta_effect,
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
      breaks = sort(c(0, seq(effect_min, effect_max, multiple))),
      labels = format(sort(c(0, seq(effect_min, effect_max, multiple))), nsmall = 2)
    ) +
    coord_cartesian(
      xlim = c(effect_min - boundary_pad, effect_max + boundary_pad)
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

## Forest plots 

### Direct effect

loftus_h4_direct_forest <- 
  forest_plot_med(
    meta_analysis    = loftus_h4_meta, 
    replication_data = loftus_h4_med_long %>% filter(type == "direct"), 
    title            = "Loftus & Palmer (1974), Hypothesis 4, Direct Effect",
    study_color      = loftus_color_1,
    org_med          = loftus_org[loftus_org$hypothesis == "h4", ]$estimate, 
    org_ci_lower     = loftus_org[loftus_org$hypothesis == "h4", ]$ci_lower, 
    org_ci_upper     = loftus_org[loftus_org$hypothesis == "h4", ]$ci_upper,
    meta_effect      = loftus_h4_meta$beta[[1]],
    meta_ci_lower    = loftus_h4_meta$ci.lb[[1]],
    meta_ci_upper    = loftus_h4_meta$ci.ub[[1]]
  )

### Indirect effect

loftus_h4_indirect_forest <- 
  forest_plot_med(
    meta_analysis    = loftus_h4_meta, 
    replication_data = loftus_h4_med_long %>% filter(type == "indirect"), 
    title            = "Loftus & Palmer (1974), Hypothesis 4, Indirect Effect",
    study_color      = loftus_color_1,
    org_med          = loftus_org[loftus_org$hypothesis == "h4", ]$estimate, 
    org_ci_lower     = loftus_org[loftus_org$hypothesis == "h4", ]$ci_lower, 
    org_ci_upper     = loftus_org[loftus_org$hypothesis == "h4", ]$ci_upper,
    meta_effect      = loftus_h4_meta$beta[[2]],
    meta_ci_lower    = loftus_h4_meta$ci.lb[[2]],
    meta_ci_upper    = loftus_h4_meta$ci.ub[[2]]
  )


