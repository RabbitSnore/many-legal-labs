#######################################################################

# General Effect Size Functions

#######################################################################

# STANDARDIZED MEAN DIFFERENCE ----------------------------------------

## Calculate standardized mean difference, sampling variance, and 95% confidence intervals

### Note that this function is designed to compute a mean difference standardized by the standard deviation pooled across all observations, and it will ignore any dependence of observations.
### This approach is consistent with the recommendations of Jake Westfall in the documentation for the PANGEA power calculation tool, as well as in discussions of computing generalized standardized mean differences.
### One key advantage of this approach is that it is highly general and produces effect sizes that are easy to compare to other standardized effect sizes. However, it does not account for the correlation of errors between observations that, e.g., come from the same person. But this is on purpose.

d_calc <- function(ID, x, y, cond_1, cond_2) {
  
  # ID is an arbitrary identifier attached to the output
  # x is a vector of condition codes
  # y is a vector of dependent variable values
  # cond_1 and cond_2 are values for the conditions being compared
  
  m_diff <- mean(y[x == cond_1], na.rm = TRUE) - mean(y[x == cond_2], na.rm = TRUE)
  
  n_1 <- sum(!is.na(y[x == cond_1])) 
  
  n_2 <- sum(!is.na(y[x == cond_2]))

  df <- n_1 + n_2 - 2
  
  sd_1 <- sd(y[x == cond_1], na.rm = TRUE)
  
  sd_2 <- sd(y[x == cond_2], na.rm = TRUE)
  
  sd_pooled <- sqrt( (((n1 - 1) * sd_1^2) + ((n_2 - 1) * sd_2^2)) / df )
  
  d <- m_diff/sd_pooled
  
  var <- ((n_1 + n_2) / (n_1 * n_2)) + ((d^2) / (2*(df)))
  
  ci_upper <- d + sqrt(var) * qt(.975, df)
  
  ci_lower <- d - sqrt(var) * qt(.975, df)
  
  out <- data.frame(ID = ID, d = d, var = var, ci_lower = ci_lower, ci_upper = ci_upper)
  
  return(out)
  
}

# LOG ODDS RATIO ------------------------------------------------------

## Calculate logs odds ratio, sampling variance, and 95% confidence intervals

odds_calc <- function(ID, x, y, cond_1, cond_2) {
  
  # ID is an arbitrary identifier attached to the output
  # x is a vector of condition codes
  # y is a vector of dependent variable values (0/1)
  # cond_1 and cond_2 are values for the conditions being compared
  
  a <- sum(y[x == cond_1] == 1, na.rm = TRUE)
  b <- sum(y[x == cond_1] == 0, na.rm = TRUE)
  c <- sum(y[x == cond_2] == 1, na.rm = TRUE)
  d <- sum(y[x == cond_2] == 0, na.rm = TRUE)
  
  log_odds <- log( ( a/b )/( c/d ) )
  
  var <- 1/a + 1/b + 1/c + 1/d
  
  ci_upper <- log_odds + sqrt(var)*qnorm(.975)
  
  ci_lower <- log_odds - sqrt(var)*qnorm(.975)
  
  out <- data.frame(ID = ID, log_odds = log_odds, var = var, ci_lower = ci_lower, ci_upper = ci_upper)
  
  return(out)
  
}

### CREATING EMPTY DATA SETS FOR EFFECT SIZES -------------------------

#### The data frame produced by this function is designed to work with the d_calc() function

empty_smd_data <- function(n) {
  
  out <- data.frame(
    ID       = 1:n,
    d        = rep(NA, n),
    var      = rep(NA, n),
    ci_lower = rep(NA, n),
    ci_upper = rep(NA, n)
  )
  
  return(out)
  
}

#### The data frame produced by this function is designed to work with the odds_calc() function

empty_lor_data <- function(n) {
  
  out <- data.frame(
    ID       = 1:n,
    log_odds = rep(NA, n),
    var      = rep(NA, n),
    ci_lower = rep(NA, n),
    ci_upper = rep(NA, n)
  )
  
  return(out)
  
}

# FOREST PLOT FOR STANDARDIZED MEAN DIFFERENCES -----------------------

### Forest plot

forest_plot_smd <- function(meta_analysis, replication_data, org_d, org_ci_lower, org_ci_upper, title, study_color = "black", boundary_pad = 0.20, breaks = 0.10) {
  
  ### Set up original and meta-analytic estimates
  
  forest_estimates <- data.frame(
    ID = c("Replication", "Original"),
    d = c(meta_analysis$beta[[1]], org_d),
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
                                  as.character(arrange(replication_data, d)$ID)
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

# FOREST PLOT FOR LOG ODDS RATIO --------------------------------------

### Forest plot

forest_plot_lor <- function(meta_analysis, replication_data, org_lor, org_ci_lower, org_ci_upper, title, study_color = "black", boundary_pad = 0.20, breaks = 0.10) {
  
  ### Set up original and meta-analytic estimates
  
  forest_estimates <- data.frame(
    ID = c("Replication", "Original"),
    log_odds = c(meta_analysis$beta[[1]], org_lor),
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
                                  as.character(arrange(replication_data, log_odds)$ID)
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
             x = log_odds,
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
      x = expression(paste("Effect size (", italic("log odds ratio"), ")", sep = "")),
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
