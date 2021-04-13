#######################################################################

# Main Figure and Table

#######################################################################

# Set up environment --------------------------------------------------

## Functions

### Function to format lines for the main table

meta_extraction <- function(study, hypothesis, meta, metric, original, org_lower, org_upper, index = 1, data = NULL) {
  
  require(metafor)
  
  rounder <- function(x, n) {
    
    format(round(x, n), nsmall = n)
    
  }
  
  # Estimate
  
  b        <- meta$beta[index]
  lower    <- meta$ci.lb[index]
  upper    <- meta$ci.ub[index]
  
  meta_est <- paste(rounder(b, 3), " [", rounder(lower, 3), ", ", rounder(upper, 3), "]", sep = "")
  
  org_est <- paste(rounder(original, 3), " [", rounder(org_lower, 3), ", ", rounder(org_upper, 3), "]", sep = "")
  
  # Heterogeneity
  
  Q        <- rounder(meta$QE, 3)
  Q_df     <- meta$k - length(meta$beta)
  
  if (meta$QEp < .001) {
    
    Q_p    <- "< .001"
    
  } else {
    
    if (round(meta$QEp, 3) == 1) {
      
      Q_p  <- "= .999"
      
    } else {
      
      Q_p  <- paste("= ", sub("0.", ".", rounder(meta$QEp, 3)), sep = "")
      
    }
    
  } 
  
  
  
  
  
  
  Q_stat   <- paste("Q (", Q_df, ") = ", Q, ", p ", Q_p, sep = "") 
  
  tau      <- rounder(sqrt(meta$tau2), 3)
  I2       <- meta$I2
  
  if (is.null(I2)) {
    
    # This code is adapted from Viechtbauer (2010)
    # http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate#dokuwiki__top
    
    W <- diag(1/data$var)
    
    X <- model.matrix(meta)
    
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    
    I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k - meta$p)/sum(diag(P)))
    
  }
  
  I2 <- rounder(I2, 3)
  
  # Interpretation
  
  ## Signal
  
  if (lower > 0 | upper < 0) {
    
    signal <- "Signal - "
    
  } else {
    
    signal <- "No Signal - "
    
  }
  
  ## Consistency
    
  if (original < upper & original > lower) {
    
    consistency <- "Consistent"
    
    modifier    <- ""
    
  } else {
    
    consistency <- "Inconsistent"
    
    if (signal == "Signal - ") {
      
      if (original > b) {
        
        size     <- round(b/original * 100)
        
        modifier <- paste(" (Smaller, ", size, "% of original)", sep = "")
        
      }
      
      if (original < b) {
        
        size     <- round(b/original * 100)
        
        modifier <- paste(" (Larger, ", size, "% of original)", sep = "")
        
      }
      
      if ((original * b) < 0) {
        
        modifier <- " (Opposite)"
        
      }
      
    } else {
      
      modifier   <-  ""
      
    }
    
    
    
  }
  
  interpret <- paste(signal, consistency, modifier, sep = "")
  
  # Output
  
  out <- data.frame(
    study       = study,
    Hypothesis  = hypothesis,
    Effect      = metric,
    Replication = meta_est, 
    Original    = org_est,
    Outcome     = interpret,
    Q           = Q_stat,
    Tau         = tau,
    I2          = paste(I2, "%", sep = "")
    )
  
  return(out)
  
}

# Organize data -------------------------------------------------------

## Set up empty data frame for table

focal_effects <- 15 # Number of effects to include in main report

main_table_data <- data.frame(
  study         = rep(NA, focal_effects),
  hypothesis    = rep(NA, focal_effects),
  metric        = rep(NA, focal_effects),
  meta_analysis = rep(NA, focal_effects),
  data          = rep(NA, focal_effects),
  original      = rep(NA, focal_effects),
  org_ci_lower  = rep(NA, focal_effects),
  org_ci_upper  = rep(NA, focal_effects),
  index         = rep(NA, focal_effects)
)

## Correll et al  (2002, Study 1)

# Race-Biased Shooting

main_table_data[1, ] <- data.frame(
  study         = "Correll et al (2002, Study 1)",
  hypothesis    = "Race-Biased Shooting",
  metric        = "d",
  meta_analysis = "correll_h2_meta",
  data          = "correll_h2_smd",
  original      = correll_org$d[correll_org$hypothesis == "h2"],
  org_ci_lower  = correll_org$ci_lower[correll_org$hypothesis == "h2"],
  org_ci_upper  = correll_org$ci_upper[correll_org$hypothesis == "h2"],
  index         = 1
)

# Race-Biased Holding Fire

main_table_data[2, ] <- data.frame(
  study         = "Correll et al (2002, Study 1)",
  hypothesis    = "Race-Biased Holding Fire",
  metric        = "d",
  meta_analysis = "correll_h3_meta",
  data          = "correll_h3_smd",
  original      = correll_org$d[correll_org$hypothesis == "h3"],
  org_ci_lower  = correll_org$ci_lower[correll_org$hypothesis == "h3"],
  org_ci_upper  = correll_org$ci_upper[correll_org$hypothesis == "h3"],
  index         = 1
)

# Race-Biased False Alarms

main_table_data[3, ] <- data.frame(
  study         = "Correll et al (2002, Study 1)",
  hypothesis    = "Race-Biased False Alarms",
  metric        = "d",
  meta_analysis = "correll_h5_meta",
  data          = "correll_h5_smd",
  original      = correll_org$d[correll_org$hypothesis == "h5"],
  org_ci_lower  = correll_org$ci_lower[correll_org$hypothesis == "h5"],
  org_ci_upper  = correll_org$ci_upper[correll_org$hypothesis == "h5"],
  index         = 1
)

## Darley et al (2000, Study 2)

# Punishment, Jealousy vs. Inoperable Tumor

main_table_data[4, ] <- data.frame(
  study         = "Darley et al (2000, Study 2)",
  hypothesis    = "Punishment, Jealousy vs. Inoperable Tumor",
  metric        = "d",
  meta_analysis = "darley_h1_meta",
  data          = "darley_h1_smd",
  original      = darley_org$d[darley_org$hypothesis == "h1"],
  org_ci_lower  = darley_org$ci_lower[darley_org$hypothesis == "h1"],
  org_ci_upper  = darley_org$ci_upper[darley_org$hypothesis == "h1"],
  index         = 1
)

# Punishment, Jealousy vs. Operable Tumor

main_table_data[5, ] <- data.frame(
  study         = "Darley et al (2000, Study 2)",
  hypothesis    = "Punishment, Jealousy vs. Operable Tumor",
  metric        = "d",
  meta_analysis = "darley_h2_meta",
  data          = "darley_h2_smd",
  original      = darley_org$d[darley_org$hypothesis == "h2"],
  org_ci_lower  = darley_org$ci_lower[darley_org$hypothesis == "h2"],
  org_ci_upper  = darley_org$ci_upper[darley_org$hypothesis == "h2"],
  index         = 1
)

# Punishment, Inoperable vs. Operable Tumor

main_table_data[6, ] <- data.frame(
  study         = "Darley et al (2000, Study 2)",
  hypothesis    = "Punishment, Inoperable vs. Operable Tumor",
  metric        = "d",
  meta_analysis = "darley_h3_meta",
  data          = "darley_h3_smd",
  original      = darley_org$d[darley_org$hypothesis == "h3"],
  org_ci_lower  = darley_org$ci_lower[darley_org$hypothesis == "h3"],
  org_ci_upper  = darley_org$ci_upper[darley_org$hypothesis == "h3"],
  index         = 1
)

## Loftus & Palmer (1974, Study 2)

# Speed Estimates, Smashed vs. Hit

main_table_data[7, ] <- data.frame(
  study         = "Loftus & Palmer (1974, Study 2)",
  hypothesis    = "Speed Estimates, Smashed vs. Hit",
  metric        = "d",
  meta_analysis = "loftus_h1_meta",
  data          = "loftus_h1_smd",
  original      = loftus_org$d[loftus_org$hypothesis == "h1"],
  org_ci_lower  = loftus_org$ci_lower[loftus_org$hypothesis == "h1"],
  org_ci_upper  = loftus_org$ci_upper[loftus_org$hypothesis == "h1"],
  index         = 1
)

# Broken Glass, Smashed vs. Hit

main_table_data[8, ] <- data.frame(
  study         = "Loftus & Palmer (1974, Study 2)",
  hypothesis    = "Broken Glass, Smashed vs. Hit",
  metric        = "log odds ratio",
  meta_analysis = "loftus_h2_meta",
  data          = "loftus_h2_lor",
  original      = loftus_org$lor[loftus_org$hypothesis == "h2"],
  org_ci_lower  = loftus_org$ci_lower[loftus_org$hypothesis == "h2"],
  org_ci_upper  = loftus_org$ci_upper[loftus_org$hypothesis == "h2"],
  index         = 1
)

# Broken Glass, Smashed vs. Control

main_table_data[9, ] <- data.frame(
  study         = "Loftus & Palmer (1974, Study 2)",
  hypothesis    = "Broken Glass, Smashed vs. Control",
  metric        = "log odds ratio",
  meta_analysis = "loftus_h3_meta",
  data          = "loftus_h3_lor",
  original      = loftus_org$lor[loftus_org$hypothesis == "h3"],
  org_ci_lower  = loftus_org$ci_lower[loftus_org$hypothesis == "h3"],
  org_ci_upper  = loftus_org$ci_upper[loftus_org$hypothesis == "h3"],
  index         = 1
)

# Speed Estimates as a Partial Mediator (indirect)

main_table_data[10, ] <- data.frame(
  study         = "Loftus & Palmer (1974, Study 2)",
  hypothesis    = "Speed Estimates as a Partial Mediator (indirect)",
  metric        = "b",
  meta_analysis = "loftus_h4_meta",
  data          = "loftus_h4_med_long",
  original      = loftus_org$b[loftus_org$hypothesis == "h4_indirect"],
  org_ci_lower  = loftus_org$ci_lower[loftus_org$hypothesis == "h4_indirect"],
  org_ci_upper  = loftus_org$ci_upper[loftus_org$hypothesis == "h4_indirect"],
  index         = 2
)

# Speed Estimates as a Partial Mediator (direct)

main_table_data[11, ] <- data.frame(
  study         = "Loftus & Palmer (1974, Study 2)",
  hypothesis    = "Speed Estimates as a Partial Mediator (direct)",
  metric        = "b",
  meta_analysis = "loftus_h4_meta",
  data          = "loftus_h4_med_long",
  original      = loftus_org$b[loftus_org$hypothesis == "h4_direct"],
  org_ci_lower  = loftus_org$ci_lower[loftus_org$hypothesis == "h4_direct"],
  org_ci_upper  = loftus_org$ci_upper[loftus_org$hypothesis == "h4_direct"],
  index         = 1
)

## Serota et al (2010, Study 3)

# Average Number of Lies

main_table_data[12, ] <- data.frame(
  study         = "Serota et al (2010, Study 3)",
  hypothesis    = "Average Number of Lies",
  metric        = "mean",
  meta_analysis = "serota_mean_meta",
  data          = "serota_desc",
  original      = serota_org$mean[serota_org$hypothesis == "mean_student"],
  org_ci_lower  = serota_org$ci_lower[serota_org$hypothesis == "mean_student"],
  org_ci_upper  = serota_org$ci_upper[serota_org$hypothesis == "mean_student"],
  index         = 1
)

# Distribution of Lies

main_table_data[13, ] <- data.frame(
  study         = "Serota et al (2010, Study 3)",
  hypothesis    = "Distribution of Lies",
  metric        = "exponent",
  meta_analysis = "serota_h1_meta",
  data          = "serota_h1_k",
  original      = serota_org$k[serota_org$hypothesis == "h1_student"],
  org_ci_lower  = serota_org$ci_lower[serota_org$hypothesis == "h1_student"],
  org_ci_upper  = serota_org$ci_upper[serota_org$hypothesis == "h1_student"],
  index         = 1
)

# Distribution of Lies (US)

main_table_data[14, ] <- data.frame(
  study         = "Serota et al (2010, Study 3)",
  hypothesis    = "Distribution of Lies (US)",
  metric        = "exponent",
  meta_analysis = "serota_usa_meta",
  data          = "serota_h1_k",
  original      = serota_org$k[serota_org$hypothesis == "h1_student"],
  org_ci_lower  = serota_org$ci_lower[serota_org$hypothesis == "h1_student"],
  org_ci_upper  = serota_org$ci_upper[serota_org$hypothesis == "h1_student"],
  index         = 2
)

# Distribution of Lies (non-US)

main_table_data[15, ] <- data.frame(
  study         = "Serota et al (2010, Study 3)",
  hypothesis    = "Distribution of Lies (non-US)",
  metric        = "exponent",
  meta_analysis = "serota_usa_meta",
  data          = "serota_h1_k",
  original      = serota_org$k[serota_org$hypothesis == "h1_student"],
  org_ci_lower  = serota_org$ci_lower[serota_org$hypothesis == "h1_student"],
  org_ci_upper  = serota_org$ci_upper[serota_org$hypothesis == "h1_student"],
  index         = 1
)

# Create main table ---------------------------------------------------

## Empty table

main_table <- 
data.frame(
  Study       = rep(NA, focal_effects),
  Hypothesis  = rep(NA, focal_effects),
  Effect  = rep(NA, focal_effects),
  Replication = rep(NA, focal_effects), 
  Original    = rep(NA, focal_effects),
  Outcome     = rep(NA, focal_effects),
  Q           = rep(NA, focal_effects),
  Tau         = rep(NA, focal_effects),
  I2          = rep(NA, focal_effects)
)

## Add table data

for (i in 1:nrow(main_table_data)) {
  
  temp_effect <- main_table_data[i, ]
  
  main_table[i, ] <- meta_extraction(
    study      = temp_effect$study,
    hypothesis = temp_effect$hypothesis, 
    metric     = temp_effect$metric,
    meta       = get(temp_effect$meta_analysis), 
    original   = temp_effect$original, 
    org_lower  = temp_effect$org_ci_lower, 
    org_upper  = temp_effect$org_ci_upper, 
    index      = temp_effect$index, 
    data       = get(temp_effect$data)
  )
  
}

## Export main table

if (write_data == TRUE) {
  
  write.csv(
    main_table,
    "./data/main_table.csv",
    row.names = FALSE
  )
  
}



