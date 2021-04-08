#######################################################################

# Main Figure and Table

#######################################################################

meta_extraction <- function(meta, original, org_lower, org_upper, index = 1, data = NULL) {
  
  require(metafor)
  
  # Estimate
  
  b        <- meta$beta[index]
  lower    <- meta$ci.lb[index]
  upper    <- meta$ci.ub[index]
  
  meta_est <- paste(round(b, 3), " [", round(lower, 3), ", ", round(upper, 3), "]", sep = "")
  
  org_est <- paste(round(original, 3), " [", round(org_lower, 3), ", ", round(org_upper, 3), "]", sep = "")
  
  # Heterogeneity
  
  Q        <- round(meta$QE, 3)
  Q_df     <- meta$k - length(meta$beta)
  
  if (meta$QEp < .001) {
    
    Q_p    <- "< .001"
    
  } else (
    
    Q_p      <- paste("= ", round(meta$QEp, 3), sep = "")
    
  )
  
  
  
  
  Q_stat   <- paste("Q (", Q_df, ") = ", Q, ", p ", Q_p, sep = "") 
  
  tau      <- round(sqrt(meta$tau2), 3)
  I2       <- meta$I2
  
  if (is.null(I2)) {
    
    # This code is adapted from Viechtbauer (2010)
    # http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate#dokuwiki__top
    
    W <- diag(1/data$var)
    
    X <- model.matrix(meta)
    
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    
    I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k - meta$p)/sum(diag(P)))
    
  }
  
  I2 <- round(I2, 3)
  
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
    
    if (signal == "Signal") {
      
      if (original > b) {
        
        size     <- round(b/original * 100)
        
        modifier <- paste(" (Smaller, ", size, "% of original)")
        
      }
      
      if (original < b) {
        
        size     <- round(b/original * 100)
        
        modifier <- paste(" (Larger, ", size, "% of original)")
        
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
    Replication = meta_est, 
    Original    = org_est,
    Outcome     = interpret,
    Q           = Q_stat,
    Tau         = tau,
    I2          = paste(I2, "%", sep = "")
    )
  
  return(out)
  
}
