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
  
  sd_pooled <- sd(y, na.rm = TRUE)
  
  n_1 <- length(y[x == cond_1]) 
  
  n_2 <- length(y[x == cond_2])
  
  df <- n_1 + n_2 - 2
  
  d <- m_diff/sd_pooled
  
  var <- ((n_1 + n_2) / (n_1 * n_2)) + ((d^2) / (2*(n_1 + n_2 - 2)))
  
  ci_upper <- d + sqrt(var) * qt(.975, df)
  
  ci:lower <- d - sqrt(var) * qt(.975, df)
  
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
  
  a <- sum(y[x == cond_1] == 1)
  b <- sum(y[x == cond_1] == 0)
  c <- sum(y[x == cond_2] == 1)
  d <- sum(y[x == cond_2] == 0)
  
  log.odds <- log( ( a/b )/( c/d ) )
  
  var <- 1/a + 1/b + 1/c + 1/d
  
  ci_upper <- log_odds + sqrt(var)*qnorm(.975)
  
  ci_lower <- log_odds - sqrt(var)*qnorm(.975)
  
  out <- data.frame(ID = ID, log_odds = log_odds, var = var, ci_lower = ci_lower, ci_upper = ci_upper)
  
  return(out)
  
}