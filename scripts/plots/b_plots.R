
## Loftus & Palmer (1974, Study 2), hypothesis 4

# Indirect

loftus_h4_plot_indirect<- filter(loftus_h4_med_long, type == "indirect")
loftus_h4_plot_indirect$hypothesis <- "h4_indirect"

# Direct

loftus_h4_plot_direct<- filter(loftus_h4_med_long, type == "direct")
loftus_h4_plot_direct$hypothesis <- "h4_direct"


## Estimate data function b

estimate_data_b <- function(meta_analysis, org_b, org_ci_lower, org_ci_upper, index) {
  # Set up original and meta-analytic estimates
  estimates <- data.frame(
    ID       = c("Replication", "Original"),
    b      = c(meta_analysis$beta[[index]], org_b),
    var      = c(NA, NA),
    ci_lower = c(meta_analysis$ci.lb[[index]], org_ci_lower),
    ci_upper = c(meta_analysis$ci.ub[[index]], org_ci_upper)
  )
  return(estimates)
}


## Estimate data Loftus, Indirect

estimates_loftus_4_indirect <- estimate_data_b(
  meta_analysis    = loftus_h4_meta,
  org_b          = loftus_org$b[loftus_org$hypothesis == "h4_indirect"],
  org_ci_lower     = loftus_org$ci_lower[loftus_org$hypothesis == "h4_indirect"], 
  org_ci_upper     = loftus_org$ci_upper[loftus_org$hypothesis == "h4_indirect"],
  index = 2)

estimates_loftus_4_indirect$ID <- factor(estimates_loftus_4_indirect$ID, levels = c("Replication", "Original"))

estimates_loftus_4_indirect$hypothesis <- c(rep("h4_indirect", 2))

estimates_loftus_4_indirect$hypothesis <- factor(estimates_loftus_4_indirect$hypothesis, levels = rev(c("h4_indirect")))

loftus_h4_plot_indirect$hypothesis <- factor(loftus_h4_plot_indirect$hypothesis, levels = rev(c("h4_indirect")))


## Estimate data Loftus, Direct

estimates_loftus_4_direct <- estimate_data_b(
  meta_analysis    = loftus_h4_meta,
  org_b          = loftus_org$b[loftus_org$hypothesis == "h4_direct"],
  org_ci_lower     = loftus_org$ci_lower[loftus_org$hypothesis == "h4_direct"], 
  org_ci_upper     = loftus_org$ci_upper[loftus_org$hypothesis == "h4_direct"],
  index = 1)

estimates_loftus_4_direct$ID <- factor(estimates_loftus_4_direct$ID, levels = c("Replication", "Original"))

estimates_loftus_4_direct$hypothesis <- c(rep("h4_direct", 2))

estimates_loftus_4_direct$hypothesis <- factor(estimates_loftus_4_direct$hypothesis, levels = rev(c("h4_direct")))

loftus_h4_plot_direct$hypothesis <- factor(loftus_h4_plot_direct$hypothesis, levels = rev(c("h4_direct")))


## Plot function b

plot_func_b <- function(complete, estimate, study_colors, titles, boundary_pad = .25) {
  
  # Set up plot boundaries
  
  if (sum(estimate$b > 4, estimate$ci_lower > 4, estimate$ci_upper > 4) >1) {
    mutiple = 1
  } else if (sum(estimate$b < 0.5, estimate$ci_lower < 0.5, estimate$ci_upper < 0.5) == 6) {
    multiple = 0.10
  } else {
    multiple = 0.5
  }  
  
  effect_max <- round(max(c(complete$estimate, estimate$ci_upper), na.rm = TRUE) / multiple) * multiple
  
  effect_min <- round(min(c(complete$estimate, estimate$ci_lower), na.rm = TRUE) / multiple) * multiple
  
  
  
  # Draw figure
  fig_new <- 
    ggplot(complete,
           aes(
             x = estimate,
             y = hypothesis,
             color = hypothesis
           )
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "longdash"
    ) +
    geom_quasirandom(
      width = .30,
      groupOnX = FALSE,
      alpha = .20,
      aes(
        # size = 1/var
      )
    ) +
    scale_size_continuous(
      range = c(.25, 2)
    ) +
    geom_errorbarh(
      data = estimate,
      inherit.aes = FALSE,
      aes(
        y = hypothesis,
        xmax = ci_upper,
        xmin = ci_lower,
        group = ID,
        color = hypothesis,
        alpha = ID
      ),
      height = .25,
      size = 1,
      position = position_dodgev(height = .25)
    ) +
    geom_point(
      data = estimate,
      inherit.aes = FALSE,
      aes(
        y = hypothesis,
        x = b,
        group = ID,
        shape = ID,
        color = hypothesis,
        alpha = ID),
      size = 3,
      position = position_dodgev(height = .25)
    ) +
    scale_alpha_manual(
      values = c(1, .66)
    ) +
    scale_color_manual(
      values = rev(study_colors)
    ) +
    scale_y_discrete(
      labels = rev(titles)
    ) +
    scale_shape_manual(
      values = c(16, 15)
    ) +
    scale_x_continuous(
      breaks = sort(c(0, seq(effect_min, effect_max, multiple))),
      labels = format(sort(c(0, seq(effect_min, effect_max, multiple))), nsmall = 2)
    ) +
    coord_cartesian(
      xlim = c(effect_min - boundary_pad, effect_max + boundary_pad)
    ) +
    labs(
      shape = "",
      title = titles,
      y = "",
      x = expression(paste("Effect size (", italic("b"), ")", sep = ""))
    ) +
    guides(
      group = FALSE,
      size = FALSE,
      color = FALSE,
      alpha = FALSE
    ) +
    theme_classic() +
    theme(
      axis.text = element_text(color = "black"),
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_blank(),
      legend.position = "none"
    )
}


# Loftus Indirect plot

loftus_plot_h4_indirect <- plot_func_b(
  complete     = loftus_h4_plot_indirect,
  estimate     = estimates_loftus_4_indirect,
  study_colors = loftus_color_1, 
  titles       = c("Speed Estimates as a \n Partial Mediator (indirect)")
)


# Loftus Direct plot

loftus_plot_h4_direct <- plot_func_b(
  complete     = loftus_h4_plot_direct,
  estimate     = estimates_loftus_4_direct,
  study_colors = loftus_color_1, 
  titles       = c("Speed Estimates as a \n Partial Mediator (direct)")
)
