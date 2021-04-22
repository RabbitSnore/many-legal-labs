##Loftus 

loftus_h2_plot<- loftus_h2_lor
loftus_h2_plot$hypothesis <- "h2"
loftus_h2_plot$experiment <- "Loftus & Palmer (1974, Study 2)"

loftus_h3_plot<- loftus_h3_lor
loftus_h3_plot$hypothesis <- "h3"
loftus_h3_plot$experiment <- "Loftus & Palmer (1974, Study 2)"

##

estimate_data_lor <- function(meta_analysis, org_lor, org_ci_lower, org_ci_upper) {
  # Set up original and meta-analytic estimates
  estimates <- data.frame(
    ID       = c("Replication", "Original"),
    lor      = c(meta_analysis$beta[[1]], org_lor),
    var      = c(NA, NA),
    ci_lower = c(meta_analysis$ci.lb, org_ci_lower),
    ci_upper = c(meta_analysis$ci.ub, org_ci_upper)
  )
  return(estimates)
}

estimates_loftus_2 <- estimate_data_lor(
  meta_analysis    = loftus_h2_meta,
  org_lor          = loftus_org$lor[loftus_org$hypothesis == "h2"],
  org_ci_lower     = loftus_org$ci_lower[loftus_org$hypothesis == "h2"], 
  org_ci_upper     = loftus_org$ci_upper[loftus_org$hypothesis == "h2"])


estimates_loftus_2$ID <- factor(estimates_loftus_2$ID, levels = c("Replication", "Original"))
estimates_loftus_2$hypothesis <- c(rep("h2", 2))
estimates_loftus_2$hypothesis <- factor(estimates_loftus_2$hypothesis, levels = rev(c("h2")))

loftus_h2_plot$hypothesis <- factor(loftus_h2_plot$hypothesis, levels = rev(c("h2")))

estimates_loftus_3 <- estimate_data_lor(
  meta_analysis    = loftus_h3_meta,
  org_lor          = loftus_org$lor[loftus_org$hypothesis == "h3"],
  org_ci_lower     = loftus_org$ci_lower[loftus_org$hypothesis == "h3"], 
  org_ci_upper     = loftus_org$ci_upper[loftus_org$hypothesis == "h3"])

estimates_loftus_3$ID <- factor(estimates_loftus_3$ID, levels = c("Replication", "Original"))
estimates_loftus_3$hypothesis <- c(rep("h3", 2))
estimates_loftus_3$hypothesis <- factor(estimates_loftus_3$hypothesis, levels = rev(c("h3")))

loftus_h3_plot$hypothesis <- factor(loftus_h3_plot$hypothesis, levels = rev(c("h3")))

##

plot_func_lor <- function(meta, complete, estimate, study_colors, titles, boundary_pad = .25) {
  
  if (sum(estimate$lor > 4, estimate$ci_lower > 4, estimate$ci_upper > 4) >1) {
    mutiple = 1
  } else if (sum(estimate$lor < 0.5, estimate$ci_lower < 0.5, estimate$ci_upper < 0.5) == 6) {
    multiple = 0.1
  } else {
    multiple = 0.5
  }
  
  # Set up plot boundaries
  effect_max <- round(max(c(complete$log_odds, estimate$ci_upper), na.rm = TRUE) / multiple) * multiple
  effect_min <- round(min(c(complete$log_odds, estimate$ci_lower), na.rm = TRUE) / multiple) * multiple
  
  
  
  # Draw figure
  correll_new <- 
    ggplot(complete,
           aes(
             x = log_odds,
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
        x = lor,
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
      x = expression(paste("Effect size (", italic("log odds ratio"), ")", sep = ""))
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
      legend.position = "bottom",
      legend.background = element_rect( 
        size = 0.5,
        linetype = "solid",
        color= "black")
    )
}


loftus_plot_h2 <- plot_func_lor(
  meta         = loftus_h2_meta, 
  complete     = loftus_h2_plot,
  estimate     = estimates_loftus_2,
  study_colors = loftus_color_1, 
  titles       = c("Broken Glass, Smashed vs. Hit")
)

loftus_plot_h3 <- plot_func_lor(
  meta         = loftus_h3_meta, 
  complete     = loftus_h3_plot,
  estimate     = estimates_loftus_3,
  study_colors = loftus_color_1, 
  titles       = c("Broken Glass, Smashed vs. Control")
)