
## Serota et al (2010, Study 3), average number of lies

serota_mean <- serota_desc
serota_mean$hypothesis <- "mean"


## Estimate data function mean

estimate_data_mean <- function(meta_analysis, org_mean, org_ci_lower, org_ci_upper) {
  # Set up original and meta-analytic estimates
  estimates <- data.frame(
    ID       = c("Replication", "Original"),
    mean     = c(meta_analysis$beta[[1]], org_mean),
    var      = c(NA, NA),
    ci_lower = c(meta_analysis$ci.lb, org_ci_lower),
    ci_upper = c(meta_analysis$ci.ub, org_ci_upper)
  )
  return(estimates)
}


## Estimate data Serota, average number of lies

estimates_serota_mean <- estimate_data_mean(
  meta_analysis    = serota_mean_meta,
  org_mean             = serota_org$mean[serota_org$hypothesis == "mean_student"],
  org_ci_lower     = serota_org$ci_lower[serota_org$hypothesis == "mean_student"],
  org_ci_upper     = serota_org$ci_upper[serota_org$hypothesis == "mean_student"])

estimates_serota_mean$ID <- factor(estimates_serota_mean$ID, levels = c("Replication", "Original"))

estimates_serota_mean$hypothesis <- c(rep("mean", 2))

estimates_serota_mean$hypothesis <- factor(estimates_serota_mean$hypothesis, levels = rev(c("mean")))

serota_mean$hypothesis <- factor(serota_mean$hypothesis, levels = rev(c("mean")))


## Plot function mean

plot_func_mean <- function(meta, complete, estimate, study_colors, titles, boundary_pad = .25) {
  
  # Set up plot boundaries

if(sum(estimate$mean > 4, estimate$ci_lower > 4, estimate$ci_upper > 4) > 1) {
  multiple = 1
} else if (sum(estimate$mean < 0.5, estimate$ci_lower < 0.5, estimate$ci_upper < 0.5) > 1) {
  multiple = 0.1
} else {
  multiple = 0.5
}
  
  effect_max <- round(max(c(complete$mean, estimate$ci_upper), na.rm = TRUE) / multiple) * multiple
  
  effect_min <- round(min(c(complete$mean, estimate$ci_lower), na.rm = TRUE) / multiple) * multiple
  
  
  
  # Draw figure
  correll_new <- 
    ggplot(complete,
           aes(
             x = mean,
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
        x = mean,
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
      x = expression(paste("Mean"))
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
      legend.text = element_text(size = 18),
      legend.key.size = unit(1, 'cm'),
      legend.background = element_rect( 
        size = 1,
        linetype = "solid",
        color = "black")
    )
}


# Serota average lies plot

serota_plot_mean <- plot_func_mean(
  meta         = serota_mean_meta, 
  complete     = serota_mean,
  estimate     = estimates_serota_mean,
  study_colors = serota_color_1, 
  titles       = c("Average Number of Lies")
)

serota_plot_mean + theme()
