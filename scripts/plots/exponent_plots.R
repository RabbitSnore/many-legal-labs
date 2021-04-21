serota_h1_all <- serota_h1_k 
serota_h1_all$hypothesis <- "k"

serota_h1_us <- filter(serota_h1_k, usa == "US")
serota_h1_us$hypothesis <- "k"

serota_h1_nonus <- filter(serota_h1_k, usa == "Non-US")
serota_h1_nonus$hypothesis <- "k"

estimate_data_k <- function(meta_analysis, org_k, index, org_ci_lower, org_ci_upper) {
  # Set up original and meta-analytic estimates
  estimates <- data.frame(
    ID       = c("Replication", "Original"),
    k        = c(meta_analysis$beta[[index]], org_k),
    var      = c(NA, NA),
    ci_lower = c(meta_analysis$ci.lb[[index]], org_ci_lower),
    ci_upper = c(meta_analysis$ci.ub[[index]], org_ci_upper)
  )
  return(estimates)
}

estimates_serota_all <- estimate_data_k(
  meta_analysis    = serota_h1_meta,
  org_k            = serota_org$k[serota_org$hypothesis == "h1_student"],
  index = 1,
  org_ci_lower     = serota_org$ci_lower[serota_org$hypothesis == "h1_student"], 
  org_ci_upper     = serota_org$ci_upper[serota_org$hypothesis == "h1_student"])

estimates_serota_all$ID <- factor(estimates_serota_all$ID, levels = c("Replication", "Original"))
estimates_serota_all$hypothesis <- c(rep("k", 2))
estimates_serota_all$hypothesis <- factor(estimates_serota_all$hypothesis, levels = rev(c("k")))

serota_h1_all$hypothesis <- factor(serota_h1_all$hypothesis, levels = rev(c("k")))

##US

estimates_serota_us <- estimate_data_k(
  meta_analysis    = serota_usa_meta,
  org_k            = serota_org$k[serota_org$hypothesis == "h1_student"],
  org_ci_lower     = serota_org$ci_lower[serota_org$hypothesis == "h1_student"], 
  org_ci_upper     = serota_org$ci_upper[serota_org$hypothesis == "h1_student"],
  index = 2)

estimates_serota_us$ID <- factor(estimates_serota_us$ID, levels = c("Replication", "Original"))
estimates_serota_us$hypothesis <- c(rep("k", 2))
estimates_serota_us$hypothesis <- factor(estimates_serota_us$hypothesis, levels = rev(c("k")))

serota_h1_us$hypothesis <- factor(serota_h1_us$hypothesis, levels = rev(c("k")))

## Non-US

estimates_serota_nonus <- estimate_data_k(
  meta_analysis    = serota_usa_meta,
  org_k            = serota_org$k[serota_org$hypothesis == "h1_student"],
  org_ci_lower     = serota_org$ci_lower[serota_org$hypothesis == "h1_student"], 
  org_ci_upper     = serota_org$ci_upper[serota_org$hypothesis == "h1_student"],
  index = 1)

estimates_serota_nonus$ID <- factor(estimates_serota_nonus$ID, levels = c("Replication", "Original"))
estimates_serota_nonus$hypothesis <- c(rep("k", 2))
estimates_serota_nonus$hypothesis <- factor(estimates_serota_nonus$hypothesis, levels = rev(c("k")))

serota_h1_nonus$hypothesis <- factor(serota_h1_nonus$hypothesis, levels = rev(c("k")))

## Plot function

plot_func_k <- function(meta, complete, estimate, study_colors, titles, boundary_pad = .25, multiple = .50) {
  
  # Set up plot boundaries
  effect_max <- round(max(c(complete$k, estimate$ci_upper), na.rm = TRUE) / multiple) * multiple
  effect_min <- round(min(c(complete$k, estimate$ci_lower), na.rm = TRUE) / multiple) * multiple
  
  
  
  # Draw figure
  k_new <- 
    ggplot(complete,
           aes(
             x = k,
             y = hypothesis,
             color = hypothesis
           )
    ) +
    #geom_vline(
      #xintercept = 0,
      #linetype = "longdash"
    #) +
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
        x = k,
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
      y = "",
      x = expression(paste("Effect size (", italic("exponent"), ")", sep = ""))
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
      legend.position = "bottom",
      legend.background = element_rect( 
        size = 0.5,
        linetype = "solid",
        color= "black")
    )
}

serota_plot_all <- plot_func_k(
  meta         = serota_h1_meta, 
  complete     = serota_h1_all,
  estimate     = estimates_serota_all,
  study_colors = serota_color_1, 
  titles       = c("H1")
)

serota_plot_us <- plot_func_k(
  meta         = serota_usa_meta, 
  complete     = serota_h1_us,
  estimate     = estimates_serota_us,
  study_colors = serota_color_1, 
  titles       = c("US")
)

serota_plot_nonus <- plot_func_k(
  meta         = serota_usa_meta, 
  complete     = serota_h1_us,
  estimate     = estimates_serota_nonus,
  study_colors = serota_color_1, 
  titles       = c("Non-US")
)
