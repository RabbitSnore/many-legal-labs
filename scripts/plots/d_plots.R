library(ggbeeswarm)

## Correll et al (2002, Study 1), hypothesis 2,3 & 5

correll_h2_plot<- correll_h2_smd
correll_h2_plot$hypothesis <- "h2"
correll_h2_plot$experiment <- "Correll et al (2002, Study 1)"

correll_h3_plot<- correll_h3_smd
correll_h3_plot$hypothesis <- "h3"
correll_h3_plot$experiment <- "Correll et al (2002, Study 1)"

correll_h5_plot<- correll_h5_smd
correll_h5_plot$hypothesis <- "h5"
correll_h5_plot$experiment <- "Correll et al (2002, Study 1)"

## Darley et al (2000, Study 2), hypothesis 1-3

darley_h1_plot<- darley_h1_smd
darley_h1_plot$hypothesis <- "h1"
darley_h1_plot$experiment <- "Darley et al (2000, Study 2)"

darley_h2_plot<- darley_h2_smd
darley_h2_plot$hypothesis <- "h2"
darley_h2_plot$experiment <- "Darley et al (2000, Study 2)"

darley_h3_plot<- darley_h3_smd
darley_h3_plot$hypothesis <- "h3"
darley_h3_plot$experiment <- "Darley et al (2000, Study 2)"

## Loftus & Palmer (1974, Study 2), hypothesis 1

loftus_h1_plot<- loftus_h1_smd
loftus_h1_plot$hypothesis <- "h1"
loftus_h1_plot$experiment <- "Loftus & Palmer (1974, Study 2)"


##Estimate data function d

  estimate_data <- function(meta_analysis, org_d, org_ci_lower, org_ci_upper) {
    # Set up original and meta-analytic estimates
    estimates <- data.frame(
      ID       = c("Replication", "Original"),
      d        = c(meta_analysis$beta[[1]], org_d),
      var      = c(NA, NA),
      ci_lower = c(meta_analysis$ci.lb, org_ci_lower),
      ci_upper = c(meta_analysis$ci.ub, org_ci_upper)
    )
    return(estimates)
  }

  
##Estimate data Correll, hypothesis 2
  
estimates_correll_2 <- estimate_data(
  meta_analysis    = correll_h2_meta,
  org_d            = correll_org$d[correll_org$hypothesis == "h2"],
  org_ci_lower     = correll_org$ci_lower[correll_org$hypothesis == "h2"], 
  org_ci_upper     = correll_org$ci_upper[correll_org$hypothesis == "h2"])


estimates_correll_2$ID <- factor(estimates_correll_2$ID, levels = c("Replication", "Original"))

estimates_correll_2$hypothesis <- c(rep("h2", 2))

estimates_correll_2$hypothesis <- factor(estimates_correll_2$hypothesis, levels = rev(c("h2")))

correll_h2_plot$hypothesis <- factor(correll_h2_plot$hypothesis, levels = rev(c("h2")))


##Estimate data Correll, hypothesis 3

estimates_correll_3 <- estimate_data(
  meta_analysis    = correll_h3_meta,
  org_d            = correll_org$d[correll_org$hypothesis == "h3"],
  org_ci_lower     = correll_org$ci_lower[correll_org$hypothesis == "h3"], 
  org_ci_upper     = correll_org$ci_upper[correll_org$hypothesis == "h3"])


estimates_correll_3$ID <- factor(estimates_correll_3$ID, levels = c("Replication", "Original"))

estimates_correll_3$hypothesis <- c(rep("h3", 2))

estimates_correll_3$hypothesis <- factor(estimates_correll_3$hypothesis, levels = rev(c("h3")))

correll_h3_plot$hypothesis <- factor(correll_h3_plot$hypothesis, levels = rev(c("h3")))


##Estimate data Correll, hypothesis 5

estimates_correll_5 <- estimate_data(
  meta_analysis    = correll_h5_meta,
  org_d            = correll_org$d[correll_org$hypothesis == "h5"],
  org_ci_lower     = correll_org$ci_lower[correll_org$hypothesis == "h5"], 
  org_ci_upper     = correll_org$ci_upper[correll_org$hypothesis == "h5"])


estimates_correll_5$ID <- factor(estimates_correll_5$ID, levels = c("Replication", "Original"))

estimates_correll_5$hypothesis <- c(rep("h5", 2))

estimates_correll_5$hypothesis <- factor(estimates_correll_5$hypothesis, levels = rev(c("h5")))

correll_h5_plot$hypothesis <- factor(correll_h5_plot$hypothesis, levels = rev(c("h5")))


##Estimate data Darley, hypothesis 1

estimates_darley_1 <- estimate_data(
  meta_analysis    = darley_h1_meta,
  org_d            = darley_org$d[darley_org$hypothesis == "h1"],
  org_ci_lower     = darley_org$ci_lower[darley_org$hypothesis == "h1"], 
  org_ci_upper     = darley_org$ci_upper[darley_org$hypothesis == "h1"])

estimates_darley_1$ID <- factor(estimates_darley_1$ID, levels = c("Replication", "Original"))

estimates_darley_1$hypothesis <- c(rep("h1", 2))

estimates_darley_1$hypothesis <- factor(estimates_darley_1$hypothesis, levels = rev(c("h1")))

darley_h1_plot$hypothesis <- factor(darley_h1_plot$hypothesis, levels = rev(c("h1")))


##Estimate data Darley, hypothesis 2

estimates_darley_2 <- estimate_data(
  meta_analysis    = darley_h2_meta,
  org_d            = darley_org$d[darley_org$hypothesis == "h2"],
  org_ci_lower     = darley_org$ci_lower[darley_org$hypothesis == "h2"], 
  org_ci_upper     = darley_org$ci_upper[darley_org$hypothesis == "h2"])

estimates_darley_2$ID <- factor(estimates_darley_2$ID, levels = c("Replication", "Original"))

estimates_darley_2$hypothesis <- c(rep("h2", 2))

estimates_darley_2$hypothesis <- factor(estimates_darley_2$hypothesis, levels = rev(c("h2")))

darley_h2_plot$hypothesis <- factor(darley_h2_plot$hypothesis, levels = rev(c("h2")))


##Estimate data Darley, hypothesis 3

estimates_darley_3 <- estimate_data(
  meta_analysis    = darley_h3_meta,
  org_d            = darley_org$d[darley_org$hypothesis == "h3"],
  org_ci_lower     = darley_org$ci_lower[darley_org$hypothesis == "h3"], 
  org_ci_upper     = darley_org$ci_upper[darley_org$hypothesis == "h3"])

estimates_darley_3$ID <- factor(estimates_darley_3$ID, levels = c("Replication", "Original"))

estimates_darley_3$hypothesis <- c(rep("h3", 2))

estimates_darley_3$hypothesis <- factor(estimates_darley_3$hypothesis, levels = rev(c("h3")))

darley_h3_plot$hypothesis <- factor(darley_h3_plot$hypothesis, levels = rev(c("h3")))


## Estimate data Loftus, hypothesis 1

estimates_loftus_1 <- estimate_data(
  meta_analysis    = loftus_h1_meta,
  org_d            = loftus_org$d[loftus_org$hypothesis == "h1"],
  org_ci_lower     = loftus_org$ci_lower[loftus_org$hypothesis == "h1"], 
  org_ci_upper     = loftus_org$ci_upper[loftus_org$hypothesis == "h1"])

estimates_loftus_1$ID <- factor(estimates_loftus_1$ID, levels = c("Replication", "Original"))

estimates_loftus_1$hypothesis <- c(rep("h1", 2))

estimates_loftus_1$hypothesis <- factor(estimates_loftus_1$hypothesis, levels = rev(c("h1")))

loftus_h1_plot$hypothesis <- factor(loftus_h1_plot$hypothesis, levels = rev(c("h1")))


## Plot function d

plot_func_d <- function(meta, complete, estimate, study_colors, titles, boundary_pad = .25) {

# Set up plot boundaries
  
  if (sum(estimate$d > 4, estimate$ci_lower > 4, estimate$ci_upper > 4) >1) {
    mutiple = 1
  } else if (sum(estimate$d < 0.5, estimate$ci_lower < 0.5, estimate$ci_upper < 0.5) == 6) {
    multiple = 0.10
  } else {
    multiple = 0.50
  }  
  
  
effect_max <- round(max(c(complete$d, estimate$ci_upper), na.rm = TRUE) / multiple) * multiple

effect_min <- round(min(c(complete$d, estimate$ci_lower), na.rm = TRUE) / multiple) * multiple



# Draw figure
correll_new <- 
  ggplot(complete,
         aes(
           x = d,
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
      x = d,
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
    x = expression(paste("Effect size (", italic("d"), ")", sep = ""))
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
    legend.position = "none"#,
    #legend.background = element_rect( 
      #size = 0.5,
      #linetype = "solid",
      #color= "black")
  )
}


# Correll hypothesis 2 plot

correll_plot_h2 <- plot_func_d(
  meta = correll_h2_meta, 
  complete     = correll_h2_plot,
  estimate     = estimates_correll_2,
  study_colors    = correll_color_1, 
  titles          = c("Race-Biased Shooting")
)


# Correll hypothesis 3 plot

correll_plot_h3 <- plot_func_d(
  meta = correll_h3_meta, 
  complete     = correll_h3_plot,
  estimate     = estimates_correll_3,
  study_colors    = correll_color_1, 
  titles          = c("Race-Biased Holding Fire")
)


# Correll hypothesis 5 plot

correll_plot_h5 <- plot_func_d(
  meta = correll_h5_meta, 
  complete     = correll_h5_plot,
  estimate     = estimates_correll_5,
  study_colors    = correll_color_1, 
  titles          = c("Race-Biased False Alarms")
)


# Darley hypothesis 1 plot

darley_plot_h1 <- plot_func_d(
  meta         = darley_h1_meta, 
  complete     = darley_h1_plot,
  estimate     = estimates_darley_1,
  study_colors = darley_color_1, 
  titles       = c("Punishment, \n Jealousy vs. Inoperable Tumor")
)


# Darley hypothesis 2 plot

darley_plot_h2 <- plot_func_d(
  meta         = darley_h2_meta, 
  complete     = darley_h2_plot,
  estimate     = estimates_darley_2,
  study_colors = darley_color_1, 
  titles       = c("Punishment, \n Jealousy vs. Operable Tumor")
)


# Darley hypothesis 3 plot

darley_plot_h3 <- plot_func_d(
  meta         = darley_h3_meta, 
  complete     = darley_h3_plot,
  estimate     = estimates_darley_3,
  study_colors = darley_color_1, 
  titles       = c("Punishment, \n Inoperable vs. Operable Tumor")
)

# Example Plot

# Darley hypothesis 3 plot (to use with simulated data)

example_plot <- plot_func_d(
  meta         = darley_h3_meta, 
  complete     = darley_h3_plot,
  estimate     = estimates_darley_3,
  study_colors = 	"#696969", 
  titles       = c("Hypothesis")
)

## Add annotations to the example plot

example_plot <-
example_plot +
  annotate(
    geom      = "curve",
    x         = .50, 
    y         = .75, 
    xend      = darley_h3_meta$beta[[1]], 
    yend      = .92, 
    curvature = -.3, 
    arrow     = arrow(length = unit(2, "mm")),
    size      = 1
  ) + 
  annotate(
    geom      = "text",
    x         = .50, 
    y         = .75, 
    label     = "Replication point estimate",
    hjust     = "left"
  )



# Loftus hypothesis 1 plot

loftus_plot_h1 <- plot_func_d(
  meta         = loftus_h1_meta, 
  complete     = loftus_h1_plot,
  estimate     = estimates_loftus_1,
  study_colors = loftus_color_1, 
  titles       = c("Speed Estimates, \n Smashed vs. Hit")
)