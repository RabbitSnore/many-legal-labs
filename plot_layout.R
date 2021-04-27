#######################################################################

# Main Figure Layout

#######################################################################

library(cowplot)
library(grid)

# Prepare component plots ---------------------------------------------

## Extract legend

legend <- get_legend(serota_plot_mean)

## Remove Legend from plot

serota_plot_mean <- serota_plot_mean + theme(legend.position = "none")

## Preparing titles

title_correll <- ggdraw() + 
  draw_label(
    "Correll et al (2002, Study 1)",
    fontface = 'bold',
    size = 18,
  ) +
  theme(
    plot.margin = margin(2, 0, 2, 0)
  )

title_darley <- ggdraw() + 
  draw_label(
    "Darley et al (2000, Study 2)",
    fontface = 'bold',
    size = 18
  ) +
  theme(
    plot.margin = margin(2, 0, 2, 0)
  )

title_loftus <- ggdraw() + 
  draw_label(
    "Loftus & Palmer (1974, Study 2)",
    fontface = 'bold',
    size = 18
  ) +
  theme(
    plot.margin = margin(2, 0, 2, 0)
  )

title_serota <- ggdraw() + 
  draw_label(
    "Serota et al (2010, Study 3)",
    fontface = 'bold',
    size = 18
  ) +
  theme(
    plot.margin = margin(2, 0, 2, 0)
  )

## Organize plots by columns

correll_plots_col <- plot_grid(title_correll,
                               correll_plot_h2,
                               correll_plot_h3,
                               correll_plot_h5,
                               ncol = 1,
                               rel_heights = c(0.3, 1, 1, 1))

darley_plots_col <- plot_grid(title_darley,
                              darley_plot_h1,
                              darley_plot_h2,
                              darley_plot_h3,
                              ncol = 1,
                              rel_heights = c(0.3, 1, 1, 1))

loftus_plots_col <- plot_grid(title_loftus,
                              loftus_plot_h1,
                              loftus_plot_h2,
                              loftus_plot_h3,
                              loftus_plot_h4_direct,
                              loftus_plot_h4_indirect,
                              ncol = 1,
                              rel_heights = c(0.3, 1, 1, 1, 1, 1))

serota_plots_col <- plot_grid(title_serota,
                              serota_plot_all,
                              serota_plot_us,
                              serota_plot_nonus,
                              serota_plot_mean,
                              legend,
                              ncol = 1,
                              rel_heights = c(0.3, 1, 1, 1, 1, 1))


# Full layout ---------------------------------------------------------

## Plots organized in columns. Columns aligned + Example plot WITH margins

step_1 <- plot_grid (correll_plots_col, darley_plots_col, ncol = 2)


### This step adds a margin to the sides of the example plot. 
step_1_2 <- plot_grid(NULL, example_plot, NULL, ncol = 3, rel_widths = c(0.1,1,0.1))

### Adds margins to top and bottom of example plot.
step_1_3 <- plot_grid(NULL, step_1_2, NULL, ncol = 1, rel_heights = c(0.1,1,0.1))

step_2 <- plot_grid (step_1, step_1_3, ncol = 1, rel_heights = c(1, 0.6))

step_3 <- plot_grid (loftus_plots_col, serota_plots_col, ncol = 2)

### Final grid with example plot

main_grid_1 <- plot_grid(step_2, step_3, ncol = 2)

## Plots organized in columns. Columns aligned + Example plot WITHOUT margins

step_01 <- plot_grid (correll_plots_col, darley_plots_col, ncol = 2)

step_02 <- plot_grid (step_01, example_plot, ncol = 1, rel_heights = c(1, 0.6))

step_03 <- plot_grid (loftus_plots_col, serota_plots_col, ncol = 2)

### Final grid with example plot - no margins.

main_grid_2 <- plot_grid(step_02, step_03, ncol = 2)


## Plots organized in columns, no example plot. 

test_2 <- plot_grid (correll_plots_col, darley_plots_col, loftus_plots_col, serota_plots_col, ncol = 4)


# Export completed plot -----------------------------------------------

save_plot("./reports/main_figure.png", main_grid_1, base_height = 13, base_width = 18)
save_plot("./reports/main_figure.tiff", main_grid_1, base_height = 13, base_width = 18)
save_plot("./reports/main_figure.eps" , main_grid_1, base_height = 13, base_width = 18)
save_plot("./reports/main_figure.svg" , main_grid_1, base_height = 13, base_width = 18)


