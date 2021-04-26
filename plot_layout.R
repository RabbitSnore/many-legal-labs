library(cowplot)
library(grid)


## Extract legend
legend <- cowplot::get_legend(serota_plot_mean)

## Remove Legend from plot

serota_plot_mean <- serota_plot_mean + theme(legend.position='none')

## Plots organized in 1 column per study. Columns aligned, uneven number of plots in each column.

test_1 <- plot_grid(correll_plot_h2,
          correll_plot_h3,
          correll_plot_h5,
          NULL,
          NULL,
          darley_plot_h1,
          darley_plot_h2,
          darley_plot_h3,
          NULL,
          NULL,
          loftus_plot_h1,
          loftus_plot_h2,
          loftus_plot_h3,
          loftus_plot_h4_direct,
          loftus_plot_h4_indirect,
          serota_plot_all,
          serota_plot_us,
          serota_plot_nonus,
          serota_plot_mean,
          legend,
          nrow = 5,
          ncol = 4,
          byrow = FALSE)

## Organize plots by columns

correll_plots_col <- plot_grid(correll_plot_h2,
                          correll_plot_h3,
                          correll_plot_h5,
                          ncol = 1)

darley_plots_col <- plot_grid(darley_plot_h1,
                          darley_plot_h2,
                          darley_plot_h3,
                          ncol = 1)

loftus_plots_col <- plot_grid(loftus_plot_h1,
                         loftus_plot_h2,
                         loftus_plot_h3,
                         loftus_plot_h4_direct,
                         loftus_plot_h4_indirect,
                         ncol = 1)

serota_plots_col <- plot_grid(serota_plot_all,
                          serota_plot_us,
                          serota_plot_nonus,
                          serota_plot_mean,
                          legend,
                          ncol = 1)


## Plots organized in columns. Columns not aligned. 

test_2 <- plot_grid (correll_plots_col, darley_plots_col, loftus_plots_col, serota_plots_col, ncol = 4)

## Plots organized in rows. Aligned. Uneven amount of plots in rows. 
test_3 <- plot_grid(NULL,
          correll_plot_h2,
          correll_plot_h3,
          correll_plot_h5,
          NULL,
          NULL,
          darley_plot_h1,
          darley_plot_h2,
          darley_plot_h3,
          NULL,
          loftus_plot_h1,
          loftus_plot_h2,
          loftus_plot_h3,
          loftus_plot_h4_direct,
          loftus_plot_h4_indirect,
          serota_plot_all,
          serota_plot_us,
          serota_plot_nonus,
          serota_plot_mean,
          legend,
          nrow = 4,
          ncol = 5)

## Organize plots by rows

correll_plots_row <- plot_grid(correll_plot_h2,
                               correll_plot_h3,
                               correll_plot_h5,
                               nrow = 1)

darley_plots_row <- plot_grid(darley_plot_h1,
                              darley_plot_h2,
                              darley_plot_h3,
                              nrow = 1)

loftus_plots_row <- plot_grid(loftus_plot_h1,
                              loftus_plot_h2,
                              loftus_plot_h3,
                              loftus_plot_h4_direct,
                              loftus_plot_h4_indirect,
                              nrow = 1)

serota_plots_row <- plot_grid(serota_plot_all,
                              serota_plot_us,
                              serota_plot_nonus,
                              serota_plot_mean,
                              legend,
                              nrow = 1)


## Plots organized in rows. Not aligned. Uneven amount of plots in rows.
test_4 <- plot_grid (correll_plots_row, darley_plots_row, loftus_plots_row, serota_plots_row, nrow = 4)
