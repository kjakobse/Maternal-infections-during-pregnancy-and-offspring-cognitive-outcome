PrepareFigureS6 <- function(plots) {
cowplot::plot_grid(
  plots$no_smoke$plot_dk + 
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          margin = ggplot2::margin(0, 5, 0, 0), 
          size = 14, 
          face = "bold", 
          family = "sans", 
          color = "black"
        ),
        plot.margin = ggplot2::margin(0, 10, 0, 5)
      ),
  plots$no_smoke$plot_mat + 
    ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          margin = ggplot2::margin(0, 5, 0, 0), 
          size = 14, 
          face = "bold", 
          family = "sans", 
          color = "black"
        ),
        plot.margin = ggplot2::margin(0, 5, 0, 10)
      ),
    ncol = 2,
    align = "h",
    rel_widths = c(1, 1)
  )
}
