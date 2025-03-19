PrepareFigure2 <- function(plots) {
  cowplot::plot_grid(
    plots$no_smoke$plot_dk + 
      ggplot2::scale_y_continuous(
        n.breaks = 5
      ) +
      ggplot2::ylab("Difference in z-score\n compared with no exposure") +
      ggplot2::theme(
        plot.margin = ggplot2::margin(20, 0, 0, 0),
        plot.title = ggplot2::element_text(
          size = 18,
          face = "bold",
          family = "sans",
          color = "black",
          hjust = 0.5,
          margin = ggplot2::margin(15, 0, -15, 0)
        ),
        axis.title.y = ggplot2::element_text(
          margin = ggplot2::margin(0, 5, -20, 0),
          size = 11,
          face = "bold",
          family = "sans",
          color = "black"
        ),
        axis.title.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          size = 14,
          face="plain",
          family="sans",
          color="black",
          margin = ggplot2::margin(0, 5, 0, 0)
        ),
        axis.text.x = ggplot2::element_text(
          size = 14,
          face = "plain",
          family = "sans",
          color = "black",
          margin = ggplot2::margin(5, 0, 0, 0),
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      ),
    plots$no_smoke$plot_mat + 
      ggplot2::scale_y_continuous(
        n.breaks = 5
      ) +
      ggplot2::ylab("Difference in z-score\n compared with no exposure") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 18,
          face = "bold",
          family = "sans",
          color = "black",
          hjust = 0.5,
          margin = ggplot2::margin(15, 0, -15, 0)
        ),
        axis.title.y = 
          ggplot2::element_text(
            margin = ggplot2::margin(0, 5, 0, 0),
            size = 11,
            face = "bold",
            family = "sans",
            color = "black"
          ),
        axis.title.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          size = 14,
          face="plain",
          family="sans",
          color="black",
          margin = ggplot2::margin(0, 5, 0, 0)
        ),
        axis.text.x = ggplot2::element_text(
          size = 14,
          face = "plain",
          family = "sans",
          color = "black",
          margin = ggplot2::margin(5, 0, 0, 0),
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      ),
    plots$no_smoke$plot_bpp + 
      ggplot2::scale_y_continuous(
        n.breaks = 5
      ) +
      ggplot2::ylab("Difference in IQ\n compared with no exposure") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 18,
          face = "bold",
          family = "sans",
          color = "black",
          hjust = 0.5,
          margin = ggplot2::margin(15, 0, -15, 0)
        ),
        axis.title.y = ggplot2::element_text(
          margin = ggplot2::margin(0, 5, 0, 0),
          size = 11,
          face = "bold",
          family = "sans",
          color = "black"
        ),
        axis.title.x = ggplot2::element_text(
          margin = ggplot2::margin(15, 0, 5, 0),
          size = 13,
          face = "bold",
          family = "sans",
          color = "black"
        ),
        axis.text.y = ggplot2::element_text(
          size = 14,
          face="plain",
          family="sans",
          color="black",
          margin = ggplot2::margin(0, 5, 0, 0)
        ),
        axis.text.x = ggplot2::element_text(
          size = 14,
          face = "plain",
          family = "sans",
          color = "black",
          margin = ggplot2::margin(5, 0, 0, 0),
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      ),
    ncol = 1,
    align = "v",
    rel_heights = c(0.35, 0.325, 0.325)
  ) +
    ggplot2::theme(
      plot.margin = grid::unit(c(-10, 0, 0, 0), "mm")
    )
}