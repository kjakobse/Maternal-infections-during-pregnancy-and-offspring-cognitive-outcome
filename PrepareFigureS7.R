PrepareFigureS7 <- function(
    plot, 
    xlab = TRUE, 
    xlim = NULL, 
    ylim = NULL, 
    y_breaks = NULL,
    point_size = NULL
) {
  plot$layers[[2]]$aes_params$size <- 0.75
  if (!is.null(point_size)) {
    plot$layers[[5]]$aes_params$size <- point_size
  }
  out <- plot +
    scale_y_continuous(
      n.breaks = 5
    )
  out <- out +
    ylab("OR of 5% lowest z-scores\n compared with no exposure")
  if (!is.null(xlim)) {
    out <- out +
      coord_cartesian(
        xlim = xlim
      )
  }
  if (!is.null(ylim)) {
    out <- out +
      coord_cartesian(
        ylim = ylim
      )
  }
  if (!is.null(y_breaks)) {
    out <- out +
      scale_y_continuous(
        breaks = y_breaks,
        trans = "log"
      )
  }
  out <- out +
    theme(
      plot.title = element_text(
        size = 14,
        face = "bold",
        family = "sans",
        color = "black",
        hjust = 0.5,
        margin = margin(15, 0, -15, 0)
      ),
      axis.title.y = element_text(
        margin = margin(0, 5, 0, 3),
        size = 7,
        face = "bold",
        family = "sans",
        color = "black"
      ),
      axis.title.x = if (xlab) {
        element_text(
          margin = margin(10, 0, 5, 0),
          size = 8,
          face = "bold",
          family = "sans",
          color = "black"
        )
      } else {
        element_blank()
      },
      axis.text.y = element_text(
        size = 10,
        face="plain",
        family="sans",
        color="black",
        margin = margin(0, 5, 0, 0)
      ),
      axis.text.x = element_text(
        size = 10,
        face = "plain",
        family = "sans",
        color = "black",
        margin = margin(5, 0, 0, 0),
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    )
  return(out)
}