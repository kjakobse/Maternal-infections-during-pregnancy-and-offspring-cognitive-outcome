PrepareFigureS2 <- function(plot, grade = TRUE, xlab = TRUE) {
  plot$layers[[2]]$aes_params$size <- 0.75
  out <- plot +
    ggplot2::scale_y_continuous(
      n.breaks = 5
    )
  
    if(grade) {
      out <- out +
        ggplot2::ylab("Difference in z-score\n compared with no exposure")
    } else {
      out <- out +
        ggplot2::ylab("Difference in IQ\n compared with no exposure")
    }
  out <- out +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 14,
        face = "bold",
        family = "sans",
        color = "black",
        hjust = 0.5,
        margin = ggplot2::margin(15, 0, -15, 0)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(0, 5, 0, 3),
        size = 7,
        face = "bold",
        family = "sans",
        color = "black"
      ),
      axis.title.x = if (xlab) {
        ggplot2::element_text(
          margin = ggplot2::margin(10, 0, 5, 0),
          size = 8,
          face = "bold",
          family = "sans",
          color = "black"
        )
      } else {
        ggplot2::element_blank()
      },
      axis.text.y = ggplot2::element_text(
        size = 10,
        face="plain",
        family="sans",
        color="black",
        margin = ggplot2::margin(0, 5, 0, 0)
      ),
      axis.text.x = ggplot2::element_text(
        size = 10,
        face = "plain",
        family = "sans",
        color = "black",
        margin = ggplot2::margin(5, 0, 0, 0),
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    )
  return(out)
}