AdjustFigure1 <- function(plot, cb) {
  # functions to desaturate colors
  desat <- function(cols, sat = 0.5) {
    X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
    hsv(X[1,], X[2,], X[3,])
  }
  # modify line height
  for (i in 2:27) {
    plot$heights[[i]] <- list(list(8, NULL, 7L)) |> 
      structure(class = c("unit", "unit_v2"))
  }
  purrr::map_dbl(
    1:189,
    \(i) {
      plot$grobs[[i]]$gp["lineheight"] <<- 1.5
    }
  )
  # modify colors
  purrr::map_dbl(
    365:494,
    \(i) {
      if (i %in% 365:387) {
        if (cb) {
          plot$grobs[[i]]$gp[c("col", "fill", "bg", "alpha")] <<- 
            c(desat("#882255", 0.5), desat("#882255", 0.5), "white", 1)
        } else {
          plot$grobs[[i]]$gp[c("col", "fill", "bg", "alpha")] <<- 
            c(desat("red", 0.5), desat("red", 0.5), "white", 1)
        }
      } else if (i %in% 388:410) {
        if (cb) {
          plot$grobs[[i]]$gp[c("col", "fill", "bg", "alpha")] <<- 
            c(desat("#5ea5e8", 0.5), desat("#5ea5e8", 0.5), "white", 1)
        } else {
          plot$grobs[[i]]$gp[c("col", "fill", "bg", "alpha")] <<- 
            c(desat("dodgerblue2", 0.5), desat("dodgerblue3", 0.5), "white", 1)
        }
      } else if (i %in% 411:429) {
        if (cb) {
          plot$grobs[[i]]$gp[c("col", "fill", "bg", "alpha")] <<- 
            c(desat("#117733", 0.5), desat("#117733", 0.5), "white", 1)
        } else {
          plot$grobs[[i]]$gp[c("col", "fill", "bg", "alpha")] <<- 
            c(desat("springgreen2", 0.5), desat("springgreen3", 0.5), "white", 1)
        }
      } else if (i %in% 430:452) {
        if (cb) {
        plot$grobs[[i]]$gp[c("col", "fill")] <<- 
          c("#882255", "#882255") 
        } else {
          plot$grobs[[i]]$gp[c("col", "fill")] <<- 
            c("red", "red2") 
        }
      } else if (i %in% 453:475) {
        if (cb) {
        plot$grobs[[i]]$gp[c("col", "fill")] <<- 
          c("#5ea5e8", "#5ea5e8")
        } else {
          plot$grobs[[i]]$gp[c("col", "fill")] <<- 
            c("dodgerblue2", "dodgerblue4") 
        }
      } else {
        if (cb) {
        plot$grobs[[i]]$gp[c("col", "fill")] <<- 
          c("#117733", "#117733")
        } else {
          plot$grobs[[i]]$gp[c("col", "fill")] <<- 
            c("springgreen2", "springgreen4")
        }
      }
      return(invisible(1))
    }
  )
  
  # adjust location and size of column titles
  plot$grobs[[3]]$hjust <- -0.72 
  plot$grobs[[5]]$hjust <- -0.45
  plot$grobs[[7]]$hjust <- -0.55
  plot$grobs[[3]]$gp$fontsize <- 13
  plot$grobs[[5]]$gp$fontsize <- 13
  plot$grobs[[7]]$gp$fontsize <- 13
  
  # adjust x-axis label and arrows
  for (i in c(496, 500, 504)) {
    plot$grobs[[i]]$children$xlab$vjust <- 2.1
    plot$grobs[[i]]$children$xlab$gp$font <- c("bold" = 2)
  }
  for (i in c(498, 502, 506)) {
    plot$grobs[[i]]$children[["arrow.text.left"]]$vjust <- -3.2
    plot$grobs[[i]]$children[["arrow.text.right"]]$vjust <- -3.2
    plot$grobs[[i]]$children[["arrow.text.left"]]$font <- c("italic" = 3)
    plot$grobs[[i]]$children[["arrow.text.right"]]$font <- c("italic" = 3)
    plot$grobs[[i]]$children[["arrow.left"]][c("y0", "y1")] <- 
      list(grid::unit(.06, "grobheight", plot), grid::unit(.06, "grobheight", plot))
    plot$grobs[[i]]$children[["arrow.right"]][c("y0", "y1")] <- 
      list(grid::unit(.06, "grobheight", plot), grid::unit(.06, "grobheight", plot))
  }
  
  # adjust legend
  plot <- forestploter::add_grob(
    plot,
    row = 26,
    col = 1,
    part = "body",
    order = "top",
    gb_fn = grid::textGrob,
    label = "Adjustment",
    just = "left",
    x = grid::unit(0.05, "npc"),
    y = grid::unit(0.2, "npc"),
    gp = grid::gpar(fontsize = 11, fontfamily = "sans", fill = "black", font = c("bold" = 2))
  )
  plot <- forestploter::add_grob(
    plot,
    row = 27,
    col = 1,
    part = "body",
    order = "top",
    gb_fn = grid::grobTree,
    grid::linesGrob(
      x = grid::unit(c(0.05,0.15), "npc"),
      y = grid::unit(0.8, "npc"),
      gp = grid::gpar(fontsize = 11, fontfamily = "sans", lwd = 2, col = "#666666", fill = "black")
    ),
    grid::pointsGrob(
      x = grid::unit(0.1, "npc"),
      y = grid::unit(0.8, "npc"),
      pch = 23,
      size = grid::unit(0.8, "char"),
      gp = grid::gpar(fontsize = 11, fontfamily = "sans", lwd = 1, col = "#666666", fill = "white")
    ),
    grid::textGrob(
      x = grid::unit(0.2, "npc"),
      y = grid::unit(0.8, "npc"),
      just = "left",
      label = "Covariate-adjusted only",
      gp = grid::gpar(fontsize = 11, fontfamily = "sans", fill = "black", lineheight = 1)
    )
  )
  plot <- forestploter::add_grob(
    plot,
    row = 28,
    col = 1,
    part = "body",
    order = "top",
    gb_fn = grid::grobTree,
    grid::linesGrob(
      x = grid::unit(c(0.05,0.15), "npc"),
      y = grid::unit(0.8, "npc"),
      gp = grid::gpar(fontsize = 11, fontfamily = "sans", lwd = 2, col = "black", fill = "black")
    ),
    grid::pointsGrob(
      x = grid::unit(0.1, "npc"),
      y = grid::unit(0.8, "npc"),
      pch = 22,
      size = grid::unit(0.8, "char"),
      gp = grid::gpar(fontsize = 11, fontfamily = "sans", lwd = 1, col = "black", fill = "black")
    ),
    grid::textGrob(
      x = grid::unit(0.2, "npc"),
      y = grid::unit(0.8, "npc"),
      vjust = 0.8,
      hjust = 0,
      label = "Additionally adjusted for \nshared family-factors",
      gp = grid::gpar(fontsize = 11, fontfamily = "sans", fill = "black", lineheight = 1)
    )
  )
  return(plot)
}