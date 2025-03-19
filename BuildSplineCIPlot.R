# connect   lgl:  TRUE to draw a line between point estimates.
# time
# iq        lgl:  If true, intelligence plots have iq on y-axis.
# path      chr:  path where plots will be saved. If NULL, plots are not saved.
# return    lgl:  If TRUE, plots are returned by the function. Otherwise it
#                 silently returns NULL.

BuildSplineCIPlot <- function(
    plot_data_dk,
    plot_data_mat,
    plot_data_bpp,
    cat = c("dk", "mat", "bpp"),
    ns = c("dk", "mat", "bpp"),
    cat_grp = FALSE,
    cat_grp_var = NULL,
    connect = TRUE,
    time = c("days", "weeks"),
    iq = TRUE,
    xlab = "Gestational age (days)",
    xlab_dk = NULL,
    xlab_mat = NULL,
    xlab_bpp = NULL,
    ylab = "Difference in z-score compared with no exposure",
    ylab_dk = NULL,
    ylab_mat = NULL,
    ylab_bpp = NULL,
    ggtitle_dk = "Language",
    ggtitle_mat = "Mathematics",
    ggtitle_bpp = "Intelligence",
    xlim = c(0, 310),
    xlim_dk = NULL,
    xlim_mat = NULL,
    xlim_bpp = NULL,
    ylim = c(-0.4, 0.4),
    ylim_dk = NULL,
    ylim_mat = NULL,
    ylim_bpp = NULL,
    n_breaks_x = NULL,
    n_breaks_x_dk = NULL,
    n_breaks_x_mat = NULL,
    n_breaks_x_bpp = NULL,
    n_breaks_y = 6,
    n_breaks_y_dk = NULL,
    n_breaks_y_mat = NULL,
    n_breaks_y_bpp = NULL,
    y_trans = "identity",
    y_trans_dk = NULL,
    y_trans_mat = NULL,
    y_trans_bpp = NULL,
    hline_yintercept = 0,
    path = NULL,
    device = "png",
    return = TRUE
) {
  # check arguments ------------------------------------------------------------
  if (!is_logical(return)) {
    abort("return must be TRUE/FALSE.")
  }
  if (!is.null(path) && !is_character(path) && length(path) != 1) {
    stop(
      paste0(
        "path must be a character vector of length 1 containing the path ",
        "where the plots are to be saved, \n",
        "or NULL if plots should not be saved"
      )
    )
  }
  # check either path is given or return is true
  if (is.null(path) && !return) {
    warning("No path is provided and return is FALSE")
    return(invisible(NULL))
  }
  
  # determine plot limits ------------------------------------------------------
  if (is.null(xlim_dk)) xlim_dk <- xlim
  if (is.null(xlim_mat)) xlim_mat <- xlim
  if (is.null(xlim_bpp)) xlim_bpp <- xlim
  if (is.null(ylim_dk)) ylim_dk <- ylim
  if (is.null(ylim_mat)) ylim_mat <- ylim
  if (is.null(ylim_bpp)) ylim_bpp <- ylim
  # determine number of axis ticks ---------------------------------------------
  if (is.null(n_breaks_x_dk)) n_breaks_x_dk <- n_breaks_x
  if (is.null(n_breaks_x_mat)) n_breaks_x_mat <- n_breaks_x
  if (is.null(n_breaks_x_bpp)) n_breaks_x_bpp <- n_breaks_x
  if (is.null(n_breaks_y_dk)) n_breaks_y_dk <- n_breaks_y
  if (is.null(n_breaks_y_mat)) n_breaks_y_mat <- n_breaks_y
  if (is.null(n_breaks_y_bpp)) n_breaks_y_bpp <- n_breaks_y
  # determine plot axis titles -------------------------------------------------
  if (is.null(xlab_dk)) xlab_dk <- xlab
  if (is.null(xlab_mat)) xlab_mat <- xlab
  if (is.null(xlab_bpp)) xlab_bpp <- xlab
  if (is.null(ylab_dk)) ylab_dk <- ylab
  if (is.null(ylab_mat)) ylab_mat <- ylab
  if (is.null(ylab_bpp)) ylab_bpp <- ylab
  # determine y-axis transformation --------------------------------------------
  if (is.null(y_trans_dk)) y_trans_dk <- y_trans
  if (is.null(y_trans_mat)) y_trans_mat <- y_trans
  if (is.null(y_trans_bpp)) y_trans_bpp <- y_trans
  # remove cat or ns data if not required --------------------------------------
  if (!"dk" %in% ns) plot_data_dk$ns_data <- NULL
  if (!"mat" %in% ns) plot_data_dk$ns_data <- NULL
  if (!"bpp" %in% ns) plot_data_dk$ns_data <- NULL
  if (!"dk" %in% cat) plot_data_dk$cat_data <- NULL
  if (!"mat" %in% cat) plot_data_mat$cat_data <- NULL
  if (!"bpp" %in% cat) plot_data_bpp$cat_data <- NULL
  # modify data according to time ----------------------------------------------
  time <- time[1]
  if (time == "weeks") {
    if (!is.null(plot_data_dk$ns_data)) {
      plot_data_dk$ns_data$day <- plot_data_dk$ns_data$day / 7
    }
    if (!is.null(plot_data_mat$ns_data)) {
      plot_data_mat$ns_data$day <- plot_data_mat$ns_data$day / 7
    }
    if (!is.null(plot_data_bpp$ns_data)) {
      plot_data_bpp$ns_data$day <- plot_data_bpp$ns_data$day / 7
    }
    if (!is.null(plot_data_dk$cat_data)) {
      plot_data_dk$cat_data$day <- plot_data_dk$cat_data$day / 7
    }
    if (!is.null(plot_data_mat$cat_data)) {
      plot_data_mat$cat_data$day <- plot_data_mat$cat_data$day / 7
    }
    if (!is.null(plot_data_bpp$cat_data)) {
      plot_data_bpp$cat_data$day <- plot_data_bpp$cat_data$day / 7
    }
    xlab <- stringr::str_replace(xlab, "days", "weeks")
  }

  # Remove data with infinite CI -----------------------------------------------
  if (!is.null(plot_data_dk$ns_data)) {
  plot_data_dk$ns_data <- plot_data_dk$ns_data |>
    dplyr::filter(!is.infinite(conf.high))
  }
  if (!is.null(plot_data_dk$cat_data)) {
  plot_data_dk$cat_data <- plot_data_dk$cat_data |>
    dplyr::filter(!is.infinite(conf.high))
  }
  if (!is.null(plot_data_mat$ns_data)) {
  plot_data_mat$ns_data <- plot_data_mat$ns_data |>
    dplyr::filter(!is.infinite(conf.high))
  }
  if (!is.null(plot_data_mat$cat_data)) {
  plot_data_mat$cat_data <- plot_data_mat$cat_data |>
    dplyr::filter(!is.infinite(conf.high))
  }
  if (!is.null(plot_data_bpp$ns_data)) {
    plot_data_bpp$ns_data <- plot_data_bpp$ns_data |>
      dplyr::filter(!is.infinite(conf.high))
  }
  if (!is.null(plot_data_bpp$cat_data)) {
    plot_data_bpp$cat_data <- plot_data_bpp$cat_data |>
      dplyr::filter(!is.infinite(conf.high))
  }
  
  # Fonts ----------------------------------------------------------------------
  windowsFonts(
    Calibri=windowsFont("TT Calibri"),
    Helvetica=windowsFont("TT Helvetica")
  )
  # dk plot --------------------------------------------------------------------
  exp_dk <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = hline_yintercept, linetype = 5)
  if (!is.null(plot_data_dk$ns_data)) {
    exp_dk <- exp_dk +
      ggplot2::geom_line(
        ggplot2::aes(x = day, y = estimate),
        data = plot_data_dk$ns_data,
        color = "red",
        linewidth = 0.85
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high),
        data = plot_data_dk$ns_data,
        fill = "red",
        alpha = 0.2
      )
  }
  if (!is.null(plot_data_dk$cat_data)) {
    if (cat_grp) {
      exp_dk <- exp_dk +
        ggplot2::geom_errorbar(
          ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high, color = !!dplyr::sym(cat_grp_var)),
          position = ggplot2::position_dodge(width = 1),
          data = plot_data_dk$cat_data, 
          linewidth = 0.85, 
          width = 0, 
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = day, y = estimate, color = !!dplyr::sym(cat_grp_var)), 
          position = ggplot2::position_dodge(width = 1),
          data = plot_data_dk$cat_data, 
          size = 3.5
        )
    } else {
      exp_dk <- exp_dk +
        ggplot2::geom_errorbar(
          ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high), 
          data = plot_data_dk$cat_data,
          color="darkred", 
          linewidth = 0.85, 
          width = 0, 
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = day, y = estimate), 
          data = plot_data_dk$cat_data,
          color = "darkred", 
          size = 3.5
        )
    }
    if (connect) {
      exp_dk <- exp_dk +
        ggplot2::geom_line(
          ggplot2::aes(x = day, y = estimate), 
          data = plot_data_dk$cat_data,
          color = "darkred", 
          linewidth = 0.85
        )
    }
  }
  
  if (is.null(n_breaks_x_dk)) {
    exp_dk <- exp_dk +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = seq(1, 41, 2), 
        labels = c(
          "0-1","2-3","4-5","6-7","8-9","10-11","12-13","14-15",
          "16-17","18-19","20-21","22-23","24-25","26-27","28-29",
          "30-31","32-33","34-35", "36-37","38-39","\u226540"
        )
      ) } else {
        exp_dk <- exp_dk +
          ggplot2::scale_x_continuous(
            expand = c(0, 0),
            n.breaks = n_breaks_x_dk
          )
      }
  
  if (cat_grp) {
    exp_dk <- exp_dk +
      ggplot2::scale_color_manual(
        values = c("#660000", "#CC0000")
      )
  }
  exp_dk <- exp_dk +
    ggplot2::xlab(xlab_dk) +
    ggplot2::ylab(ylab_dk) +
    ggplot2::ggtitle(ggtitle_dk) +
    ggplot2::coord_cartesian(
      xlim = xlim_dk,
      ylim = ylim_dk
    ) +
    ggplot2::scale_y_continuous(
      n.breaks = n_breaks_y_dk,
      trans = y_trans_dk
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position.inside = c(0.3, 0.2),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = 20, 
        face = "plain", 
        family = "sans", 
        color = "black"
      ),
      legend.key.size = grid::unit(2, 'lines'),
      axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.line.y = ggplot2::element_line(color = "black", linewidth = 0.3),
      plot.title = ggplot2::element_text(
        size = 26, 
        face = "bold", 
        family = "sans", 
        color = "black", 
        hjust = 0.5,
        margin = ggplot2::margin(20, 0, -20, 0)
      ),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0.10, "cm"),
      axis.text.y = ggplot2::element_text(
        size = 16, 
        face="plain", 
        family="sans", 
        color="black", 
        margin = ggplot2::margin(0, 5, 0, 0)
      ),
      axis.text.x = ggplot2::element_text(
        size = 16, 
        face = "plain", 
        family = "sans", 
        color = "black", 
        margin = ggplot2::margin(5, 0, 0, 0), 
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(0, 10, 0, 0), 
        size = 14, 
        face = "bold", 
        family = "sans", 
        color = "black"
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(15, 0, 5, 0), 
        size = 14, 
        face = "bold", 
        family = "sans", 
        color = "black"
      )
    )
  # mat plot -------------------------------------------------------------------
  exp_mat <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = hline_yintercept, linetype = 5)
  if (!is.null(plot_data_mat$ns_data)) {
    exp_mat <- exp_mat +
      ggplot2::geom_line(
        ggplot2::aes(x = day, y = estimate),
        data = plot_data_mat$ns_data,
        color = "deepskyblue2",
        linewidth = 0.85
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high),
        data = plot_data_mat$ns_data,
        fill = "deepskyblue2",
        alpha = 0.2
      )
  }
  if(!is.null(plot_data_mat$cat_data)) {
    if (cat_grp) {
      exp_mat <- exp_mat +
        ggplot2::geom_errorbar(
          ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high, color = !!dplyr::sym(cat_grp_var)), 
          position = ggplot2::position_dodge(width = 1),
          data = plot_data_mat$cat_data,
          linewidth = 0.85, 
          width = 0, 
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = day, y = estimate, color = !!dplyr::sym(cat_grp_var)), 
          position = ggplot2::position_dodge(width = 1),
          data = plot_data_mat$cat_data,
          size = 3.5
        )
    } else {
      exp_mat <- exp_mat +
        ggplot2::geom_errorbar(
          ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high), 
          data = plot_data_mat$cat_data,
          color="blue4", 
          linewidth = 0.85, 
          width = 0, 
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = day, y = estimate), 
          data = plot_data_mat$cat_data,
          color = "blue4", 
          size = 3.5
        )
    }
    if (connect) {
      exp_mat <- exp_mat +
        ggplot2::geom_line(
          ggplot2::aes(x = day, y = estimate), 
          data = plot_data_mat$cat_data,
          color = "blue4", 
          linewidth = 0.85
        )
    }
  }
  if (is.null(n_breaks_x_mat)) {
    exp_mat <- exp_mat +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = seq(1, 41, 2), 
        labels = c(
          "0-1","2-3","4-5","6-7","8-9","10-11","12-13","14-15",
          "16-17","18-19","20-21","22-23","24-25","26-27","28-29",
          "30-31","32-33","34-35", "36-37","38-39","\u226540"
        )
      ) } else {
        exp_mat <- exp_mat +
          ggplot2::scale_x_continuous(
            expand = c(0, 0),
            n.breaks = n_breaks_x_mat
          )
      }
  
  if (cat_grp) {
    exp_mat <- exp_mat +
      ggplot2::scale_color_manual(
        values = c("#000066", "#0000CC")
      )
  }
  
  exp_mat <- exp_mat +
    ggplot2::xlab(xlab_mat) +
    ggplot2::ylab(ylab_mat) +
    ggplot2::ggtitle(ggtitle_mat) +
    ggplot2::coord_cartesian(
      xlim = xlim_mat,
      ylim = ylim_mat
    ) +
    ggplot2::scale_y_continuous(
      n.breaks = n_breaks_y_mat,
      trans = y_trans_mat
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position.inside = c(0.3, 0.2),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = 20, 
        face = "plain", 
        family = "sans", 
        color = "black"
      ),
      legend.key.size = grid::unit(2, 'lines'),
      axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.line.y = ggplot2::element_line(color = "black", linewidth = 0.3),
      plot.title = ggplot2::element_text(
        size = 26, 
        face = "bold", 
        family = "sans", 
        color = "black", 
        hjust = 0.5,
        margin = ggplot2::margin(20, 0, -20, 0)
      ),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0.10, "cm"),
      axis.text.y = ggplot2::element_text(
        size = 16, 
        face="plain", 
        family="sans", 
        color="black", 
        margin = ggplot2::margin(0, 5, 0, 0)
      ),
      axis.text.x = ggplot2::element_text(
        size = 16, 
        face = "plain", 
        family = "sans", 
        color = "black", 
        margin = ggplot2::margin(5, 0, 0, 0), 
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(0, 10, 0, 0), 
        size = 14, 
        face = "bold", 
        family = "sans", 
        color = "black"
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(15, 0, 5, 0), 
        size = 14, 
        face = "bold", 
        family = "sans", 
        color = "black"
      )
    )
  
  # bpp plot -------------------------------------------------------------------
  if (iq) {
    plot_data_bpp$ns_data <- plot_data_bpp$ns_data %>%
      dplyr::mutate(
        estimate = 15 * estimate,
        conf.low = 15 * conf.low,
        conf.high = 15 * conf.high
      )
    if (!is.null(plot_data_bpp$cat_data)) {
      plot_data_bpp$cat_data <- plot_data_bpp$cat_data %>%
        dplyr::mutate(
          estimate = 15 * estimate,
          conf.low = 15 * conf.low,
          conf.high = 15 * conf.high
        )
    }
    ylim_bpp <- 15 * ylim_bpp
    ylab_bpp <- "Difference in IQ compared with no exposure"
    hline_yintercept <- 15 * hline_yintercept
  }
  exp_bpp <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = hline_yintercept, linetype = 5)
  if (!is.null(plot_data_bpp$ns_data)) {
    exp_bpp <- exp_bpp +
      ggplot2::geom_line(
        ggplot2::aes(x = day, y = estimate),
        data = plot_data_bpp$ns_data,
        color = "green",
        linewidth = 0.85
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high),
        data = plot_data_bpp$ns_data,
        fill = "green",
        alpha = 0.2
      )
  }
  if(!is.null(plot_data_bpp$cat_data)) {
    if (cat_grp) {
      exp_bpp <- exp_bpp +
        ggplot2::geom_errorbar(
          ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high, color = !!dplyr::sym(cat_grp_var)), 
          position = ggplot2::position_dodge(width = 1),
          data = plot_data_bpp$cat_data,
          linewidth = 0.85, 
          width = 0, 
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = day, y = estimate, color = !!dplyr::sym(cat_grp_var)), 
          position = ggplot2::position_dodge(width = 1),
          data = plot_data_bpp$cat_data, 
          size = 3.5
        )
    } else {
      exp_bpp <- exp_bpp +
        ggplot2::geom_errorbar(
          ggplot2::aes(x = day, ymin = conf.low, ymax = conf.high), 
          data = plot_data_bpp$cat_data,
          color="darkgreen", 
          linewidth = 0.85, 
          width = 0, 
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = day, y = estimate), 
          data = plot_data_bpp$cat_data,
          color = "darkgreen", 
          size = 3.5
        )
    }
    if (connect) {
      exp_bpp <- exp_bpp +
        ggplot2::geom_line(
          ggplot2::aes(x = day, y = estimate), 
          data = plot_data_bpp$cat_data,
          color = "darkgreen", 
          linewidth = 0.85
        )
    }
  }
  if (is.null(n_breaks_x_bpp)) {
    exp_bpp <- exp_bpp +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = seq(1, 41, 2), 
        labels = c(
          "0-1","2-3","4-5","6-7","8-9","10-11","12-13","14-15",
          "16-17","18-19","20-21","22-23","24-25","26-27","28-29",
          "30-31","32-33","34-35", "36-37","38-39","\u226540"
        )
      ) } else {
        exp_bpp <- exp_bpp +
          ggplot2::scale_x_continuous(
            expand = c(0, 0),
            n.breaks = n_breaks_x_bpp
          )
      }
  
  if (cat_grp) {
    exp_bpp <- exp_bpp +
      ggplot2::scale_color_manual(
        values = c("#006600", "#00CC00")
      )
  }
  
  exp_bpp <- exp_bpp +
    ggplot2::xlab(xlab_bpp) +
    ggplot2::ylab(ylab_bpp) +
    ggplot2::ggtitle(ggtitle_bpp) +
    ggplot2::coord_cartesian(
      xlim = xlim_bpp,
      ylim = ylim_bpp
    ) +
    ggplot2::scale_y_continuous(
      n.breaks = n_breaks_y_bpp,
      trans = y_trans_bpp
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position.inside = c(0.3, 0.2),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = 20, 
        face = "plain", 
        family = "sans", 
        color = "black"
      ),
      legend.key.size = grid::unit(2, 'lines'),
      axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.line.y = ggplot2::element_line(color = "black", linewidth = 0.3),
      plot.title = ggplot2::element_text(
        size = 26, 
        face = "bold", 
        family = "sans", 
        color = "black", 
        hjust = 0.5,
        margin = ggplot2::margin(20, 0, -20, 0)
      ),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0.10, "cm"),
      axis.text.y = ggplot2::element_text(
        size = 16, 
        face="plain", 
        family="sans", 
        color="black", 
        margin = ggplot2::margin(0, 5, 0, 0)
      ),
      axis.text.x = ggplot2::element_text(
        size = 16, 
        face = "plain", 
        family = "sans", 
        color = "black", 
        margin = ggplot2::margin(5, 0, 0, 0), 
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(0, 10, 0, 0), 
        size = 14, 
        face = "bold", 
        family = "sans", 
        color = "black"
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(15, 0, 5, 0), 
        size = 14, 
        face = "bold", 
        family = "sans", 
        color = "black"
      )
    )
  
  # Combined plot --------------------------------------------------------------
  exp_comb <- cowplot::plot_grid(
    exp_dk + ggplot2::theme(plot.margin = ggplot2::margin(0, 10, 0, 5)),
    exp_mat + ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 10)),
    ncol = 2,
    align = "h",
    rel_widths = c(1, 1)
  )
  
  # save plots -----------------------------------------------------------------
  if (!is.null(path)) {
    ggsave(
      filename = paste0(path, "_language.", device), 
      plot = exp_dk, 
      width = 210, 
      height = 148, 
      units = "mm", 
      dpi = 300
    )
    ggsave(
      filename = paste0(path, "_mathematics.", device), 
      plot = exp_mat, 
      width = 210, 
      height = 148, 
      units = "mm", 
      dpi = 300
    )
    ggsave(
      filename = paste0(path, "_intelligence.", device), 
      plot = exp_bpp, 
      width = 210, 
      height = 148, 
      units = "mm", 
      dpi = 300
    )
    ggsave(
      filename = paste0(path, "_combined.", device), 
      plot=exp_comb, 
      width = 410, 
      height = 148, 
      units = "mm", 
      dpi = 300
    )
  }
  if (return) {
    return(
      list(
        plot_dk = exp_dk, 
        plot_mat = exp_mat, 
        plot_bpp = exp_bpp,
        plot_comb = exp_comb
      )
    )
  } else {
    return(invisible(NULL))
  }
}