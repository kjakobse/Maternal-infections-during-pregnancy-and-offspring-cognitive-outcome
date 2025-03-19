# Prepare figures and tables that appear in the manuscript and supplementary materials.

# Setup ================================================================== ====
data_path <- "PATH"
sas_data_path <- "PATH"
function_path <- "PATH"
result_path <- "PATH"
table_path <- "PATH"
figure_path <- "PATH"
library(tidyverse)
library(grid)
library(gridExtra)
library(forestploter)
library(colorspace)
library(RColorBrewer)
library(cowplot)
library(forcats)
source(paste0(function_path, "BuildSplineCIPlot.R"))
source(paste0(function_path, "BuildSplineCIPlotCb.R"))
source(paste0(function_path, "PrepareFigure1Data.R"))
source(paste0(function_path, "AdjustFigure1.R"))
source(paste0(function_path, "PrepareFigure2.R"))
source(paste0(function_path, "PrepareFigure3.R"))
source(paste0(function_path, "PrepareFigureS2.R"))
source(paste0(function_path, "PrepareFigureS3.R"))
source(paste0(function_path, "PrepareFigureS4.R"))
source(paste0(function_path, "PrepareFigureS6.R"))
source(paste0(function_path, "PrepareFigureS7.R"))
# functions to manipulate and show colors
pal <- function(col, border = "lightgray", ...) {
  n <- length(col)
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
# function to scale plots 
get_scale <- function(plot,
                      width_wanted,
                      height_wanted,
                      unit = "in") {
  h <- convertHeight(sum(plot$heights), unit, TRUE)
  w <- convertWidth(sum(plot$widths), unit, TRUE)
  max(c(w/width_wanted,  h/height_wanted))
}

# read in data 
koh_antimi <- read_rds(paste0(data_path, "koh_antimi.Rdata"))
koh_hosp <- read_rds(paste0(data_path, "koh_hosp.Rdata"))

# read in results
simple_exposure_lms <- read_rds(paste0(result_path, "simple_exposure_lms.Rdata"))
simple_exposure_lm <- read_rds(paste0(result_path, "simple_exposure_lm.Rdata"))
simple_exposure_lms_imp <- read_rds(paste0(result_path, "simple_exposure_lms_imp.Rdata"))
simple_exposure_lm_imp <- read_rds(paste0(result_path, "simple_exposure_lm_imp.Rdata"))
betalac_analysis_result <-  read_rds(paste0(result_path, "betalac_analysis_result.Rdata"))
betalac_imp_analysis_result <- read_rds(paste0(result_path, "betalac_imp_analysis_result.Rdata"))
betalac_term_analysis_result <- read_rds(paste0(result_path, "betalac_term_analysis_result.Rdata"))
macro_analysis_result <- read_rds(paste0(result_path, "macro_analysis_result.Rdata"))
sulf_analysis_result <- read_rds(paste0(result_path, "sulf_analysis_result.Rdata"))
triaz_analysis_result <- read_rds(paste0(result_path, "triaz_analysis_result.Rdata"))
hosp_analysis_result <-  read_rds(paste0(result_path, "hosp_analysis_result.Rdata"))
hosp_term_analysis_result <- read_rds(paste0(result_path, "hosp_term_analysis_result.Rdata"))

# Publication plots ====================================================== ====
# figure 1 --------------------------------------------------------------------
# plot data
figure_1_data_lms <- PrepareFigure1Data(simple_exposure_lms)
figure_1_data_lm <- PrepareFigure1Data(simple_exposure_lm)
figure_1_data_lms_imp <- PrepareFigure1Data(simple_exposure_lms_imp)
figure_1_data_lm_imp <- PrepareFigure1Data(simple_exposure_lm_imp)

# create table with forestplot data combining lms and lm data
figure_1_table_lm <- mutate(figure_1_data_lm, Adjustment = "Covariate-adjusted only")
figure_1_table_lms <- mutate(figure_1_data_lms, Adjustment = "Additionally adjusted for shared family factors")
figure_1_table_lm_imp <- mutate(figure_1_data_lm_imp, Adjustment = "Covariate-adjusted only")
figure_1_table_lms_imp <- mutate(figure_1_data_lms_imp, Adjustment = "Additionally adjusted for shared family factors")
figure_1_table_lm <- figure_1_table_lm |>
  mutate(pval = ifelse(p.value<0.001, "<0.001", p_value)) |>
  select(`Antimicrobial prescription`, Adjustment, outcome, Estimate = estimate_tbl, CI = conf_int, pval) |>
  pivot_wider(names_from = c("outcome"), values_from = c("Estimate", "CI", "pval")) |>
  mutate(across(contains("intelligence"), \(x) {x[c(10, 15, 16, 18)] <- NA; x})) |>
  select(1, 2, 3, 6, 9, 4, 7, 10, 5, 8, 11)
figure_1_table_lms <- figure_1_table_lms |>
  mutate(pval = ifelse(p.value<0.001, "<0.001", p_value)) |>
  select(`Antimicrobial prescription`, Adjustment, outcome, Estimate = estimate_tbl, CI = conf_int, pval) |>
  pivot_wider(names_from = c("outcome"), values_from = c("Estimate", "CI", "pval")) |>
  mutate(across(contains("intelligence"), \(x) {x[c(10, 15, 16, 18)] <- NA; x})) |>
  select(1, 2, 3, 6, 9, 4, 7, 10, 5, 8, 11)
figure_1_table_lm_imp <- figure_1_table_lm_imp |>
  mutate(pval = ifelse(p.value<0.001, "<0.001", p_value)) |>
  select(`Antimicrobial prescription`, Adjustment, outcome, Estimate = estimate_tbl, CI = conf_int, pval) |>
  pivot_wider(names_from = c("outcome"), values_from = c("Estimate", "CI", "pval")) |>
  mutate(across(contains("intelligence"), \(x) {x[c(10, 15, 16, 18)] <- NA; x})) |>
  select(1, 2, 3, 6, 9, 4, 7, 10, 5, 8, 11)
figure_1_table_lms_imp <- figure_1_table_lms_imp |>
  mutate(pval = ifelse(p.value<0.001, "<0.001", p_value)) |>
  select(`Antimicrobial prescription`, Adjustment, outcome, Estimate = estimate_tbl, CI = conf_int, pval) |>
  pivot_wider(names_from = c("outcome"), values_from = c("Estimate", "CI", "pval")) |>
  mutate(across(contains("intelligence"), \(x) {x[c(10, 15, 16, 18)] <- NA; x})) |>
  select(1, 2, 3, 6, 9, 4, 7, 10, 5, 8, 11)
figure_1_table_lm <- figure_1_table_lm |>
  add_row(`Antimicrobial prescription` = "Type of antimicrobial prescription", .after = 5) |>
  add_row(`Antimicrobial prescription` = "Hospitalization for infection", .after = 21)
figure_1_table_lms <- figure_1_table_lms |>
  add_row(`Antimicrobial prescription` = "Type of antimicrobial prescription", .after = 5) |>
  add_row(`Antimicrobial prescription` = "Hospitalization for infection", .after = 21)
figure_1_table_lm_imp <- figure_1_table_lm_imp |>
  add_row(`Antimicrobial prescription` = "Type of antimicrobial prescription", .after = 5) |>
  add_row(`Antimicrobial prescription` = "Hospitalization for infection", .after = 21)
figure_1_table_lms_imp <- figure_1_table_lms_imp |>
  add_row(`Antimicrobial prescription` = "Type of antimicrobial prescription", .after = 5) |>
  add_row(`Antimicrobial prescription` = "Hospitalization for infection", .after = 21)
figure_1_table <- bind_rows(figure_1_table_lm, figure_1_table_lms)
figure_1_table_imp <- bind_rows(figure_1_table_lm_imp, figure_1_table_lms_imp)

# save table
writexl::write_xlsx(
  figure_1_table,
  paste0(table_path, "figure1_table.xlsx")
)
writexl::write_xlsx(
  figure_1_table_imp,
  paste0(table_path, "figure1_table_imp.xlsx")
)

# create forestplot combining lms and lm data
figure_1_data_lm <- mutate(figure_1_data_lm, model = "lm")
figure_1_data_lms <- mutate(figure_1_data_lms, model = "lms")
figure_1_data_lm_imp <- mutate(figure_1_data_lm_imp, model = "lm")
figure_1_data_lms_imp <- mutate(figure_1_data_lms_imp, model = "lms")

figure_1_data_lm <- figure_1_data_lm |>
  select(`Antimicrobial prescription`, model, outcome, estimate, conf.low, conf.high) |>
  pivot_wider(names_from = c("model", "outcome"), values_from = c("estimate", "conf.low", "conf.high")) |>
  mutate(across(contains("intelligence"), \(x) {x[c(10, 15, 16, 18)] <- NA; x}))
figure_1_data_lms <- figure_1_data_lms |>
  select(`Antimicrobial prescription`, model, outcome, estimate, conf.low, conf.high) |>
  pivot_wider(names_from = c("model", "outcome"), values_from = c("estimate", "conf.low", "conf.high")) |>
  mutate(across(contains("intelligence"), \(x) {x[c(10, 15, 16, 18)] <- NA; x}))
figure_1_data_lm_imp <- figure_1_data_lm_imp |>
  select(`Antimicrobial prescription`, model, outcome, estimate, conf.low, conf.high) |>
  pivot_wider(names_from = c("model", "outcome"), values_from = c("estimate", "conf.low", "conf.high")) |>
  mutate(across(contains("intelligence"), \(x) {x[c(10, 15, 16, 18)] <- NA; x}))
figure_1_data_lms_imp <- figure_1_data_lms_imp |>
  select(`Antimicrobial prescription`, model, outcome, estimate, conf.low, conf.high) |>
  pivot_wider(names_from = c("model", "outcome"), values_from = c("estimate", "conf.low", "conf.high")) |>
  mutate(across(contains("intelligence"), \(x) {x[c(10, 15, 16, 18)] <- NA; x}))

# Add rows with category names
figure_1_data_lm <- figure_1_data_lm |>
  add_row(`Antimicrobial prescription` = "Type of antimicrobial prescription", .after = 5) |>
  add_row(`Antimicrobial prescription` = "Hospitalization for infection", .after = 21)
figure_1_data_lms <- figure_1_data_lms |>
  add_row(`Antimicrobial prescription` = "Type of antimicrobial prescription", .after = 5) |>
  add_row(`Antimicrobial prescription` = "Hospitalization for infection", .after = 21)
figure_1_data_lm_imp <- figure_1_data_lm_imp |>
  add_row(`Antimicrobial prescription` = "Type of antimicrobial prescription", .after = 5) |>
  add_row(`Antimicrobial prescription` = "Hospitalization for infection", .after = 21)
figure_1_data_lms_imp <- figure_1_data_lms_imp |>
  add_row(`Antimicrobial prescription` = "Type of antimicrobial prescription", .after = 5) |>
  add_row(`Antimicrobial prescription` = "Hospitalization for infection", .after = 21)

# combine lms and lm data
figure_1_data_comb <- figure_1_data_lms |>
  bind_cols(figure_1_data_lm |> select(-1))
figure_1_data_comb_imp <- figure_1_data_lms_imp |>
  bind_cols(figure_1_data_lm_imp |> select(-1))

# Add three blank column for CI
figure_1_data_comb$` ` <- "  "
figure_1_data_comb$`Language` <- paste(rep(" ", 26), collapse = " ")
figure_1_data_comb$`  ` <- "  "
figure_1_data_comb$`Mathematics` <- paste(rep(" ", 26), collapse = " ")
figure_1_data_comb$`   ` <- "  "
figure_1_data_comb$`Intelligence` <- paste(rep(" ", 26), collapse = " ")

figure_1_data_comb_imp$` ` <- "  "
figure_1_data_comb_imp$`Language` <- paste(rep(" ", 26), collapse = " ")
figure_1_data_comb_imp$`  ` <- "  "
figure_1_data_comb_imp$`Mathematics` <- paste(rep(" ", 26), collapse = " ")
figure_1_data_comb_imp$`   ` <- "  "
figure_1_data_comb_imp$`Intelligence` <- paste(rep(" ", 26), collapse = " ")

# modify forestploter function
assignInNamespace(
  "makeci_static",
  function (est, lower, upper, pch, size = 1, gp = gpar(), t_height = NULL, 
            xlim = c(0, 1), nudge_y = 0) 
  {
    if (upper < min(xlim) | lower > max(xlim)) {
      return(gList(nullGrob()))
    }
    rec_gp <- gp
    rec_gp$col <- gp$fill
    rec_gp$lwd <- gp$lwd / 2
    if (!is.null(gp$bg)) {
      rec_gp$fill <- gp$bg
    }
    rec <- pointsGrob(x = unit(est, "native"), y = 0.5 + 
                        nudge_y, pch = pch, size = unit(size, "char"), 
                      gp = rec_gp, name = "point")
    cent_gp <- nullGrob()
    if (upper > max(xlim) | lower < min(xlim)) {
      if (upper > max(xlim) & lower < min(xlim)) {
        x_pos <- unit(c(0, 1), c("npc", "npc"))
        arrow_side <- "both"
        x_vert <- NULL
      }
      else if (lower < min(xlim) & upper < max(xlim)) {
        x_pos <- unit(c(0, upper), c("npc", "native"))
        arrow_side <- "first"
        x_vert <- unit(upper, "native")
      }
      else {
        x_pos <- unit(c(lower, 1), c("native", "npc"))
        arrow_side <- "last"
        x_vert <- unit(lower, "native")
      }
      lng <- linesGrob(x = x_pos, y = 0.5 + nudge_y, arrow = arrow(length = unit(0.05, 
                                                                                 "inches"), ends = arrow_side), gp = gp)
    }
    else {
      lng <- linesGrob(x = unit(c(lower, upper), "native"), 
                       y = 0.5 + nudge_y, gp = gp)
      x_vert <- unit(c(lower, upper), "native")
    }
    vert <- nullGrob()
    if (est > max(xlim) | est < min(xlim)) 
      rec <- nullGrob()
    gList(cent_gp, lng, vert, rec)
  },
  "forestploter"
)

# create forest plot theme
tm_comb <- forest_theme(
  base_size = 11,
  base_family = "sans",
  core = list(bg_params = list(fill = c("white"))),
  rowhead = list(fg_params = list(hjust = 0.5, x = 0.5)),
  refline_lty = "dashed",
  xaxis_lwd = 1,
  ci_pch = c(23, 22),
  ci_lwd = c(2, 2),
  ci_col = c("red", "red"),
  ci_fill = c("white", "white"),
  vertline_lty = c("dotted", "dotted"),
  vertline_col = c("#bababa", "#bababa"),
  arrow_cex = 0.7,
  legend_name = "Adjustment",
  legend_value = c("Covariate-adjusted only", "Additionally adjusted for shared family-factors"),
  legend_position = "none"
)

# create forest plot
figure_1_comb <- figure_1_comb_cb <- 
  forest(figure_1_data_comb[, c(1, 20:25)],
         est = list(figure_1_data_comb$estimate_lm_Language,
                    figure_1_data_comb$estimate_lm_Mathematics,
                    figure_1_data_comb$estimate_lm_Intelligence*15,
                    figure_1_data_comb$estimate_lms_Language,
                    figure_1_data_comb$estimate_lms_Mathematics,
                    figure_1_data_comb$estimate_lms_Intelligence*15),
         lower = list(figure_1_data_comb$conf.low_lm_Language,
                      figure_1_data_comb$conf.low_lm_Mathematics,
                      figure_1_data_comb$conf.low_lm_Intelligence*15,
                      figure_1_data_comb$conf.low_lms_Language,
                      figure_1_data_comb$conf.low_lms_Mathematics,
                      figure_1_data_comb$conf.low_lms_Intelligence*15), 
         upper = list(figure_1_data_comb$conf.high_lm_Language,
                      figure_1_data_comb$conf.high_lm_Mathematics,
                      figure_1_data_comb$conf.high_lm_Intelligence*15,
                      figure_1_data_comb$conf.high_lms_Language,
                      figure_1_data_comb$conf.high_lms_Mathematics,
                      figure_1_data_comb$conf.high_lms_Intelligence*15),
         sizes = 0.6,
         ci_column = c(3, 5, 7),
         nudge_y = 0.3,
         xlim = list(c(-0.41, 0.41), c(-0.41, 0.41), c(-6.2, 6.2)),
         xlab = c("Difference in language z-score\ncompared with no exposure",
                  "Difference in mathematics z-score\ncompared with no exposure",
                  "Difference in IQ\ncompared with no exposure"),
         ticks_at = list(seq(-0.4, 0.4, 0.2), seq(-0.4, 0.4, 0.2), seq(-6, 6, 2)),
         ref_line = 0,
         vert_line = list(c(-0.2, 0.2), c(-0.2, 0.2), c(-3, 3)),
         arrow_lab = c("suggests harm", "suggests benefit"),
         theme = tm_comb)

figure_1_comb_imp <- figure_1_comb_imp_cb <- 
  forest(figure_1_data_comb_imp[, c(1, 20:25)],
         est = list(figure_1_data_comb_imp$estimate_lm_Language,
                    figure_1_data_comb_imp$estimate_lm_Mathematics,
                    figure_1_data_comb_imp$estimate_lm_Intelligence*15,
                    figure_1_data_comb_imp$estimate_lms_Language,
                    figure_1_data_comb_imp$estimate_lms_Mathematics,
                    figure_1_data_comb_imp$estimate_lms_Intelligence*15),
         lower = list(figure_1_data_comb_imp$conf.low_lm_Language,
                      figure_1_data_comb_imp$conf.low_lm_Mathematics,
                      figure_1_data_comb_imp$conf.low_lm_Intelligence*15,
                      figure_1_data_comb_imp$conf.low_lms_Language,
                      figure_1_data_comb_imp$conf.low_lms_Mathematics,
                      figure_1_data_comb_imp$conf.low_lms_Intelligence*15), 
         upper = list(figure_1_data_comb_imp$conf.high_lm_Language,
                      figure_1_data_comb_imp$conf.high_lm_Mathematics,
                      figure_1_data_comb_imp$conf.high_lm_Intelligence*15,
                      figure_1_data_comb_imp$conf.high_lms_Language,
                      figure_1_data_comb_imp$conf.high_lms_Mathematics,
                      figure_1_data_comb_imp$conf.high_lms_Intelligence*15),
         sizes = 0.6,
         ci_column = c(3, 5, 7),
         nudge_y = 0.3,
         xlim = list(c(-0.41, 0.41), c(-0.41, 0.41), c(-6.2, 6.2)),
         xlab = c("Difference in language z-score\ncompared with no exposure",
                  "Difference in mathematics z-score\ncompared with no exposure",
                  "Difference in IQ\ncompared with no exposure"),
         ticks_at = list(seq(-0.4, 0.4, 0.2), seq(-0.4, 0.4, 0.2), seq(-6, 6, 2)),
         ref_line = 0,
         vert_line = list(c(-0.2, 0.2), c(-0.2, 0.2), c(-3, 3)),
         arrow_lab = c("suggests harm", "suggests benefit"),
         theme = tm_comb)

# edit forest plot with correct bold fontface
figure_1_comb <- edit_plot(
  figure_1_comb, 
  row = c(6, 22),
  col = 1,
  gp = gpar(font = c("bold" = 2))
)
figure_1_comb_cb <- edit_plot(
  figure_1_comb_cb, 
  row = c(6, 22),
  col = 1,
  gp = gpar(font = c("bold" = 2))
)
figure_1_comb_imp <- edit_plot(
  figure_1_comb_imp, 
  row = c(6, 22),
  col = 1,
  gp = gpar(font = c("bold" = 2))
)
figure_1_comb_imp_cb <- edit_plot(
  figure_1_comb_imp_cb, 
  row = c(6, 22),
  col = 1,
  gp = gpar(font = c("bold" = 2))
)

### modify underlying grobs in forestplot object to get desired look
figure_1_comb <- AdjustFigure1(figure_1_comb, cb = FALSE)
figure_1_comb_cb <- AdjustFigure1(figure_1_comb_cb, cb = TRUE)
figure_1_comb_imp <- AdjustFigure1(figure_1_comb_imp, cb = FALSE)
figure_1_comb_imp_cb <- AdjustFigure1(figure_1_comb_imp_cb, cb = TRUE)

# determine correct scale for saving plot
fig1_comb_sc <- get_scale(plot = figure_1_comb, width_wanted = 26, height_wanted = 19, unit = "cm")

# save forestplot
ggplot2::ggsave(filename = paste0(figure_path, "figure1.png"), 
                plot = figure_1_comb,
                dpi = 320,
                width = 26, 
                height = 21,
                units = "cm",
                scale = fig1_comb_sc)
ggplot2::ggsave(filename = paste0(figure_path, "figure1_cb.png"), 
                plot = figure_1_comb_cb,
                dpi = 320,
                width = 26, 
                height = 21,
                units = "cm",
                scale = fig1_comb_sc)
ggplot2::ggsave(filename = paste0(figure_path, "figure1_imp.png"), 
                plot = figure_1_comb_imp,
                dpi = 320,
                width = 26, 
                height = 21,
                units = "cm",
                scale = fig1_comb_sc)
ggplot2::ggsave(filename = paste0(figure_path, "figure1_imp_cb.png"), 
                plot = figure_1_comb_imp_cb,
                dpi = 320,
                width = 26, 
                height = 21,
                units = "cm",
                scale = fig1_comb_sc)
ggplot2::ggsave(filename = paste0(figure_path, "figure1.tif"), 
                plot = figure_1_comb,
                device = "tif",
                dpi = 300,
                width = 30, 
                height = 28,
                units = "cm",
                compression = "zip")
ggplot2::ggsave(filename = paste0(figure_path, "figure1_cb.tif"), 
                plot = figure_1_comb_cb,
                device = "tif",
                dpi = 300,
                width = 30, 
                height = 28,
                units = "cm",
                compression = "zip")
ggplot2::ggsave(filename = paste0(figure_path, "figure1_imp.tif"), 
                plot = figure_1_comb_imp,
                device = "tif",
                dpi = 300,
                width = 30, 
                height = 28,
                units = "cm",
                compression = "zip")
ggplot2::ggsave(filename = paste0(figure_path, "figure1_imp_cb.tif"), 
                plot = figure_1_comb_imp_cb,
                device = "tif",
                dpi = 300,
                width = 30, 
                height = 28,
                units = "cm",
                compression = "zip")

# figure 2 --------------------------------------------------------------------
# create betalac plots
betalac_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = betalac_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = betalac_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = betalac_analysis_result[[.x]][["bpp_data"]],
    time = "weeks",
    xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(betalac_plots) <- c("no_smoke", "smoke")
betalac_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = betalac_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = betalac_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = betalac_analysis_result[[.x]][["bpp_data"]],
    time = "weeks",
    xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(betalac_plots_cb) <- c("no_smoke", "smoke")

# prepare figure2 from betalac plots
figure2 <- PrepareFigure2(betalac_plots)
figure2_cb <- PrepareFigure2(betalac_plots_cb)

# save figure 2
ggsave(
  filename = paste0(figure_path, "figure2.png"), 
  plot = figure2, 
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figure2_cb.png"), 
  plot = figure2_cb, 
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figure2.tif"), 
  plot = figure2,
  device = "tif",
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figure2_cb.tif"), 
  plot = figure2_cb,
  device = "tif",
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# figure 3 --------------------------------------------------------------------
# build hospital plots
hosp_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = hosp_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = hosp_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = hosp_analysis_result[[.x]][["bpp_data"]],
    cat = c("dk", "mat"),
    time = "weeks",
    xlab = "Gestational age at maternal hospitalization (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.75, 0.75),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(hosp_plots) <- c("no_smoke", "smoke")
hosp_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = hosp_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = hosp_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = hosp_analysis_result[[.x]][["bpp_data"]],
    cat = c("dk", "mat"),
    time = "weeks",
    xlab = "Gestational age at maternal hospitalization (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.75, 0.75),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(hosp_plots_cb) <- c("no_smoke", "smoke")

# prepare figure 3 from hospital plots
figure3 <- PrepareFigure3(hosp_plots)
figure3_cb <- PrepareFigure3(hosp_plots_cb)

# save figure 3
ggsave(
  filename = paste0(figure_path, "figure3.png"), 
  plot = figure3, 
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figure3_cb.png"), 
  plot = figure3_cb, 
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figure3.tif"), 
  plot = figure3,
  device = "tif",
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figure3_cb.tif"), 
  plot = figure3_cb,
  device = "tif",
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# figure s2 -------------------------------------------------------------------
# build macrolide, sulfonamide, and triazole plots
macro_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = macro_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = macro_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = macro_analysis_result[[.x]][["bpp_data"]],
    cat = NULL,
    time = "weeks",
    xlab = "Gestational age at macrolide exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.8, 0.8),
    n_breaks_y = 7,
    n_breaks_y_bpp = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(macro_plots) <- c("no_smoke", "smoke")
macro_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = macro_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = macro_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = macro_analysis_result[[.x]][["bpp_data"]],
    cat = NULL,
    time = "weeks",
    xlab = "Gestational age at macrolide exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.8, 0.8),
    n_breaks_y = 7,
    n_breaks_y_bpp = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(macro_plots_cb) <- c("no_smoke", "smoke")
sulf_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = sulf_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = sulf_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = sulf_analysis_result[[.x]][["bpp_data"]],
    cat = NULL,
    time = "weeks",
    xlab = "Gestational age at sulfonamide exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.8, 0.8),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(sulf_plots) <- c("no_smoke", "smoke")
sulf_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = sulf_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = sulf_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = sulf_analysis_result[[.x]][["bpp_data"]],
    cat = NULL,
    time = "weeks",
    xlab = "Gestational age at sulfonamide exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.8, 0.8),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(sulf_plots_cb) <- c("no_smoke", "smoke")
triaz_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = triaz_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = triaz_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = triaz_analysis_result[[.x]][["bpp_data"]],
    cat = NULL,
    time = "weeks",
    xlab = "Gestational age at triazole exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-1, 1),
    ylim_bpp = c(-1.8, 1.8),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(triaz_plots) <- c("no_smoke", "smoke")
triaz_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = triaz_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = triaz_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = triaz_analysis_result[[.x]][["bpp_data"]],
    cat = NULL,
    time = "weeks",
    xlab = "Gestational age at triazole exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-1, 1),
    ylim_bpp = c(-1.8, 1.8),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(triaz_plots_cb) <- c("no_smoke", "smoke")

# prepare figure s2 from macrolide, sulfonamide, and triazole plots
figures2 <- plot_grid(
  PrepareFigureS2(macro_plots$no_smoke$plot_dk, T, T),
  PrepareFigureS2(macro_plots$no_smoke$plot_mat, T, T),
  PrepareFigureS2(macro_plots$no_smoke$plot_bpp, F, T),
  PrepareFigureS2(sulf_plots$no_smoke$plot_dk, T, T),
  PrepareFigureS2(sulf_plots$no_smoke$plot_mat, T, T),
  PrepareFigureS2(sulf_plots$no_smoke$plot_bpp, F, T),
  PrepareFigureS2(triaz_plots$no_smoke$plot_dk, T, T) +
    coord_cartesian(ylim = c(-0.4, 0.4)),
  PrepareFigureS2(triaz_plots$no_smoke$plot_mat, T, T) +
    coord_cartesian(ylim = c(-0.4, 0.4)),
  PrepareFigureS2(triaz_plots$no_smoke$plot_bpp, F, T) +
    coord_cartesian(ylim = c(-12, 12)),
  align = "h",
  nrow = 3,
  rel_heights = c(1, 1, 1)
)
figures2_cb <- plot_grid(
  PrepareFigureS2(macro_plots_cb$no_smoke$plot_dk, T, T),
  PrepareFigureS2(macro_plots_cb$no_smoke$plot_mat, T, T),
  PrepareFigureS2(macro_plots_cb$no_smoke$plot_bpp, F, T),
  PrepareFigureS2(sulf_plots_cb$no_smoke$plot_dk, T, T),
  PrepareFigureS2(sulf_plots_cb$no_smoke$plot_mat, T, T),
  PrepareFigureS2(sulf_plots_cb$no_smoke$plot_bpp, F, T),
  PrepareFigureS2(triaz_plots_cb$no_smoke$plot_dk, T, T) +
    coord_cartesian(ylim = c(-0.4, 0.4)),
  PrepareFigureS2(triaz_plots_cb$no_smoke$plot_mat, T, T) +
    coord_cartesian(ylim = c(-0.4, 0.4)),
  PrepareFigureS2(triaz_plots_cb$no_smoke$plot_bpp, F, T) +
    coord_cartesian(ylim = c(-12, 12)),
  align = "h",
  nrow = 3,
  rel_heights = c(1, 1, 1)
)

# save figure s2
ggsave(
  filename = paste0(figure_path, "figures2.png"), 
  plot = figures2, 
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures2_cb.png"), 
  plot = figures2_cb, 
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures2.tif"), 
  plot = figures2, 
  device = "tif",
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figures2_cb.tif"), 
  plot = figures2_cb, 
  device = "tif",
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# figure s3 -------------------------------------------------------------------
# build betalac term plots
betalac_term_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = betalac_term_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = betalac_term_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = betalac_term_analysis_result[[.x]][["bpp_data"]],
    time = "weeks",
    xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks) among term children",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.4, 0.6),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(betalac_term_plots) <- c("no_smoke", "smoke")
betalac_term_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = betalac_term_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = betalac_term_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = betalac_term_analysis_result[[.x]][["bpp_data"]],
    time = "weeks",
    xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks) among term children",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.4, 0.6),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(betalac_term_plots_cb) <- c("no_smoke", "smoke")

# prepare figure s3 from betalac term plots
figures3 <- PrepareFigureS3(betalac_term_plots)
figures3_cb <- PrepareFigureS3(betalac_term_plots_cb)

# save figure s3
ggsave(
  filename = paste0(figure_path, "figures3.png"), 
  plot = figures3, 
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures3_cb.png"), 
  plot = figures3_cb, 
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures3.tif"), 
  plot = figures3, 
  device = "tif",
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figures3_cb.tif"), 
  plot = figures3_cb, 
  device = "tif",
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# figure s4 -------------------------------------------------------------------
# build betalac plots with and without smoking adjustment
figures4_data <- betalac_analysis_result$no_smoke[1:3]
figures4_data$dan_data$cat_data <- figures4_data$dan_data$cat_data |>
  mutate(cat_grp = "No maternal smoking adjustment") |>
  bind_rows(
    mutate(betalac_analysis_result$smoke$dan_data$cat_data, cat_grp = "Maternal smoking adjusted")
  )
figures4_data$mat_data$cat_data <- figures4_data$mat_data$cat_data |>
  mutate(cat_grp = "No maternal smoking adjustment") |>
  bind_rows(
    mutate(betalac_analysis_result$smoke$mat_data$cat_data, cat_grp = "Maternal smoking adjusted")
  )
figures4_data$bpp_data$cat_data <- figures4_data$bpp_data$cat_data |>
  mutate(cat_grp = "No maternal smoking adjustment") |>
  bind_rows(
    mutate(betalac_analysis_result$smoke$bpp_data$cat_data, cat_grp = "Maternal smoking adjusted")
  )
figures4_plots <- BuildSplineCIPlot(
  plot_data_dk = figures4_data[["dan_data"]],
  plot_data_mat = figures4_data[["mat_data"]],
  plot_data_bpp = figures4_data[["bpp_data"]],
  cat_grp = TRUE,
  cat_grp_var = "cat_grp",
  time = "weeks",
  xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks)",
  xlim = c(0, 42),
  ylim = c(-0.4, 0.4),
  n_breaks_y = 9,
  connect = FALSE,
  iq = TRUE
)
figures4_cb_plots <- BuildSplineCIPlotCb(
  plot_data_dk = figures4_data[["dan_data"]],
  plot_data_mat = figures4_data[["mat_data"]],
  plot_data_bpp = figures4_data[["bpp_data"]],
  cat_grp = TRUE,
  cat_grp_var = "cat_grp",
  time = "weeks",
  xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks)",
  xlim = c(0, 42),
  ylim = c(-0.4, 0.4),
  n_breaks_y = 9,
  connect = FALSE,
  iq = TRUE
)

# prepare figure s4 from betalac plots with and without smoking adjustment
figures4 <- PrepareFigureS4(figures4_plots)
figures4_cb <- PrepareFigureS4(figures4_cb_plots)

# save figure s4
ggsave(
  filename = paste0(figure_path, "figures4.png"), 
  plot = figures4, 
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures4_cb.png"), 
  plot = figures4_cb, 
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures4.tif"), 
  plot = figures4, 
  device = "tif",
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figures4_cb.tif"), 
  plot = figures4_cb, 
  device = "tif",
  width = 200, 
  height = 297, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# figure s5 -------------------------------------------------------------------
# extract figure
figures5 <- hosp_plots$no_smoke$plot_bpp
figures5_cb <- hosp_plots_cb$no_smoke$plot_bpp

# save figure s5
ggsave(
  filename = paste0(figure_path, "figures5.png"), 
  plot = figures5, 
  width = 210, 
  height = 148, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures5_cb.png"), 
  plot = figures5_cb, 
  width = 210, 
  height = 148, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures5.tif"), 
  plot = figures5, 
  device = "tif",
  width = 210, 
  height = 148, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figures5_cb.tif"), 
  plot = figures5_cb, 
  device = "tif",
  width = 210, 
  height = 148, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# figure s6 -------------------------------------------------------------------
# build hospital plots with restriction to term births
figures6_plots <- map(
  "no_smoke",
  ~ BuildSplineCIPlot(
    plot_data_dk = hosp_term_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = hosp_term_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = hosp_term_analysis_result[[.x]][["bpp_data"]],
    cat = c("dk", "mat"),
    time = "weeks",
    xlab = "Gestational age at maternal hospitalization (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.75, 0.75),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(figures6_plots) <- "no_smoke"
figures6_cb_plots <- map(
  "no_smoke",
  ~ BuildSplineCIPlotCb(
    plot_data_dk = hosp_term_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = hosp_term_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = hosp_term_analysis_result[[.x]][["bpp_data"]],
    cat = c("dk", "mat"),
    time = "weeks",
    xlab = "Gestational age at maternal hospitalization (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.75, 0.75),
    n_breaks_y = 9,
    connect = FALSE,
    iq = TRUE
  )
)
names(figures6_cb_plots) <- "no_smoke"

# prepare figure s6 from hospital term plots
figures6 <- PrepareFigureS6(figures6_plots)
figures6_cb <- PrepareFigureS6(figures6_cb_plots)

# save figure s6
ggsave(
  filename = paste0(figure_path, "figures6.png"), 
  plot = figures6, 
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures6_cb.png"), 
  plot = figures6_cb, 
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures6.tif"), 
  plot = figures6,
  device = "tif",
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figures6_cb.tif"), 
  plot = figures6_cb,
  device = "tif",
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# figure s7 -------------------------------------------------------------------
# build beta-lactam, macrolide, and hospital plots for OR of achieving below 5% lowest score.
betalac_sens_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = betalac_analysis_result[[.x]][["dan_data_sens"]],
    plot_data_mat = betalac_analysis_result[[.x]][["mat_data_sens"]],
    plot_data_bpp = betalac_analysis_result[[.x]][["bpp_data_sens"]],
    time = "weeks",
    xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks)",
    ylab = "OR of 5% lowest z-scores compared with no exposure",
    xlim = c(0, 42),
    ylim = c(0.25, 3.4),
    n_breaks_y = 9,
    y_trans = "log", 
    hline_yintercept = 1,
    connect = FALSE,
    iq = FALSE
  )
)
names(betalac_sens_plots) <- c("no_smoke", "smoke")
betalac_sens_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = betalac_analysis_result[[.x]][["dan_data_sens"]],
    plot_data_mat = betalac_analysis_result[[.x]][["mat_data_sens"]],
    plot_data_bpp = betalac_analysis_result[[.x]][["bpp_data_sens"]],
    time = "weeks",
    xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks)",
    ylab = "OR of 5% lowest z-scores compared with no exposure",
    xlim = c(0, 42),
    ylim = c(0.25, 3.4),
    n_breaks_y = 9,
    y_trans = "log", 
    hline_yintercept = 1,
    connect = FALSE,
    iq = FALSE
  )
)
names(betalac_sens_plots_cb) <- c("no_smoke", "smoke")
macro_sens_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = macro_analysis_result[[.x]][["dan_data_sens"]],
    plot_data_mat = macro_analysis_result[[.x]][["mat_data_sens"]],
    plot_data_bpp = macro_analysis_result[[.x]][["bpp_data_sens"]],
    cat = NULL,
    time = "weeks",
    xlab = "Gestational age at macrolide exposure (weeks)",
    ylab = "OR of 5% lowest z-scores compared with no exposure",
    xlim = c(0, 42),
    ylim = c(0.25, 3.4),
    n_breaks_y = 9,
    y_trans = "log",
    hline_yintercept = 1,
    connect = FALSE,
    iq = FALSE
  )
)
names(macro_sens_plots) <- c("no_smoke", "smoke")
macro_sens_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = macro_analysis_result[[.x]][["dan_data_sens"]],
    plot_data_mat = macro_analysis_result[[.x]][["mat_data_sens"]],
    plot_data_bpp = macro_analysis_result[[.x]][["bpp_data_sens"]],
    cat = NULL,
    time = "weeks",
    xlab = "Gestational age at macrolide exposure (weeks)",
    ylab = "OR of 5% lowest z-scores compared with no exposure",
    xlim = c(0, 42),
    ylim = c(0.25, 3.4),
    n_breaks_y = 9,
    y_trans = "log",
    hline_yintercept = 1,
    connect = FALSE,
    iq = FALSE
  )
)
names(macro_sens_plots_cb) <- c("no_smoke", "smoke")
hosp_sens_plots <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlot(
    plot_data_dk = hosp_analysis_result[[.x]][["dan_data_sens"]],
    plot_data_mat = hosp_analysis_result[[.x]][["mat_data_sens"]],
    plot_data_bpp = hosp_analysis_result[[.x]][["bpp_data_sens"]],
    cat = c("dk", "mat"),
    time = "weeks",
    xlab = "Gestational age at maternal hospitalization (weeks)",
    ylab = "OR of 5% lowest z-scores compared with no exposure",
    xlim = c(0, 42),
    ylim = c(0.1, 10),
    n_breaks_y = 5,
    y_trans = "log",
    hline_yintercept = 1,
    connect = FALSE,
    iq = FALSE
  )
)
names(hosp_sens_plots) <- c("no_smoke", "smoke")
hosp_sens_plots_cb <- map(
  c("no_smoke", "smoke"),
  ~ BuildSplineCIPlotCb(
    plot_data_dk = hosp_analysis_result[[.x]][["dan_data_sens"]],
    plot_data_mat = hosp_analysis_result[[.x]][["mat_data_sens"]],
    plot_data_bpp = hosp_analysis_result[[.x]][["bpp_data_sens"]],
    cat = c("dk", "mat"),
    time = "weeks",
    xlab = "Gestational age at maternal hospitalization (weeks)",
    ylab = "OR of 5% lowest z-scores compared with no exposure",
    xlim = c(0, 42),
    ylim = c(0.1, 10),
    n_breaks_y = 5,
    y_trans = "log",
    hline_yintercept = 1,
    connect = FALSE,
    iq = FALSE
  )
)
names(hosp_sens_plots_cb) <- c("no_smoke", "smoke")

# prepare figure s7
figures7 <- plot_grid(
  PrepareFigureS7(betalac_sens_plots$no_smoke$plot_dk, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(betalac_sens_plots$no_smoke$plot_mat, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(betalac_sens_plots$no_smoke$plot_bpp, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(macro_sens_plots$no_smoke$plot_dk, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5)),
  PrepareFigureS7(macro_sens_plots$no_smoke$plot_mat, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5)),
  PrepareFigureS7(macro_sens_plots$no_smoke$plot_bpp, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5)),
  PrepareFigureS7(hosp_sens_plots$no_smoke$plot_dk, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(hosp_sens_plots$no_smoke$plot_mat, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(hosp_sens_plots$no_smoke$plot_bpp, 
               ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5)),
  align = "h",
  nrow = 3,
  rel_heights = c(1, 1, 1)
)
figures7_cb <- plot_grid(
  PrepareFigureS7(betalac_sens_plots_cb$no_smoke$plot_dk, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(betalac_sens_plots_cb$no_smoke$plot_mat, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(betalac_sens_plots_cb$no_smoke$plot_bpp, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(macro_sens_plots_cb$no_smoke$plot_dk, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5)),
  PrepareFigureS7(macro_sens_plots_cb$no_smoke$plot_mat, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5)),
  PrepareFigureS7(macro_sens_plots_cb$no_smoke$plot_bpp, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5)),
  PrepareFigureS7(hosp_sens_plots_cb$no_smoke$plot_dk, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(hosp_sens_plots_cb$no_smoke$plot_mat, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5), point_size = 2),
  PrepareFigureS7(hosp_sens_plots_cb$no_smoke$plot_bpp, 
                  ylim = c(0.1, 10), y_breaks = c(0.2, 0.5, 1, 2, 5)),
  align = "h",
  nrow = 3,
  rel_heights = c(1, 1, 1)
)

# save figure s7
ggsave(
  filename = paste0(figure_path, "figures7.png"), 
  plot = figures7, 
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures7_cb.png"), 
  plot = figures7_cb, 
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures7.tif"), 
  plot = figures7, 
  device = "tif",
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figures7_cb.tif"), 
  plot = figures7_cb, 
  device = "tif",
  width = 297, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# figure s8 -------------------------------------------------------------------
# build beta-lactam plots with imputation of missing grades
figures8_plots <- map(
  "no_smoke",
  ~ BuildSplineCIPlot(
    plot_data_dk = betalac_imp_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = betalac_imp_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = betalac_imp_analysis_result[[.x]][["bpp_data"]],
    cat = c("dk", "mat"),
    time = "weeks",
    xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.75, 0.75),
    n_breaks_y = 9,
    connect = FALSE,
    iq = FALSE
  )
)
names(figures8_plots) <- "no_smoke"
figures8_cb_plots <- map(
  "no_smoke",
  ~ BuildSplineCIPlotCb(
    plot_data_dk = betalac_imp_analysis_result[[.x]][["dan_data"]],
    plot_data_mat = betalac_imp_analysis_result[[.x]][["mat_data"]],
    plot_data_bpp = betalac_imp_analysis_result[[.x]][["bpp_data"]],
    cat = c("dk", "mat"),
    time = "weeks",
    xlab = "Gestational age at \u03b2-lactam antibiotic exposure (weeks)",
    xlim = c(0, 42),
    ylim = c(-0.4, 0.4),
    ylim_bpp = c(-0.75, 0.75),
    n_breaks_y = 9,
    connect = FALSE,
    iq = FALSE
  )
)
names(figures8_cb_plots) <- "no_smoke"

# prepare figure s8 from hospital term plots
figures8 <- PrepareFigureS6(figures8_plots)
figures8_cb <- PrepareFigureS6(figures8_cb_plots)

# save figure s8
ggsave(
  filename = paste0(figure_path, "figures8.png"), 
  plot = figures8, 
  width = 317, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures8_cb.png"), 
  plot = figures8_cb, 
  width = 317, 
  height = 200, 
  units = "mm", 
  dpi = 300
)
ggsave(
  filename = paste0(figure_path, "figures8.tif"), 
  plot = figures8,
  device = "tif",
  width = 317, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)
ggsave(
  filename = paste0(figure_path, "figures8_cb.tif"), 
  plot = figures8_cb,
  device = "tif",
  width = 317, 
  height = 200, 
  units = "mm", 
  dpi = 300,
  compression = "zip"
)

# Publication tables ===================================================== ====
# table 1 ---------------------------------------------------------------------
# create table with demographic information
 table_1 <- koh_antimi |>
  distinct(pnr, C_KON) |> 
  count(C_KON) |>
  mutate(
    C_KON = case_when(
      C_KON == "K" ~ "  Female",
      C_KON == "M" ~ "  Male"
    ),
    percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
    n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
  ) |>
  add_row(
    C_KON = "Sex", 
    n = "", 
    percent = "", 
    .before = 1
  ) |>
  rename("var" = "C_KON") |>
  bind_rows( 
    koh_antimi |>
      mutate(
        value = case_when(
          value < 37 ~ "  < 37",
          value %in% 37:41 ~ "  37-41",
          value %in% 42:50 ~ "  \u2265 42",
          value > 50 ~ "  Missing information"
        )
      ) |>
      distinct(pnr, value) |> 
      count(value) |>
      mutate(
        percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
        n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
      ) |>
      slice(2, 1, 4, 3) |>
      add_row(
        value = "Gestational age at birth (weeks)", 
        n = "", 
        percent = "", 
        .before = 1
      ) |>
      rename("var" = "value")
  ) |>
  bind_rows( 
    koh_antimi |>
      distinct(pnr, tot_aeld) |> 
      count(tot_aeld) |>
      mutate(
        tot_aeld = case_when(
          tot_aeld == 0 ~ "  0",
          tot_aeld == 1 ~ "  1",
          tot_aeld == 2 ~ "  2",
          tot_aeld == 3 ~ "  3",
          tot_aeld == 4 ~ "  \u2265 4"
        ),
        percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
        n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
      ) |>
      add_row(
        tot_aeld = "Number of older full-siblings", 
        n = "", 
        percent = "", 
        .before = 1
      ) |>
      rename("var" = "tot_aeld")
  ) |>
  bind_rows( 
    koh_antimi |>
      distinct(pnr, mat_alder) |> 
      count(mat_alder) |>
      mutate(
        mat_alder = case_when(
          mat_alder == 12 ~ "  <20",
          mat_alder == 20 ~ "  20-24",
          mat_alder == 25 ~ "  25-29",
          mat_alder == 30 ~ "  30-34",
          mat_alder == 35 ~ "  35-39",
          mat_alder == 40 ~ "  \u2265 40"
        ),
        percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
        n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
      ) |>
      add_row(
        mat_alder = "Maternal age at childbirth (years)", 
        n = "", 
        percent = "", 
        .before = 1
      ) |>
      rename("var" = "mat_alder")
  ) |>
  bind_rows(
    koh_antimi |>
      distinct(pnr, hf_grp_mor) |> 
      count(hf_grp_mor) |>
      mutate(
        hf_grp_mor = case_when(
          hf_grp_mor == "10" ~ "  Primary education",
          hf_grp_mor == "20" ~ "  Upper secondary education",
          hf_grp_mor == "30" ~ "  Vocational education and training",
          hf_grp_mor == "40" ~ "  Short-term higher education",
          hf_grp_mor == "50" ~ "  Vocational bachelor education",
          hf_grp_mor == "60" ~ "  Academic bachelor's degree",
          hf_grp_mor == "70" ~ "  Academic master's degree",
          hf_grp_mor == "80" ~ "  PhD or other doctoral degree",
          TRUE ~ "  No paternal education level registered"
        ),
        percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
        n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
      ) |>
      slice(2:n(), 1) |>
      add_row(
        hf_grp_mor = "Maternal education level*", 
        n = "", 
        percent = "", 
        .before = 1
      ) |>
      rename("var" = "hf_grp_mor")
  ) |>
  bind_rows( 
    koh_antimi |>
      summarise(
        gest_drug = any(!is.na(gest_drug_d)),
        .by = pnr
      ) |> 
      count(gest_drug) |>
      mutate(
        gest_drug = case_when(
          gest_drug ~ "  Any",
          !gest_drug ~ "  None"
        ),
        percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
        n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
      ) |>
      slice(2, 1) |>
      add_row(
        gest_drug = "Maternal filled antimicrobial prescription during pregnancy", 
        n = "", 
        percent = "", 
        .before = 1
      ) |>
      rename("var" = "gest_drug")
  ) |>
  bind_rows( 
    koh_hosp |>
      summarise(
        ghosp = !any(!(ghosp_grp == "no_visit")),
        .by = pnr
      ) |> 
      count(ghosp) |>
      mutate(
        ghosp = case_when(
          ghosp ~ "  None",
          !ghosp ~ "  Any"
        ),
        percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
        n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
      ) |>
      add_row(
        ghosp = "Maternal in-patient hospitalization due to infection during pregnancy", 
        n = "", 
        percent = "", 
        .before = 1
      ) |>
      rename("var" = "ghosp")
  ) |>
  bind_rows(
    koh_antimi |>
      distinct(pnr, pat_alder) |> 
      count(pat_alder) |>
      mutate(
        pat_alder = case_when(
          pat_alder == 12 ~ "  <20",
          pat_alder == 20 ~ "  20-24",
          pat_alder == 25 ~ "  25-29",
          pat_alder == 30 ~ "  30-34",
          pat_alder == 35 ~ "  35-39",
          pat_alder == 40 ~ "  \u2265 40"
        ),
        percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
        n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
      ) |>
      add_row(
        pat_alder = "Paternal age at childbirth (years)", 
        n = "", 
        percent = "", 
        .before = 1
      ) |>
      rename("var" = "pat_alder")
  ) |>
  bind_rows(
    koh_antimi |>
      distinct(pnr, hf_grp_far) |> 
      count(hf_grp_far) |>
      mutate(
        hf_grp_far = case_when(
          hf_grp_far == "10" ~ "  Primary education",
          hf_grp_far == "20" ~ "  Upper secondary education",
          hf_grp_far == "30" ~ "  Vocational education and training",
          hf_grp_far == "40" ~ "  Short-term higher education",
          hf_grp_far == "50" ~ "  Vocational bachelor education",
          hf_grp_far == "60" ~ "  Academic bachelor's degree",
          hf_grp_far == "70" ~ "  Academic master's degree",
          hf_grp_far == "80" ~ "  PhD or other doctoral degree",
          TRUE ~ "  No paternal education level registered"
        ),
        percent = glue::glue("({sprintf('%.1f', 100 * n / sum(n))}%)"),
        n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))
      ) |>
      slice(2:n(), 1) |>
      add_row(
        hf_grp_far = "Paternal education level*", 
        n = "", 
        percent = "", 
        .before = 1
      ) |>
      rename("var" = "hf_grp_far")
  )

# save table 1
writexl::write_xlsx(table_1, paste0(table_path, "table_1.xlsx"))

# table s1 --------------------------------------------------------------------
table_s1 <- koh_antimi |>
  summarise(
    `Any Antimicrobial` = sum(ATC != ""),
    `Any beta-lactam` = sum(grepl("^J01[CD]", ATC)),
    `  Beta-lactamase sensitive penicillin` = sum(grepl("^J01CE", ATC)),
    `  Beta-lactamase resistant penicillin` = sum(grepl("^J01CF", ATC)),
    `  Extended-spectrum penicillin` = sum(grepl("^J01CA", ATC)),
    `  Cephalosporin` = sum(grepl("^J01D", ATC)),
    `Any macrolide` = sum(grepl("^J01FA", ATC)),
    `  Azithromycin` = sum(grepl("^J01FA10", ATC)),
    `  Erythromycin` = sum(grepl("^J01FA01", ATC)),
    `  Roxythromycin` = sum(grepl("^J01FA06", ATC)),
    `  Other macrolides` = sum(grepl("^J01FA0[29]", ATC)),
    `Trimethoprim` = sum(grepl("^J01EA", ATC)),
    `Sulfonamide` = sum(grepl("^J01EB", ATC)),
    `Quinolone` = sum(grepl("^J01MA", ATC)),
    `Triazole antifungal` = sum(grepl("^J02AC", ATC)),
    `Nucleotide/nucleoside-analogue` = sum(grepl("^J05AB", ATC))
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "Antimicrobial subtype",
    values_to = "n"
  ) |>
  mutate(n = gsub("(\\d+)(\\d{3})$", "\\1,\\2", as.character(n))) |>
  add_row(
    "Antimicrobial subtype" = "Antimicrobial subtype",
    n = "Number of prescriptions",
    .before = 1
  )

# save table s1
writexl::write_xlsx(table_s1, paste0(table_path, "table_s1.xlsx"))

# table s2 --------------------------------------------------------------------
# This table contains information on all mothers hospitalizations from the school cohort:
koh_p_mor_lpr <- haven::read_sas(paste0(sas_data_path, "koh_p_mor_lpr.sas7bdat"))

# remove data with blank pnr for the mother
koh_hosp_red <- filter(koh_hosp, pnr_mor != "")

# create table with only those hospitalizations from koh_hosp that have an A-diagnosis given
table_s2 <- inner_join(
  select(koh_hosp_red, "pnr", "pnr_mor", "sibs_id", "gest_hosp_dage", "C_PATTYPE"),
  # filter on inpatient and acute hospitalizations
  filter(koh_p_mor_lpr, C_PATTYPE %in% c(0, 1) | (C_PATTYPE == 2 & C_INDM == 1)),
  by = c(
    "pnr" = "pnr_barn", 
    "pnr_mor" = "pnr_mor", 
    "gest_hosp_dage" = "gest_hosp_dage",
    "C_PATTYPE" = "C_PATTYPE"
  )
) |>
  # look at distinct A-diagnoses given to a mother during a pregnancy
  distinct(pnr_mor, fdato, C_DIAG) |>
  rename("Diagnosis Code" = "C_DIAG") |>
  count(`Diagnosis Code`) |>
  mutate(percent = sprintf("%.2f", 100 * n / sum(n))) |>
  arrange(desc(n)) |>
  filter(n > 100)

# save table s2
writexl::write_xlsx(table_s2, paste0(table_path, "table_s2.xlsx"))

# cleanup ================================================================ ====
rm(list = ls())
gc()
