# Include gestational age at maternal infection as exposure in linear model using a natural cubic spline basis.
# Further, use two-week intervals of gestational age as a categorical exposure.

# Setup =========================================================================
data_path <- "PATH"
function_path <- "PATH"
result_path <- "PATH"
library(tidyverse)
library(dtplyr)
library(EpiForsk)
library(data.table)
library(splines)
library(rlang)
library(CVXR)
library(survival)
source(paste0(function_path, "RunlmsSplineAnalysis.R"))
source(paste0(function_path, "CalculateSplineConfidenceInterval.R"))
source(paste0(function_path, "CalculateClogitSplineConfidenceInterval.R"))
source(paste0(function_path, "ExtractWeek.R"))

# Run analyses ==================================================================
# import antimi data to get max_knot value
koh_antimi <- read_rds(paste0(data_path, "koh_antimi.rdata"))
knot_max <- max(koh_antimi$gest_drug_d, na.rm = TRUE)

# any antimicrobial prescription analysis
antimi_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_antimi.rdata", "skoh_antimi.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75, 0.9),
      skoh = c(0.25, 0.5, 0.75, 0.9)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(antimi_analysis_result) <- c("no_smoke", "smoke")

# any antimicrobial prescription analysis restricted to term births
antimi_term_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_antimi_term.rdata", "skoh_antimi_term.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75, 0.9),
      skoh = c(0.25, 0.5, 0.75, 0.9)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(antimi_term_analysis_result) <- c("no_smoke", "smoke")

# beta-lactam analysis
betalac_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_betalac.rdata", "skoh_betalac.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75, 0.9),
      skoh = c(0.25, 0.5, 0.75, 0.9)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(betalac_analysis_result) <- c("no_smoke", "smoke")

# beta-lactam analysis restricted to term births
betalac_term_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_betalac_term.rdata", "skoh_betalac_term.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75, 0.9),
      skoh = c(0.25, 0.5, 0.75, 0.9)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(betalac_term_analysis_result) <- c("no_smoke", "smoke")

# any macrolide analysis
macro_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_macro.rdata", "skoh_macro.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75),
      skoh = c(0.25, 0.5, 0.75)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(macro_analysis_result) <- c("no_smoke", "smoke")

# Trimethoprim analysis
trimet_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_trimet.rdata", "skoh_trimet.rdata")),
    knot_quantiles = list(
      koh = c(0.33, 0.66),
      skoh = c(0.33, 0.66)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(trimet_analysis_result) <- c("no_smoke", "smoke")

# Sulfonamide analysis
sulf_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_sulf.rdata", "skoh_sulf.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75),
      skoh = c(0.25, 0.5, 0.75)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(sulf_analysis_result) <- c("no_smoke", "smoke")

# Quinolone analysis
quino_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_quino.rdata", "skoh_quino.rdata")),
    knot_quantiles = list(
      koh = c(0.33, 0.66),
      skoh = c(0.33, 0.66)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(quino_analysis_result) <- c("no_smoke", "smoke")

# Triazole analysis
triaz_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_triaz.rdata", "skoh_triaz.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75),
      skoh = c(0.25, 0.5, 0.75)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(triaz_analysis_result) <- c("no_smoke", "smoke")

# Hospitalization anaylsis
hosp_analysis_result <- map(
  c(FALSE, TRUE),
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_hosp.rdata", "skoh_hosp.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75),
      skoh = c(0.25, 0.5, 0.75)
    ),
    alpha = 0.05,
    beta = 0.05,
    hosp = TRUE,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(hosp_analysis_result) <- c("no_smoke", "smoke")

# any antimicrobial prescription analysis with imputed missing school results
antimi_imp_analysis_result <- map(
  FALSE,
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_antimi_imp.rdata", "skoh_antimi_imp.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75, 0.9),
      skoh = c(0.25, 0.5, 0.75, 0.9)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(antimi_imp_analysis_result) <- "no_smoke"

# beta-lactamase analysis with imputed missing school results
betalac_imp_analysis_result <- map(
  FALSE,
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_betalac_imp.rdata", "skoh_betalac_imp.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75, 0.9),
      skoh = c(0.25, 0.5, 0.75, 0.9)
    ),
    alpha = 0.05,
    beta = 0.05,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(betalac_imp_analysis_result) <- "no_smoke"

# hospitalization analysis with imputed missing school results
hosp_term_analysis_result <- map(
  FALSE,
  ~ RunlmsSplineAnalysis(
    data_paths = paste0(data_path, c("koh_hosp_term.rdata", "skoh_hosp_term.rdata")),
    knot_quantiles = list(
      koh = c(0.25, 0.5, 0.75),
      skoh = c(0.25, 0.5, 0.75)
    ),
    alpha = 0.05,
    beta = 0.05,
    hosp = TRUE,
    sensitivity = TRUE,
    knot_max = knot_max,
    exp_max = 294L,
    include_smoking = .x,
    verbose = TRUE
  )
)
names(hosp_term_analysis_result) <- "no_smoke"

# Save analyis results ==========================================================
write_rds(antimi_analysis_result, paste0(result_path, "antimi_analysis_result.rdata"))
write_rds(antimi_imp_analysis_result, paste0(result_path, "antimi_imp_analysis_result.rdata"))
write_rds(antimi_term_analysis_result, paste0(result_path, "antimi_term_analysis_result.rdata"))
write_rds(betalac_analysis_result, paste0(result_path, "betalac_analysis_result.rdata"))
write_rds(betalac_imp_analysis_result, paste0(result_path, "betalac_imp_analysis_result.rdata"))
write_rds(betalac_term_analysis_result, paste0(result_path, "betalac_term_analysis_result.rdata"))
write_rds(macro_analysis_result, paste0(result_path, "macro_analysis_result.rdata"))
write_rds(trimet_analysis_result, paste0(result_path, "trimet_analysis_result.rdata"))
write_rds(sulf_analysis_result, paste0(result_path, "sulf_analysis_result.rdata"))
write_rds(quino_analysis_result, paste0(result_path, "quino_analysis_result.rdata"))
write_rds(triaz_analysis_result, paste0(result_path, "triaz_analysis_result.rdata"))
write_rds(hosp_analysis_result, paste0(result_path, "hosp_analysis_result.rdata"))
write_rds(hosp_term_analysis_result, paste0(result_path, "hosp_term_analysis_result.rdata"))

# cleanup ======================================================================
rm(list = ls())
gc()
