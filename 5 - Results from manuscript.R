# Extract numbers/results presented in the manuscript

# Setup ================================================================== ====
data_path <- "PATH"
table_path <- "PATH"
library(tidyverse)
koh_antimi <- readr::read_rds(paste0(data_path, "koh_antimi.rdata"))
skoh_antimi <- readr::read_rds(paste0(data_path, "skoh_antimi.rdata"))
koh_hosp <- readr::read_rds(paste0(data_path, "koh_hosp.rdata"))
skoh_hosp <- readr::read_rds(paste0(data_path, "skoh_hosp.rdata"))
figure1_table <- xlsx::read.xlsx(paste0(table_path, "figure1_table.xlsx"), 1)

# Numbers and results ==================================================== ====
# count children in full-sibling cohort ---------------------------------------
nrow(distinct(koh_antimi, pnr)) # 274,166

# number of sibling groups full-sibling cohort --------------------------------
nrow(distinct(koh_antimi, sibs_id)) # 128,727

# count children whose mother filled a prescription during pregnancy ----------
nrow(distinct(filter(koh_antimi, ATC != ""), pnr)) # 80,817

# count female/male in full-sibling cohort ------------------------------------
koh_antimi |> 
  distinct(pnr, C_KON) |>
  count(C_KON) |>
  mutate(percent = sprintf("%.2f", 100 * n / sum(n)))
# Female: 133,285 (48.61%), Male: 140,881 (51.39%)

# count children whose mother was hospitalized due to an infection during pregnancy
koh_hosp |>
  mutate(in_patient = any(C_PATTYPE %in% c(0, 1)), .by = "pnr") |>
  distinct(pnr, in_patient) |>
  count(in_patient) |>
  mutate(percent = sprintf("%.2f", 100 * n / sum(n)))
# FALSE: 268,538 (97.95%), TRUE: 5,628 (2.05%)

# count number of older siblings ----------------------------------------------
koh_antimi |> 
  distinct(pnr, tot_aeld) |>
  count(tot_aeld) |>
  mutate(percent = sprintf("%.2f", 100 * n / sum(n)))
# 0: 112,503 (41.03%), 1: 120,170 (43.83%), 2: 31,514 (11.49%), 3: 7,026 (2.56%), 4: 2,953 (1.08%)

# distribution of maternal age at child birth ---------------------------------
koh_antimi |> 
  distinct(pnr, mat_alder) |>
  count(mat_alder) |> 
  mutate(percent = sprintf("%.2f", 100 * n / sum(n)))
#   <20:  3,073 ( 1.12%), 20-24: 35,920 (13.10%), 25-29: 106,452 (38.83%), 
# 30-34: 95,360 (34.78%), 35-39: 30,188 (11.01%),   >39:   3,173 ( 1.16%)

# distribution of maternal education at time of childbirth --------------------
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
  )
# Primary education: 58,319 (21.3%), Upper secondary education: 29,785 (10.9%)
# Vocational education and training: 93,827 (34.2%), Short-term higher education: 11,861 (4.3%)
# Vocational bachelor education: 55,078 (20.1%), Academic bachelor's degree: 4,915 (1.8%) 
# Academic master's degree: 19,585 (7.1%), PhD or other doctoral degree: 796 (0.3%) 

# availability of gestational age ---------------------------------------------
koh_antimi |> 
  distinct(pnr, value) |>
  count(value < 46) |> 
  mutate(percent = sprintf("%.2f", 100 * n / sum(n)))
# FALSE: 7,001 (2.55%), TRUE: 267,165 (97.45%)

# count born before 37+0 ------------------------------------------------------
koh_antimi |> 
  distinct(pnr, value) |>
  filter(value < 900) |>
  count(value < 37) |> 
  mutate(percent = sprintf("%.2f", 100 * n / sum(n)))
# FALSE: 248,004 (92.83%), TRUE: 19,161 (7.17%)

# count siblings whose mother filled a prescription for a systemic antimicrobial 
koh_antimi |> 
  distinct(pnr, ATC != "") |>
  count(`ATC != ""`) |>
  mutate(percent = sprintf("%.2f", 100 * n / sum(n)))
# FALSE: 193,349 (70.52%), TRUE: 80,817 (29.48%)

# z-score difference (any infection) in language, mathematics, and in IQ when controlling for shared family factors
figure1_table[26, ] 
# language:     0.002 (95% CI [-0.006,  0.011]; p=0.593)
# mathematics: -0.011 (95% CI [-0.018, -0.004]; p=0.003)
# IQ:           0.296 (95% CI [-0.061,  0.653]; p=0.105)

# z-score difference (two infections) in language, mathematics, and in IQ when controlling for shared family factors
figure1_table[28, ] 
# language:     0.007 (95% CI [-0.009,  0.023]; p=0.374)
# mathematics: -0.021 (95% CI [-0.034, -0.008]; p=0.002)
# IQ:           0.555 (95% CI [-0.119,  1.228]; p=0.106)

# z-score difference (Roxithromycin) in language, mathematics, and in IQ when controlling for shared family factors
figure1_table[40, ] 
# language:     0.080 (95% CI [ 0.009,  0.152]; p=0.028)
# mathematics:  0.001 (95% CI [-0.058, -0.060]; p=0.977)
# IQ:           2.227 (95% CI [-0.785,  5.238]; p=0.147)

# z-score difference (hospitalization) in language, mathematics, and in IQ when controlling for shared family factors
figure1_table[48, ] 
# language:    -0.023 (95% CI [-0.051, 0.005]; p=0.103)
# mathematics:  0.003 (95% CI [-0.020, 0.026]; p=0.809)
# IQ:           0.371 (95% CI [-0.831, 1.573]; p=0.545)

# cleanup ================================================================ ====
rm(list = ls())
gc()