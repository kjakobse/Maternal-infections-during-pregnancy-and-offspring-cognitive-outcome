# Prepare cohorts created in SAS. This includes recoding variables to factors
# and imputing missing values using mode imputation.

# setup ------------------------------------------------------------------------
sas_data_path <- "PATH"
data_path <- "PATH"
library(tidyverse)
library(dtplyr)
library(lubridate)
library(haven)

# read in data from SAS --------------------------------------------------------
koh_antimi <- read_sas(paste0(sas_data_path, "koh_antimi.sas7bdat"))
koh_betalac <- read_sas(paste0(sas_data_path, "koh_betalac.sas7bdat"))
koh_macro <- read_sas(paste0(sas_data_path, "koh_macro.sas7bdat"))
koh_trimet <- read_sas(paste0(sas_data_path, "koh_trimet.sas7bdat"))
koh_sulf <- read_sas(paste0(sas_data_path, "koh_sulf.sas7bdat"))
koh_quino <- read_sas(paste0(sas_data_path, "koh_quino.sas7bdat"))
koh_triaz <- read_sas(paste0(sas_data_path, "koh_triaz.sas7bdat"))
koh_hosp <- read_sas(paste0(sas_data_path, "koh_hosp.sas7bdat"))

skoh_antimi <- read_sas(paste0(sas_data_path, "skoh_antimi.sas7bdat"))
skoh_betalac <- read_sas(paste0(sas_data_path, "skoh_betalac.sas7bdat"))
skoh_macro <- read_sas(paste0(sas_data_path, "skoh_macro.sas7bdat"))
skoh_trimet <- read_sas(paste0(sas_data_path,  "skoh_trimet.sas7bdat"))
skoh_sulf <- read_sas(paste0(sas_data_path, "skoh_sulf.sas7bdat"))
skoh_quino <- read_sas(paste0(sas_data_path, "skoh_quino.sas7bdat"))
skoh_triaz <- read_sas(paste0(sas_data_path, "skoh_triaz.sas7bdat"))
skoh_hosp <- read_sas(paste0(sas_data_path, "skoh_hosp.sas7bdat"))

# Recode covariates ------------------------------------------------------------
tmpf <- function(tbl) {
  out <- tbl %>%
    mutate(
      rygning = factor(
        rygning, 
        levels = c(0, 1, 999), 
        labels = c("No", "Yes", "Missing")
      ),
      tot_aeld = factor(tot_aeld),
      mat_alder = factor(mat_alder),
      pat_alder = factor(pat_alder),
      # impute missing education to primary education
      hf_grp_far = factor(ifelse(hf_grp_far == "", "10", hf_grp_far)),
      hf_grp_mor = factor(ifelse(hf_grp_mor == "", "10", hf_grp_mor)),
      C_KON = factor(C_KON),
      gdrug_grp = factor(
        case_when(
          is.na(gdrug_grp) ~ "no_use",
          gdrug_grp == 0 ~ "0_1",
          gdrug_grp == 2 ~ "2_3",
          gdrug_grp == 4 ~ "4_5",
          gdrug_grp == 6 ~ "6_7",
          gdrug_grp == 8 ~ "8_9",
          gdrug_grp == 10 ~ "10_11",
          gdrug_grp == 12 ~ "12_13",
          gdrug_grp == 14 ~ "14_15",
          gdrug_grp == 16 ~ "16_17",
          gdrug_grp == 18 ~ "18_19",
          gdrug_grp == 20 ~ "20_21",
          gdrug_grp == 22 ~ "22_23",
          gdrug_grp == 24 ~ "24_25",
          gdrug_grp == 26 ~ "26_27",
          gdrug_grp == 28 ~ "28_29",
          gdrug_grp == 30 ~ "30_31",
          gdrug_grp == 32 ~ "32_33",
          gdrug_grp == 34 ~ "34_35",
          gdrug_grp == 36 ~ "36_37",
          gdrug_grp == 38 ~ "38_39",
          gdrug_grp == 40 ~ "40",
        )
      )
    )
  return(out)
}

koh_antimi <- tmpf(koh_antimi)
koh_betalac <- tmpf(koh_betalac) 
koh_macro <- tmpf(koh_macro)
koh_trimet <- tmpf(koh_trimet)
koh_sulf <- tmpf(koh_sulf)
koh_quino <- tmpf(koh_quino)
koh_triaz <- tmpf(koh_triaz)
skoh_antimi <- tmpf(skoh_antimi)
skoh_betalac <- tmpf(skoh_betalac) 
skoh_macro <- tmpf(skoh_macro)
skoh_trimet <- tmpf(skoh_trimet)
skoh_sulf <- tmpf(skoh_sulf)
skoh_quino <- tmpf(skoh_quino)
skoh_triaz <- tmpf(skoh_triaz)

tmpg <- function(tbl) {
  out <- tbl %>%
    mutate(
      rygning = factor(
        rygning, 
        levels = c(0, 1, 999), 
        labels = c("No", "Yes", "Missing")
      ),
      tot_aeld = factor(tot_aeld),
      mat_alder = factor(mat_alder),
      pat_alder = factor(pat_alder),
      # impute missing education to primary education
      hf_grp_far = factor(ifelse(hf_grp_far == "", "10", hf_grp_far)),
      hf_grp_mor = factor(ifelse(hf_grp_mor == "", "10", hf_grp_mor)),
      C_KON = factor(C_KON),
      ghosp_grp = case_when(
          is.na(ghosp_grp) ~ "no_visit",
          ghosp_grp == 0 ~ "0_1",
          ghosp_grp == 2 ~ "2_3",
          ghosp_grp == 4 ~ "4_5",
          ghosp_grp == 6 ~ "6_7",
          ghosp_grp == 8 ~ "8_9",
          ghosp_grp == 10 ~ "10_11",
          ghosp_grp == 12 ~ "12_13",
          ghosp_grp == 14 ~ "14_15",
          ghosp_grp == 16 ~ "16_17",
          ghosp_grp == 18 ~ "18_19",
          ghosp_grp == 20 ~ "20_21",
          ghosp_grp == 22 ~ "22_23",
          ghosp_grp == 24 ~ "24_25",
          ghosp_grp == 26 ~ "26_27",
          ghosp_grp == 28 ~ "28_29",
          ghosp_grp == 30 ~ "30_31",
          ghosp_grp == 32 ~ "32_33",
          ghosp_grp == 34 ~ "34_35",
          ghosp_grp == 36 ~ "36_37",
          ghosp_grp == 38 ~ "38_39",
          ghosp_grp == 40 ~ "40",
        )
    ) |>
    mutate(# only consider in-patient hospitalization. "_all" columns have other types as well.
      delta_hosp_all = delta_hosp,
      gest_hosp_dage_all = gest_hosp_dage,
      gest_hosp_all = gest_hosp,
      ghosp_grp_all = factor(ghosp_grp),
      delta_hosp = ifelse(C_PATTYPE %in% c(0, 1) | (C_PATTYPE == 2 & C_INDM == 1), delta_hosp, NA),
      gest_hosp_dage = ifelse(C_PATTYPE %in% c(0, 1) | (C_PATTYPE == 2 & C_INDM == 1), gest_hosp_dage, NA),
      gest_hosp = ifelse(C_PATTYPE %in% c(0, 1) | (C_PATTYPE == 2 & C_INDM == 1), gest_hosp, NA),
      ghosp_grp = factor(ifelse(C_PATTYPE %in% c(0, 1) | (C_PATTYPE == 2 & C_INDM == 1), ghosp_grp, "no_visit"))
    )
  return(out)
}

koh_hosp <- tmpg(koh_hosp)
skoh_hosp <- tmpg(skoh_hosp)

# restrict to term (week 37-41)
koh_antimi_term <- koh_antimi %>%
  lazy_dt() %>%
  group_by(sibs_id) %>%
  filter(all(value %in% 37:41)) %>%
  as_tibble()
skoh_antimi_term <- skoh_antimi %>%
  lazy_dt() %>%
  group_by(sibs_id) %>%
  filter(all(value %in% 37:41)) %>%
  as_tibble()
koh_betalac_term <- koh_betalac %>%
  lazy_dt() %>%
  group_by(sibs_id) %>%
  filter(all(value %in% 37:41)) %>%
  as_tibble()
skoh_betalac_term <- skoh_betalac %>%
  lazy_dt() %>%
  group_by(sibs_id) %>%
  filter(all(value %in% 37:41)) %>%
  as_tibble()

koh_hosp_term <- koh_hosp %>%
  lazy_dt() %>%
  group_by(sibs_id) %>%
  filter(all(value %in% 37:41)) %>%
  as_tibble()
skoh_hosp_term <- skoh_hosp %>%
  lazy_dt() %>%
  group_by(sibs_id) %>%
  filter(all(value %in% 37:41)) %>%
  as_tibble()


### Impute missing with 1st percentile z-scores ----------------------
#z-score for dk
lowest_1_percentile_dk <- quantile(koh_antimi$z_score_dk, probs = 0.01, na.rm = T)

#z-score for mat
lowest_1_percentile_mat <- quantile(koh_antimi$z_score_mat, probs = 0.01, na.rm = T)

#z-score for bpp
lowest_1_percentile_bpp <- quantile(skoh_antimi$z_sc_bpp, probs = 0.01, na.rm = T)

#imputerer 1% laveste z-score
koh_antimi_imp <- koh_antimi
skoh_antimi_imp <- skoh_antimi
koh_betalac_imp <- koh_betalac
skoh_betalac_imp <- skoh_betalac

#all antimicrobials
koh_antimi_imp <- koh_antimi_imp |>
  mutate(z_score_dk = replace_na(z_score_dk, lowest_1_percentile_dk))

koh_antimi_imp <- koh_antimi_imp |>
  mutate(z_score_mat = replace_na(z_score_mat, lowest_1_percentile_mat)) 

skoh_antimi_imp <- skoh_antimi_imp |>
  mutate(z_sc_bpp = replace_na(z_sc_bpp, lowest_1_percentile_bpp)) 

#all betalac
koh_betalac_imp <- koh_betalac_imp |>
  mutate(z_score_dk = replace_na(z_score_dk, lowest_1_percentile_dk))

koh_betalac_imp <- koh_betalac_imp |>
  mutate(z_score_mat = replace_na(z_score_mat, lowest_1_percentile_mat)) 

skoh_betalac_imp <- skoh_betalac_imp |>
  mutate(z_sc_bpp = replace_na(z_sc_bpp, lowest_1_percentile_bpp))

# Removing smoking from datasets
koh_antimi_imp <- select(koh_antimi_imp, -c("rygning"))

skoh_antimi_imp <- select(skoh_antimi_imp, -c("rygning"))

koh_betalac_imp <- select(koh_betalac_imp, -c("rygning"))

skoh_betalac_imp <- select(skoh_betalac_imp, -c("rygning"))

# Indicators of different antimicrobial prescriptions + hospitalization for infection ------
# table with pnr and number of hospitalizations
koh_n_hosp <- koh_hosp |>
  summarise(
    n_hosp = sum(ghosp_grp != "no_visit"),
    .by = "pnr"
  )
skoh_n_hosp <- skoh_hosp |>
  summarise(
    n_hosp = sum(ghosp_grp != "no_visit"),
    .by = "pnr"
  )

# create columns with indicators of antimicrobial prescriptions + hospitalization
data_prescrip <- map(
  list(koh_antimi, skoh_antimi),
  \(x) {
    x |>
      mutate(
        n_prescription = sum(ATC != ""),
        `Any beta-lactam` = sum(grepl("^J01[CD]", ATC)),
        `Beta-lactamase sensitive penicillin` = sum(grepl("^J01CE", ATC)),
        `Beta-lactamase resistant penicillin` = sum(grepl("^J01CF", ATC)),
        `Extended-spectrum penicillin` = sum(grepl("^J01CA", ATC)),
        `Cephalosporin` = sum(grepl("^J01D", ATC)),
        `Any macrolide` = sum(grepl("^J01FA", ATC)),
        `Azithromycin` = sum(grepl("^J01FA10", ATC)),
        `Erythromycin` = sum(grepl("^J01FA01", ATC)),
        `Roxythromycin` = sum(grepl("^J01FA06", ATC)),
        `Other macrolides` = sum(grepl("^J01FA0[29]", ATC)),
        `Trimethoprim` = sum(grepl("^J01EA", ATC)),
        `Sulfonamide` = sum(grepl("^J01EB", ATC)),
        `Quinolone` = sum(grepl("^J01MA", ATC)),
        `Triazole antifungal` = sum(grepl("^J02AC", ATC)),
        `Nucleotide/nucleoside-analogue` = sum(grepl("^J05AB", ATC)),
        .by = "pnr"
      ) |>
      select(!c(gest_drug_d, gest_drug, gdrug_grp, ATC)) |>
      distinct(pnr, .keep_all = TRUE) |>
      mutate(
        any_prescription = factor(ifelse(n_prescription > 0, "yes", "no")),
        n_prescription = factor(case_when(
          n_prescription == 0 ~ "Non",
          n_prescription == 1 ~ "One",
          n_prescription == 2 ~ "Two",
          n_prescription == 3 ~ "Three",
          n_prescription > 3 ~ "Four or more"
        ),
        levels = c("Non", "One", "Two", "Three", "Four or more")),
        `Any beta-lactam` = factor(ifelse(`Any beta-lactam` > 0, "yes", "no")),
        `Beta-lactamase sensitive penicillin` = factor(ifelse(`Beta-lactamase sensitive penicillin` > 0, "yes", "no")),
        `Beta-lactamase resistant penicillin` = factor(ifelse(`Beta-lactamase resistant penicillin` > 0, "yes", "no")),
        `Extended-spectrum penicillin` = factor(ifelse(`Extended-spectrum penicillin` > 0, "yes", "no")),
        `Cephalosporin` = factor(ifelse(`Cephalosporin` > 0, "yes", "no")),
        `Any macrolide` = factor(ifelse(`Any macrolide` > 0, "yes", "no")),
        `Azithromycin` = factor(ifelse(`Azithromycin` > 0, "yes", "no")),
        `Erythromycin` = factor(ifelse(`Erythromycin` > 0, "yes", "no")),
        `Roxythromycin` = factor(ifelse(`Roxythromycin` > 0, "yes", "no")),
        `Other macrolides` = factor(ifelse(`Other macrolides` > 0, "yes", "no")),
        `Trimethoprim` = factor(ifelse(`Trimethoprim` > 0, "yes", "no")),
        `Sulfonamide` = factor(ifelse(`Sulfonamide` > 0, "yes", "no")),
        `Quinolone` = factor(ifelse(`Quinolone` > 0, "yes", "no")),
        `Triazole antifungal` = factor(ifelse(`Triazole antifungal` > 0, "yes", "no")),
        `Nucleotide/nucleoside-analogue` = factor(ifelse(`Nucleotide/nucleoside-analogue` > 0, "yes", "no")),
        id = seq_len(n())
      ) |>
      left_join(
        koh_n_hosp,
        by = "pnr"
      ) |>
      replace_na(list(n_hosp = 0)) |>
      mutate(
        any_hosp = factor(ifelse(n_hosp > 0, "yes", "no")),
        n_hosp_cat = factor(case_when(
          n_hosp == 0 ~ "Non",
          n_hosp == 1 ~ "One",
          n_hosp > 1 ~ "Two or more"
        ),
        levels = c("Non", "One", "Two or more"))
      )
  }
)
koh_prescrip <- data_prescrip[[1]]
skoh_prescrip <- data_prescrip[[2]]

data_prescrip_imp <- map(
  list(koh_antimi_imp, skoh_antimi_imp),
  \(x) {
    x |>
      mutate(
        n_prescription = sum(ATC != ""),
        `Any beta-lactam` = sum(grepl("^J01[CD]", ATC)),
        `Beta-lactamase sensitive penicillin` = sum(grepl("^J01CE", ATC)),
        `Beta-lactamase resistant penicillin` = sum(grepl("^J01CF", ATC)),
        `Extended-spectrum penicillin` = sum(grepl("^J01CA", ATC)),
        `Cephalosporin` = sum(grepl("^J01D", ATC)),
        `Any macrolide` = sum(grepl("^J01FA", ATC)),
        `Azithromycin` = sum(grepl("^J01FA10", ATC)),
        `Erythromycin` = sum(grepl("^J01FA01", ATC)),
        `Roxythromycin` = sum(grepl("^J01FA06", ATC)),
        `Other macrolides` = sum(grepl("^J01FA0[29]", ATC)),
        `Trimethoprim` = sum(grepl("^J01EA", ATC)),
        `Sulfonamide` = sum(grepl("^J01EB", ATC)),
        `Quinolone` = sum(grepl("^J01MA", ATC)),
        `Triazole antifungal` = sum(grepl("^J02AC", ATC)),
        `Nucleotide/nucleoside-analogue` = sum(grepl("^J05AB", ATC)),
        .by = "pnr"
      ) |>
      select(!c(gest_drug_d, gest_drug, gdrug_grp, ATC)) |>
      distinct(pnr, .keep_all = TRUE) |>
      mutate(
        any_prescription = factor(ifelse(n_prescription > 0, "yes", "no")),
        n_prescription = factor(case_when(
          n_prescription == 0 ~ "Non",
          n_prescription == 1 ~ "One",
          n_prescription == 2 ~ "Two",
          n_prescription == 3 ~ "Three",
          n_prescription > 3 ~ "Four or more"
        ),
        levels = c("Non", "One", "Two", "Three", "Four or more")),
        `Any beta-lactam` = factor(ifelse(`Any beta-lactam` > 0, "yes", "no")),
        `Beta-lactamase sensitive penicillin` = factor(ifelse(`Beta-lactamase sensitive penicillin` > 0, "yes", "no")),
        `Beta-lactamase resistant penicillin` = factor(ifelse(`Beta-lactamase resistant penicillin` > 0, "yes", "no")),
        `Extended-spectrum penicillin` = factor(ifelse(`Extended-spectrum penicillin` > 0, "yes", "no")),
        `Cephalosporin` = factor(ifelse(`Cephalosporin` > 0, "yes", "no")),
        `Any macrolide` = factor(ifelse(`Any macrolide` > 0, "yes", "no")),
        `Azithromycin` = factor(ifelse(`Azithromycin` > 0, "yes", "no")),
        `Erythromycin` = factor(ifelse(`Erythromycin` > 0, "yes", "no")),
        `Roxythromycin` = factor(ifelse(`Roxythromycin` > 0, "yes", "no")),
        `Other macrolides` = factor(ifelse(`Other macrolides` > 0, "yes", "no")),
        `Trimethoprim` = factor(ifelse(`Trimethoprim` > 0, "yes", "no")),
        `Sulfonamide` = factor(ifelse(`Sulfonamide` > 0, "yes", "no")),
        `Quinolone` = factor(ifelse(`Quinolone` > 0, "yes", "no")),
        `Triazole antifungal` = factor(ifelse(`Triazole antifungal` > 0, "yes", "no")),
        `Nucleotide/nucleoside-analogue` = factor(ifelse(`Nucleotide/nucleoside-analogue` > 0, "yes", "no")),
        id = seq_len(n())
      ) |>
      left_join(
        koh_n_hosp,
        by = "pnr"
      ) |>
      replace_na(list(n_hosp = 0)) |>
      mutate(
        any_hosp = factor(ifelse(n_hosp > 0, "yes", "no")),
        n_hosp_cat = factor(case_when(
          n_hosp == 0 ~ "Non",
          n_hosp == 1 ~ "One",
          n_hosp > 1 ~ "Two or more"
        ),
        levels = c("Non", "One", "Two or more"))
      )
  }
)
koh_prescrip_imp <- data_prescrip_imp[[1]]
skoh_prescrip_imp <- data_prescrip_imp[[2]]

# Save analysis_data in rds format ---------------------------------------------
write_rds(koh_antimi, paste0(data_path, "koh_antimi.rdata"))
write_rds(koh_betalac, paste0(data_path, "koh_betalac.rdata"))
write_rds(koh_macro, paste0(data_path, "koh_macro.rdata"))
write_rds(koh_trimet, paste0(data_path, "koh_trimet.rdata"))
write_rds(koh_sulf, paste0(data_path, "koh_sulf.rdata"))
write_rds(koh_quino, paste0(data_path, "koh_quino.rdata"))
write_rds(koh_triaz, paste0(data_path, "koh_triaz.rdata"))
write_rds(koh_prescrip, paste0(data_path, "koh_prescrip.rdata"))

write_rds(skoh_antimi, paste0(data_path, "skoh_antimi.rdata"))
write_rds(skoh_betalac, paste0(data_path, "skoh_betalac.rdata"))
write_rds(skoh_macro, paste0(data_path, "skoh_macro.rdata"))
write_rds(skoh_trimet, paste0(data_path, "skoh_trimet.rdata"))
write_rds(skoh_sulf, paste0(data_path, "skoh_sulf.rdata"))
write_rds(skoh_quino, paste0(data_path, "skoh_quino.rdata"))
write_rds(skoh_triaz, paste0(data_path, "skoh_triaz.rdata"))
write_rds(skoh_prescrip, paste0(data_path, "skoh_prescrip.rdata"))

write_rds(koh_hosp, paste0(data_path, "koh_hosp.rdata"))
write_rds(skoh_hosp, paste0(data_path, "skoh_hosp.rdata"))

write_rds(koh_antimi_term, paste0(data_path, "koh_antimi_term.rdata"))
write_rds(skoh_antimi_term, paste0(data_path, "skoh_antimi_term.rdata"))
write_rds(koh_betalac_term, paste0(data_path, "koh_betalac_term.rdata"))
write_rds(skoh_betalac_term, paste0(data_path, "skoh_betalac_term.rdata"))

write_rds(koh_hosp_term, paste0(data_path, "koh_hosp_term.rdata"))
write_rds(skoh_hosp_term, paste0(data_path, "skoh_hosp_term.rdata"))

write_rds(koh_antimi_imp, paste0(data_path, "koh_antimi_imp.rdata"))
write_rds(skoh_antimi_imp, paste0(data_path, "skoh_antimi_imp.rdata"))

write_rds(koh_betalac_imp, paste0(data_path, "koh_betalac_imp.rdata"))
write_rds(skoh_betalac_imp, paste0(data_path, "skoh_betalac_imp.rdata"))

write_rds(koh_prescrip_imp, paste0(data_path, "koh_prescrip_imp.rdata"))
write_rds(skoh_prescrip_imp, paste0(data_path, "skoh_prescrip_imp.rdata"))

# Cleanup ----------------------------------------------------------------------
rm(list = ls())
gc()
