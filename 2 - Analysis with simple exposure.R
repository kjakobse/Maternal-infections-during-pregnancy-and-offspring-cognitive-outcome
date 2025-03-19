# Run data analysis with indicators of different antimicrobial prescriptions as exposure.
# Analyses include both linear models with no adjustment for shared family factors as 
# well as a sibling adjusted linear model with adjustment for shared family factors.

# setup  ========================================================================
data_path <- "PATH"
result_path <- "PATH"
library(tidyverse)
library(EpiForsk)

# Import data ==================================================================
koh_prescrip <- read_rds(paste0(data_path, "koh_prescrip.Rdata"))
skoh_prescrip <- read_rds(paste0(data_path, "skoh_prescrip.Rdata"))
koh_prescrip_imp <- read_rds(paste0(data_path, "koh_prescrip_imp.Rdata"))
skoh_prescrip_imp <- read_rds(paste0(data_path, "skoh_prescrip_imp.Rdata"))

# run analyses =================================================================
# sibling adjusted analysis
lms_results <- pmap(
  list(
    var = rep(
      c(
        "any_prescription", "n_prescription", "`Any beta-lactam`",
        "`Beta-lactamase sensitive penicillin`", "`Beta-lactamase resistant penicillin`",
        "`Extended-spectrum penicillin`", "Cephalosporin", "`Any macrolide`",
        "Azithromycin", "Erythromycin", "Roxythromycin", "`Other macrolides`", 
        "Trimethoprim", "Sulfonamide", "Quinolone", 
        "`Triazole antifungal`", "`Nucleotide/nucleoside-analogue`",
        "any_hosp", "n_hosp_cat"
      ), 
      times = 3
    ),
    data = c(rep(list(koh_prescrip), 38), rep(list(skoh_prescrip), 19)),
    outcome = rep(c("z_score_dk", "z_score_mat", "z_sc_bpp") , each = 19)
  ),
  \(var, data, outcome) {
    out <- lms(
      formula = formula(
        str_c(
          outcome,
          "~",
          "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
          var
        )
      ),
      data = data,
      grp_id = sibs_id,
      obs_id = id
    )
    cat("model fit for '", var, "' and '", outcome, "'\n", sep = "")
    return(out)
  }
)

# ordinary lm without sibling adjustment for comparison
lm_results <- pmap(
  list(
    var = rep(
      c(
        "any_prescription", "n_prescription", "`Any beta-lactam`",
        "`Beta-lactamase sensitive penicillin`", "`Beta-lactamase resistant penicillin`",
        "`Extended-spectrum penicillin`", "Cephalosporin", "`Any macrolide`",
        "Azithromycin", "Erythromycin", "Roxythromycin", "`Other macrolides`", 
        "Trimethoprim", "Sulfonamide", "Quinolone", 
        "`Triazole antifungal`", "`Nucleotide/nucleoside-analogue`",
        "any_hosp", "n_hosp_cat"
      ), 
      times = 3
    ),
    data = c(rep(list(koh_prescrip), 38), rep(list(skoh_prescrip), 19)),
    outcome = rep(c("z_score_dk", "z_score_mat", "z_sc_bpp") , each = 19)
  ),
  \(var, data, outcome) {
    out <- lm(
      formula = formula(
        str_c(
          outcome,
          "~",
          "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
          var
        )
      ),
      data = data
    )
    cat("model fit for '", var, "' and '", outcome, "'\n", sep = "")
    return(out)
  }
)

# sibling adjusted analysis with missing outcome inputed to 1st percentile
lms_results_imp <- pmap(
  list(
    var = rep(
      c(
        "any_prescription", "n_prescription", "`Any beta-lactam`",
        "`Beta-lactamase sensitive penicillin`", "`Beta-lactamase resistant penicillin`",
        "`Extended-spectrum penicillin`", "Cephalosporin", "`Any macrolide`",
        "Azithromycin", "Erythromycin", "Roxythromycin", "`Other macrolides`", 
        "Trimethoprim", "Sulfonamide", "Quinolone", 
        "`Triazole antifungal`", "`Nucleotide/nucleoside-analogue`",
        "any_hosp", "n_hosp_cat"
      ), 
      times = 3
    ),
    data = c(rep(list(koh_prescrip_imp), 38), rep(list(skoh_prescrip_imp), 19)),
    outcome = rep(c("z_score_dk", "z_score_mat", "z_sc_bpp") , each = 19)
  ),
  \(var, data, outcome) {
    out <- lms(
      formula = formula(
        str_c(
          outcome,
          "~",
          "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
          var
        )
      ),
      data = data,
      grp_id = sibs_id,
      obs_id = id
    )
    cat("model fit for '", var, "' and '", outcome, "'\n", sep = "")
    return(out)
  }
)

# ordinary lm without sibling adjustment for comparison with missing outcome inputed to 1st percentile
lm_results_imp <- pmap(
  list(
    var = rep(
      c(
        "any_prescription", "n_prescription", "`Any beta-lactam`",
        "`Beta-lactamase sensitive penicillin`", "`Beta-lactamase resistant penicillin`",
        "`Extended-spectrum penicillin`", "Cephalosporin", "`Any macrolide`",
        "Azithromycin", "Erythromycin", "Roxythromycin", "`Other macrolides`", 
        "Trimethoprim", "Sulfonamide", "Quinolone", 
        "`Triazole antifungal`", "`Nucleotide/nucleoside-analogue`",
        "any_hosp", "n_hosp_cat"
      ), 
      times = 3
    ),
    data = c(rep(list(koh_prescrip_imp), 38), rep(list(skoh_prescrip_imp), 19)),
    outcome = rep(c("z_score_dk", "z_score_mat", "z_sc_bpp") , each = 19)
  ),
  \(var, data, outcome) {
    out <- lm(
      formula = formula(
        str_c(
          outcome,
          "~",
          "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
          var
        )
      ),
      data = data
    )
    cat("model fit for '", var, "' and '", outcome, "'\n", sep = "")
    return(out)
  }
)

# collect results ==============================================================
lms_tidy <- map(
  lms_results,
  \(x) broom::tidy(x, conf.int = TRUE, conf.level = 0.95)
)
lm_tidy <- map(
  lm_results,
  \(x) broom::tidy(x, conf.int = TRUE, conf.level = 0.95)
)
lms_tidy_imp <- map(
  lms_results_imp,
  \(x) broom::tidy(x, conf.int = TRUE, conf.level = 0.95)
)
lm_tidy_imp <- map(
  lm_results_imp,
  \(x) broom::tidy(x, conf.int = TRUE, conf.level = 0.95)
)

# Save results =================================================================
write_rds(lms_tidy, paste0(result_path, "simple_exposure_lms.Rdata"))
write_rds(lm_tidy, paste0(result_path, "simple_exposure_lm.Rdata"))
write_rds(lms_tidy_imp, paste0(result_path, "simple_exposure_lms_imp.Rdata"))
write_rds(lm_tidy_imp, paste0(result_path, "simple_exposure_lm_imp.Rdata"))

# cleanup ======================================================================
rm(list = ls())
gc()
