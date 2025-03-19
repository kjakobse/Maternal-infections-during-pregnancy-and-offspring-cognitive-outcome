# data_path       chr:   Vector with two paths pointing to the rdata needed for 
#                        the analysis. The first path must point to the full 
#                        sibling cohort and the second path to the brother cohort.
# knot_quantiles  list:  a named list of length two with names "koh", and 
#                        "skoh", containing the vector of quantiles to be used 
#                        when determining the knots of the natural cubic spline.
# alpha           dbl:   passed on to CalculateSplineConfidenceInterval.
# par             chr:   passed on to CalculateSplineConfidenceInterval.
# hosp            lgl:   TRUE if the exposure is admission to hospital with infection.
# cat             lgl:   TRUE to return results with categorical exposure.
# ns              lgl:   TRUE to return results with exposure spline expansion.
# sensitivity     lgl:   TRUE to return results for beta lowest z-score.
# beta            dbl:   In [0,1]. When sensitivity is TRUE, compare beta 
#                        lowest z-scores to 1-beta highest z-scores.
# exponentiate    lgl:   If TRUE, return odds ratios from sensitivity analysis.
#                        If FALSE, return log-odds ratios.
# knot_mat        int:   The right boundary knot of the splines.
# exp_max         int:   Largest day to include in output analysis results.
#                        days are assumed to go from 0 to at least exp_mat with
#                        data for each day.
# include_smoking lgl:   TRUE to include smoking in the covariates.

RunlmsSplineAnalysis <- function(
    data_paths,
    knot_quantiles,
    alpha = 0.05,
    par = c("reduced", "all"),
    hosp = FALSE,
    cat = TRUE,
    ns = TRUE,
    sensitivity = FALSE,
    beta = 0.05,
    exponentiate = TRUE,
    knot_max = 310L,
    exp_max = 294L,
    include_smoking = FALSE,
    verbose = TRUE
) {
  # pick first element of par
  par <- match.arg(par)
  
  # import datasets ------------------------------------------------------------
  koh <- readr::read_rds(data_paths[1])
  skoh <- readr::read_rds(data_paths[2])

  # define days of gestation at outcome (hospitalization or prescription) ------
  if (hosp) {
    koh <- mutate(koh, gest_age_days = gest_hosp_dage)
    skoh <- mutate(skoh, gest_age_days = gest_hosp_dage)
  } else {
    koh <- mutate(koh, gest_age_days = gest_drug_d)
    skoh <- mutate(skoh, gest_age_days = gest_drug_d)
  }
  
  # Generate spline basis matrices ---------------------------------------------
  if (ns) {
    koh_spline <- splines::ns(
      koh$gest_age_days, 
      knots = quantile(koh$gest_age_days, knot_quantiles[["koh"]], na.rm = TRUE), 
      Boundary.knots = c(0, knot_max)
    )
    skoh_spline <- splines::ns(
      skoh$gest_age_days, 
      knots = quantile(skoh$gest_age_days, knot_quantiles[["skoh"]], na.rm = TRUE), 
      Boundary.knots = c(0, knot_max)
    )
    koh_spline_num <- koh_spline
    skoh_spline_num <- skoh_spline
    attributes(koh_spline_num) <- list(dim = dim(koh_spline))
    attributes(skoh_spline_num) <- list(dim = dim(skoh_spline))
  }
  
  # Data for categorical analyses ----------------------------------------------
  if (cat) {
    if (hosp) {
      koh_cat <- koh %>%
        dplyr::select(-c(gest_hosp_dage, gest_age_days, gest_hosp)) %>%
        dplyr::count(dplyr::across(dplyr::everything()), name = "tmp") %>%
        dplyr::mutate(exp_grp = stringr::str_c("exp_", ghosp_grp)) %>%
        dplyr::select(-ghosp_grp) %>%
        dplyr::group_by(pnr) %>%
        tidyr::pivot_wider(
          names_from = exp_grp,
          values_from = tmp,
          values_fill = 0
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          id = seq_len(n())
        )
      skoh_cat <- skoh %>%
        dplyr::select(-c(gest_hosp_dage, gest_age_days, gest_hosp)) %>%
        dplyr::count(dplyr::across(dplyr::everything()), name = "tmp") %>%
        dplyr::mutate(exp_grp = stringr::str_c("exp_", ghosp_grp)) %>%
        dplyr::select(-ghosp_grp) %>%
        dplyr::group_by(pnr) %>%
        tidyr::pivot_wider(
          names_from = exp_grp,
          values_from = tmp,
          values_fill = 0
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          id = seq_len(n())
        )
    } else {
      koh_cat <- koh %>%
        dplyr::select(-c(gest_drug_d, gest_age_days, gest_drug, ATC)) %>%
        dplyr::count(dplyr::across(dplyr::everything()), name = "tmp") %>%
        dplyr::mutate(exp_grp = stringr::str_c("exp_", gdrug_grp)) %>%
        dplyr::select(-gdrug_grp) %>%
        dplyr::group_by(pnr) %>%
        tidyr::pivot_wider(
          names_from = exp_grp,
          values_from = tmp,
          values_fill = 0
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          id = seq_len(n())
        )
      skoh_cat <- skoh %>%
        dplyr::select(-c(gest_drug_d, gest_age_days, gest_drug, ATC)) %>%
        dplyr::count(dplyr::across(dplyr::everything()), name = "tmp") %>%
        dplyr::mutate(exp_grp = stringr::str_c("exp_", gdrug_grp)) %>%
        dplyr::select(-gdrug_grp) %>%
        dplyr::group_by(pnr) %>%
        tidyr::pivot_wider(
          names_from = exp_grp,
          values_from = tmp,
          values_fill = 0
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          id = seq_len(n())
        )
    }
  }
  
  # Data for spline analyses ---------------------------------------------------
  if (ns) {
    ns_list_koh <- list()
    for(i in seq_len(length(knot_quantiles[["koh"]]) + 1)) {
      ns_list_koh <- c(
        ns_list_koh,
        rlang::list2(stringr::str_c("exp_ns_", i))
      )
    }
    ns_list_koh <- dplyr::syms(ns_list_koh)
    koh_ns <- dplyr::bind_cols(
      koh,
      dplyr::as_tibble(koh_spline_num, .name_repair = "minimal") %>%
        structure(
          names = stringr::str_c("exp_ns_", seq_len(length(knot_quantiles[["koh"]]) + 1))
        )
    )
    for(i in seq_along(ns_list_koh)) {
      koh_ns <- koh_ns %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(pnr) %>%
        dplyr::mutate(!!ns_list_koh[[i]] := sum(!!ns_list_koh[[i]])) %>%
        dplyr::as_tibble()
    }
    koh_ns <- koh_ns %>%
      dplyr::distinct(pnr, .keep_all = TRUE) %>%
      dplyr::mutate(
        exp_used = dplyr::case_when(is.na(gest_age_days) ~ 0, TRUE ~ 1),
        id = seq_len(n())
      )
    for(i in seq_len(length(knot_quantiles[["koh"]]) + 1)) {
      koh_ns[[stringr::str_c("exp_ns_", i)]] <- koh_ns[[stringr::str_c("exp_ns_", i)]] %>%
        tidyr::replace_na(0)
    }
    
    ns_list_skoh <- list()
    for(i in seq_len(length(knot_quantiles[["skoh"]]) + 1)) {
      ns_list_skoh <- c(
        ns_list_skoh,
        rlang::list2(stringr::str_c("exp_ns_", i))
      )
    }
    ns_list_skoh <- dplyr::syms(ns_list_skoh)
    skoh_ns <- dplyr::bind_cols(
      skoh,
      dplyr::as_tibble(skoh_spline_num, .name_repair = "minimal") %>%
        structure(
          names = str_c("exp_ns_", seq_len(length(knot_quantiles[["skoh"]]) + 1))
        )
    )
    for(i in seq_along(ns_list_skoh)) {
      skoh_ns <- skoh_ns %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(pnr) %>%
        dplyr::mutate(!!ns_list_skoh[[i]] := sum(!!ns_list_skoh[[i]])) %>%
        dplyr::as_tibble()
    }
    skoh_ns <- skoh_ns %>%
      dplyr::distinct(pnr, .keep_all = TRUE) %>%
      dplyr::mutate(
        exp_used = dplyr::case_when(is.na(gest_age_days) ~ 0, TRUE ~ 1),
        id = seq_len(n())
      )
    for(i in seq_len(length(knot_quantiles[["skoh"]]) + 1)) {
      skoh_ns[[stringr::str_c("exp_ns_", i)]] <- skoh_ns[[stringr::str_c("exp_ns_", i)]] %>%
        tidyr::replace_na(0)
    }
  }
  
  # linear model with 2-week categories ----------------------------------------
  # no use is reference for drug use
  # no visit is reference for hospital visits
  koh_cat_exp <- stringr::str_subset(
    names(koh_cat), 
    "^exp_\\d"
  )
  koh_cat_exp[stringr::str_detect(koh_cat_exp, "\\+$")] <- stringr::str_c(
    "`", 
    koh_cat_exp[stringr::str_detect(koh_cat_exp, "\\+$")], 
    "`"
  )
  skoh_cat_exp <- stringr::str_subset(
    names(skoh_cat), 
    "^exp_\\d"
  )
  skoh_cat_exp[stringr::str_detect(skoh_cat_exp, "\\+$")] <- stringr::str_c(
    "`", 
    skoh_cat_exp[stringr::str_detect(skoh_cat_exp, "\\+$")], 
    "`"
  )
  if (cat) {
    if (include_smoking) {
      lms_dan_cat <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_score_dk ~",
            "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
            stringr::str_c(koh_cat_exp, collapse = " + ")
          )
        ),
        data = koh_cat,
        grp_id = sibs_id,
        obs_id = id
      )
      lms_mat_cat <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_score_mat ~",
            "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
            stringr::str_c(koh_cat_exp, collapse = " + ")
          )
        ),
        data = koh_cat,
        grp_id = sibs_id,
        obs_id = id
      )
      lms_bpp_cat <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_sc_bpp ~",
            "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
            stringr::str_c(skoh_cat_exp, collapse = " + ")
          )
        ),
        data = skoh_cat,
        grp_id = sibs_id,
        obs_id = id
      )
    } else {
      lms_dan_cat <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_score_dk ~",
            "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
            stringr::str_c(koh_cat_exp, collapse = " + ")
          )
        ),
        data = koh_cat,
        grp_id = sibs_id,
        obs_id = id
      )
      lms_mat_cat <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_score_mat ~",
            "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
            stringr::str_c(koh_cat_exp, collapse = " + ")
          )
        ),
        data = koh_cat,
        grp_id = sibs_id,
        obs_id = id
      )
      lms_bpp_cat <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_sc_bpp ~",
            "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
            stringr::str_c(skoh_cat_exp, collapse = " + ")
          )
        ),
        data = skoh_cat,
        grp_id = sibs_id,
        obs_id = id
      )
    }
    lms_dan_cat_z_score_diff <- lms_dan_cat %>%
      broom::tidy(conf.int = TRUE, conf.level = 0.95) %>%
      dplyr::filter(stringr::str_detect(term, "exp")) %>%
      dplyr::mutate(
        gest_age_days = 7 + 
          as.numeric(stringr::str_sub(stringr::str_extract(term, "exp_\\d+"), 5, -1)) * 7 
      ) %>%
      dplyr::arrange(gest_age_days)
    lms_mat_cat_z_score_diff <- lms_mat_cat %>%
      broom::tidy(conf.int = TRUE, conf.level = 0.95) %>%
      dplyr::filter(stringr::str_detect(term, "exp")) %>%
      dplyr::mutate(
        gest_age_days = 7 + 
          as.numeric(stringr::str_sub(stringr::str_extract(term, "exp_\\d+"), 5, -1)) * 7 
      ) %>%
      dplyr::arrange(gest_age_days)
    lms_bpp_cat_z_score_diff <- lms_bpp_cat %>%
      broom::tidy(conf.int = TRUE, conf.level = 0.95) %>%
      dplyr::filter(stringr::str_detect(term, "exp")) %>%
      dplyr::mutate(
        gest_age_days = 7 + 
          as.numeric(stringr::str_sub(stringr::str_extract(term, "exp_\\d+"), 5, -1)) * 7 
      ) %>%
      dplyr::arrange(gest_age_days)
  }
  # linear model with spline ---------------------------------------------------
  # linear model fit conditioning on sibling id
  if (ns) {
    spline_terms_koh <- stringr::str_c(
      "exp_ns_", 
      seq_len(length(knot_quantiles[["koh"]]) + 1)
    )
    spline_terms_skoh <- stringr::str_c(
      "exp_ns_", 
      seq_len(length(knot_quantiles[["skoh"]]) + 1)
    )
    if (include_smoking) {
      lms_dan_ns <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_score_dk ~ ",
            "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
            stringr::str_c(spline_terms_koh, collapse = " + "),
            " + exp_used"
          )
        ),
        data = koh_ns,
        grp_id = sibs_id,
        obs_id = id
      )
      lms_mat_ns <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_score_mat ~ ",
            "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
            stringr::str_c(spline_terms_koh, collapse = " + "),
            " + exp_used"
          )
        ),
        data = koh_ns,
        grp_id = sibs_id,
        obs_id = id
      )
      lms_bpp_ns <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_sc_bpp ~ ",
            "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
            stringr::str_c(spline_terms_skoh, collapse = " + "),
            " + exp_used"
          )
        ),
        data = skoh_ns,
        grp_id = sibs_id,
        obs_id = id
      )
    } else {
      lms_dan_ns <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_score_dk ~ ",
            "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
            stringr::str_c(spline_terms_koh, collapse = " + "),
            " + exp_used"
          )
        ),
        data = koh_ns,
        grp_id = sibs_id,
        obs_id = id
      )
      lms_mat_ns <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_score_mat ~ ",
            "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
            stringr::str_c(spline_terms_koh, collapse = " + "),
            " + exp_used"
          )
        ),
        data = koh_ns,
        grp_id = sibs_id,
        obs_id = id
      )
      lms_bpp_ns <- EpiForsk::lms(
        formula = formula(
          stringr::str_c(
            "z_sc_bpp ~ ",
            "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
            stringr::str_c(spline_terms_skoh, collapse = " + "),
            " + exp_used"
          )
        ),
        data = skoh_ns,
        grp_id = sibs_id,
        obs_id = id
      )
    }
    # predict spline basis for each day
    dan_ns_pred <- predict(
      koh_spline, 
      seq(
        0L,
        knot_max
      )
    )
    mat_ns_pred <- predict(
      koh_spline, 
      seq(
        0L, 
        knot_max
      )
    )
    bpp_ns_pred <- predict(
      skoh_spline, 
      seq(
        0L, 
        knot_max
      )
    )
    # extract estimated parameters for spline basis columns
    lms_dan_ns_coef <- matrix(
      lms_dan_ns$coefficients[
        which(stringr::str_detect(names(lms_dan_ns$coefficients), "exp_"))
      ], 
      nrow = sum(stringr::str_detect(names(lms_dan_ns$coefficients), "exp_"))
    )
    lms_mat_ns_coef <- matrix(
      lms_mat_ns$coefficients[
        which(stringr::str_detect(names(lms_mat_ns$coefficients), "exp_"))
      ], 
      nrow = sum(stringr::str_detect(names(lms_mat_ns$coefficients), "exp_"))
    )
    lms_bpp_ns_coef <- matrix(
      lms_bpp_ns$coefficients[
        which(stringr::str_detect(names(lms_bpp_ns$coefficients), "exp_"))
      ], 
      nrow = sum(stringr::str_detect(names(lms_bpp_ns$coefficients), "exp_"))
    )
    # calculate difference in z-score between no use and use on day x 
    lms_dan_ns_z_score_diff <- dplyr::tibble(
      gest_age_days = seq(0, knot_max),
      z_score_diff = as.numeric(
        cbind(
          dan_ns_pred, 
          rep(1, nrow(dan_ns_pred))
        ) %*% 
          lms_dan_ns_coef
      )
    )
    lms_mat_ns_z_score_diff <- dplyr::tibble(
      gest_age_days = seq(0, knot_max),
      z_score_diff = as.numeric(
        cbind(
          mat_ns_pred, 
          rep(1, nrow(mat_ns_pred))
        ) %*% 
          lms_mat_ns_coef
      )
    )
    lms_bpp_ns_z_score_diff <- dplyr::tibble(
      gest_age_days = seq(0, knot_max),
      z_score_diff = as.numeric(
        cbind(
          bpp_ns_pred, 
          rep(1, nrow(bpp_ns_pred))
        ) %*% 
          lms_bpp_ns_coef
      )
    )
    # Calculate confidence set for estimated spline curve ----------------------
    dan_ns_ci_data <- CalculateSplineConfidenceInterval(
      spline_mod = lms_dan_ns,
      spline = koh_spline,
      ns_z_score_diff = lms_dan_ns_z_score_diff,
      alpha = alpha,
      par = par,
      verbose = verbose
    )
    mat_ns_ci_data <- CalculateSplineConfidenceInterval(
      spline_mod = lms_mat_ns,
      spline = koh_spline,
      ns_z_score_diff = lms_mat_ns_z_score_diff,
      alpha = alpha,
      par = par,
      verbose = verbose
    )
    bpp_ns_ci_data <- CalculateSplineConfidenceInterval(
      spline_mod = lms_bpp_ns,
      spline = skoh_spline,
      ns_z_score_diff = lms_bpp_ns_z_score_diff,
      alpha = alpha,
      par = par,
      verbose = verbose
    )
  }
  # Create output data ---------------------------------------------------------
  dan_data <- list(
    ns_data = if (ns) {
      dan_ns_ci_data %>%
        dplyr::rename(
          "day" = X,
          "estimate" = Y_hat,
          "conf.low" = ci_bound_min,
          "conf.high" = ci_bound_max
        ) %>%
        dplyr::slice(1:(exp_max + 1))
    } else {
      NULL
    },
    cat_data = if(cat) {
      lms_dan_cat_z_score_diff %>%
        dplyr::select(term, gest_age_days, estimate, conf.low, conf.high) %>%
        dplyr::mutate(
          day = gest_age_days,
          grp = ExtractWeek(., term),
          exposure = if(hosp) "hosp" else "drug"
        ) %>%
        dplyr::select(exposure, day, grp, estimate, conf.low, conf.high)
    } else {
      NULL
    }
  )
  mat_data <- list(
    ns_data = if (ns) {
      mat_ns_ci_data %>%
        dplyr::rename(
          "day" = X,
          "estimate" = Y_hat,
          "conf.low" = ci_bound_min,
          "conf.high" = ci_bound_max
        ) %>%
        dplyr::slice(1:(exp_max + 1))
    } else {
      NULL
    },
    cat_data = if(cat) {
      lms_mat_cat_z_score_diff %>%
        dplyr::select(term, gest_age_days, estimate, conf.low, conf.high) %>%
        dplyr::mutate(
          day = gest_age_days,
          grp = ExtractWeek(., term),
          exposure = if(hosp) "hosp" else "drug"
        ) %>%
        dplyr::select(exposure, day, grp, estimate, conf.low, conf.high)
    } else {
      NULL
    }
  )
  bpp_data <- list(
    ns_data = if (ns) {
      bpp_ns_ci_data %>%
        dplyr::rename(
          "day" = X,
          "estimate" = Y_hat,
          "conf.low" = ci_bound_min,
          "conf.high" = ci_bound_max
        ) %>%
        dplyr::slice(1:(exp_max + 1))
    } else {
      NULL
    },
    cat_data = if(cat) {
      lms_bpp_cat_z_score_diff %>%
        dplyr::select(term, gest_age_days, estimate, conf.low, conf.high) %>%
        dplyr::mutate(
          day = gest_age_days,
          grp = ExtractWeek(., term),
          exposure = if(hosp) "hosp" else "drug"
        ) %>%
        dplyr::select(exposure, day, grp, estimate, conf.low, conf.high)
    } else {
      NULL
    }
  )
  
  # sensitivity analysis with beta lowest z-score ------------------------------
  if (sensitivity) {
    # add binary indicator of lowest  z_score
    if (cat) {
      koh_cat <- koh_cat %>%
        dplyr::mutate(
          z_score_dk_bin = (z_score_dk < qnorm(beta)),
          z_score_mat_bin = (z_score_mat < qnorm(beta))
        )
      skoh_cat <- skoh_cat %>%
        dplyr::mutate(
          z_sc_bpp_bin = (z_sc_bpp < qnorm(beta))
        )
    }
    if (ns) {
      koh_ns <- koh_ns %>%
        dplyr::mutate(
          z_score_dk_bin = (z_score_dk < qnorm(beta)),
          z_score_mat_bin = (z_score_mat < qnorm(beta))
        )
      skoh_ns <- skoh_ns %>%
        dplyr::mutate(
          z_sc_bpp_bin = (z_sc_bpp < qnorm(beta))
        )
    }
    
    if (cat) {
      # conditional logistic regression analysis
      if (include_smoking) {
        clogit_dan_cat <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_score_dk_bin ~",
              "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
              stringr::str_c(koh_cat_exp, collapse = " + "),
              " + strata(sibs_id)"
            )
          ),
          data = koh_cat,
          method = "efron"
        )
        clogit_mat_cat <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_score_mat_bin ~",
              "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
              stringr::str_c(koh_cat_exp, collapse = " + "),
              " + strata(sibs_id)"
            )
          ),
          data = koh_cat,
          method = "efron"
        )
        clogit_bpp_cat <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_sc_bpp_bin ~",
              "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
              stringr::str_c(skoh_cat_exp, collapse = " + "),
              " + strata(sibs_id)"
            )
          ),
          data = skoh_cat,
          method = "efron"
        )
      } else {
        clogit_dan_cat <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_score_dk_bin ~",
              "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
              stringr::str_c(koh_cat_exp, collapse = " + "),
              " + strata(sibs_id)"
            )
          ),
          data = koh_cat,
          method = "efron"
        )
        clogit_mat_cat <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_score_mat_bin ~",
              "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
              stringr::str_c(koh_cat_exp, collapse = " + "),
              " + strata(sibs_id)"
            )
          ),
          data = koh_cat,
          method = "efron"
        )
        clogit_bpp_cat <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_sc_bpp_bin ~",
              "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far +",
              stringr::str_c(skoh_cat_exp, collapse = " + "),
              " + strata(sibs_id)"
            )
          ),
          data = skoh_cat,
          method = "efron"
        )
      }
      # tidy confidence intervals
      clogit_dan_cat_z_score_odds <- clogit_dan_cat %>%
        broom::tidy(
          exponentiate = exponentiate, 
          conf.int = TRUE, 
          conf.level = 1 - alpha
        ) %>%
        dplyr::filter(stringr::str_detect(term, "exp")) %>%
        dplyr::mutate(
          gest_age_days = 7 + 
            as.numeric(stringr::str_sub(stringr::str_extract(term, "exp_\\d+"), 5, -1)) * 7 
        ) %>%
        dplyr::arrange(gest_age_days)
      clogit_mat_cat_z_score_odds <- clogit_mat_cat %>%
        broom::tidy(
          exponentiate = exponentiate, 
          conf.int = TRUE, 
          conf.level = 1 - alpha
        ) %>%
        dplyr::filter(stringr::str_detect(term, "exp")) %>%
        dplyr::mutate(
          gest_age_days = 7 + 
            as.numeric(stringr::str_sub(stringr::str_extract(term, "exp_\\d+"), 5, -1)) * 7 
        ) %>%
        dplyr::arrange(gest_age_days)
      clogit_bpp_cat_z_score_odds <- clogit_bpp_cat %>%
        broom::tidy(
          exponentiate = exponentiate, 
          conf.int = TRUE, 
          conf.level = 1 - alpha
        ) %>%
        dplyr::filter(stringr::str_detect(term, "exp")) %>%
        dplyr::mutate(
          gest_age_days = 7 + 
            as.numeric(stringr::str_sub(stringr::str_extract(term, "exp_\\d+"), 5, -1)) * 7 
        ) %>%
        dplyr::arrange(gest_age_days)
    }
    if (ns) {
      if (include_smoking) {
        clogit_dan_ns <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_score_dk_bin ~ ",
              "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
              stringr::str_c(spline_terms_koh, collapse = " + "),
              " + exp_used + strata(sibs_id)"
            )
          ),
          data = koh_ns,
          method = "efron"
        )
        clogit_mat_ns <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_score_mat_bin ~ ",
              "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
              stringr::str_c(spline_terms_koh, collapse = " + "),
              " + exp_used + strata(sibs_id)"
            )
          ),
          data = koh_ns,
          method = "efron"
        )
        clogit_bpp_ns <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_sc_bpp_bin ~ ",
              "rygning + tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
              stringr::str_c(spline_terms_skoh, collapse = " + "),
              " + exp_used + strata(sibs_id)"
            )
          ),
          data = skoh_ns,
          method = "efron"
        )
      } else {
        clogit_dan_ns <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_score_dk_bin ~ ",
              "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
              stringr::str_c(spline_terms_koh, collapse = " + "),
              " + exp_used + strata(sibs_id)"
            )
          ),
          data = koh_ns,
          method = "efron"
        )
        clogit_mat_ns <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_score_mat_bin ~ ",
              "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
              stringr::str_c(spline_terms_koh, collapse = " + "),
              " + exp_used + strata(sibs_id)"
            )
          ),
          data = koh_ns,
          method = "efron"
        )
        clogit_bpp_ns <- survival::clogit(
          formula = formula(
            stringr::str_c(
              "z_sc_bpp_bin ~ ",
              "tot_aeld + mat_alder + pat_alder + hf_grp_mor + hf_grp_far + ",
              stringr::str_c(spline_terms_skoh, collapse = " + "),
              " + exp_used + strata(sibs_id)"
            )
          ),
          data = skoh_ns,
          method = "efron"
        )
      }
      # extract estimated parameters for spline basis columns
      clogit_dan_ns_coef <- matrix(
        clogit_dan_ns$coefficients[
          which(stringr::str_detect(names(clogit_dan_ns$coefficients), "exp_"))
        ], 
        nrow = sum(stringr::str_detect(names(clogit_dan_ns$coefficients), "exp_"))
      )
      clogit_mat_ns_coef <- matrix(
        clogit_mat_ns$coefficients[
          which(stringr::str_detect(names(clogit_mat_ns$coefficients), "exp_"))
        ], 
        nrow = sum(stringr::str_detect(names(clogit_mat_ns$coefficients), "exp_"))
      )
      clogit_bpp_ns_coef <- matrix(
        clogit_bpp_ns$coefficients[
          which(stringr::str_detect(names(clogit_bpp_ns$coefficients), "exp_"))
        ], 
        nrow = sum(stringr::str_detect(names(clogit_bpp_ns$coefficients), "exp_"))
      )
      # calculate difference in z-score between no use and use on day x 
      clogit_dan_ns_z_score_odds <- dplyr::tibble(
        gest_age_days = seq(0, knot_max),
        z_score_odds = as.numeric(
          cbind(
            dan_ns_pred, 
            rep(1, nrow(dan_ns_pred))
          ) %*% 
            clogit_dan_ns_coef
        )
      )
      clogit_mat_ns_z_score_odds <- dplyr::tibble(
        gest_age_days = seq(0, knot_max),
        z_score_odds = as.numeric(
          cbind(
            mat_ns_pred, 
            rep(1, nrow(mat_ns_pred))
          ) %*% 
            clogit_mat_ns_coef
        )
      )
      clogit_bpp_ns_z_score_odds <- dplyr::tibble(
        gest_age_days = seq(0, knot_max),
        z_score_odds = as.numeric(
          cbind(
            bpp_ns_pred, 
            rep(1, nrow(bpp_ns_pred))
          ) %*% 
            clogit_bpp_ns_coef
        )
      )
      # pointwise confidence bands
      dan_ns_ci_sens_data <- CalculateClogitSplineConfidenceInterval(
        spline_mod = clogit_dan_ns,
        spline = koh_spline,
        ns_z_score_odds = clogit_dan_ns_z_score_odds,
        alpha = alpha,
        exponentiate = exponentiate
      )
      mat_ns_ci_sens_data <- CalculateClogitSplineConfidenceInterval(
        spline_mod = clogit_mat_ns,
        spline = koh_spline,
        ns_z_score_odds = clogit_mat_ns_z_score_odds,
        alpha = alpha,
        exponentiate = exponentiate
      )
      bpp_ns_ci_sens_data <- CalculateClogitSplineConfidenceInterval(
        spline_mod = clogit_bpp_ns,
        spline = skoh_spline,
        ns_z_score_odds = clogit_bpp_ns_z_score_odds,
        alpha = alpha,
        exponentiate = exponentiate
      )
    }
    
    # output data for sensitivity analysis
    dan_data_sens <- list(
      ns_data = if (ns) {
        dan_ns_ci_sens_data %>%
          dplyr::rename(
            "day" = X,
            "estimate" = Y_hat,
            "conf.low" = ci_bound_min,
            "conf.high" = ci_bound_max
          ) %>%
          dplyr::slice(1:(exp_max + 1))
      } else {
        NULL
      },
      cat_data = if (cat) {
        clogit_dan_cat_z_score_odds %>%
          dplyr::select(term, gest_age_days, estimate, conf.low, conf.high) %>%
          dplyr::mutate(
            day = gest_age_days,
            grp = ExtractWeek(., term),
            exposure = if(hosp) "hosp" else "drug"
          ) %>%
          dplyr::select(exposure, day, grp, estimate, conf.low, conf.high)
      } else {
        NULL
      }
    )
    mat_data_sens <- list(
      ns_data = if (ns) {
        mat_ns_ci_sens_data %>%
          dplyr::rename(
            "day" = X,
            "estimate" = Y_hat,
            "conf.low" = ci_bound_min,
            "conf.high" = ci_bound_max
          ) %>%
          dplyr::slice(1:(exp_max + 1))
      } else {
        NULL
      },
      cat_data = if (cat) {
        clogit_mat_cat_z_score_odds %>%
          dplyr::select(term, gest_age_days, estimate, conf.low, conf.high) %>%
          dplyr::mutate(
            day = gest_age_days,
            grp = ExtractWeek(., term),
            exposure = if (hosp) "hosp" else "drug"
          ) %>%
          dplyr::select(exposure, day, grp, estimate, conf.low, conf.high)
      } else {
        NULL
      }
    )
    bpp_data_sens <- list(
      ns_data = if (ns) {
        bpp_ns_ci_sens_data %>%
          dplyr::rename(
            "day" = X,
            "estimate" = Y_hat,
            "conf.low" = ci_bound_min,
            "conf.high" = ci_bound_max
          ) %>%
          dplyr::slice(1:(exp_max + 1))
      } else {
        NULL
      },
      cat_data = if (cat) {
        clogit_bpp_cat_z_score_odds %>%
          dplyr::select(term, gest_age_days, estimate, conf.low, conf.high) %>%
          dplyr::mutate(
            day = gest_age_days,
            grp = ExtractWeek(., term),
            exposure = if (hosp) "hosp" else "drug"
          ) %>%
          dplyr::select(exposure, day, grp, estimate, conf.low, conf.high)
      } else {
        NULL
      }
    )
  }
  
  # output ---------------------------------------------------------------------
  out <- list(
    dan_data = dan_data,
    mat_data = mat_data,
    bpp_data = bpp_data
  )
  if (sensitivity) {
    out <- c(
      out,
      list(
        dan_data_sens = dan_data_sens,
        mat_data_sens = mat_data_sens,
        bpp_data_sens = bpp_data_sens
      )
    )
  }
  return(out)
}
