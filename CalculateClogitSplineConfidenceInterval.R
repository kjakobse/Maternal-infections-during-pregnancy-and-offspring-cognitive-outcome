# confidence set based on 1-\alpha interval on parameter-vector
# we use the function I_ns(X\beta), where I_ns only uses the parameters from
# the natural spline.

# spline_mod       list:    List with output from clogit function for a spline model.
# spline           ns:      The spline basis matrix used in the lms call.
# ns_z_score_odds  tibble:  Tibble with two columns. The first with a sequence
#                           of values of the spline covariate, the second with
# alpha           double:   level of the confidence set. The function returns a
#                           1-alpha confidence set for the spline curve.

CalculateClogitSplineConfidenceInterval <- function(
    spline_mod,
    spline,
    ns_z_score_odds,
    alpha = 0.05,
    exponentiate = TRUE
) {
  # Is the exposure drug use or hospital visit?
  if (any(stringr::str_detect(names(coef(spline_mod)), "^drug"))) {
    expo <- "drug"
    which_expo <- stringr::str_detect(names(coef(spline_mod)), "^drug")
  } else if(any(stringr::str_detect(names(coef(spline_mod)), "^hosp"))) {
    expo <- "hosp"
    which_expo <- stringr::str_detect(names(coef(spline_mod)), "^hosp")
  } else if(any(stringr::str_detect(names(coef(spline_mod)), "^exp"))) {
    expo <- "exp"
    which_expo <- stringr::str_detect(names(coef(spline_mod)), "^exp")
  } else {
    abort("spline_mod must have either drug use or hospital visit as exposure")
  }
  
  # calculate natural spline for beta values on boundary of CI
  x <- cbind(
    predict(
      spline, 
      seq(
        attr(spline, "Boundary.knots")[1], 
        attr(spline, "Boundary.knots")[2], 
        1
      )
    ), 
    rep(
      1, 
      length(
        seq(
          attr(spline, "Boundary.knots")[1], 
          attr(spline, "Boundary.knots")[2], 
          1
        )
      )
    )
  )
  ci_r <- qnorm(1 - alpha / 2) * sqrt(
    purrr::map_dbl(
      seq_len(nrow(x)),
      ~ x[.x, , drop = FALSE] %*% 
        spline_mod$var[which_expo, which_expo] %*% 
        t(x[.x, , drop = FALSE])
    )
  )
  if (exponentiate) {
    suppressMessages(
      ci_data <- dplyr::tibble(
        ci_bound_min = exp(
          as.numeric(x %*% matrix(coef(spline_mod)[which_expo], ncol = 1)) - ci_r
        ),
        ci_bound_max = exp(
          as.numeric(x %*% matrix(coef(spline_mod)[which_expo], ncol = 1)) + ci_r
        )
      ) %>%
        dplyr::bind_cols(
          dplyr::tibble(
            X = ns_z_score_odds[[1]],
            Y_hat = exp(ns_z_score_odds[[2]])
          )
        )
    ) %>%
      dplyr::select(X, Y_hat, ci_bound_min, ci_bound_max)
  } else {
    suppressMessages(
      ci_data <- dplyr::tibble(
        ci_bound_min = as.numeric(
          x %*% matrix(coef(spline_mod)[which_expo], ncol = 1)
        ) - ci_r,
        ci_bound_max = as.numeric(
          x %*% matrix(coef(spline_mod)[which_expo], ncol = 1)
        ) + ci_r
      ) %>%
        dplyr::bind_cols(
          dplyr::tibble(
            X = ns_z_score_odds[[1]],
            Y_hat = ns_z_score_odds[[2]]
          )
        )
    ) %>%
      dplyr::select(X, Y_hat, ci_bound_min, ci_bound_max)
  }
  return(ci_data)
}