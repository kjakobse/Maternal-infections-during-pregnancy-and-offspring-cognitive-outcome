# confidence set based on 1-\alpha interval on parameter-vector
# we use the function I_ns(X\beta), where I_ns only uses the parameters from
# the natural spline.

# spline_mod       list:    List with output from lms function for a spline model.
# spline           ns:      The spline basis matrix used in the lms call.
# ns_z_score_diff  tibble:  Tibble with two columns. The first with a sequence
#                           of values of the spline covariate, the second with
#                           the z_score_diff to the baseline.
# alpha           double:   level of the confidence set. The function returns a
#                           1-alpha confidence set for the spline curve.
# par             chr:      "all" to vary all parameters sampling from a uniform,
#                           "reduced" to only use parameters associated with the 
#                           spline.
# Verbose         lgl:      Print information about the number of points used
#                           calculate the confidence band.

CalculateSplineConfidenceInterval <- function(
    spline_mod,
    spline,
    ns_z_score_diff,
    alpha = 0.05,
    par = c("all", "reduced"),
    verbose = TRUE
) {
  # pick first element of par
  par <- match.arg(par)
  # Is the exposure drug use or hospital visit?
  if (any(stringr::str_detect(names(spline_mod$model), "^drug"))) {
    expo <- "drug"
    if(par == "reduced") {
      which_expo <-
        stringr::str_detect(names(coef(spline_mod)[!is.na(coef(spline_mod))]), "^drug")
    }
  } else if(any(stringr::str_detect(names(spline_mod$model), "^hosp"))) {
    expo <- "hosp"
    if(par == "reduced") {
      which_expo <- 
        stringr::str_detect(names(coef(spline_mod)[!is.na(coef(spline_mod))]), "^hosp")
    }
  } else if(any(str_detect(names(spline_mod$model), "^exp"))) {
    expo <- "exp"
    if(par == "reduced") {
      which_expo <- 
        stringr::str_detect(names(coef(spline_mod)[!is.na(coef(spline_mod))]), "^exp")
    }
  } else {
    abort("spline_mod must have either drug use or hospital visit as exposure")
  }
  # extract MLE parameters and standard deviation
  beta_hat <- coef(spline_mod)[!is.na(coef(spline_mod))]
  sigma_hat <- summary(spline_mod)$sigma
  # design matrix
  X <- model.matrix(spline_mod)[, !is.na(coef(spline_mod)), drop = FALSE]
  # weight (here W is the inverse of the usual weight)
  rdf <- spline_mod$df.residual
  if (is.null(spline_mod$weights)) {
    rss <- sum(spline_mod$residuals^2)
  } else {
    rss <- sum(spline_mod$weights * spline_mod$residuals^2)
  }
  W <- rss / rdf
  # calculate design matrix on spline parameters only
  if (par == "reduced") {
    # xtx_red contains the contribution from \sigma^2 through W.
    # Therefore the contraint below only uses the chi-squared quantile.
    xtx_inv <- W * solve(crossprod(X))
    xtx_red <- solve(xtx_inv[which_expo, which_expo])
  } else {
    xtx_red <- crossprod(X) / W
  }
  
  # calculate natural spline coordinates for each time point
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
  if (verbose) {
    cat("Start solving optimization problem for CI bounds")
  }
  # solve optimization problems for lower CI bound
  ci_bound_min <- purrr::map_dbl(
    seq_len(nrow(x)),
    \(i) {
      beta <- CVXR::Variable(dim(xtx_red)[1])
      objective <- CVXR::Minimize(t(x[i,]) %*% beta)
      constraint <- CVXR::quad_form(beta_hat[which_expo] - beta, xtx_red) <= 
        qchisq(1 - alpha, length(beta_hat[which_expo]))
      # Note degrees of freedom used in chi-squared distribution is the number
      # of parameters we allow to vary, i.e. the spline parameters plus the
      # parameter of medication usage/hospitalization
      problem <- CVXR::Problem(objective, constraints = list(constraint))
      result <- CVXR::solve(problem)
      return(result$value)
    }
  )
  # solve optimization problems for upper CI bound
  ci_bound_max <- purrr::map_dbl(
    seq_len(nrow(x)),
    \(i) {
      beta <- CVXR::Variable(dim(xtx_red)[1])
      objective <- CVXR::Maximize(t(x[i,]) %*% beta)
      constraint <- CVXR::quad_form(beta_hat[which_expo] - beta, xtx_red) <= 
        qchisq(1 - alpha, length(beta_hat[which_expo]))
      problem <- CVXR::Problem(objective, constraints = list(constraint))
      result <- CVXR::solve(problem)
      return(result$value)
    }
  )
  if (verbose) {
    cat("\nDone solving optimization problem for CI bounds\n")
  }
  # create output data
  ci_data <- dplyr::tibble(
    X = ns_z_score_diff[[1]], 
    Y_hat = ns_z_score_diff[[2]], 
    ci_bound_min = ci_bound_min, 
    ci_bound_max = ci_bound_max
  )
  
  return(ci_data)
}
