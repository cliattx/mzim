#' Fit a Marginalized Zero-Inflated Model
#'
#' This function fits a Marginalized Zero-Inflated Poisson (MZIP) or
#' Negative Binomial (MZINB) model. It uses a two-stage optimization approach:
#' initial refinement with Nelder-Mead followed by the Quasi-Newton BFGS algorithm.
#' It provides both model-based and robust sandwich variance estimators.
#'
#' @param count_formula A formula for the marginal mean model (e.g., `y ~ x1 + x2`).
#' @param zi_formula A formula for the zero-inflation model. Defaults to `~ 1`.
#' @param data A data frame containing the variables.
#' @param family A character string: "zip" or "zinb".
#' @param ... Additional arguments passed to `optim`.
#'
#' @importFrom stats model.frame model.matrix model.response update na.omit
#'             glm.fit coef poisson plogis dpois dnbinom pnorm optim printCoefmat
#' @importFrom MASS glm.nb
#' @importFrom numDeriv jacobian
#'
#' @return An object of class `mzim`.
#' @export
mzim <- function(count_formula, zi_formula = ~ 1, data, family = c("zip", "zinb"), ...) {

  family <- match.arg(family)
  call <- match.call()

  # --- 1. Data Preparation ---
  # Combine formulas to handle NAs correctly across all variables
  all_vars_formula <- update(count_formula,
                             paste(". ~ . +", paste(all.vars(zi_formula), collapse = " + ")))
  mf <- model.frame(all_vars_formula, data = data, na.action = na.omit)

  y <- model.response(mf)
  X_mean <- model.matrix(count_formula, data = mf)
  X_zero <- model.matrix(zi_formula, data = mf)

  p_mean <- ncol(X_mean)
  p_zero <- ncol(X_zero)
  n_obs <- length(y)

  # --- 2. Log-Likelihood Helper (Vectorized) ---
  # Returns a vector of log-likelihoods per observation (needed for Sandwich)
  .ll_vec <- function(par, y, X_mean, X_zero, fam) {
    a <- par[1:p_mean]
    g <- par[(p_mean + 1):(p_mean + p_zero)]

    log_nu <- as.vector(X_mean %*% a)
    logit_psi <- as.vector(X_zero %*% g)
    psi <- plogis(logit_psi)

    # Epsilon for stability
    mu <- exp(log_nu) / (1 - psi + 1e-8)

    ll_i <- numeric(length(y))
    is_zero <- (y == 0)

    if (fam == "zip") {
      # Poisson Logic
      ll_i[!is_zero] <- log(1 - psi[!is_zero] + 1e-8) +
        dpois(y[!is_zero], lambda = mu[!is_zero], log = TRUE)

      log_p0 <- -mu[is_zero]

    } else {
      # NegBin Logic
      logk <- par[p_mean + p_zero + 1]
      k <- exp(logk)

      ll_i[!is_zero] <- log(1 - psi[!is_zero] + 1e-8) +
        dnbinom(y[!is_zero], size = k, mu = mu[!is_zero], log = TRUE)

      log_p0 <- dnbinom(0, size = k, mu = mu[is_zero], log = TRUE)
    }

    # Zero inflation mixture logic (common for zeros)
    if (any(is_zero)) {
      log_psi <- log(psi[is_zero] + 1e-8)
      log_1m_psi <- log(1 - psi[is_zero] + 1e-8)

      # Log-sum-exp trick for stability
      max_log <- pmax(log_psi, log_1m_psi + log_p0)
      ll_i[is_zero] <- max_log + log(exp(log_psi - max_log) +
                                       exp(log_1m_psi + log_p0 - max_log))
    }

    return(ll_i)
  }

  # Objective function for optim (sum of negative LL)
  .negLogLik <- function(par, ...) {
    vals <- .ll_vec(par, ...)
    if (!all(is.finite(vals))) return(1e7)
    -sum(vals)
  }

  # --- 3. Starting Values (Robust from standard models) ---
  if (family == "zip") {
    start_mod <- tryCatch(glm.fit(X_mean, y, family = poisson()), error=function(e) NULL)
    val_a <- if(!is.null(start_mod)) coef(start_mod) else rep(0, p_mean)
    val_g <- c(-1, rep(0, p_zero - 1))
    start_par <- c(val_a, val_g)
  } else {
    # Try MASS::glm.nb for robust starting values
    # Suppress warnings because glm.nb often warns about iteration limits on zero-inflated data
    start_mod <- tryCatch(suppressWarnings(MASS::glm.nb(count_formula, data = mf)),
                          error = function(e) glm.fit(X_mean, y, family = poisson()))
    val_a <- coef(start_mod)
    val_g <- c(-1, rep(0, p_zero - 1))
    # Check if theta exists (if glm.nb worked)
    start_k <- if(!is.null(start_mod$theta)) log(start_mod$theta) else 0
    start_par <- c(val_a, val_g, start_k)
  }

  # --- 4. Estimation (Two-Stage) ---

  # Stage A: Initial Nelder-Mead for robustness
  opt_nm <- optim(par = start_par, fn = .negLogLik,
                  y = y, X_mean = X_mean, X_zero = X_zero, fam = family,
                  method = "Nelder-Mead", control = list(maxit = 2000))

  # CORRECTION 1: Stability Check
  if (opt_nm$convergence != 0) {
    warning("Nelder-Mead initialization did not converge fully. Resulting estimates may be sensitive to starting values.")
  }

  # Stage B: Final Polishing with BFGS to get Hessian
  opt_bfgs <- optim(par = opt_nm$par, fn = .negLogLik,
                    y = y, X_mean = X_mean, X_zero = X_zero, fam = family,
                    method = "BFGS", hessian = TRUE, control = list(maxit = 1000))

  # CORRECTION 2: Final Convergence Check
  if (opt_bfgs$convergence != 0) {
    warning("BFGS optimization did not converge. Consider increasing maxit or checking for collinearity.")
  }

  # --- 5. Variance Estimation ---
  # A. Model-Based (Hessian inverse)
  hessian <- opt_bfgs$hessian
  vcov_model <- tryCatch(solve(hessian), error = function(e) {
    warning("Hessian is singular. Model-based variance could not be calculated.")
    matrix(NA, length(start_par), length(start_par))
  })

  # B. Robust Sandwich (Bread * Meat * Bread)
  # Bread = Hessian Inverse
  # Meat = Crossproduct of scores (gradients per observation)

  # Calculate scores numerically using numDeriv::jacobian
  scores <- numDeriv::jacobian(func = .ll_vec, x = opt_bfgs$par,
                               y = y, X_mean = X_mean, X_zero = X_zero, fam = family)

  meat <- t(scores) %*% scores
  vcov_robust <- vcov_model %*% meat %*% vcov_model

  # --- 6. Formatting Results ---
  params <- opt_bfgs$par
  names_a <- colnames(X_mean)
  names_g <- colnames(X_zero)

  names(params)[1:p_mean] <- paste0("count_", names_a)
  names(params)[(p_mean + 1):(p_mean + p_zero)] <- paste0("zero_", names_g)
  if(family == "zinb") names(params)[length(params)] <- "log_theta"

  # Assign row/col names to matrices
  colnames(vcov_model) <- rownames(vcov_model) <- names(params)
  colnames(vcov_robust) <- rownames(vcov_robust) <- names(params)

  res <- list(
    call = call,
    family = family,
    coefficients = params,
    vcov_model = vcov_model,
    vcov_robust = vcov_robust,
    loglik = -opt_bfgs$value,
    n = n_obs,
    p_mean = p_mean,
    p_zero = p_zero,
    converged = opt_bfgs$convergence == 0
  )

  class(res) <- "mzim"
  return(res)
}

#' Summary for mzim objects
#'
#' @param object An object of class `mzim`.
#' @param robust Logical. If TRUE (default), uses robust sandwich standard errors.
#' @param ... Additional arguments.
#' @export
summary.mzim <- function(object, robust = TRUE, ...) {

  # Choose covariance matrix
  vcov_use <- if(robust) object$vcov_robust else object$vcov_model
  se <- sqrt(diag(vcov_use))
  params <- object$coefficients

  # Extract components
  idx_count <- 1:object$p_mean
  idx_zero  <- (object$p_mean + 1):(object$p_mean + object$p_zero)

  # Function to make print table
  mk_tab <- function(idx) {
    est <- params[idx]
    s   <- se[idx]
    z   <- est / s
    p   <- 2 * pnorm(abs(z), lower.tail = FALSE)
    cbind(Estimate = est, `Std. Error` = s, `z value` = z, `Pr(>|z|)` = p)
  }

  tab_count <- mk_tab(idx_count)
  tab_zero  <- mk_tab(idx_zero)

  # Theta handling (ZINB)
  tab_theta <- NULL
  if (object$family == "zinb") {
    idx_theta <- length(params)
    log_theta <- params[idx_theta]
    se_log_theta <- se[idx_theta]

    # Delta method for theta = exp(log_theta)
    theta_est <- exp(log_theta)
    theta_se <- theta_est * se_log_theta

    tab_theta <- cbind(Estimate = theta_est, `Std. Error` = theta_se)
    rownames(tab_theta) <- "theta"
  }

  res <- list(
    call = object$call,
    family = object$family,
    robust = robust,
    count_coef = tab_count,
    zero_coef = tab_zero,
    theta = tab_theta,
    loglik = object$loglik
  )

  class(res) <- "summary.mzim"
  return(res)
}

#' Print method for summary.mzim
#' @export
print.summary.mzim <- function(x, ...) {
  cat("\nMarginalized Zero-Inflated", ifelse(x$family=="zip", "Poisson", "Negative Binomial"), "Model\n")
  cat("Call:\n")
  print(x$call)
  cat("\nStandard Errors:", ifelse(x$robust, "Robust (Sandwich)", "Model-Based (Hessian)"), "\n")

  cat("\nMarginal Mean Model Coefficients:\n")
  printCoefmat(x$count_coef, P.values=TRUE, has.Pvalue=TRUE)

  cat("\nZero-Inflation Model Coefficients:\n")
  printCoefmat(x$zero_coef, P.values=TRUE, has.Pvalue=TRUE)

  if (!is.null(x$theta)) {
    cat("\nOverdispersion Parameter:\n")
    print(x$theta)
  }

  cat("\nLog-likelihood:", x$loglik, "\n")
}
