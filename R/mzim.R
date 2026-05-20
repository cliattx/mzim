#' Fit a Marginalized Zero-Inflated Model
#'
#' Fits a marginalized zero-inflated Poisson or negative binomial model.
#'
#' @param count_formula Formula for the marginal mean model.
#' @param zi_formula Formula for the zero-inflation model. Defaults to `~ 1`.
#' @param data Data frame.
#' @param family Character string: `"zip"` or `"zinb"`.
#' @param ... Additional arguments passed to `optim`.
#'
#' @importFrom stats model.frame model.matrix model.response na.omit reformulate
#' @importFrom stats glm.fit coef poisson plogis dpois dnbinom pnorm optim printCoefmat
#' @importFrom MASS glm.nb
#' @importFrom numDeriv jacobian
#'
#' @return An object of class `mzim`.
#' @references
#' Li, C., Kwok, O.-M., & Lawrence, T. (2026). Estimating marginal effects
#' with zero-inflated models: A tutorial with the R package mzim.
#' Behavior Research Methods. https://doi.org/10.3758/s13428-026-03036-7
#'
#' Long, D. L., Preisser, J. S., Herring, A. H., & Golin, C. E. (2014).
#' A marginalized zero-inflated Poisson regression model with overall
#' exposure effects. Statistics in Medicine, 33(29), 5151--5165.
#' https://doi.org/10.1002/sim.6293
#'
#' Preisser, J. S., Das, K., Long, D. L., & Divaris, K. (2016).
#' Marginalized zero-inflated negative binomial regression with application
#' to dental caries. Statistics in Medicine, 35(10), 1722--1735.
#' https://doi.org/10.1002/sim.6804
#' @export
mzim <- function(count_formula, zi_formula = ~ 1, data, family = c("zip", "zinb"), ...) {

  family <- match.arg(family)
  call <- match.call()

  vars <- unique(c(all.vars(count_formula), all.vars(zi_formula)))
  mf <- model.frame(reformulate(vars), data = data, na.action = na.omit)

  y <- model.response(model.frame(count_formula, data = mf))
  X_mean <- model.matrix(count_formula, data = mf)
  X_zero <- model.matrix(zi_formula, data = mf)

  if (any(is.na(y))) stop("Outcome contains missing values after model-frame construction.")
  if (any(y < 0)) stop("Outcome must be nonnegative.")
  if (any(abs(y - round(y)) > sqrt(.Machine$double.eps))) {
    stop("Outcome must be integer-valued count data.")
  }

  y <- as.integer(round(y))

  p_mean <- ncol(X_mean)
  p_zero <- ncol(X_zero)
  n_obs <- length(y)

  if (p_zero < 1) stop("The zero-inflation model must contain at least one column.")

  .log1pexp <- function(x) {
    ifelse(x > 0, x + log1p(exp(-x)), log1p(exp(x)))
  }

  .log_sum_exp <- function(a, b) {
    m <- pmax(a, b)
    m + log(exp(a - m) + exp(b - m))
  }

  .ll_vec <- function(par, y, X_mean, X_zero, fam) {
    a <- par[seq_len(p_mean)]
    g <- par[p_mean + seq_len(p_zero)]

    log_nu <- as.vector(X_mean %*% a)
    eta_zero <- as.vector(X_zero %*% g)

    log_psi <- eta_zero - .log1pexp(eta_zero)
    log_1m_psi <- -.log1pexp(eta_zero)
    log_mu <- log_nu - log_1m_psi
    mu <- exp(pmin(log_mu, 700))

    ll_i <- numeric(length(y))
    is_zero <- y == 0

    if (fam == "zip") {
      ll_i[!is_zero] <- log_1m_psi[!is_zero] +
        dpois(y[!is_zero], lambda = mu[!is_zero], log = TRUE)

      log_p0 <- -mu[is_zero]
    } else {
      log_theta <- par[p_mean + p_zero + 1]
      theta <- exp(log_theta)

      ll_i[!is_zero] <- log_1m_psi[!is_zero] +
        dnbinom(y[!is_zero], size = theta, mu = mu[!is_zero], log = TRUE)

      log_p0 <- dnbinom(0, size = theta, mu = mu[is_zero], log = TRUE)
    }

    if (any(is_zero)) {
      ll_i[is_zero] <- .log_sum_exp(
        log_psi[is_zero],
        log_1m_psi[is_zero] + log_p0
      )
    }

    ll_i
  }

  .negLogLik <- function(par, ...) {
    vals <- .ll_vec(par, ...)
    if (!all(is.finite(vals))) return(1e12)
    -sum(vals)
  }

  if (family == "zip") {
    start_mod <- tryCatch(glm.fit(X_mean, y, family = poisson()), error = function(e) NULL)
    val_a <- if (!is.null(start_mod)) coef(start_mod) else rep(0, p_mean)
    val_a[!is.finite(val_a)] <- 0
    val_g <- c(-1, rep(0, p_zero - 1))
    start_par <- c(val_a, val_g)
  } else {
    start_mod <- tryCatch(
      suppressWarnings(MASS::glm.nb(count_formula, data = mf)),
      error = function(e) glm.fit(X_mean, y, family = poisson())
    )
    val_a <- coef(start_mod)
    val_a[!is.finite(val_a)] <- 0
    val_g <- c(-1, rep(0, p_zero - 1))
    start_theta <- if (!is.null(start_mod$theta) && is.finite(start_mod$theta)) {
      log(start_mod$theta)
    } else {
      0
    }
    start_par <- c(val_a, val_g, start_theta)
  }

  opt_nm <- optim(
    par = start_par,
    fn = .negLogLik,
    y = y,
    X_mean = X_mean,
    X_zero = X_zero,
    fam = family,
    method = "Nelder-Mead",
    control = list(maxit = 2000)
  )

  if (opt_nm$convergence != 0) {
    warning("Nelder-Mead did not converge.")
  }

  opt_bfgs <- optim(
    par = opt_nm$par,
    fn = .negLogLik,
    y = y,
    X_mean = X_mean,
    X_zero = X_zero,
    fam = family,
    method = "BFGS",
    hessian = TRUE,
    control = list(maxit = 1000)
  )

  if (opt_bfgs$convergence != 0) {
    warning("BFGS did not converge.")
  }

  hessian <- opt_bfgs$hessian
  vcov_model <- tryCatch(
    solve(hessian),
    error = function(e) {
      warning("Hessian is singular.")
      matrix(NA_real_, length(start_par), length(start_par))
    }
  )

  scores <- numDeriv::jacobian(
    func = .ll_vec,
    x = opt_bfgs$par,
    y = y,
    X_mean = X_mean,
    X_zero = X_zero,
    fam = family
  )

  meat <- t(scores) %*% scores
  vcov_robust <- vcov_model %*% meat %*% vcov_model

  params <- opt_bfgs$par
  names(params)[seq_len(p_mean)] <- paste0("count_", colnames(X_mean))
  names(params)[p_mean + seq_len(p_zero)] <- paste0("zero_", colnames(X_zero))

  if (family == "zinb") {
    names(params)[length(params)] <- "log_theta"
  }

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
  res
}

#' Summary for mzim objects
#'
#' @param object An object of class `mzim`.
#' @param robust Logical. If TRUE, uses sandwich standard errors.
#' @param ... Additional arguments.
#' @export
summary.mzim <- function(object, robust = TRUE, ...) {

  vcov_use <- if (robust) object$vcov_robust else object$vcov_model
  se <- sqrt(diag(vcov_use))
  params <- object$coefficients

  idx_count <- seq_len(object$p_mean)
  idx_zero <- object$p_mean + seq_len(object$p_zero)

  mk_tab <- function(idx) {
    est <- params[idx]
    s <- se[idx]
    z <- est / s
    p <- 2 * pnorm(abs(z), lower.tail = FALSE)
    cbind(Estimate = est, `Std. Error` = s, `z value` = z, `Pr(>|z|)` = p)
  }

  tab_count <- mk_tab(idx_count)
  tab_zero <- mk_tab(idx_zero)

  tab_theta <- NULL
  if (object$family == "zinb") {
    idx_theta <- length(params)
    theta_est <- exp(params[idx_theta])
    theta_se <- theta_est * se[idx_theta]
    phi_est <- 1 / theta_est
    phi_se <- theta_se / theta_est^2

    tab_theta <- rbind(
      theta = c(Estimate = theta_est, `Std. Error` = theta_se),
      phi = c(Estimate = phi_est, `Std. Error` = phi_se)
    )
  }

  res <- list(
    call = object$call,
    family = object$family,
    robust = robust,
    count_coef = tab_count,
    zero_coef = tab_zero,
    theta = tab_theta,
    loglik = object$loglik,
    n = object$n,
    converged = object$converged
  )

  class(res) <- "summary.mzim"
  res
}

#' Print method for summary.mzim objects
#'
#' @param x An object of class `summary.mzim`.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns `x`.
#' @export
print.summary.mzim <- function(x, ...) {
  cat("\nMarginalized Zero-Inflated", ifelse(x$family == "zip", "Poisson", "Negative Binomial"), "Model\n")
  cat("Call:\n")
  print(x$call)
  cat("\nStandard Errors:", ifelse(x$robust, "Robust (Sandwich)", "Model-Based (Hessian)"), "\n")

  cat("\nMarginal Mean Model Coefficients:\n")
  printCoefmat(x$count_coef, P.values = TRUE, has.Pvalue = TRUE)

  cat("\nZero-Inflation Model Coefficients:\n")
  printCoefmat(x$zero_coef, P.values = TRUE, has.Pvalue = TRUE)

  if (!is.null(x$theta)) {
    cat("\nOverdispersion Parameter:\n")
    print(x$theta)
  }

  cat("\nLog-likelihood:", x$loglik, "\n")

  invisible(x)
}
