---
output: github_document
---

# mzim: Marginalized Zero-Inflated Models

`mzim` is an R package for fitting marginalized zero-inflated count models. The package currently supports marginalized zero-inflated Poisson (mZIP) and marginalized zero-inflated negative binomial (mZINB) models for independent count outcomes with excess zeros.

The package was developed to accompany:

> Li, C., Kwok, O.-M., & Lawrence, T. (2026). *Estimating marginal effects with zero-inflated models: A tutorial with the R package mzim*. Behavior Research Methods. https://doi.org/10.3758/s13428-026-03036-7

## Why use `mzim`?

Traditional zero-inflated Poisson and zero-inflated negative binomial models separate the data-generating process into a zero-inflation component and a count component. The count-model coefficients from traditional ZIP/ZINB models are conditional on the latent at-risk subpopulation, which can make them difficult to interpret when the research question concerns the overall population.

Marginalized zero-inflated models directly model the marginal mean of the outcome. As a result, the exponentiated coefficients from the marginal mean model can be interpreted as population-average incidence rate ratios.

This is useful when the research question is:

> What is the overall association between a predictor and the expected count in the full sample?

rather than:

> What is the association among only the latent at-risk subgroup?

## Installation

You can install the development version from GitHub:

```{r install, eval = FALSE}
# install.packages("devtools")
devtools::install_github("cliattx/mzim")
```

After installation:

```{r load, eval = FALSE}
library(mzim)
```

## Model

The marginalized zero-inflated model is written as:

\[
\log(\nu_i) = X_i^\top \alpha,
\]

where \(\nu_i = E(Y_i)\) is the marginal mean of the count outcome for observation \(i\). The zero-inflation part is written as:

\[
\text{logit}(\psi_i) = Z_i^\top \gamma,
\]

where \(\psi_i\) is the probability of an excess zero.

For the mZIP model, the count distribution is Poisson. For the mZINB model, the count distribution is negative binomial, allowing additional overdispersion.

## Example

The package includes an example dataset, `abuse`, used to demonstrate the mZINB model.

```{r example, eval = FALSE}
library(mzim)

data("abuse")

fit <- mzim(
  count_formula = ABUSESUM ~ Gender + School,
  zi_formula = ~ Gender + School,
  data = abuse,
  family = "zinb"
)

summary(fit)
```

The `count_formula` specifies the marginal mean model. Coefficients from this part of the model are interpreted as effects on the overall population mean. The `zi_formula` specifies the zero-inflation model.

By default, `summary()` reports robust sandwich standard errors:

```{r robust, eval = FALSE}
summary(fit, robust = TRUE)
```

Model-based standard errors from the Hessian can be requested with:

```{r model_based, eval = FALSE}
summary(fit, robust = FALSE)
```

## Interpreting coefficients

For the marginal mean model, exponentiating a coefficient gives a population-average incidence rate ratio:

```{r irr, eval = FALSE}
exp(coef(fit))
```

For example, if a coefficient in the marginal mean model is \(-0.40\), then:

\[
\exp(-0.40) = 0.67.
\]

This means that the expected count in the full sample is approximately 33% lower for a one-unit increase in the predictor, holding other variables constant.

## Citation

To cite `mzim`, please use:

```{r citation, eval = FALSE}
citation("mzim")
```

Main citation:

Li, C., Kwok, O.-M., & Lawrence, T. (2026). *Estimating marginal effects with zero-inflated models: A tutorial with the R package mzim*. Behavior Research Methods. https://doi.org/10.3758/s13428-026-03036-7

The statistical models implemented in `mzim` are based on:

Long, D. L., Preisser, J. S., Herring, A. H., & Golin, C. E. (2014). A marginalized zero-inflated Poisson regression model with overall exposure effects. *Statistics in Medicine, 33*(29), 5151–5165. https://doi.org/10.1002/sim.6293

Preisser, J. S., Das, K., Long, D. L., & Divaris, K. (2016). Marginalized zero-inflated negative binomial regression with application to dental caries. *Statistics in Medicine, 35*(10), 1722–1735. https://doi.org/10.1002/sim.6804

## Current scope

The current version of `mzim` focuses on cross-sectional or independent-observation settings. Longitudinal, clustered, mixed-effects, and Bayesian extensions are not implemented in the current release.

## License

`mzim` is released under the GPL-3 license.
