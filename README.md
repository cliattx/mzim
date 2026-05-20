---
output: github_document
---

# mzim: Marginalized Zero-Inflated Models

`mzim` is an R package for fitting marginalized zero-inflated count models. The package currently supports marginalized zero-inflated Poisson (mZIP) and marginalized zero-inflated negative binomial (mZINB) models for independent count outcomes with excess zeros.

The package was developed to accompany:

> Li, C., Kwok, O.-M., & Lawrence, T. (2026). *Estimating marginal effects with zero-inflated models: A tutorial with the R package mzim*. Behavior Research Methods. In Press.

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

## Citation

To cite `mzim`, please use:

Li, C., Kwok, O.-M., & Lawrence, T. (2026). *Estimating marginal effects with zero-inflated models: A tutorial with the R package mzim*. Behavior Research Methods. In Press.


## License

`mzim` is released under the GPL-3 license.
