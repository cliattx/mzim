# mzim: Marginalized Zero-Inflated Models

The `mzim` package implements Marginalized Zero-Inflated Poisson (MZIP) and Negative Binomial (MZINB) models. 

Unlike standard zero-inflated models (like `pscl` or `glmmTMB`) which interpret coefficients conditional on the latent state, `mzim` models the **marginal mean** directly. This provides easier population-level interpretation, which is critical for public health and social science research.

## Installation

You can install the development version of mzim like so:

``` r
# install.packages("devtools")
devtools::install_github("cliattx/mzim")
## Example

This is a basic example which shows you how to fit a Marginalized Zero-Inflated Negative Binomial (MZINB) model using the included `abuse` dataset:

```{r example}
library(mzim)

# Load the embedded dataset
data("abuse")

# Fit the model
# We model the count (ABUSESUM) with Gender and School status
# We model the zero-inflation with Gender
fit <- mzim(count_formula = ABUSESUM ~ Gender + School, 
            zi_formula = ~ Gender, 
            data = abuse, 
            family = "zinb")

# View the results (Defaults to Robust Sandwich Estimators)
summary(fit)
```
