# mzim: Marginalized Zero-Inflated Models

The `mzim` package implements Marginalized Zero-Inflated Poisson (MZIP) and Negative Binomial (MZINB) models. 

Unlike standard zero-inflated models (like `pscl` or `glmmTMB`) which interpret coefficients conditional on the latent state, `mzim` models the **marginal mean** directly. This provides easier population-level interpretation, which is critical for public health and social science research.

## Installation

You can install the development version of mzim like so:

``` r
# install.packages("devtools")
devtools::install_github("cliattx/mzim")
