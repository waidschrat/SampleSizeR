
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SampleSizeR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bayesROE)](https://CRAN.R-project.org/package=SampleSizeR)
[![Codecov test
coverage](https://codecov.io/gh/waidschrat/bayesROE/branch/main/graph/badge.svg)](https://app.codecov.io/gh/waidschrat/SampleSizeR?branch=main)

<!-- badges: end -->

The package SampleSizeR for R statistical software provides an intuitive
Shiny application to easily perform sample size estimation and power
analysis for interventional and observational clinical studies. To this
end, SampleSizeR facilitates the conversion of design-specific
assumptions into the parameters required by generic sample size
calculation procedures. For designs that lack analytical solutions, a
simulation engine is provided.

## Installation

You can install the development version of SampleSizeR like so:

``` r
remotes::install_github(repo = "waidschrat/SampleSizeR")
```

## Example

The following code extends the example from Hoefler and Miller (2023) by
visualizing the RoE for one non-inferiority claim (delta = -3 pts) and
the ROEs for two superiority claims (delta = 0 pts and delta = 3 pts)
for an additional reduction of MADRS scores due to adjunct Esketamine
treatment of patients suffering from moderate to severe major
depression:

``` r
# library(SampleSizeR)
# 
# # Arguments to reproduce Figure from Hoefler and Miller (2023)
# init <- list(ee = 3.07, se = 1.19, delta = c(-3, 0, 3), alpha = 0.025)
# cols <- list(col_lower = "#F5FF82", col_upper = "#27CC1E")
# 
# # Pass Arguments to Locally Run Shiny Application using run_app()
# if(interactive()){
#   run_app(init = init, cols = cols)
# }
# 
# # Alternatively Generate and Customize Regions of Evidence Plot using ribbonROE()
# HM23.3 <- ribbonROE(ee = init$ee, se = init$se, delta = init$delta, alpha = init$alpha, 
#                     cols = c(cols$col_lower, cols$col_upper))$plot + 
#   ggplot2::annotate(geom = "point", y = init$ee, x = init$se, shape = 4) +
#   ggplot2::coord_flip(ylim = c(-5, 15))
```

The resulting RoE plot enables to evaluate the presence of the
respective claim (colored areas) or their absence (white area) as a
function of prior expected mean (x-axis) and prior standard deviation
(y-axis) that could be used by an assessor. The cross marks the effect
estimate that was used to construct the RoE. The dashed vertical line
represents all the sceptical priors that assume the true effect around
0, with a varying degree of uncertainty.

``` r
# print(HM23.3)
```
