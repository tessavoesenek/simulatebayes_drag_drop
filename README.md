
# simulatebayes

<!-- badges: start -->
<!-- badges: end -->

The goal of simulatebayes is to make simulated data and run bayesian regression on it. The functions should therefore be called in the right order. First, you simulate participants with make_participants. You chose how many intervention participants and how many control. Second, you create the intervention data with make_interventions. Third, you run the regressions with run_regressions. Finally, you can plot the samples from the posterior with plot_samples. If you make regression output for different priors, you can compare the estimate results for two different priors with compare_priors.

## Installation

You can install the development version of simulatebayes like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(simulatebayes)

participants <- make_participants(3,4)
intervention_output <- make_interventions(participants, -0.1, -0.04, 2)
regression_output <- run_regressions(intervention_output$interventions, prior_type = 1)
sample_plots <- plot_samples(regression_output$samples, 0, intervention_output$Effect)

## basic example code
```

