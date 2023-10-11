#' run regressions
#' this function runs a bayesian regression with
#' Formula: "Verwijzingen ~ 0 + Intercept + id + Kwartaal + DiD_9 + DiD_10 + DiD_11 + DiD_12"
#'
#' @param data dataframe with interventions
#' @param prior_type priors for intercept and DiD variables. 1 = non-informative prior, 0 = informative prior
#'
#' @return list containing fixed effect coefficients and samples
#' @export

run_regressions <- function(data, prior_type){

  Formula <- "Verwijzingen ~ 0 + Intercept + id + Kwartaal + DiD_9 + DiD_10 + DiD_11 + DiD_12"
  Runs <- max(as.numeric(data$Run))
  samples <- tibble::tibble()
  coefficients <- data.frame()

  # Priors

  ## niet informatieve priors

  prior1 <- c(brms::set_prior("normal(0.4, 0.04)", class = "b", coef = "Intercept"),
              brms::set_prior("normal(0, 0.05)", class = "b", coef = "DiD_9"),
              brms::set_prior("normal(0, 0.05)", class = "b", coef = "DiD_10"),
              brms::set_prior("normal(0, 0.05)", class = "b", coef = "DiD_11"),
              brms::set_prior("normal(0, 0.05)", class = "b", coef = "DiD_12"))

  ## informatieve priors
  prior2 <- c(brms::set_prior("normal(0.4, 0.04)", class = "b", coef = "Intercept"),
              brms::set_prior("normal(-0.2, 0.2)", class = "b", coef = "DiD_9"),
              brms::set_prior("normal(-0.2, 0.2)", class = "b", coef = "DiD_10"),
              brms::set_prior("normal(-0.2, 0.2)", class = "b", coef = "DiD_11"),
              brms::set_prior("normal(-0.2, 0.2)", class = "b", coef = "DiD_12"))

  ifelse(prior_type == 1, chosen_prior <- prior1, chosen_prior <- prior2)


  for(i in 1:Runs){
    df <- data %>% dplyr::filter(Run == i)

    reg1 <- brms::brm(
      formula = Formula,
      prior = chosen_prior,
      warmup = 1000,
      iter = 2500,
      data = df,
      chains = 4,
      cores = 6,
      init = "random",
      control = list(adapt_delta = 0.8, max_treedepth = 12),
      seed = 123,
      backend = "cmdstanr",
    )

    # get posterior draws

    samples_temp <- tibble::tibble(brms::as_draws_df(reg1)) %>%
      dplyr::mutate(Run = as.factor(i))

    samples <- samples %>%
      rbind(samples_temp)

    # get coefficients

    Estimates <- as.data.frame(brms::fixef(reg1)[,"Estimate"]) %>%
      tibble::rownames_to_column() %>%
      dplyr::mutate(Run = as.factor(i))

    Q2.5 <- as.data.frame(brms::fixef(reg1)[,"Q2.5"]) %>%
      tibble::rownames_to_column() %>%
      dplyr::mutate(Run = as.factor(i))

    Q97.5 <- as.data.frame(brms::fixef(reg1)[,"Q97.5"]) %>%
      tibble::rownames_to_column() %>%
      dplyr::mutate(Run = as.factor(i))

    coefficients_temp <- dplyr::left_join(Estimates, Q2.5, by = c("rowname", "Run")) %>%
      dplyr::left_join(Q97.5, by = c("rowname", "Run")) %>%
      dplyr::rename("Estimate" = "brms::fixef(reg1)[, \"Estimate\"]") %>%
      dplyr::rename("Q2.5" = "brms::fixef(reg1)[, \"Q2.5\"]") %>%
      dplyr::rename("Q97.5" = "brms::fixef(reg1)[, \"Q97.5\"]") %>%
      dplyr::mutate(Kwartaal = as.numeric(stringr::str_sub(rowname, start = 9)))

    coefficients <- coefficients %>%
      rbind(coefficients_temp) %>%
      dplyr::relocate(Run, .after = rowname)

  }

  result <- dplyr::lst(coefficients, samples)
  return(result)

}
