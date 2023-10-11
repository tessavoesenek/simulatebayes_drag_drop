#' Compare priors
#'
#' @param coefficients_priortype_1 dataframe with coefficients from regression output with priortype 1
#' @param coefficients_priortype_0 dataframe with coefficients from regression output with priortype 0
#' @param Effect Effect from intervention_output
#'
#' @return plot
#' @export


compare_priors <- function(coefficients_priortype_1, coefficients_priortype_0, Effect){
  # prior 1 = non informative
  # prior 0 = informative
  RR <- coefficients_priortype_1 %>%
    group_by(Kwartaal) %>%
    summarise(ME = mean(Estimate), ML = mean(Q2.5), MU = mean(Q97.5)) %>%
    ungroup() %>%
    mutate(Prior = "non informative")

  RR2 <- coefficients_priortype_0 %>%
    group_by(Kwartaal) %>%
    summarise(ME = mean(Estimate), ML = mean(Q2.5), MU = mean(Q97.5)) %>%
    ungroup() %>%
    mutate(Prior = "informative")

  RR3 <- rbind(RR, RR2) %>%
    filter(!is.na(Kwartaal))


    p1 <- ggplot2::ggplot(data = RR3,
                  ggplot2::aes(x = ML, xend = MU,
                               y = Prior, yend = Prior)) +
    ggplot2::geom_segment() +
    ggplot2::geom_point(
      ggplot2::aes(x = ME,
                   y = Prior),
      size = 2) +
    ggplot2::facet_wrap(~factor(Kwartaal)) +
    ggplot2::geom_vline(xintercept = 0,
                        color = "firebrick",
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = Effect,
                        color = "darkgreen") +
    ggplot2::xlim(-0.2, 0.2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(y = "", x = "Geschatte effect")
print(p1)

return(p1)

}

