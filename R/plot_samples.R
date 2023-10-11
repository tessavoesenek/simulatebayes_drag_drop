#' Plot sample distribution
#'
#' @param samples dataframe with posterior samples
#' @param Grens boundary indicator: estimates smaller than?
#' @param Effect What intervention effect is plotted?
#'
#' @return density plots for a specific quarter with percentage indicator
#' @export

plot_samples <- function(samples, Grens, Effect){

  columns <- c("b_DiD_9", "b_DiD_10", "b_DiD_11", "b_DiD_12")
  kwartaal <- c("Kwartaal 9", "Kwartaal 10", "Kwartaal 11", "Kwartaal 12")
  myplots <- vector('list', length(columns))
  Subplottitle <-  paste0("De kansverdeling van het geschatte effect per simulatie\nen in het blauw de kans dat het effect kleiner is dan ", Grens)

    for (i in 1:length(columns)){

      percentage_below_grens <- samples %>%
        mutate(GrensO = ifelse(test = !!rlang::sym(columns[i]) < Grens,
                               yes = 1,
                               no = 0)) %>%
        group_by(Run) %>%
        summarise(Percentage1 = mean(GrensO)) %>%
        ungroup() %>%
        mutate(Percentage = paste0(round(Percentage1 * 100, 0), " %"))

  # make plots

  myplots[[i]] <-
    p1 <- ggplot2::ggplot(data = samples,
  ggplot2::aes(x = !!rlang::sym(columns[i]),
                          y = Run)) +
    ggridges::geom_density_ridges2(scale = 0.85,
                                  quantile_lines=TRUE,
                                  quantile_fun = function(x,...)mean(x)) +
    ggplot2::geom_vline(xintercept = 0,
                        color = "firebrick",
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = Effect,
                        color = "darkgreen") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(y = " ",
                  x = "Geschatte effect",
                  title = print(kwartaal[i]),
                  subtitle = Subplottitle) +
    ggplot2::geom_text(data = percentage_below_grens,
    ggplot2::aes(x = 0.05,
                y = (as.numeric(Run) + 0.25),
                label = Percentage),
                size = 2.5,
                color = "steelblue") +
    ggplot2::xlim(-0.1, 0.1)
    print(p1)
    }
  return(myplots)
  }







