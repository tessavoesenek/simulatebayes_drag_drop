
#' Make interventions
#'
#' @param data a dataframe with colnames id, Interventie and K1
#' @param Effect value of the intervention effect on referrals
#' @param Kwartaaleffect value of the quarterly change in referrals
#' @param Runs number of simulations
#'
#' @return dataframe with 9 columns and runs * id * 12 rows. containing # of referrals per quarter per id
#' @export

make_interventions <- function(data, Effect, Kwartaaleffect, Runs){

  n_participants <- nrow(data)
  interventions <- data.frame()
  Effect <- Effect
  Kwartaaleffect <- Kwartaaleffect
  Runs <- Runs

  for(i in 1:Runs){

    df <- data %>%
      dplyr::mutate(K2 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect) * K1, sd = 0.02),
                    K3 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^2 * K1, sd = 0.02),
                    K4 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^3 * K1, sd = 0.02),
                    K5 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^4 * K1, sd = 0.02),
                    K6 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^5 * K1, sd = 0.02),
                    K7 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^6 * K1, sd = 0.02),
                    K8 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^7 * K1, sd = 0.02),
                    # nu begint de interventie
                    K9 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^8 * K1, sd = 0.02) + Effect * Interventie,
                    K10 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^9 * K1, sd = 0.02) + Effect * Interventie,
                    K11 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^10 * K1, sd = 0.02) + Effect * Interventie,
                    K12 = rnorm(n = n_participants, mean = (1 + Kwartaaleffect)^11 * K1, sd = 0.02) + Effect * Interventie)%>%
      # lang maken data
      tidyr::pivot_longer(cols = K1:K12,
                          names_to = "Kwartaal",
                          values_to = "Verwijzingen") %>%
      # maak kwartaal numeriek om de interventievariabele te kunnen maken
      dplyr::mutate(Kwartaal = stringr::str_sub(Kwartaal, start = 2)) %>%
      # maak onze effect variabelen
      dplyr::mutate(DiD_9 = ifelse(test = as.numeric(Kwartaal) == 9 & Interventie == 1,
                                   yes = 1,
                                   no = 0)) %>%
      dplyr::mutate(DiD_10 = ifelse(test = as.numeric(Kwartaal) == 10 & Interventie == 1,
                                    yes = 1,
                                    no = 0)) %>%
      dplyr::mutate(DiD_11 = ifelse(test = as.numeric(Kwartaal) == 11 & Interventie == 1,
                                    yes = 1,
                                    no = 0)) %>%
      dplyr::mutate(DiD_12 = ifelse(test = as.numeric(Kwartaal) == 12 & Interventie == 1,
                                    yes = 1,
                                    no = 0)) %>%
      # we maken kwartaal een factor
      dplyr::mutate(Kwartaal = as.factor(Kwartaal),
                    Run = as.factor(i))

    interventions <- rbind(interventions, df)
    intervention_output <- dplyr::lst(interventions, Effect, Kwartaaleffect, n_participants, Runs)
  }
  return(intervention_output)
}
