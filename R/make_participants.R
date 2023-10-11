#' Make participants
#' simulates an intervention and control group
#'
#' @param IP the number of intervention participants
#' @param CP the number of control participants
#'
#' @return data frame with participants, indicated with interventie (yes/no) and mean number of verwijzingen.
#' @export

make_participants <- function(IP, CP){
  # make a dataframe with total number of participants
  participants <- data.frame(id = 1:(IP + CP)) %>%
    # label intervention participants
    dplyr::mutate(Interventie = ifelse(test = id <= IP, yes = 1, no = 0 )) %>%
    # calculate mean number of referrals for IP and CP
    dplyr::mutate(K1 = c(rep(40, each = IP), sample(x = 37:43,
                                                    size = CP,
                                                    replace = T))/100)
return(participants)

}


