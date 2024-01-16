#' model_interest
#'
#' @param yearly_contributions numeric vector of yearly contributions going into the system. from year 1 to year n
#' @param interest_rate numeric scalar interest_rate
#'
#' @return numeric vector of interest generated in each year
#' @export
#'
#' @examples model_interest(c(1000, 1250, 1600), 0.06)
model_interest = function(yearly_contributions, interest_rate) {

  if(length(yearly_contributions) == 0) {
    stop("yearly_contributions needs to be length >= 1")
  }

  balance_from_previous_years = 0
  interest_gained = rep(NA_real_, times = length(yearly_contributions))

  for(i in 1:length(yearly_contributions)) {

    contribution = yearly_contributions[i]

    # We assume that only half the interest rate applies to the contributions from the
    # current year, as the contributions will be spread out roughly evenly throughout the year
    interest_from_current_contribution = contribution * interest_rate / 2

    # The full interest rate applies to our balance from past year(s)
    interest_from_previous_balance = balance_from_previous_years * interest_rate

    interest_gained[i] = interest_from_current_contribution + interest_from_previous_balance

    # Update the balance for the next year
    balance_from_previous_years = balance_from_previous_years + interest_from_previous_balance + contribution + interest_from_current_contribution

  }

  return(interest_gained)


}
