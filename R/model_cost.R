
#' model_cost
#'
#' @param num_informal_workers number of informal workers in the given year
#' @param participation_rate percentage of workers who are signed up to the social security program
#' @param minimum_contribution_monthly_usd total monthly minimum contribution (worker + government share)
#' @param government_share what percentage of the minimum_contribution_monthly_usd does the government shoulder?
#'
#' @return total cost of the subsidy for the given year
#' @export
#'
#' @examples model_cost(100000, 0.1)
model_cost = function(num_informal_workers,
                      participation_rate,
                      minimum_contribution_monthly_usd = 5.05,
                      government_share = 0.5) {

  total_cost = 12 * minimum_contribution_monthly_usd * government_share * num_informal_workers * participation_rate

  return(total_cost)

}

calculate_government_share = function(government_share_initial,
                                      period_in_years,
                                      start_decrease_after_x_years) {


  # This whole validation block doesnt work and i cant figure out why.


#   if(!is.null(government_share_initial)) {
#     if(government_share_initial > 0 & government_share_initial < 1) stop("government_share_initial needs to be > 0 and < 1")
#   }
#   if(period_in_years > 0) stop(message = "period_in_years needs to be > 0")
#   if(start_decrease_after_x_years > 0) stop( message = "start_decrease_after_x_years needs to be > 0")


  start_decrease_after_x_years = pmin(start_decrease_after_x_years, period_in_years)

  government_share = rep(government_share_initial, start_decrease_after_x_years+1)

  decay_period = period_in_years - start_decrease_after_x_years

  if(decay_period == 0) return(government_share)

  decaying_share = government_share_initial - ( 1:decay_period / decay_period * government_share_initial)

  out = c(government_share, decaying_share)

  return(out)


}
