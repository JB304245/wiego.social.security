
#' model_cost
#'
#' @param num_informal_workers number of informal workers in the given year
#' @param participation_rate percentage of workers who are signed up to the social security program
#' @param minimum_contribution_monthly_usd total monthly minimum contribution (worker + government share)
#' @param government_share what percentage of the minimum_contribution_monthly_usd does the government shoulder?
#' @param give_subsidy_only_to_subgroup should the subsidy only be given to a specific subgroup of all informal workers?
#' @param percent_Subgroup percentage of informal workers belonging to the subgroup (out of all informal workers)
#'
#' @return total cost of the subsidy for the given year
#' @export
#'
#' @examples model_cost(100000, 0.1)
model_cost = function(num_informal_workers,
                      participation_rate,
                      minimum_contribution_monthly_usd = 5.05,
                      government_share = 0.5,
                      give_subsidy_only_to_subgroup=FALSE,
                      percent_Subgroup=0.5) {

  total_cost = 12 * minimum_contribution_monthly_usd * government_share * num_informal_workers * participation_rate

  if(give_subsidy_only_to_subgroup) {
    total_cost = total_cost * percent_Subgroup
  }

  return(total_cost)

}

calculate_government_share = function(government_share_initial,
                                      period_in_years,
                                      start_decrease_after_x_years) {


  start_decrease_after_x_years = pmin(start_decrease_after_x_years, period_in_years)

  government_share = rep(government_share_initial, start_decrease_after_x_years+1)

  decay_period = period_in_years - start_decrease_after_x_years

  if(decay_period == 0) return(government_share)

  decaying_share = government_share_initial - ( 1:decay_period / decay_period * government_share_initial)

  out = c(government_share, decaying_share)

  return(out)


}
