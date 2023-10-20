
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
