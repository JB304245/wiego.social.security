#' model_population_and_workforce
#'
#' @param current_population current_population
#' @param pop_growth_current population growth rate
#' @param percent_working_age percentage of people who are of working age
#' @param workforce_participation_rate percentage of working age people who actually work
#' @param informal_percent_current percentage of informal workers (out of all workers)
#' @param period_in_years number of years to simulate
#'
#' @return a data.table containing year number, population, informal workers etc.
#' @export
#'
#' @examples model_population_and_workforce()
model_population_and_workforce = function(current_population = 57000000,
                                          pop_growth_current = 0.02,
                                          percent_working_age = 0.6,
                                          workforce_participation_rate = 0.42,
                                          informal_percent_current = 0.80,
                                          period_in_years = 10) {


  DT = data.table(year = 0:period_in_years)
  DT[, pop_multiplier := (1 + pop_growth_current) ^ year]
  DT[, population := current_population * pop_multiplier]
  DT[, pop_age_15_to_65 := population * percent_working_age]
  DT[, workforce_total := pop_age_15_to_65 * workforce_participation_rate]
  DT[, workforce_informal := workforce_total * informal_percent_current]

  return(DT)


}
