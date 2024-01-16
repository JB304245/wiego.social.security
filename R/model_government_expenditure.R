#' model_government_expenditure
#'
#' @param num_years integer number of years to simulate
#' @param initial_expenditure expenditure in year 1
#' @param inflation inflation rate
#'
#' @return vector of length num_years
#' @export
#'
#' @examples model_government_expenditure(5, 1000, 0.01)
model_government_expenditure = function(num_years,
                                        initial_expenditure,
                                        inflation) {

  year = 1:num_years

  expenditure_by_year = initial_expenditure * (1 + inflation) ^ (year - 1)

  return(expenditure_by_year)

}
