#' ILO population data
#'
#' Population data by country
#'
#' @format ## `who`
#' A data.table
#' \describe{
#'   \item{ref_area}{ISO country codes}
#'   \item{reference_area}{Country names}
#'   \item{population}{Population}
#'   \item{year}{Year}
#'   ...
#' }
#' @source https://db.nomics.world/ ILO
"DT_population"

#' ILO population growth data
#'
#' Population growth data by country
#'
#' @format ## `who`
#' A data.table
#' \describe{
#'   \item{ref_area}{ISO country codes}
#'   \item{reference_area}{Country names}
#'   \item{population_growth}{Population growth for the given year}
#'   \item{year}{Year}
#'   ...
#' }
#' @source https://db.nomics.world/ ILO
"DT_population_growth"
