

get_population_data = function() {

  provider_code = "ILO"
  dataset_code = "POP_2POP_SEX_AGE_NB"

  pop_DT = rdbnomics::rdb_series(provider_code = "ILO", dataset_code = dataset_code, simplify = TRUE)
  # Remove non-ASCII characters
  pop_DT[, series_name := stringi::stri_trans_general(series_name, "latin-ascii")]

  pop_DT = pop_DT[series_name %like% "Total - Sex: Total - Annual"]
  pop_DT = pop_DT[series_name %like% "Aggregate bands"]

  get_series = function(series_code) {
    rdbnomics::rdb(glue::glue("{provider_code}/{dataset_code}/{series_code}"))
  }

  raw_DT  = pop_DT[, get_series(series_code),
                       by = 1:nrow(pop_DT)]

  raw_DT = janitor::clean_names(raw_DT)


# Population --------------------------------------------------------------



  DT_population = raw_DT[period < Sys.Date()]
  DT_population = DT_population[order(period, decreasing = TRUE)]

  DT_population = DT_population %>%
    dplyr::group_by(ref_area) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  data.table::setDT(DT_population)

  DT_population = DT_population[, .(ref_area, reference_area, population=value,
              year = original_period)]

  DT_population[, population := population * 1000]


# Population Growth -------------------------------------------------------

  DT_pop_growth = raw_DT[period < Sys.Date()]
  DT_pop_growth = DT_pop_growth[order(period, decreasing = TRUE)]

  DT_pop_growth$original_period = as.numeric(DT_pop_growth$original_period)

  latest_year = DT_pop_growth[, .SD[1], by = "ref_area"]
  prior_year =  DT_pop_growth[, .SD[2], by = "ref_area"]

  latest_year = latest_year[, .(ref_area,
                  reference_area,
                  original_period,
                  value)]

  prior_year = prior_year[, .(ref_area,
                              reference_area,
                              prior_period = original_period,
                              prior_value = value)]

  latest_year[prior_year, c("prior_period",
                                      "prior_value") := .(prior_period,
                                                          prior_value),
                        on = c("ref_area")]

  # Make sure that the data prior year is one year prior.
  latest_year = latest_year[original_period == prior_period + 1]
  latest_year[, population_growth := (value - prior_value) / prior_value]

  DT_population_growth = latest_year
  DT_population_growth = DT_population_growth[, .(ref_area, reference_area,
                                                  population_growth,
                                                  year = original_period)]



# Output ------------------------------------------------------------------



  out = list(population = DT_population,
             population_growth = DT_population_growth)

  return(out)

}

get_gdp_data = function() {

  provider_code = "ND_GAIN"
  dataset_code = "gdp"

  all_datasets = rdbnomics::rdb_series(provider_code = provider_code,
                                       dataset_code = dataset_code,
                                       simplify = TRUE)

  all_datasets = all_datasets[series_name %like% "Input$"]

  get_series = function(series_code) {
    rdbnomics::rdb(glue::glue("{provider_code}/{dataset_code}/{series_code}"))
  }

  list_of_DTs  = lapply(all_datasets$series_code, get_series)

  raw_DT = data.table::rbindlist(l=list_of_DTs, use.names = TRUE, fill = TRUE)



}

get_public_spending_data = function() {


}
