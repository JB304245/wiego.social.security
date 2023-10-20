

get_population_data = function() {


  provider_code = "ILO"
  dataset_code = "POP_2POP_SEX_AGE_NB"

  pop_DT = rdb_series(provider_code = "ILO", dataset_code = dataset_code, simplify = TRUE)
  pop_DT = pop_DT[series_name %like% "Total – Sex: Total – Annual"]
  pop_DT = pop_DT[series_name %like% "Aggregate bands"]

  get_series = function(series_code) {
    rdb(glue::glue("{provider_code}/{dataset_code}/{series_code}"))
  }

  raw_DT  = pop_DT[, get_series(series_code),
                       by = 1:nrow(pop_DT)]

  raw_DT = janitor::clean_names(raw_DT)

  DT = raw_DT[period < Sys.Date()]
  DT = DT[order(period, decreasing = TRUE)]

  DT = DT %>%
    dplyr::group_by(ref_area) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  data.table::setDT(DT)

  DT = DT[, .(ref_area, reference_area, population=value,
              year = original_period)]

  DT[, population := population * 1000]

  return(DT)

}

