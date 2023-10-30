
get_recent_indicator = function(indicator_name,
                                new_column_name,
                                years_to_look_back = 5) {

  DT = wbstats::wb_data(indicator = indicator_name,
                        start_date = lubridate::year(Sys.Date())-years_to_look_back,
                        end_date=lubridate::year(Sys.Date())-1)

  data.table::setDT(DT)
  data.table::setorderv(DT, cols= "date", order=-1)

  data.table::setnames(DT, old = indicator_name, new = new_column_name)

  DT = DT[!is.na(DT[[new_column_name]])]

  DT = DT[, .SD[1], by = "iso3c"]

  return(DT)

}
