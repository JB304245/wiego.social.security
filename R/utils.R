determine_date_breaks = function(num_years,
                                 num_ticks_to_show=10) {

  year_breaks = ceiling(num_years / num_ticks_to_show)
  out = glue::glue("{year_breaks} year")

  return(out)

}

format_number_in_list = function(list_entry,
                                 digits = 2) {

  raw = unname(unlist(list_entry))

  if(!is.null(raw)) {

    out = round(raw, digits=digits)

    return(out)
  }

}
