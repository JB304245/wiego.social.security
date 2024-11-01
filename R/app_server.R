#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2 data.table forcats
#' @noRd
app_server <- function(input, output, session) {

  tryCatch({
    sysfonts::font_add_google("Lato")
    showtext::showtext_auto()
  },
  error = function(e) {
    print("An error occured while downloading the font: ")
    print(e)
  })


# Assemble country data ---------------------------------------------------

    country_data = shiny::reactive({

      out = as.list(DT_country_data[country == input$country])

      out

    })

# Update inputs -----------------------------------------------------------

    shiny::observe({

      new_val = input$num_years
      shiny::updateSliderInput(session,
                               inputId = "year_start_decrease",
                               value = new_val)
    })


    shiny::observe({

      new_val = round(country_data()$inflation_percent)
      if(is.na(new_val)) new_val = 2

      shiny::updateSliderInput(session,
                               inputId ='inflation_rate',
                               value = new_val)

      shiny::updateSliderInput(session,
                               inputId ='percent_workforce_participation',
                               value = round(country_data()$labor_force_participation_percent))

      shiny::updateSliderInput(session,
                               inputId ='percent_working_age',
                               value = round(country_data()$working_age_percent))

    })



# Model table -------------------------------------------------------------


    model_table = shiny::reactive({

      population = country_data()$population

      population_growth = country_data()$population_growth / 100
      workforce_participation_rate = input$percent_workforce_participation / 100
      percent_working_age = input$percent_working_age / 100
      informal_percent_current = input$percent_informal / 100
      government_share_pct = input$ss_government_share / 100

      participation_rate_initial = input$participation_rate_in_ss / 100
      participation_rate_increase_yearly = input$participation_rate_in_ss_yearly_growth / 100
      inflation_rate = input$inflation_rate / 100

      government_expenses_pct_of_gdp = as.numeric(country_data()$government_expenses_pct_of_gdp)
      gdp = as.numeric(country_data()$gdp)
      current_spending = gdp * government_expenses_pct_of_gdp / 100

      give_subsidy_only_to_subgroup = input$give_subsidy_only_to_subgroup == "Yes"
      percent_Subgroup = input$percent_Subgroup / 100

      DT_workforce = model_population_and_workforce(current_population = population,
                                                    pop_growth_current = population_growth,
                                                    percent_working_age = percent_working_age,
                                                    workforce_participation_rate = workforce_participation_rate,
                                                    informal_percent_current = informal_percent_current,
                                                    period_in_years = input$num_years)

      DT_workforce[, government_spending := model_government_expenditure(num_years = input$num_years+1,
                                                                         initial_expenditure = current_spending,
                                                                         inflation = inflation_rate)]

      DT_workforce[, percent_informal_workers_signed_up_to_social_security := participation_rate_initial + participation_rate_increase_yearly * year]
      DT_workforce[, percent_informal_workers_signed_up_to_social_security := pmin(percent_informal_workers_signed_up_to_social_security, 1.0)]


      DT_workforce[, government_share := calculate_government_share(government_share_pct,
                                                                    period_in_years = input$num_years,
                                                                    start_decrease_after_x_years = input$year_start_decrease)]

      DT_workforce[, worker_share := 1 - government_share]

      DT_workforce[, government_cost_usd := model_cost(num_informal_workers = workforce_informal,
                                                       participation_rate = percent_informal_workers_signed_up_to_social_security,
                                                       minimum_contribution_monthly_usd = input$ss_min_contribution,
                                                       government_share = government_share,
                                                       give_subsidy_only_to_subgroup=give_subsidy_only_to_subgroup,
                                                       percent_Subgroup=percent_Subgroup)]

      DT_workforce[, num_informal_workers_signed_up_to_social_security := workforce_informal * percent_informal_workers_signed_up_to_social_security]

      if(give_subsidy_only_to_subgroup) {
        DT_workforce[, num_informal_workers_signed_up_to_social_security := num_informal_workers_signed_up_to_social_security * percent_Subgroup]
      }

      DT_workforce[, inflation_factor := (1 + inflation_rate) ^ year]
      DT_workforce[, government_cost_usd_inflation_adjusted := government_cost_usd * inflation_factor]
      DT_workforce[, government_cost_pct_of_spending := government_cost_usd_inflation_adjusted / government_spending]

      # For display later
      DT_workforce[, year_date := lubridate::as_date(paste0(year + lubridate::year(Sys.Date()), "-01-01"))]

      DT_workforce

    })

    output$model_table <- shiny::renderDataTable({

      DT = model_table()

      DT[, year := year + lubridate::year(Sys.Date())]

      DT = DT[, .(year,
                  population = round(population),
                  workforce_informal = round(workforce_informal),
                  num_signed_up_for_ss = round(num_informal_workers_signed_up_to_social_security),
                  government_cost_usd = round(government_cost_usd),
                  government_cost_usd_inflation_adjusted = round(government_cost_usd_inflation_adjusted))]

      DT[, population := scales::comma(population)]
      DT[, workforce_informal := scales::comma(workforce_informal)]
      DT[, num_signed_up_for_ss := scales::comma(num_signed_up_for_ss)]
      DT[, government_cost_usd := scales::dollar(government_cost_usd)]
      DT[, government_cost_usd_inflation_adjusted := scales::dollar(government_cost_usd_inflation_adjusted)]

      data.table::setnames(DT,
                           old = c("year", "population",
                                   "workforce_informal",
                                   "num_signed_up_for_ss",
                                   "government_cost_usd",
                                   "government_cost_usd_inflation_adjusted"),
                           new = c("Year", "Population",
                                   "Workforce informal",
                                   "Signed up for social security",
                                   "Government cost",
                                   "Government cost inflation adjusted"),
                           skip_absent = TRUE)

      DT

    })


    output$country_data_table = shiny::renderTable({

      old_keys = c("country",
                   "population",
                   "population_growth_percent",
                   "inflation_percent",
                   "labor_force_participation_percent",
                   "working_age_percent",
                   "gdp_ppp",
                   "gdp",
                   "governemnt_expenses_pct_of_gdp")

      new_keys = c("Country",
                   "Population",
                   "Population Growth (%)",
                   "Inflation (%)",
                   "Labor force participation (%)",
                   "Working age (%)",
                   "GDP (PPP)",
                   "GDP",
                   "Governemnt expenses (% of GDP)")

      country_data = country_data()

      country_data["population_growth_percent"] = format_number_in_list(country_data["population_growth_percent"])
      country_data["inflation_percent"] = format_number_in_list(country_data["inflation_percent"])
      country_data["labor_force_participation_percent"] = format_number_in_list(country_data["labor_force_participation_percent"])
      country_data["working_age_percent"] = format_number_in_list(country_data["working_age_percent"])
      country_data["governemnt_expenses_pct_of_gdp"] = format_number_in_list(country_data["governemnt_expenses_pct_of_gdp"])

      country_data["gdp"] = format_number_in_list(country_data["gdp"], digits=0)
      country_data["gdp_ppp"] = format_number_in_list(country_data["gdp_ppp"], digits=0)

      country_data[['population']] = scales::number(country_data[['population']], big.mark = ",")
      country_data[['gdp']] = scales::number(country_data[['gdp']], big.mark = ",")
      country_data[['gdp_ppp']] = scales::number(country_data[['gdp_ppp']], big.mark = ",")


      available_data = country_data[old_keys]
      available_data_names = names(available_data)
      available_data = available_data[!is.na(available_data_names)]

      DT = data.table::data.table(` ` = new_keys[old_keys %in% available_data_names],
                                  Value = unname(unlist(available_data)))

      DT

    })

    output$cost_plot = shiny::renderPlot({

      DT = model_table()

      text_size = 15
      legend_additional_size = 2
      font = "Lato"

      DT %>%
        ggplot2::ggplot(ggplot2::aes(x = year_date, y = government_cost_usd_inflation_adjusted, group = 1)) +
        ggplot2::geom_point(col = wiego_color("orange")) +
        ggplot2::geom_line(col = wiego_color("orange")) +
        ggplot2::scale_y_continuous(labels=scales::dollar) +
        ggplot2::scale_x_date(date_breaks = determine_date_breaks(nrow(DT)), date_labels = "%Y") +
        ggplot2::labs(title = "Yearly cost of subsidy",
             x = "Year",
             y = "Cost in $USD, inflation adjusted") +
        ggplot2::theme(axis.text.x = element_text(size=text_size, family = font),
                       axis.text.y = element_text(size=text_size, family = font),
                       plot.title = element_text(face="bold", family = font,
                                                 size=text_size+legend_additional_size))

    })


    output$cost_plot_pct_of_spending = shiny::renderPlot({

      DT = model_table()

      text_size = 15
      legend_additional_size = 2
      font = "Lato"

      DT %>%
        ggplot2::ggplot(ggplot2::aes(x = year_date, y = government_cost_pct_of_spending, group = 1)) +
        ggplot2::geom_point(col = wiego_color("orange")) +
        ggplot2::geom_line(col = wiego_color("orange")) +
        ggplot2::scale_y_continuous(labels=scales::percent) +
        ggplot2::scale_x_date(date_breaks = determine_date_breaks(nrow(DT)), date_labels = "%Y") +
        ggplot2::labs(title = "Yearly cost of subsidy as percentage of government spending",
                      x = "Year",
                      y = "Percentage of government spending") +
        ggplot2::theme(axis.text.x = element_text(size=text_size, family = font),
                       axis.text.y = element_text(size=text_size, family = font),
                       plot.title = element_text(face="bold", family = font,
                                                 size=text_size+legend_additional_size))


    })

    output$totals = shiny::renderTable({

      DT = model_table()

      total_cost = sum(DT$government_cost_usd)
      total_cost_inflation_adjusted = sum(DT$government_cost_usd_inflation_adjusted)

      out_DT = data.table(` ` = c('Total cost for period', 'Total cost adjusted for inflation'),
                          Amount = c(total_cost, total_cost_inflation_adjusted))

      out_DT$Amount = scales::dollar(out_DT$Amount)

      out_DT

    })


    output$totals_per_worker = shiny::renderPlot({

      text_size = 15
      legend_additional_size = 2
      font = "Lato"

      DT = model_table()

      total_cost_per_worker_for_worker = sum(DT$worker_share * input$ss_min_contribution * 12)
      total_cost_per_worker_for_government = sum(DT$government_share * input$ss_min_contribution * 12)
      total_amount_to_ss_per_worker = sum(nrow(DT) * 12 * input$ss_min_contribution)

      yearly_interest_gained = model_interest(rep(input$ss_min_contribution * 12, nrow(DT)), input$interest_rate / 100)

      plot_DT = data.table(comment = c('Cost for worker',
                                      'Cost for government',
                                      'Interest gained from contributions',
                                      'Total to social security'),
                          amount = c(total_cost_per_worker_for_worker,
                                     total_cost_per_worker_for_government,
                                     sum(yearly_interest_gained),
                                     total_amount_to_ss_per_worker + sum(yearly_interest_gained)))

      plot_DT[, comment := forcats::fct_reorder(comment, 1:nrow(plot_DT))]

      plot_DT %>%
        ggplot2::ggplot(ggplot2::aes(x=comment, y=amount, fill = comment)) +
        ggplot2::geom_col() +
        ggplot2::scale_y_continuous(labels = scales::dollar) +
        ggplot2::scale_x_discrete() +
        scale_fill_wiego(palette = "main") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Total cost per worker for the given time frame",
             subtitle = "Number of years * 12 * monthly share",
             y = "Amount",
             x = "") +
        ggplot2::theme(axis.text.x = element_text(size=text_size, family = font),
                       axis.text.y = element_text(size=text_size, family = font),
                       plot.title = element_text(face="bold", family = font,
                                                 size=text_size+legend_additional_size)) +
        ggplot2::guides(fill='none')

    })

}
