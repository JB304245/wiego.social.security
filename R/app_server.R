#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2 data.table
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic



# Assemble country data ---------------------------------------------------


    country_data = shiny::reactive({

      # out = list(population = DT_country_data[country == input$country, population],
      #            population_growth_percent = DT_country_data[country == input$country, population_growth_percent],
      #            inflation_percent = DT_country_data[country == input$country, inflation_percent],
      #            labor_force_participation_percent = DT_country_data[country == input$country, labor_force_participation_percent],
      #            working_age_percent = DT_country_data[country == input$country, working_age_percent])

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

      # population = input$current_population * 1000000
      population = country_data()$population
      population_growth = country_data()$population_growth / 100

      workforce_participation_rate = input$percent_workforce_participation / 100

      percent_working_age = input$percent_working_age / 100

      informal_percent_current = input$percent_informal / 100



      DT_workforce = model_population_and_workforce(current_population = population,
                                                    pop_growth_current = population_growth,
                                                    percent_working_age = percent_working_age,
                                                    workforce_participation_rate = workforce_participation_rate,
                                                    informal_percent_current = informal_percent_current,
                                                    period_in_years = input$num_years)


      participation_rate_initial = input$participation_rate_in_ss
      participation_rate_increase_yearly = input$participation_rate_in_ss_yearly_growth


      DT_workforce[, percent_informal_workers_signed_up_to_social_security := participation_rate_initial + participation_rate_increase_yearly * year]
      DT_workforce[, percent_informal_workers_signed_up_to_social_security := pmin(percent_informal_workers_signed_up_to_social_security, 1.0)]

      DT_workforce[, government_share := calculate_government_share(input$ss_government_share,
                                                                    period_in_years = input$num_years,
                                                                    start_decrease_after_x_years = input$year_start_decrease)]

      DT_workforce[, worker_share := 1 - government_share]


      DT_workforce[, government_cost_usd := model_cost(num_informal_workers = workforce_informal,
                                                       participation_rate = percent_informal_workers_signed_up_to_social_security,
                                                       minimum_contribution_monthly_usd = input$ss_min_contribution,
                                                       government_share = government_share)]

      DT_workforce[, num_informal_workers_signed_up_to_social_security := workforce_informal * percent_informal_workers_signed_up_to_social_security]


      inflation_rate = input$inflation_rate / 100

      DT_workforce[, inflation_factor := (1 + inflation_rate) ^ year]

      DT_workforce[, goverment_cost_usd_inflation_adjusted := government_cost_usd * inflation_factor]

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
                  goverment_cost_usd_inflation_adjusted = round(goverment_cost_usd_inflation_adjusted))]

      DT[, population := scales::comma(population)]
      DT[, workforce_informal := scales::comma(workforce_informal)]
      DT[, num_signed_up_for_ss := scales::comma(num_signed_up_for_ss)]
      DT[, government_cost_usd := scales::dollar(government_cost_usd)]
      DT[, goverment_cost_usd_inflation_adjusted := scales::dollar(goverment_cost_usd_inflation_adjusted)]

      DT

    })


    output$country_data_table = shiny::renderTable({

      country_data = country_data()

      df = data.frame(name = names(country_data),
                      value = unname(unlist(country_data)))

      df


    })

    output$cost_plot = shiny::renderPlot({

      DT = model_table()

      DT[, year_date := lubridate::as_date(paste0(year, "-01-01"))]

      DT %>%
        ggplot2::ggplot(ggplot2::aes(x = year_date, y = goverment_cost_usd_inflation_adjusted, group = 1)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::scale_y_continuous(labels=scales::dollar) +
        ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        ggplot2::labs(title = "Yearly cost of subsidy",
             x = "Year",
             y = "Cost in $USD, inflation adjusted")



    })

    output$totals = shiny::renderTable({

      DT = model_table()

      total_cost = sum(DT$government_cost_usd)
      total_cost_inflation_adjusted = sum(DT$goverment_cost_usd_inflation_adjusted)

      out_DT = data.table(comment = c('Total cost for period', 'Total cost adjusted for inflation'),
                          amount = c(total_cost, total_cost_inflation_adjusted))

      out_DT$amount = scales::dollar(out_DT$amount)



      # out = glue::glue("Total cost for period: {total_cost} \n
      #                   Total cost adjusted for inflation: {total_cost_inflation_adjusted}")

      out_DT

    })


    output$totals_per_worker = shiny::renderPlot({

      DT = model_table()

      total_cost_per_worker_for_worker = sum(DT$worker_share * input$ss_min_contribution * 12)
      total_cost_per_worker_for_government = sum(DT$government_share * input$ss_min_contribution * 12)
      total_amount_to_ss_per_worker = sum(nrow(DT) * 12 * input$ss_min_contribution)

      out_DT = data.table(comment = c('Total cost for worker',
                                      'Total cost for government',
                                      'Total going to social security'),
                          amount = c(total_cost_per_worker_for_worker,
                                     total_cost_per_worker_for_government,
                                     total_amount_to_ss_per_worker))

      out_DT %>%
        ggplot2::ggplot(ggplot2::aes(x=comment, y=amount, fill = comment)) +
        geom_col() +
        scale_y_continuous(labels = scales::dollar) +
        scale_x_discrete(labels = NULL) +
        labs(title = "Total cost per worker for the given time frame",
             subtitle = "(number of years * 12 * monthly share)",
             y = "Amount",
             x = "")

      # out_DT$amount = scales::dollar(out_DT$amount)
      #
      #
      # out_DT

    })



}
