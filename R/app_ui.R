#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(


      titlePanel("Social Security subsidy for informal workers cost calculator"),
      shiny::h5("App maintainer: Jonathan Belke"),

      sidebarLayout(
        sidebarPanel(
          shiny::selectInput("country", "Country", choices = DT_population$reference_area),
          sliderInput("num_years", "Number of years to simulate",
                      min = 1, max = 20, value = 10, step = 1),
          sliderInput("ss_min_contribution", "Monthly minimum contribution total (Worker + Government share), USD",
                      min = 1, max = 20, value = 5, step = 0.50),
          sliderInput("ss_government_share", "Government share",
                      min = 0.33, max = 0.67, value = 0.5, step = 0.17),
          sliderInput("year_start_decrease", "Start decreasing subsidy after X years",
                      min = 0, max = 20, value = 10, step = 1),
          # sliderInput("current_population", "Current population, million",
          #             min = 1, max = 250, value = 57),
          # sliderInput("pop_growth", "Population growth rate",
          #             min = 0, max = 0.03, value = 0.02, step = 0.0025),
          sliderInput("percent_working_age", "Percent working age",
                      min = 0.4, max = 0.8, value = 0.6, step = 0.05),
          sliderInput("percent_workforce_participation", "Workforce participation rate",
                      min = 0.3, max = 0.7, value = 0.42, step = 0.01),
          sliderInput("percent_informal", "Percentage of informal workers",
                      min = 0.3, max = 0.9, value = 0.8, step = 0.01),
          sliderInput("participation_rate_in_ss", "Percentage of informal workers who sign up for SS immidiately",
                      min = 0, max = 1, value = 0.05, step = 0.01),
          sliderInput("participation_rate_in_ss_yearly_growth", "Additional sign up percentage per year",
                      min = 0, max = 0.2, value = 0.01, step = 0.005),
          sliderInput("inflation_rate", "Inflation rate",
                      min = 0, max = 0.2, value = 0.05, step = 0.01),
        ),

        mainPanel(
          shiny::plotOutput('cost_plot'),
          shiny::plotOutput('totals_per_worker'),
          shiny::dataTableOutput(outputId = 'model_table'),
          shiny::tableOutput('totals'),
          shiny::h3("Comments:"),
          shiny::h4("Population growth, workforce participation, inflation and so on are kept constant throughout the simulation."),
          shiny::h4("It is assumed that informal workers will contribute the minimum and the government will match that according to the government share"),
          shiny::h4("Percentage of informal workers who sign up for SS immidiately: Refers to how many people will sign up immidiately upon introduction of the program."),
          shiny::h4("Additional sign up percentage per year: Additional percentage points of how many informal workers will sign up each year following the introduction. Presumably the participation rate will slowly increase with time.")
        )
      )
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "wiego.social.security"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
