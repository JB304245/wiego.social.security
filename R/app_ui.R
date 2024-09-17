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

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/sliders.css"),
        tags$style('body {font-family: Lato;}')
      ),

      shiny::fluidRow(
        shiny::column(width = 2,
                      tags$img(src = "www/wiego_logo_main.jpg",
                               height = "70%",
                               width = "70%")),
        shiny::column(width = 10,
                      h1("WIEGO's Social Security Subsidy"),
                      h1("for Informal Workers Cost Calculator"))
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(width=6,
                      shiny::h5("Social security is a human right and labour right for all workers, including workers in informal employment. For workers in formal wage employment, the affordability of social insurance contributions and adequacy of benefits is generally ensured by dividing contributions between themselves and their employer."
                                ),
                      shiny::h5("Self-employed workers, on the other hand, are often expected to shoulder the entire burden of paying contributions, which results in unaffordably high contribution rates or contribution payments that are too low to yield adequate benefits. This is a major barrier for the nearly 80 percent of informal workers in developing countries that are self-employed."
                                )
                      ),
        shiny::column(width=6,
                      shiny::h5("The most effective way to address this affordability gap is for governments to subsidize social insurance contributions for low-income informal workers. Global evidence shows that countries that have managed to significantly expand social insurance coverage to informal workers have recognized this and implemented various forms of subsidies."
                                ),
                      shiny::h5("This calculator helps estimate the costs of different subsidy options, as well as the impacts of those on workers’ social security benefits."
                                )
                      )
      ),
      shiny::fluidRow(
        shiny::column(width=8, offset=2,
                      shiny::tags$a(href="mailto: Florian.Juergens-Grant@WIEGO.org",
                                    "For more information on WIEGO’s efforts to support the expansion of social protection to all informal workers, contact Florian Juergens-Grant (Florian.Juergens-Grant@WIEGO.org).")
                      )
        ),
      shiny::hr(),
      sidebarLayout(
        sidebarPanel(
          shiny::selectInput("country", "Country", choices = sort(DT_country_data$country),
                             selected = "Kenya"),
          shiny::hr(),
          shiny::h4("Model control"),
          sliderInput("num_years", "Number of years to simulate",
                      min = 1, max = 35, value = 10, step = 1),
          shiny::numericInput("ss_min_contribution", "Monthly minimum contribution total (Worker + Government share), USD",
                      min = 1.0, max = Inf, value = 5.0, step = 0.50),
          sliderInput("ss_government_share", "Government share of total monthly contribution (%)",
                      min = 0, max = 100, value = 50, step = 5),
          sliderInput("year_start_decrease", "Start decreasing subsidy after X years",
                      min = 0, max = 35, value = 10, step = 1),
          shiny::numericInput("participation_rate_in_ss", "Percentage of informal workers who sign up for social security immediately",
                      min = 0, max = 100, value = 5, step = 1),
          shiny::numericInput("participation_rate_in_ss_yearly_growth", "Additional sign up percentage per year",
                      min = 0, max = 100, value = 1, step = 0.5),
          # sliderInput("current_population", "Current population, million",
          #             min = 1, max = 250, value = 57),
          # sliderInput("pop_growth", "Population growth rate",
          #             min = 0, max = 0.03, value = 0.02, step = 0.0025),
          shiny::hr(),
          shiny::h4("Economic data"),
          sliderInput("percent_working_age", "Working age (%) - populated based on World Bank data",
                      min = 0, max = 100, value = 60, step = 1),
          sliderInput("percent_workforce_participation", "Workforce participation rate (%) - populated based on World Bank data",
                      min = 0, max = 100, value = 66, step = 1),
          sliderInput("percent_informal", "Informal workers (%)",
                      min = 0, max = 100, value = 80, step = 1),
          sliderInput("inflation_rate", "Inflation rate (%) - populated based on World Bank data",
                      min = -2, max = 50, value = 5, step = 1),
          sliderInput("interest_rate", "Interest rate (%)",
                      min = 0, max = 20, value = 6, step = 1),
        ),

        mainPanel(
          shiny::plotOutput('cost_plot'),
          shiny::hr(),
          shiny::plotOutput('cost_plot_pct_of_spending'),
          shiny::hr(),
          shiny::plotOutput('totals_per_worker')
      )), # End of sidebar layout
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(width=4,
                      shiny::h4("Summary"),
                      shiny::tableOutput("totals"),
                      shiny::h4("Country Information"),
                      shiny::tableOutput("country_data_table")
                      ),
        shiny::column(width=8,
                      shiny::dataTableOutput("model_table"),
                      shiny::h3("Comments:"),
                      shiny::h4("World Bank data retrieved via the World Bank API using the wbstats R Package."),
                      shiny::tags$a(href="https://documents.worldbank.org/en/publication/documents-reports/api", "World Bank API Documentation"),
                      shiny::tags$a(href="https://cran.r-project.org/web/packages/wbstats/vignettes/wbstats.html", "wbstats R Package"),
                      shiny::hr(),
                      shiny::h4("Population growth, workforce participation, inflation and so on are kept constant throughout the simulation."),
                      shiny::h4("It is assumed that informal workers will contribute the minimum and the government will match that according to the government share"),
                      shiny::h4("Percentage of informal workers who sign up for social security immediately: Refers to how many people will sign up immidiately upon introduction of the program."),
                      shiny::h4("Additional sign up percentage per year: Additional percentage points of how many informal workers will sign up each year following the introduction. Presumably the participation rate will slowly increase with time.")
                      )
        )
    ) # End of fluidPage
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
