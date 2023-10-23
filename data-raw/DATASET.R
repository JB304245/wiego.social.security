## code to prepare `DATASET` dataset goes

devtools::load_all()

population_data = get_population_data()

DT_population = population_data$population
DT_population_growth = population_data$population_growth

usethis::use_data(DT_population, overwrite = TRUE)
usethis::use_data(DT_population_growth, overwrite = TRUE)

