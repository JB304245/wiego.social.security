## code to prepare `DATASET` dataset goes

devtools::load_all()

DT_population = get_population_data()

usethis::use_data(DT_population, overwrite = TRUE)

