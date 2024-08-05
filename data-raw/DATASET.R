## code to prepare datasets goes here

devtools::load_all()

# Population --------------------------------------------------------------


DT_country_data = get_recent_indicator(indicator_name = "SP.POP.TOTL",
                     new_column_name = "population")

DT_pop_growth = get_recent_indicator(indicator_name = "SP.POP.GROW",
                               new_column_name = "population_growth_percent")

DT_country_data[DT_pop_growth,
                population_growth_percent := population_growth_percent,
                on = "iso3c"]


# Inflation ---------------------------------------------------------------

DT_inflation = get_recent_indicator(indicator_name = "FP.CPI.TOTL.ZG",
                                    new_column_name = "inflation_percent")

DT_country_data[DT_inflation,
                inflation_percent := inflation_percent,
                on = 'iso3c']


# Labor force -------------------------------------------------------------

DT_labor_force_participation = get_recent_indicator("SL.TLF.ACTI.ZS",
                                                    "labor_force_participation_percent")


DT_country_data[DT_labor_force_participation,
                labor_force_participation_percent := labor_force_participation_percent,
                on = 'iso3c']


DT_working_age = get_recent_indicator(indicator_name = "SP.POP.1564.TO.ZS",
                                      new_column_name = "working_age_percent")


DT_country_data[DT_working_age,
                working_age_percent := working_age_percent,
                on = 'iso3c']

# DT_informal = get_recent_indicator("SL.ISV.IFRM.MA.ZS",
#                                    new_column_name = "informal_percent")


# GDP ---------------------------------------------------------------------

DT_GDP_PPP = get_recent_indicator(indicator_name = "NY.GDP.MKTP.PP.CD",
                              new_column_name = "gdp_ppp")

DT_country_data[DT_GDP_PPP,
                gdp_ppp := gdp_ppp,
                on = 'iso3c']

DT_GDP = get_recent_indicator(indicator_name = "NY.GDP.MKTP.CD",
                              new_column_name = "gdp")


DT_country_data[DT_GDP,
                gdp := gdp,
                on = 'iso3c']

# Expenses ----------------------------------------------------------------

DT_expenses = get_recent_indicator(indicator_name = "GC.XPN.TOTL.GD.ZS",
                                    new_column_name = "government_expenses_pct_of_gdp")


DT_country_data[DT_expenses,
                government_expenses_pct_of_gdp := government_expenses_pct_of_gdp,
                on = 'iso3c']

# Store -------------------------------------------------------------------

DT_country_data[, c("date", "unit", "obs_status",
                    "footnote", "last_updated") := NULL]


usethis::use_data(DT_country_data, overwrite = TRUE)


# Tests -------------------------------------------------------------------


# DT_wb_indicators = wbstats::wb_indicators()

test_DT = wbstats::wb_cachelist$indicators
data.table::setDT(test_DT)

s = test_DT[tolower(indicator) %like% "informal"]
s = test_DT[tolower(indicator) %like% "workforce"]
s = test_DT[tolower(indicator) %like% "middle class"]
