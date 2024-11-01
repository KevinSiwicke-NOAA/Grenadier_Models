library(dplyr)
library(DBI)
library(keyring)
library(ggplot2)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

# Get longline survey RPWs
cpue <- dbGetQuery(channel_akfin,
                   "select    *
                from      afsc.lls_area_rpn_all_strata
                where     species_code = '21230' and
                          fmp_management_area = 'BSAI' and
                          council_management_area = 'Bering Sea' and
                          exploitable = 1 and
                          country = 'United States'
                order by  year asc
                ") |>
  rename_all(tolower) |> 
  group_by(year) |> 
  summarize(cpue = sum(rpw, na.rm = TRUE),
            cv = sqrt(sum(rpw_var, na.rm = TRUE)) / cpue) |> 
  mutate(strata = "EBS")

# Get bottom trawl survey biomass data
biomass <- dbGetQuery(channel_akfin, 
                      "select    *
                from      gap_products.akfin_biomass
                where     species_code = '21230' and
                          survey_definition_id = 78 and 
                          area_id = 99905
                order by  year asc
                ") |>  
  rename_all(tolower) |> 
  group_by(year) |>  
  summarize(n = n(), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) |> 
  mutate(strata = "EBS")

DBI::dbDisconnect(channel_akfin)

model_yrs <- 1997:YEAR

# This is the data that is brought into rema
model_dat <- list('biomass_dat' = biomass, 'cpue_dat' = cpue, 
                  'model_yrs' = model_yrs)
