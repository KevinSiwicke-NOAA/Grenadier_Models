# Set up ----
# assessment year
YEAR <- 2024

# Consider whether the rema package needs to be 'updated' - will need to update to get new extra_cv fxns
# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE) #, force = TRUE

libs <- c('rema', 'readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot', 'DBI', 'keyring')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/AI_", YEAR); dir.create(dat_path)
out_path <- paste0("results/AI_", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 15) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

# Get longline survey RPWs
# First all of AI which extrapolates out west
cpue <- dbGetQuery(channel_akfin,
                   "select    *
                from      afsc.lls_area_rpn_all_strata
                where     species_code = '21230' and
                          fmp_management_area = 'BSAI' and
                          council_sablefish_management_area = 'Aleutians' and
                          exploitable = 1 and
                          country = 'United States'
                order by  year asc
                                ") |>
  rename_all(tolower)

ggplot(cpue |> filter(geographic_area_name %in% c("NE Aleutians slope", "SE Aleutians slope")), aes(year, rpn/100000)) +
  geom_line() +
  geom_point() + 
  facet_wrap(~geographic_area_name, nrow = 2) + 
  geom_ribbon(aes(ymin = rpn/100000 - 1.96*sqrt(rpn_var)/100000, 
                  ymax = rpn/100000 + 1.96*sqrt(rpn_var)/100000), alpha = 0.2) +
  labs(x = 'Year', y = 'RPN') +
  theme_bw()

ggsave(file = paste0(out_path, "/AI_strata_rpn.png"), dpi = 300, units = "in", height = 6, width = 4)

ggplot(cpue |> filter(geographic_area_name %in% c("NE Aleutians slope", "SE Aleutians slope")), aes(year, rpw/100000)) +
  geom_line() +
  geom_point() + 
  facet_wrap(~geographic_area_name, nrow = 2) + 
  geom_ribbon(aes(ymin = rpw/100000 - 1.96*sqrt(rpw_var)/100000, 
                  ymax = rpw/100000 + 1.96*sqrt(rpw_var)/100000), alpha = 0.2) +
  labs(x = 'Year', y = 'RPW') +
  theme_bw()

ggsave(file = paste0(out_path, "/AI_strata_rpw.png"), dpi = 300, units = "in", height = 6, width = 4)

cpue_dat <- cpue |>
  filter(geographic_area_name %in% c("NE Aleutians slope", "SE Aleutians slope")) |> 
  group_by(year) |>
  summarize(rpw = sum(rpw, na.rm = TRUE),
            rpw_var = sum(rpw_var, na.rm = TRUE),
            rpn = sum(rpn, na.rm = TRUE),
            rpn_var = sum(rpn_var, na.rm = TRUE)) |>
  mutate(strata = "AI")

ggplot(cpue_dat, aes(year, rpw/100000)) +
  geom_line() +
  geom_point() + 
  geom_ribbon(aes(ymin = rpw/100000 - 1.96*sqrt(rpw_var)/100000, 
                  ymax = rpw/100000 + 1.96*sqrt(rpw_var)/100000), alpha = 0.2) +
  labs(x = 'Year', y = 'RPW') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 16)) +
  theme_bw()

ggsave(file = paste0(out_path, "/AI_rpw.png"), dpi = 300, units = "in", height = 4, width = 8)

ggplot(cpue_dat, aes(year, rpn/100000)) +
  geom_line() +
  geom_point() + 
  geom_ribbon(aes(ymin = rpn/100000 - 1.96*sqrt(rpn_var)/100000, 
                  ymax = rpn/100000 + 1.96*sqrt(rpn_var)/100000), alpha = 0.2) +
  labs(x = 'Year', y = 'RPN') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 4)) +
  theme_bw()

ggsave(file = paste0(out_path, "/AI_rpn.png"), dpi = 300, units = "in", height = 4, width = 8)

# Get bottom trawl survey biomass data
biom <- dbGetQuery(channel_akfin, 
                   "select    *
                from      gap_products.akfin_biomass
                where     species_code = '21230' and 
                          survey_definition_id = 52 
                order by  year asc
                ") |> 
  rename_all(tolower)

DBI::dbDisconnect(channel_akfin)

# Using all data like previous model
ai_biom <- biom |> 
  filter(area_id == 99904) |> 
  group_by(year) |> 
  summarize(n = sum(n_haul), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

# write_csv(paste0(out_path, "/")

ggplot(ai_biom, aes(year, biomass / 100000)) + 
  geom_line() +
  geom_point() + 
  geom_ribbon(aes(ymin = biomass /100000 - biomass/100000*cv, 
                  ymax = biomass/100000 + biomass/100000*cv), alpha = 0.2) +
  labs(x = "Year", y = "Biomass (100,000 t)") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.9))

ggsave(paste0(out_path, '/AI_biom.png'), dpi = 300, height = 4, width = 8)

biomass <- biom |> 
  mutate(strata = ifelse(area_id == 299, 'WAI (1-500 m)',
                         ifelse(area_id == 3499, 'CAI (1-500 m)',
                                ifelse(area_id == 5699, 'EAI (1-500 m)',
                                       ifelse(area_id == 799, 'SBS (1-500 m)', NA))))) |> 
  filter(!is.na(strata)) |> 
  group_by(year, strata) |> 
  summarize(n = sum(n_haul), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

ggplot(biomass, aes(year, biomass / 10000)) + 
  geom_line() +
  geom_point() + 
  facet_wrap(~factor(strata, levels = c("WAI (1-500 m)", "CAI (1-500 m)", "EAI (1-500 m)", "SBS (1-500 m)")), nrow = 1) + 
  geom_ribbon(aes(ymin = biomass /10000 - biomass/10000*cv, ymax = biomass/10000 + biomass/10000*cv), alpha = 0.2) +
  labs(x = "Year", y = "Biomass (10,000 t)") +
  theme_bw()

ggsave(file = paste0(out_path, "/area_BTS_AI_index.png"), dpi = 300, units = "in", height = 3, width = 10)

# Combined plot
# Plot raw data by from both surveys
ll_dat <- cpue_dat |> 
  mutate(cv = sqrt(rpw_var) / rpw,
         lci = rpw + rpw*cv,
         rel_rpw = rpw / mean(rpw)) |> 
  rename(index = rel_rpw) |> 
  select(year, index, cv) |>
  mutate(Survey = "LLS")

bt_dat <- ai_biom |> 
  mutate(rel_biom = biomass / mean(biomass)) |> 
  rename(index = rel_biom) |> 
  select(year, index, cv) |>
  mutate(Survey = "BTS")

combo <- bind_rows(ll_dat, bt_dat) 

ggplot(combo, aes(year, index, col = Survey)) +
  geom_line(linewidth = 2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1,  lty = 2) +
  # geom_errorbar(aes(ymin = index - index*cv, ymax = index + index*cv)) +
  labs(x = "Year", y = "Relative Index") +
  theme(legend.position = "top")

ggsave(filename = paste0(out_path, '/AI_index_raw.png'), bg = 'white',
       dpi = 300, units = 'in', height = 5, width = 8)
