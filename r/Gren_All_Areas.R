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
dat_path <- paste0("data/ALL_", YEAR); dir.create(dat_path)
out_path <- paste0("results/ALL_", YEAR); dir.create(out_path)

# plot catch from akfin data
gren_cat <- read.csv(paste0(dat_path, "/gren_catch.csv"), header=T) |> 
  pivot_longer(cols = c("EBS", "AI", "GOA"), names_to = "Area", values_to = "Catch") 

ggplot(gren_cat, aes(Year, Catch, col = Area)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(expand = c(0,500)) +
  scale_color_viridis_d() +
  ylab('Catch (t)') +
  theme(legend.position = 'top')

ggsave(filename = paste0(out_path, '/gren_catch.png'),
       dpi = 300, bg = 'white', units = 'in', height = 4, width = 7)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- dbConnect (odbc::odbc(),
                            dsn = db,
                            uid = keyring::key_list(db)$username,
                            pwd =  keyring::key_get(db, keyring::key_list(db)$username))

area_dat <- dbGetQuery(channel_akfin,
                       "select    *
                from      afsc.lls_area_rpn_all_strata
                where     species_code = '21230' and
                          exploitable = 1 and
                          country = 'United States'
                order by  year asc
                ") |> 
  rename_all(tolower)

area_index <- area_dat |>
  filter(year > 1991, year < YEAR + 1) |> 
  group_by(year, strata = council_management_area) |> 
  mutate(strata = ifelse(strata == 'Western Gulf of Alaska', 'WGOA',
                         ifelse(strata == 'Central Gulf of Alaska', 'CGOA',
                                ifelse(strata == 'Eastern Gulf of Alaska', 'EGOA',
                                       ifelse(strata == 'Bering Sea', 'EBS', 
                                              ifelse(geographic_area_name %in% c("NE Aleutians slope", "SE Aleutians slope"), 'EAI', 'WAI')))))) |> 
  summarize(rpw = sum(rpw, na.rm = TRUE),
            rpw_var = sum(rpw_var, na.rm = TRUE),
            rpn = sum(rpn, na.rm = TRUE),
            rpn_var = sum(rpn_var, na.rm = TRUE)) |> 
  write_csv(paste0(out_path, '/LLS_area_indices.csv'))

ggplot(area_index |> filter(!strata == "WAI"), aes(year, rpn / 100000)) + 
  # geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = rpn / 100000 - 1.96*sqrt(rpn_var) / 100000, 
                  ymax = rpn / 100000+ 1.96*sqrt(rpn_var) / 100000)) + 
  labs(x = 'Year', y = 'Relative Population Numbers') +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 4)) +
  theme_bw() +
  facet_wrap(~factor(strata, levels = c("EAI", "EBS", "WGOA", "CGOA", "EGOA")), nrow = 2)

ggsave(paste0(out_path, '/Area_LL_rpn.png'), dpi = 300, height = 6, width = 12)

ggplot(area_index |> filter(!strata == "WAI"), aes(year, rpw / 100000)) + 
  # geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = rpw / 100000 - 1.96*sqrt(rpw_var) / 100000, 
                  ymax = rpw / 100000+ 1.96*sqrt(rpw_var) / 100000)) + 
  labs(x = 'Year', y = 'Relative Population Weight') +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 16)) +
  theme_bw() +
  facet_wrap(~factor(strata, levels = c("EAI", "EBS", "WGOA", "CGOA", "EGOA")), nrow = 2)

ggsave(paste0(out_path, '/Area_LL_rpw.png'), dpi = 300, height = 6, width = 12)

fmp_index <- area_dat |>
  filter(year > 1991, year < YEAR + 1) |> 
  group_by(year, strata = council_management_area) |> 
  mutate(strata = ifelse(strata == 'Western Gulf of Alaska', 'GOA',
                         ifelse(strata == 'Central Gulf of Alaska', 'GOA',
                                ifelse(strata == 'Eastern Gulf of Alaska', 'GOA',
                                       ifelse(strata == 'Bering Sea', 'EBS', 
                                              ifelse(geographic_area_name %in% c("NE Aleutians slope", "SE Aleutians slope"), 'EAI', 'WAI')))))) |> 
  summarize(rpw = sum(rpw, na.rm = TRUE),
            rpw_var = sum(rpw_var, na.rm = TRUE),
            rpn = sum(rpn, na.rm = TRUE),
            rpn_var = sum(rpn_var, na.rm = TRUE)) |> 
  write_csv(paste0(out_path, '/LLS_fmp_indices.csv'))

ggplot(fmp_index |> filter(!strata == "WAI"), aes(year, rpn / 100000)) + 
  # geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = rpn / 100000 - 1.96*sqrt(rpn_var) / 100000, 
                  ymax = rpn / 100000 + 1.96*sqrt(rpn_var) / 100000)) + 
  labs(x = 'Year', y = 'Relative Population Numbers') +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 4.5)) +
  theme_bw() +
  facet_wrap(~factor(strata, levels = c("EAI", "EBS", "GOA")), nrow = 1)

ggsave(paste0(out_path, '/fmp_LL_rpn.png'), dpi = 300, height = 3.5, width = 10)

ggplot(fmp_index |> filter(!strata == "WAI"), aes(year, rpw / 100000)) + 
  # geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = rpw / 100000 - 1.96*sqrt(rpw_var) / 100000, 
                  ymax = rpw / 100000 + 1.96*sqrt(rpw_var) / 100000)) + 
  labs(x = 'Year', y = 'Relative Population Weight') +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 16)) +
  theme_bw() +
  facet_wrap(~factor(strata, levels = c("EAI", "EBS", "GOA")), nrow = 1)

ggsave(paste0(out_path, '/fmp_LL_rpw.png'), dpi = 300, height = 3.5, width = 10.5)

biom <- dbGetQuery(channel_akfin, 
                   "select    *
                from      gap_products.akfin_biomass
                where     species_code = '21230' and 
                          year < 2024
                order by  year asc
                ") |> 
  rename_all(tolower)

biomass <- biom |> 
  mutate(strata = ifelse(area_id == 99905, 'EBS',
                         ifelse(area_id == 99904, 'AI',
                                ifelse(area_id == 99903, 'GOA', NA)))) |> 
  filter(!is.na(strata)) |> 
  mutate(biomass = biomass_mt / 100000,
         lci = biomass - 1.96*sqrt(biomass_var)/ 100000,
         uci = biomass + 1.96*sqrt(biomass_var)/ 100000) |> 
  mutate(lci = ifelse(lci < 0, 0, lci))

ggplot(biomass, aes(year, biomass)) + 
  # geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = lci, ymax = uci)) + 
  labs(x = 'Year', y = 'Biomass (100,000 t)') +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 12.6)) +
  theme_bw() +
  facet_wrap(~factor(strata, levels = c("AI", "EBS", "GOA")), nrow = 1)

ggsave(paste0(out_path, '/fmp_bts_biomass.png'), dpi = 300, height = 3.5, width = 10)

lls.gren.len <- dbGetQuery(channel_akfin, 
                           "select    *
                from      afsc.lls_length_rpn_by_area_all_strata
                where     species_code = '21230' 
                order by  year asc") |> 
  rename_all(tolower) |> 
  filter(year > 1991)

ll.len <- lls.gren.len |> 
  filter(!length == 999) |> 
  rename(strata = council_sablefish_management_area) |> 
  mutate(strata = ifelse(strata == 'Western Gulf of Alaska', 'GOA',
                         ifelse(strata == 'Central Gulf of Alaska', 'GOA',
                                ifelse(strata == 'West Yakutat' | strata == 'East Yakutat/Southeast', 'GOA',
                                       ifelse(strata == 'Bering Sea', 'EBS', 
                                              ifelse(geographic_area_name %in% c("NE Aleutians slope", "SE Aleutians slope"), 'AI', NA)))))) |> 
  filter(!is.na(strata)) |> 
  group_by(strata, year, length) |> 
  summarize(freq = sum(rpn, na.rm = T))

ll.len$calc = ll.len$freq*ll.len$length

ll.means = ll.len |> group_by(year, strata) |> 
  summarize(tot = sum(freq), l_calc = sum(calc))

ll.means$mean = ll.means$l_calc/ll.means$tot
ll.len = merge(ll.len, ll.means, by=c("year", "strata")) |> 
  mutate(survey = "LLS")

ggplot(ll.len, aes(year, mean, col = factor(strata, levels = c("AI", "EBS", "GOA")))) + 
  # geom_line() +
  geom_point() + 
  labs(x = "Year", y = "Mean pre-anal fin length (cm)") +
  scale_color_viridis_d("Area")

ggsave(paste0(out_path, '/LL_length.png'), dpi = 300, height = 5, width = 10)

# Trawl lengths
bts.gren.len <- dbGetQuery(channel_akfin, 
                           "select    *
                from      gap_products.akfin_sizecomp
                where     species_code = '21230' and
                          year > 1989 and
                          year < 2025 and
                          length_mm > 0") %>% 
  rename_all(tolower) 

dbDisconnect(channel_akfin)

bts.len = bts.gren.len |> 
  mutate(length = length_mm / 10,
  strata = ifelse(area_id == 99905, 'EBS',
                         ifelse(area_id == 99904, 'AI',
                                ifelse(area_id == 99903, 'GOA', NA)))) |> 
  filter(!is.na(strata)) |> 
  group_by(strata, year, length) |> 
  summarize(freq = sum(population_count, na.rm = T))

bts.len$calc = bts.len$freq*bts.len$length

bts.means = bts.len |> group_by(year, strata) |> 
  summarize(tot = sum(freq), l_calc = sum(calc))

bts.means$mean = bts.means$l_calc/bts.means$tot
bts.len = merge(bts.len, bts.means, by=c("year", "strata")) |> 
  mutate(survey = 'BTS')

ggplot(bts.len, aes(year, mean, col = factor(strata, levels = c('EBS', 'AI', 'GOA')))) + 
  # geom_line() +
  geom_point() + 
  labs(x = "Year", y = "Mean pre-anal fin length (cm)") +
  scale_color_viridis_d("Area")

ggsave(paste0(out_path, '/BT_length.png'), dpi = 300, height = 5, width = 10)

all.len <- bind_rows(ll.len, bts.len) |> 
  mutate(id = paste(as.character(survey), as.character(strata)))

ggplot(all.len, aes(year, mean, shape = survey)) + 
  scale_shape_manual(values = c(1,16)) +
  # geom_line() +
  geom_point(size = 3) + 
  facet_wrap(~strata) +
  # geom_smooth(method = 'lm') +
  labs(x = "Year", y = "Mean pre-anal fin length (cm)") +
  scale_color_viridis_d("Survey")

ggsave(paste0(out_path, '/All_length.png'), dpi = 300, height = 5, width = 10)
