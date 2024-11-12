# EBS grenadier biomass estimation used the slope bottom trawl in 2020
# Taking the average of the last three surveys as the biomass estimate
# This survey last occured in 2016, so no new data, and estimate unchanged
# Here we look at covnerting to random effects of the trawl survey, then adding
# the longline survey, so new data can continue to come into the model

# Model naming conventions:
# M24.0 - slope trawl only, ends in 2016
# M24.1 - add LLS to trawl, extends data to 2023

# Set up ----
# assessment year
YEAR <- 2024

# Consider whether the rema package needs to be 'updated' - will need to update to get new extra_cv fxns
# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE) #, force = TRUE

libs <- c('rema', 'readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/EBS_", YEAR); dir.create(dat_path)
out_path <- paste0("results/EBS_", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 15) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# Read data ----
source("r/EBS_data_pull_24.r")

# bottom trawl survey
biomass_dat <- model_dat$biomass_dat
biomass_dat |> 
  write_csv(paste0(dat_path, "/ebs_gren_biomass_", YEAR, ".csv"))

biomass_dat |>
  tidyr::expand(year = min(biomass_dat$year):(YEAR), strata) |>
  left_join(biomass_dat |>
              mutate(value = ifelse(is.na(biomass), NA,
                                    paste0(prettyNum(round(biomass, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) |>
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) |>
  arrange(year) |>
  write_csv(paste0(out_path, '/biomass_data_wide.csv'))

# longline survey rpws
cpue_dat <- model_dat$cpue_dat 
cpue_dat |> 
  write_csv(paste0(dat_path, "/ebs_gren_rpw_", YEAR, ".csv"))

cpue_dat |> 
  tidyr::expand(year = min(cpue_dat$year):(YEAR), strata) |>
  left_join(cpue_dat |>
              mutate(value = ifelse(is.na(cpue), NA,
                                    paste0(prettyNum(round(cpue, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) |>
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) |>
  arrange(year) |>
  write_csv(paste0(out_path, '/cpue_data_wide.csv'))

# Plot raw data by from both surveys
combo <- cpue_dat |> rename(index = cpue) |> mutate(Survey = "LLS") |> 
  bind_rows(biomass_dat |> rename(index = biomass) |> mutate(Survey = "BTS"))

ggplot(combo, aes(year, index / 100000, col = Survey)) +
  geom_point() +
  geom_errorbar(aes(ymin = (index - index*cv) / 100000, ymax = (index + index*cv) / 100000)) +
  labs(x = "Year", y = "Index (hundred thousand)") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 8))

ggsave(filename = paste0(out_path, '/EBS_index_raw.png'), bg = 'white',
       dpi = 300, units = 'in', height = 4, width = 8)

# Model 24.0 - Slope trawl survey only ----
input <- prepare_rema_input(model_name = 'Model 24.0 - BTS Only',
                            multi_survey = 0,
                            biomass_dat = biomass_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1997,
                            end_year = YEAR + 2)

m24.0 <- fit_rema(input)
out24.0 <- tidy_rema(m24.0)
out24.0$parameter_estimates 

compare <- compare_rema_models(rema_models = list(m24.0))

compare$plots$biomass_by_strata +
  theme(legend.position = 'top') +
  geom_line() +
  labs(x = NULL, y = NULL, subtitle = 'Trawl survey biomass (t)',
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#440154FF')) +
  scale_color_discrete(type = c('#440154FF'))

ggsave(filename = paste0(out_path, '/BTS_REMA_totalbiomass.png'),
       dpi = 300, bg = 'white', units = 'in', height = 4, width = 6)

# Model 24.1, add longline survey index ----
input <- prepare_rema_input(model_name = 'Model 24.1 - 2-survey',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1997,
                            end_year = YEAR + 2)

m24.1 <- fit_rema(input)
out24.1 <- tidy_rema(m24.1)
out24.1$parameter_estimates 

# Compare M24.0 and 24.1 ----
compare <- compare_rema_models(rema_models = list(m24.0, m24.1))

compare$plots$total_predicted_biomass +
  labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_y_continuous(expand = c(0,0), limits =c(0, 900000)) 

ggsave(filename = paste0(out_path, '/Mod_comp_biomass.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 10)

comp <- compare_rema_models(rema_models = list(m24.1))

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     theme(legend.position = 'top') +
                     geom_line() +
                     labs(y = 'Biomass (t)', x = 'Year', 
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#FDE725FF', '#440154FF')) +
                     scale_color_discrete(type = c('#FDE725FF', '#440154FF')) +
                     theme(legend.position = 'top'), 
                   comp$plots$cpue_by_strata +
                     theme(legend.position = 'none') +
                     geom_line() +
                     labs(y = 'RPW', x = 'Year',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF')) +
                     scale_color_discrete(type = c('#440154FF')),
                   ncol = 2, align = "h")

ggsave(filename = paste0(out_path, '/mod_comp_strata.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 9)

# Selected Model plots

comp$plots$biomass_by_strata +
  geom_point(aes(comp$plots$cpue_by_strata$data$year, comp$plots$cpue_by_strata$data$obs))+
  comp$plots$cpue_by_strata +
  theme(legend.position = 'top') +
  geom_line() +
  labs(y = 'Biomass (t)', x = 'Year', 
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#FDE725FF', '#440154FF')) +
  scale_color_discrete(type = c('#FDE725FF', '#440154FF')) +
  theme(legend.position = 'top')

comp$plots$total_predicted_biomass +
  labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_y_continuous(expand = c(0,0), limits =c(0, 900000)) +
  theme(legend.position = 'none') 

ggsave(filename = paste0(out_path, '/Mod24_biomass.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 10)

cowplot::plot_grid(comp$plots$biomass_by_strata +
                     theme(legend.position = 'top') +
                     geom_line() +
                     labs(y = 'Biomass (t)', x = 'Year', 
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF')) +
                     scale_color_discrete(type = c('#440154FF')) +
                     theme(legend.position = 'none'), 
                   comp$plots$cpue_by_strata +
                     theme(legend.position = 'none') +
                     geom_line() +
                     labs(y = 'RPW', x = 'Year',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF')) +
                     scale_color_discrete(type = c('#440154FF')),
                   ncol = 2, align = "h")

ggsave(filename = paste0(out_path, '/Mod24_biomass_strata.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 9)

comp$output$total_predicted_biomass |> 
  write_csv(paste0(out_path, '/ebs_tot_pred_biom.csv')) 

out24.1$parameter_estimates  |> 
  write_csv(paste0(out_path, '/ebs_param_est.csv')) 
