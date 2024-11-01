# GOA grenadier biomass estimation using the bottom trawl

# Model naming conventions:
# Model 20  ADMB: original version of of 5-depth strata, 5 PE model, from ADMB
# Model 20 = rema (all after use rema) version of 5-depth strata, 5 PE model
# Model 20.1 = m20.1: 5-depth strata, 5 PE model remove 1984, 1987 trawl survey (all after (don't include 84/87)
# Model 20.2 = m20.2: m24.2 but data pulled using GAP Products instead of AFSC.RACE (all after use GAP Products for data)
# Model 20.3 = M20.3: 3 depth strata (0-500, 501-700, 701 -1000), 3-areas (WGOA, CGOA, EGOA), 1-PE shared across all

# Set up ----

# assessment year
YEAR <- 2020

# Consider whether the rema package needs to be 'updated' - will need to update to get new extra_cv fxns
# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE) #, force = TRUE

libs <- c('rema', 'readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot', 'knitr')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/GOA_bridge", YEAR); dir.create(dat_path)
out_path <- paste0("results/GOA_bridge", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 14) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# Read data ----
source("r/GOA_bridge_data_pull_24.r")

# bottom trawl survey
# Old data separated by 5 depth strata
biomass_dat_old <- model_dat$biomass_dat_5_old
biomass_dat_old |> 
  write_csv(paste0(dat_path, "/goa_gren_biomass_OLD_", YEAR, ".csv"))
biomass_dat_old |>
  tidyr::expand(year = min(biomass_dat_old$year):(YEAR),
                strata) |>
  left_join(biomass_dat_old |>
              mutate(value = ifelse(is.na(biomass), NA,
                                    paste0(prettyNum(round(biomass, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) |>
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) |>
  arrange(year) |>
  write_csv(paste0(out_path, '/biomass_data_wide_OLD_.csv'))

# GAP products of the same 5 depth strata
biomass_dat_5 <- model_dat$biomass_dat_5_new
biomass_dat_5 |> 
  write_csv(paste0(dat_path, "/goa_gren_biomass_5_NEW_", YEAR, ".csv"))
biomass_dat_5 |>
  tidyr::expand(year = min(biomass_dat_5$year):(YEAR),
                strata) |>
  left_join(biomass_dat_5 |>
              mutate(value = ifelse(is.na(biomass), NA,
                                    paste0(prettyNum(round(biomass, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) |>
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) |>
  arrange(year) |>
  write_csv(paste0(out_path, '/biomass_data_wide_5_NEW_.csv'))

# GAP product of 3 regions with (E, C, W) and 3 depths (0-500, 500-700, 700-1000)
biomass_dat_9 <- model_dat$biomass_dat_9
biomass_dat_9 |> 
  write_csv(paste0(dat_path, "/goa_gren_biomass_9_NEW_", YEAR, ".csv"))
biomass_dat_9 |>
  tidyr::expand(year = min(biomass_dat_9$year):(YEAR),
                strata) |>
  left_join(biomass_dat_9 |>
              mutate(value = ifelse(is.na(biomass), NA,
                                    paste0(prettyNum(round(biomass, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) |>
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) |>
  arrange(year) |>
  write_csv(paste0(out_path, '/biomass_data_wide_9_NEW_.csv'))

# Read rwout from 2020 ADMB model
# Model 0 status quo ADMB from 2020 ----
m20_admb <- read_admb_re(paste0(dat_path, '/rwout.rep'), model_name = 'Model 20 ADMB')
names(m20_admb)

# Model 20 - 5-depth strata for BTS, with 5 process errors ----
# This is rema version of old model, and uses old RACE query to include 84/87
input <- prepare_rema_input(model_name = 'Model 20',
                            biomass_dat = biomass_dat_old,
                            start_year = 1984,
                            end_year = YEAR + 1,
                            # zeros = list(assumption = 'small_constant', options_small_constant = c(0.00001, NA)),
                            PE_options = list(pointer_PE_biomass = c(1, 2, 3, 4, 5)),
)

m20 <- fit_rema(input)
out20 <- tidy_rema(m20)
out20$parameter_estimates

# Compare M20 ADMB with M20 ----
compare <- compare_rema_models(admb_re = m20_admb, rema_models = list(m20))

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF', '#2A788EFF')) +
                     coord_cartesian(ylim=c(0, 500000)) +
                     scale_color_discrete(type = c('#440154FF', '#2A788EFF')) +
                     theme(legend.position = "top"))

ggsave(filename = paste0(out_path, '/ADMB_vs_rema_fits.png'),
       dpi = 300, bg = 'white', units = 'in', height = 7, width = 10)

# Accept that rema is nearly identical to previous ADMB version

# Model 24.1, same as 20 but with 1984 and 1987 removed ----
# This still uses the old data pull for comparison
input <- prepare_rema_input(model_name = 'Model 20.1 NO 84/87',
                            biomass_dat = biomass_dat_old,
                            start_year = 1990,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 2, 3, 4, 5)))

m20.1 <- fit_rema(input)
out20.1 <- tidy_rema(m24.1)
out20.1$parameter_estimates

# Compare 20 and 20.1
compare <- compare_rema_models(rema_models = list(m20, m20.1))

compare$plots$total_predicted_biomass +
  labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#440154FF', '#2A788EFF')) +
  coord_cartesian(ylim=c(0, 750000)) +
  scale_color_discrete(type = c('#440154FF', '#2A788EFF')) +
  theme(legend.position = "top")

ggsave(filename = paste0(out_path, '/Drop_84_87_fits.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 8)

# Model 20.2, same as 20.1 but using GAP products ----
# This uses the new data pull
input <- prepare_rema_input(model_name = 'Model 20.2 GAP Prod',
                            biomass_dat = biomass_dat_5,
                            start_year = 1990,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 2, 3, 4, 5)))

m20.2 <- fit_rema(input)
out20.2 <- tidy_rema(m20.2)
out20.2$parameter_estimates

# Compare 20.1 and 20.2, 
compare <- compare_rema_models(rema_models = list(m20.1, m20.2))

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF', '#2A788EFF')) +
                     coord_cartesian(ylim=c(0, 500000)) +
                     scale_color_discrete(type = c('#440154FF', '#2A788EFF')) +
                     theme(legend.position = "top"))

ggsave(filename = paste0(out_path, '/new_v_old_gap_data.png'),
       dpi = 300, bg = 'white', units = 'in', height = 7, width = 10)

compare$plots$total_predicted_biomass +
  labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#440154FF', '#2A788EFF')) +
  coord_cartesian(ylim=c(0, 750000)) +
  scale_color_discrete(type = c('#440154FF', '#2A788EFF')) +
  theme(legend.position = "top")

ggsave(filename = paste0(out_path, '/new_v_old_gap_data_total.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 8)
# SAME!

# Model 20.3 - 9 strata, by WGOA, CGOA, EGOA, and 3 depths 0-500, 501-700, and 701-1000; 1-PE
input <- prepare_rema_input(model_name = 'Model 20.3 1-PE',
                            biomass_dat = biomass_dat_9,
                            start_year = 1990,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1, 1, 1, 1, 1)))

m20.3 <- fit_rema(input)
out20.3 <- tidy_rema(m20.3)
out20.3$parameter_estimates

# Compare 24.2 and 24.3
compare <- compare_rema_models(rema_models = list(m20.2, m20.3))

compare$plots$total_predicted_biomass +
  labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#440154FF', '#FDE725FF')) +
  scale_color_discrete(type = c('#440154FF', '#FDE725FF')) +
  coord_cartesian(ylim=c(0, 850000)) +
  theme(legend.position = "top")

ggsave(filename = paste0(out_path, '/5strata_to_9strata.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 8)

# Model 20.4 with both surveys 1-PE, 1-q ----
input <- prepare_rema_input(model_name = 'Model 20.4, 1-PE, 1-q',
                            multi_survey = 1,
                            biomass_dat = biomass_dat_9,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1990,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1, 1, 1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                              pointer_q_cpue = c(1, 1, 1)))
m20.4 <- fit_rema(input)
out20.4 <- tidy_rema(m20.4)
out20.4$parameter_estimates

# Compare 24.3 and 24.4
compare <- compare_rema_models(rema_models = list(m20.3, m20.4))

compare$plots$total_predicted_biomass +
  labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#440154FF', '#2A788EFF')) +
  coord_cartesian(ylim=c(0, 850000)) +
  scale_color_discrete(type = c('#440154FF', '#2A788EFF')) +
  theme(legend.position = "top")

ggsave(filename = paste0(out_path, '/single_to_multi_survey_1pe_1q.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 8)

# Model 24.5 with both surveys 1-PE, 1-q, add extra LLS obs error ----
input <- prepare_rema_input(model_name = 'Model 20.5, 1-PE, 1-q, XTRA LLS OE',
                            multi_survey = 1,
                            biomass_dat = biomass_dat_9,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1990,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1, 1, 1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                              pointer_q_cpue = c(1, 1, 1)),
                            extra_cpue_cv = list(assumption = 'extra_cv'))
m20.5 <- fit_rema(input)
out20.5 <- tidy_rema(m20.5)
out20.5$parameter_estimates

# Compare 20.3, m20.4, and 24.5
compare <- compare_rema_models(rema_models = list(m20.3, m20.5))

compare$plots$total_predicted_biomass +
  labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#440154FF',  '#FDE725FF')) +
  coord_cartesian(ylim=c(0, 850000)) +
  scale_color_discrete(type = c('#440154FF',  '#FDE725FF')) +
  theme(legend.position = "top")

ggsave(filename = paste0(out_path, '/single_to_multi_survey_1pe_1q_xtraOE.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 8)

compareLL <- compare_rema_models(rema_models = list(m20.5))

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     theme(legend.position = 'top') +
                     labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     coord_cartesian(ylim=c(0, 500000)) +
                     scale_fill_discrete(type = c('#440154FF',  '#FDE725FF')) +
                     scale_color_discrete(type = c('#440154FF',  '#FDE725FF')),
                   compareLL$plots$cpue_by_strata  +
                     theme(legend.position = 'none') +
                     labs(x = 'Year', y = 'RPW', subtitle = 'LLS',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     facet_wrap(~strata, ncol = 1)  +
                     scale_fill_discrete(type = c('#FDE725FF')) +
                     scale_color_discrete(type = c('#FDE725FF')), 
                   ncol = 2,
                   rel_widths = c(1.5, 1))

ggsave(filename = paste0(out_path, '/multi_survey_1pe_1q_extra_lls_oe_stratum.png'),
       dpi = 300, bg = 'white', units = 'in', height = 7, width = 10)