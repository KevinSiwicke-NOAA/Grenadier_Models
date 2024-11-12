# GOA grenadier biomass estimation using the bottom trawl and longline survey indices

# Model naming conventions:
# Model 24 - m24 is the Model 20.5 from the bridging: 3 area/3 depths, 2 surveys, 1-PE, 1-q, additional obs error for Longline Survey

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
dat_path <- paste0("data/GOA_", YEAR); dir.create(dat_path)
out_path <- paste0("results/GOA_", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 15) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# Read data ----
source("r/GOA_data_pull_24.r")

# bottom trawl survey
biomass_dat <- model_dat$biomass_dat 
biomass_dat |> 
  write_csv(paste0(dat_path, "/goa_gren_biomass_", YEAR, ".csv"))
biomass_dat |> 
  tidyr::expand(year = min(biomass_dat$year):(YEAR),
                strata) |> 
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
  write_csv(paste0(dat_path, "/goa_gren_rpw_", YEAR, ".csv"))
cpue_dat |> 
  tidyr::expand(year = min(cpue_dat$year):(YEAR),
                strata) |> 
  left_join(cpue_dat |> 
              mutate(value = ifelse(is.na(cpue), NA,
                                    paste0(prettyNum(round(cpue, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) |> 
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) |> 
  arrange(year) |> 
  write_csv(paste0(out_path, '/cpue_data_wide.csv'))

# Model for 2024
input <- prepare_rema_input(model_name = 'Model 24',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1990,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1, 1, 1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                              pointer_q_cpue = c(1, 1, 1)),
                            extra_cpue_cv = list(assumption = 'extra_cv'))
m24 <- fit_rema(input)
out24 <- tidy_rema(m24)
out24$parameter_estimates

# Plots of Model 24
compare <- compare_rema_models(rema_models = list(m24))

compare$plots$total_predicted_biomass +
  labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_y_continuous(expand = c(0,0), limits =c(0, 700000)) +
  theme(legend.position = 'none')

ggsave(filename = paste0(out_path, '/Mod24_biomass.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 10)

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     labs(x = 'Year', y = 'Biomass (t)', subtitle = NULL,
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     coord_cartesian(ylim=c(0, 500000)) +
                     facet_wrap(~factor(strata, levels=c('WGOA (0-500 m)','WGOA (501-700 m)','WGOA (701-1000 m)',
                                                         'CGOA (0-500 m)','CGOA (501-700 m)','CGOA (701-1000 m)',
                                                         'EGOA (0-500 m)','EGOA (501-700 m)','EGOA (701-1000 m)')), ncol = 3) +
                     theme(legend.position = "none"),
                   compare$plots$cpue_by_strata  +
                     facet_wrap(~factor(strata, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 1) +
                     geom_line() +
                     labs(x = 'Year', y = 'RPW', 
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     theme(legend.position = "none"),
                   ncol = 2,
                   rel_widths = c(3, 1))

ggsave(filename = paste0(out_path, '/Mod24_strata.png'),
       dpi = 300, bg = 'white', units = 'in', height = 10, width = 16)

# full_sumtable <- compare$output$total_predicted_biomass |>
#   filter(year == YEAR + 1) |>
#   mutate(natmat = 0.078,
#          OFL = natmat * pred,
#          maxABC = 0.75 * natmat * pred,
#          ABC = maxABC)
# 
# sumtable_std <- full_sumtable |>
#   distinct(model_name, year, biomass = total_biomass, OFL, maxABC) |>
#   select(model_name, year, biomass, OFL, maxABC) |>
#   write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

compare$output$total_predicted_biomass |> 
  write_csv(paste0(out_path, '/goa_tot_pred_biom.csv'))

out24$parameter_estimates  |> 
  write_csv(paste0(out_path, '/goa_param_est.csv')) 
