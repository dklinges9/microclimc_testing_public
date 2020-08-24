# David Klinges
# This script uses curated flux tower and soiltemp datasets to conduct test
# runs of microclimc

## 1. Workspace prep ###############

site <- "AMF_US-Ha1" 
# Sites to choose from:
# "AMF_US-Ha1" Ameriflux site in MA, USA https://ameriflux.lbl.gov/sites/siteinfo/US-Ha1
# "AMF_US-MMS" Ameriflux site in IN, USA https://ameriflux.lbl.gov/sites/siteinfo/US-MMS
# "AMF_BR-Sa1" Ameriflux site in Brazil https://ameriflux.lbl.gov/sites/siteinfo/BR-Sa1
# "AMF_CA-Cbo" Ameriflux site in Canada https://ameriflux.lbl.gov/sites/siteinfo/CA-Cbo
# "AMF_US-SRC" Ameriflux site in AZ, USA https://ameriflux.lbl.gov/sites/siteinfo/US-SRC

# "BKS" Asiaflux site in Indonesia http://asiaflux.net/index.php?page_id=38
# "FHK" Asiaflux site in Japan http://asiaflux.net/index.php?page_id=54

ref_source <- "era5"
# Referenced datasets to choose from:
# "era5"
# "ncep"


# Dependencies and source code
library(tidyverse)
library(lubridate)
source("scripts/06_visualization/plot_point_models.R")

## Read in data ##############

# Read in input data 
prep_out <- readRDS(paste0("data/meteo_in/intermediate/", site, "_", ref_source, 
                           "_data_in.rds"))
# microclimc::runmodel
runmodel_out <- readRDS(paste0("data/meteo_in/model_outputs/", 
                               site, "_", ref_source, "_runmodel_out.rds"))
# microclimc::runwithNMR
runwithNMR_out <- readRDS( paste0("data/meteo_in/model_outputs/", 
                                  site, "_", ref_source, "_runwithNMR_out.rds"))
# NicheMapR::micro_ncep
nmr_out <- readRDS(paste0("data/meteo_in/model_outputs/", 
                          site, "_", ref_source, "_nmr_out.rds"))

## ....4A. Plot microclimc transient version #############

plot_ready_data <- prep_for_plots(hourlydata_microclimc = prep_out$hourly_data, 
                                  nichemapr_out = FALSE, 
                                  empirical_air_temp = prep_out$empirical_air_temp, 
                                  data_out = runmodel_out, 
                                  plot_reference_climate = TRUE, site = site)

plot_model_outs(site, plot_ready_data, model = "microclimc_transient")

## ....4B. Plot microclimc-NMR (steady-state) #############

data_out <- runwithNMR_out$metout
data_out$tout <- data_out$Tloc

plot_ready_steady_data <- prep_for_plots(hourlydata_microclimc = prep_out$hourly_data, 
                                  nichemapr_out = FALSE, 
                                  empirical_air_temp = prep_out$empirical_air_temp, 
                                  data_out = data_out, 
                                  plot_reference_climate = TRUE, site = site)

plot_model_outs(site, plot_ready_steady_data, model = "microclimc_steady_state")

## ....4C. Plot NMR #############

plot_ready_nmr_data <- prep_for_plots(hourlydata_microclimc = prep_out$hourly_data, 
                                  nichemapr_out = nmr_out, 
                                  empirical_air_temp = prep_out$empirical_air_temp, 
                                  data_out = data_out, 
                                  plot_reference_climate = TRUE, site = site)

plot_model_outs(site, plot_ready_nmr_data, model = "nichemapr")


