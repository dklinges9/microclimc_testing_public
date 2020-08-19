# David Klinges
# This script uses curated flux tower and soiltemp datasets to conduct test
# runs of microclimc

site <- "BKS" # Asiaflux site in Indonesia
# Site page:
# http://asiaflux.net/index.php?page_id=38

## 1. Load packages ####################

# library(devtools)
# install_github("ilyamaclean/microclimc")
# install_github("ilyamaclean/microctools")
library(microclima)
library(microclimc)
library(microctools)
library(NicheMapR)
library(RNCEP)

## 2. Read in data ########

prep_out <- readRDS(paste0("data/meteo_in/intermediate/", site, "_era5_data_in.rds"))

climdata <- prep_out$climdata
hourlydata_nichemapr <- prep_out$hourlydata_nichemapr
# What height is the empirical temperature data (and therefore needs to be modeled)
required_air_temp_height <- prep_out$required_air_temp_height
# Some metadata on reference data
ref_weather_info <- prep_out$ref_weather_info
# Some metadata on the site
site_metadata <- prep_out$site_metadata
# Is the reference data from open location (e.g. ERA5) or above-canopy
metopen <- prep_out$metopen
prec <- prep_out$prec
vegp <- prep_out$vegp
soilp <- prep_out$soilp
windhgt <- prep_out$windhgt
zu <- prep_out$zu

## 3. Run models ####################

## ....3A. Run microclimc ############

system.time({
  runmodel_out <- microclimc::runmodel(climdata, vegp, soilp, lat = site_metadata$lat,
                                       long = site_metadata$lon, steps = 200, plotsteps = 200,
                                       plotout = FALSE, reqhgt = required_air_temp_height,
                                       metopen = metopen,
                                       windhgt = windhgt, # assumes wind speed and direction taken at same height
                                       zu = zu)
})

## ....3B. Run microclimc-NMR integration ############

system.time({
  runwithNMR_out <- microclimc::runwithNMR(climdata, prec = prec, 
                                           vegp = vegp, soilp = soilp, 
                                           lat = site_metadata$lat, 
                                           long = site_metadata$lon,
                                           reqhgt = required_air_temp_height,
                                           metopen = metopen, 
                                           windhgt = windhgt # assumes wind speed and direction taken at same height
  )
})

## ....3C. Run micro_ncep ############

tme <- climdata$obs_time
if (ref_weather_info[ref_weather_info$reference_climate=="temp",]$height < required_air_temp_height) {
  warning("user-inputed height is heigher than reference height, adding 0.5 to ref height")
  Refhyt <- required_air_temp_height + 0.5
} else {
  Refhyt <- ref_weather_info[ref_weather_info$reference_climate=="temp",]$height
}

nmr_out <- micro_ncep(loc = c(site_metadata$lon, site_metadata$lat),
                      dstart = format(as.Date(tme[1]), '%d/%m/%Y'),
                      dfinish = format(as.Date(tme[length(tme)]), '%d/%m/%Y'),
                      # Using temperature height as reference
                      Refhyt = Refhyt,
                      Usrhyt = required_air_temp_height,
                      hourlydata = hourlydata_nichemapr, 
                      dailyprecip = prec, 
                      runshade = 1) 
