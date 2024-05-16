library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(sf)
library(terra)
library(highcharter)
library(RColorBrewer)
library(shinyWidgets)
library(reticulate)

source_python("utils/extract_points.py") 

source("utils/leaflet_fun.R", local = T)
source("utils/map_cols_cmip_fun.R", local = T)

files_cmip6 <- list.files("www/data/ncs/cmip6", full.names = T, recursive = T)

dun <- st_read("www/data/shps/DRBMP2015_DRBD.gpkg", quiet = T)


select_seas <- read.csv("www/data/tabs/select_seas.csv")
select_seas <- setNames(select_seas$choice, select_seas$parameter)

# definitie parametri
params_def <- read.csv("www/data/tabs/params_clim.csv")

