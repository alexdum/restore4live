library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(terra)
library(highcharter)
library(RColorBrewer)
library(shinyWidgets)

source("utils/leaflet_fun.R", local = T)
source("utils/map_cols_cmip_fun.R", local = T)


dun <- st_read("www/data/shps/DRBMP2015_DRBD.gpkg", quiet = T)


select_seas <- read.csv("www/data/tabs/select_seas.csv")
select_seas <- setNames(select_seas$choice, select_seas$parameter)
