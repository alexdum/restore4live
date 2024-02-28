library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(terra)
library(highcharter)
library(RColorBrewer)

source("utils/leaflet_fun.R", local = T)
source("utils/map_cols_cmip_fun.R", local = T)

dun <- st_read("www/data/shps/restore4life_aoi_etrs89.kml", quiet = T)


select_seas <- read.csv("www/data/tabs/select_seas.csv")
select_seas <- setNames(select_seas$choice, select_seas$parameter)
