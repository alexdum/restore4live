library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(terra)

source("utils/leaflet_fun.R")

dun <- st_read("www/data/shps/restore4life_aoi_etrs89.kml", quiet = T)


