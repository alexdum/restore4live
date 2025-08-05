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
library(dplyr)



source_python("utils/extract_points.py") 

source("utils/leaflet_fun.R", local = T)
source("utils/map_cols_cmip_fun.R", local = T)
source("utils/extract_data.R", local = T) # functie pentru extragere date din netcdf
source("utils/extract_zonal.R", local = T) # functie pentru extragere date zonale
source("utils/graph_title_climate.R", local = T) # titlu grafic scenarii climatice

files_cmip6 <- list.files("www/data/ncs/cmip6", full.names = T, recursive = T)

dun <- st_read("www/data/shps/DRBMP2015_DRBD.gpkg", quiet = T)
austria_at <- st_read("www/data/shps/austria_at.gpkg", quiet = T)
austria_at <- st_zm(austria_at)
names(austria_at)[6] <- "Name"
romania_ro <- st_read("www/data/shps/carasuhat_ro.gpkg", quiet = T)
# The geometry has a Z dimension which can cause issues with leaflet.
# We will drop the Z dimension to ensure it's a 2D object.
romania_ro <- st_zm(romania_ro)
romania_ro$Name <- "Carahusat"

serbia_rs <- st_read("www/data/shps/vlasina_rs.gpkg", quiet = T)
serbia_rs <- st_zm(serbia_rs)
names(serbia_rs)[4] <- 'Name'

# List of country data and their names
country_layers <- list(
    list(data = austria_at[1,], name =  paste("Austria -",austria_at$Name[1])),
     list(data = austria_at[2,], name =  paste("Austria -",austria_at$Name[2])),
     list(data = austria_at[3,], name =  paste("Austria -",austria_at$Name[3])),
     list(data = austria_at[4,], name =  paste("Austria -",austria_at$Name[4])),
     list(data = austria_at[5,], name =  paste("Austria -",austria_at$Name[5])),
    list(data = romania_ro, name = paste("Romania -", romania_ro$Name)),
    list(data = serbia_rs, name = paste("Serbia -", serbia_rs$Name))
)

select_area <- setNames(
  c("drb", "at1", "at2", "at3", "at4", "at5", "ro", "rs"),
  c(
    "Danube River Basin",
    paste("Austria -", austria_at$Name[1]),
    paste("Austria -", austria_at$Name[2]),
    paste("Austria -", austria_at$Name[3]),
    paste("Austria -", austria_at$Name[4]),
    paste("Austria -", austria_at$Name[5]),
    paste("Romania -", romania_ro$Name),
    paste("Serbia -", serbia_rs$Name)
  )
)


select_seas <- read.csv("www/data/tabs/select_seas.csv")
select_seas <- setNames(select_seas$choice, select_seas$parameter)

# definitie parametri
params_def <- read.csv("www/data/tabs/params_clim.csv")
