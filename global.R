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
is1_austria <- st_read("www/data/shps/IS1_Austria.gpkg", quiet = T)
is2_slovakia <- st_read("www/data/shps/IS2_Rudava_Slovakia.gpkg", quiet = T)
is3_serbia <- st_read("www/data/shps/IS3_Vlasina_Serbia.gpkg", quiet = T)
is4_romania <- st_read("www/data/shps/IS4_Enisala_Romania.gpkg", quiet = T) |> st_zm() #  We will drop the Z dimension to ensure it's a 2D object.
ms1_germany <- st_read("www/data/shps/MS1_Salzbach_Germany.gpkg", quiet = T) |> st_make_valid()
ms2_slovakia <- st_read("www/data/shps/MS2_Dunajske_Slovakia.gpkg", quiet = T)
ms3_serbia <- st_read("www/data/shps/MS3_Gornje-Podunavlje_Serbia.gpkg", quiet = T) |> st_zm() #  We will drop the Z dimension to ensure it's a 2D object.
ms4_romania <- st_read("www/data/shps/MS4_Black-Sea_Romania.gpkg", quiet = T)
ms5_romania <- st_read("www/data/shps/MS5_Braila_Romania.gpkg", quiet = T)
ms6_romania <- st_read("www/data/shps/MS6_Mahmudia-Carasuhat_Romania.gpkg", quiet = T) |> st_zm() #  We will drop the Z dimension to ensure it's a 2D object.




# List of country data and their names
country_layers <- list(
    list(data = is1_austria[1,], name =  paste("IS1 Austria -",is1_austria$Name[1])),
     list(data = is1_austria[2,], name =  paste("IS1 Austria -",is1_austria$Name[2])),
     list(data = is1_austria[3,], name =  paste("IS1 Austria -",is1_austria$Name[3])),
     list(data = is1_austria[4,], name =  paste("IS1 Austria -",is1_austria$Name[4])),
     list(data = is1_austria[5,], name =  paste("IS1 Austria -",is1_austria$Name[5])),
     list(data = is2_slovakia, name =  paste("IS2 Slovakia -",is2_slovakia$Name)),
      list(data = is3_serbia, name = paste("IS3 Serbia -", is3_serbia$Name)),
      list(data = is4_romania, name = paste("IS4 Romania -",is4_romania$Name)),
      list(data = ms1_germany, name = paste("MS1 Germany -",ms1_germany$Name)),
      list(data = ms2_slovakia, name = paste("MS2 Slovakia -",ms2_slovakia$Name)),
      list(data = ms3_serbia, name = paste("MS3 Serbia -",ms3_serbia$Name)),
      list(data = ms4_romania.gpkg, name = paste("MS4 Romania -",ms4_romania$Name)),
      list(data = ms5_romania, name = paste("MS5 Romania -",ms5_romania$Name)),
      list(data = ms6_romania, name = paste("MS6 Romania -",ms6_romania$Name))

)

select_area <- setNames(
  c("drb", "at1", "at2", "at3", "at4", "at5", "sk1", "rs1", "ro1", "de1", "sk2", "rs2", "ro2", "ro3", "ro4"),
  c(
    "Danube River Basin",
    paste("IS1 Austria -", is1_austria$Name[1]),
    paste("IS1 Austria -", is1_austria$Name[2]),
    paste("IS1 Austria -", is1_austria$Name[3]),
    paste("IS1 Austria -", is1_austria$Name[4]),
    paste("IS1 Austria -", is1_austria$Name[5]),
    paste("IS2 Slovakia -", is2_slovakia$Name),
    paste("IS3 Serbia -", is3_serbia$Name),
    paste("IS4 Romania -", is4_romania$Name),
    paste("MS1 Germany -", ms1_germany$Name),
    paste("MS2 Slovakia -", ms2_slovakia$Name),
    paste("MS3 Serbia -", ms3_serbia$Name),
    paste("MS4 Romania -", ms4_romania$Name),
    paste("MS5 Romania -", ms5_romania$Name),
    paste("MS6 Romania -", ms6_romania$Name)
    
  )
)


select_seas <- read.csv("www/data/tabs/select_seas.csv")
select_seas <- setNames(select_seas$choice, select_seas$parameter)

# definitie parametri
params_def <- read.csv("www/data/tabs/params_clim.csv")
