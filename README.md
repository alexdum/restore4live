# Restore4Life - Danube Basin Explorer

A Shiny-based web application to explore climate and hydrological data for the Danube basin.

> [!IMPORTANT]
> The **Restore4Life** application was realized within the framework of the [Restore4Life project](https://restore4life.eu/).

> [!NOTE]
> The application is currently in the **development stage**.
> You can access the live version here: [http://r4l-map.unibuc.ro/](http://r4l-map.unibuc.ro/)

## Technical Stack
The application is built using a hybrid R and Python architecture:
- **Frontend/UI**: [R Shiny](https://shiny.posit.co/) with the [bslib](https://rstudio.github.io/bslib/) dashboarding framework.
- **Data Processing**: 
  - **R**: Handles the reactive logic, mapping (Leaflet), and visualization (Highcharter).
  - **Python**: Integrated via the [`reticulate`](https://rstudio.github.io/reticulate/) package to perform high-performance multi-dimensional data operations using [`xarray`](https://docs.xarray.dev/).

### Python Integration
Python is used for computationally intensive data extraction tasks:
- **`utils/extract_points.py`**: Extracts point-level timeseries data from NetCDF climate and hydrological files using `xarray`.
- **`utils/read_zarr_data.py`**: Manages communication with Zarr data stores (e.g., for Remote Sensing/NDVI data), enabling efficient spatial subsetting and timeseries extraction from cloud-optimized formats.

## Features
- **Climate Scenario**: Visualize and analyze climate scenarios data.
- **Hydrological Scenario**: Explore hydrological projections and scenarios.
- **Climate Observational Data**: Access historical climate observational data.
- **Remote Sensing**: View remote sensing data and NDVI maps.
- **Area of Interest (AoI)**: Define and explore the Area of Interest.

## Installation & Running
The application is built using R and Shiny. 

## Development
This project incorporates Python scripts for data processing (e.g., Zarr data reading, point extraction) via the `reticulate` package.
