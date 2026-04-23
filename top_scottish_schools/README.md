# Top Scottish Schools Shiny App

## Description

A Shiny app for exploring Scottish secondary school rankings, catchment areas, and contact details, integrating multiple public datasets across a range of data types including geospatial vector data.

## Functionality

- **Trend tab** – visualise a selected school's ranking over time against the current top 5
- **Map tab** – view the school's location and catchment area on an interactive map of Scotland, rendered from geospatial polygon data
- **Rankings Table tab** – browse the full league table across all years
- **Contact tab** – look up contact details for the selected school

## Data & Data Types

- **Geospatial vector data** (`.rds` / `sf` polygons) – school catchment boundaries sourced from the [Spatial Hub](https://data.spatialhub.scot), transformed to WGS84 for mapping
- **Tabular ranking data** (CSV / `.rds`) – [Scottish Secondary School League Tables](https://www.datamap-scotland.co.uk/wp-wag-2023/schsec202223/file/2022-23-secondary-ranks.csv)
- **Geolocation data** (`.rds`) – school coordinates from [Scottish School Roll and Locations](https://www.data.gov.uk/dataset/9a6f9d86-9698-4a5d-a2c8-89f3b212c52c/scottish-school-roll-and-locations)
- **Contact data** (`.xlsx`) – school details from the [Scottish Government](https://www.gov.scot/publications/school-contact-details/)

## Deployment

The app is hosted [here](https://dazcam.shinyapps.io/top_scottish_schools/).

## Copyright

Contains OS data © Crown copyright and database right 2024 and Scottish local authority data from the Spatial Hub.