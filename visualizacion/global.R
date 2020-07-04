library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(sp)
library(dplyr)
library(plotly)


options(stringsAsFactors = FALSE)


# Indicators
df_indicators <-
  read.csv("https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/indicator.csv")


# Occurrences from eBird user account
df_occurrences_ebird_user <- 
  read.csv("https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/occurrences-ebird-user-biomonitoreo_aclap.csv") %>%
  select(Scientific.Name, Common.Name, Count, State.Province, Location, Longitude, Latitude, Date, Time) %>%
  rename( # rename to DwC terms
    scientificName = Scientific.Name,
    vernacularName = Common.Name,
    individualCount = Count,
    stateProvince = State.Province,
    locality = Location,
    decimalLongitude = Longitude,
    decimalLatitude = Latitude,
    eventDate = Date,
    eventTime = Time
  ) %>%
  mutate(eventDate = as.Date(eventDate, format = "%Y-%m-%d")) %>%
  mutate(individualCount = as.integer(individualCount)) %>%
  mutate(decimalLongitude = as.double(decimalLongitude)) %>%
  mutate(decimalLatitude = as.double(decimalLatitude)) %>%
  mutate(year = as.integer(format(as.Date(eventDate), format = "%Y"))) %>%
  subset(scientificName %in% df_indicators$scientificName) # INCLUDE ONLY INDICATORS!!!

# List of scientific names from eBird user account
choices_scientific_name_ebird_user <- unique(df_occurrences_ebird_user$scientificName)
choices_scientific_name_ebird_user <- sort(choices_scientific_name_ebird_user)

# List of localities from eBird user account
choices_locality_ebird_user <- unique(df_occurrences_ebird_user$locality)
choices_locality_ebird_user <- sort(choices_locality_ebird_user)
