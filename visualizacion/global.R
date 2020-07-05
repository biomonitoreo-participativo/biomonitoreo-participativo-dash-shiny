library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(sp)
library(dplyr)
library(plotly)
library(R.utils)


options(stringsAsFactors = FALSE)


# Indicators
df_indicators <-
  read.csv(
    "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/indicator.csv"
  )

# Locations (of monitoring events)
sf_locations <-
  st_read(
    "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/location.csv",
    options=c("X_POSSIBLE_NAMES=decimalLongitude","Y_POSSIBLE_NAMES=decimalLatitude")
  )

# Occurrences from eBird user account
sf_occurrences_ebird_user <- 
  st_read(
    "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/occurrences-ebird-user-biomonitoreo_aclap.csv",
    options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")
  ) %>%
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
  mutate(month = as.integer(format(as.Date(eventDate), format = "%m"))) %>%
  subset(scientificName %in% df_indicators$scientificName) # INCLUDE ONLY INDICATORS!!!

# Union of all occurrences datasets (eBird User, GBIF, etc.)
sf_occurrences <-
  sf_occurrences_ebird_user

# Get vectors with coordinates
locations_coords <- do.call(rbind, st_geometry(sf_locations))
occurrences_coords <- do.call(rbind, st_geometry(sf_occurrences))

# Get closest location (monitoring site) for each locality
closest <- nn2(locations_coords, occurrences_coords, k = 1, searchtype = "radius", radius = 0.05)
closest <- sapply(closest, cbind) %>% as_tibble

# Add locationID column to occurrences dataset
sf_occurrences$locationID <- ifelse(closest$nn.idx == 0, 0, closest$nn.idx)
sf_occurrences$locationID <- as.character(sf_occurrences$locationID)

# Add location column to occurrences dataset
sf_occurrences <-
  left_join(
    sf_occurrences, 
    select(st_drop_geometry(sf_locations), locationID, location)
  )

# Remove rows with NA in location column
sf_occurrences <- sf_occurrences[!is.na(sf_occurrences$location), ]

# List of scientific names in occurrences
choices_scientific_name <- unique(sf_occurrences$scientificName)
choices_scientific_name <- sort(choices_scientific_name)
choices_scientific_name <- insert(choices_scientific_name, 1, "Todos")

# List of monitoring sites in occurrences
choices_location <- unique(sf_occurrences$location)
choices_location <- sort(choices_location)
choices_location <- insert(choices_location, 1, "Todos")