library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(sp)
library(sf)
library(dplyr)
library(plotly)
library(R.utils)
library(RANN)
library(rgbif)
library(protolite)
library(tibble)



#### GLOBAL PARAMETERS ####

# Radius parameter for nn2 (nearest neighbour search) function
RADIUS <- 0.02

FACTOR_INDIVIDUALS <- 100

LOAD_MERGED_OCCURRENCES = F
SAVE_MERGED_OCCURRENCES = F


options(stringsAsFactors = F)

## END GLOBAL PARAMETERS ##

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

# Grid
sf_grid <-
  st_read(
    "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/Cuadricula_4x4_nacional_wgs84.geojson"
  )

if (LOAD_MERGED_OCCURRENCES) {
  # Load previously merged occurrences
  sf_occurrences <- 
    st_read(
      "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/occurrences-merged.csv",
      options=c("X_POSSIBLE_NAMES=decimalLongitude","Y_POSSIBLE_NAMES=decimalLatitude")
    ) %>%
    st_set_crs(4326) %>%  
    mutate(eventDate = as.Date(eventDate, format = "%Y/%m/%d")) %>%
    mutate(eventTime = "") %>%  
    mutate(individualCount = as.integer(individualCount)) %>%
    mutate(decimalLongitude = as.double(decimalLongitude)) %>%
    mutate(decimalLatitude = as.double(decimalLatitude)) %>%
    mutate(year = as.integer(format(as.Date(eventDate), format = "%Y"))) %>%
    mutate(month = as.integer(format(as.Date(eventDate), format = "%m"))) %>%
    subset(scientificName %in% df_indicators$scientificName) # INCLUDE ONLY INDICATORS!!!
} else {
  # Occurrences from eBird user account
  sf_occurrences_ebird_user <- 
    st_read(
      "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/occurrences-ebird-user-biomonitoreo_aclap.csv",
      options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")
    ) %>%
    st_set_crs(4326) %>%
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
    mutate(collectionCode = "Biomonitoreo participativo - eBird") %>%
    subset(scientificName %in% df_indicators$scientificName) # INCLUDE ONLY INDICATORS!!!
  
  # Occurrences from GBIF
  sf_occurrences_gbif <- 
    st_read(
      "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/occurrences-gbif-indicators.csv",
      options=c("X_POSSIBLE_NAMES=decimalLongitude","Y_POSSIBLE_NAMES=decimalLatitude")
    ) %>%
    st_set_crs(4326) %>%
    rename(
      scientificName = species,
    ) %>%  
    mutate(vernacularName = "") %>%    
    mutate(eventDate = as.Date(eventDate, format = "%Y-%m-%d")) %>%
    mutate(eventTime = "") %>%  
    mutate(individualCount = as.integer(individualCount)) %>%
    mutate(decimalLongitude = as.double(decimalLongitude)) %>%
    mutate(decimalLatitude = as.double(decimalLatitude)) %>%
    mutate(year = as.integer(format(as.Date(eventDate), format = "%Y"))) %>%
    mutate(month = as.integer(format(as.Date(eventDate), format = "%m"))) %>%
    subset(scientificName %in% df_indicators$scientificName) # INCLUDE ONLY INDICATORS!!!
  
  # Fix GBIF data
  if (length(sf_occurrences_gbif) >= 1) {
    # Rearrange columns
    sf_occurrences_gbif <- 
      sf_occurrences_gbif[
        c("scientificName", "vernacularName", "individualCount",
          "stateProvince", "locality", "decimalLongitude",
          "decimalLatitude", "eventDate", "eventTime",
          "geometry", "year", "month",
          "collectionCode"
        )
        ]
    
    # Fix NA and 0s
    sf_occurrences_gbif$individualCount[is.na(sf_occurrences_gbif$individualCount)] <- 1
    sf_occurrences_gbif$individualCount[sf_occurrences_gbif$individualCount == 0] <- 1
    
    # Fix collection codes
    sf_occurrences_gbif <-
      mutate(sf_occurrences_gbif, 
             collectionCode = ifelse(grepl('EBIRD', collectionCode), 
                                     'eBird', 
                                     ifelse(grepl('Observations', collectionCode), 
                                            'iNaturalist', 
                                            collectionCode
                                     )
             )
      )
    
    # Occurrences from Biomonitoreo App
    sf_occurrences_biomonitoreo_app <- 
      st_read(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/occurrences_biomonitoreo.csv",
        options=c("X_POSSIBLE_NAMES=decimalLongitude","Y_POSSIBLE_NAMES=decimalLatitude")
      )  %>%
      st_set_crs(4326) %>%
      mutate(vernacularName = "") %>%    
      mutate(eventDate = as.Date(
        as.POSIXct(as.double(eventDate)/1000, origin="1970-01-01"), 
        format = "%Y-%m-%d"
      )
      ) %>%
      mutate(eventTime = "") %>%  
      mutate(individualCount = as.integer(individualCount)) %>%
      mutate(stateProvince = "") %>%
      mutate(decimalLongitude = as.double(decimalLongitude)) %>%
      mutate(decimalLatitude = as.double(decimalLatitude)) %>%
      mutate(year = as.integer(format(as.Date(eventDate), format = "%Y"))) %>%
      mutate(month = as.integer(format(as.Date(eventDate), format = "%m"))) %>%
      mutate(collectionCode = "Biomonitoreo participativo - App")
    
    # Rearrange columns
    sf_occurrences_biomonitoreo_app <- 
      sf_occurrences_biomonitoreo_app[
        c("scientificName", "vernacularName", "individualCount",
          "stateProvince", "locality", "decimalLongitude",
          "decimalLatitude", "eventDate", "eventTime",
          "geometry", "year", "month",
          "collectionCode"
        )
      ]        
  
    # Occurrences from Biomonitoreo Cameras
    sf_occurrences_biomonitoreo_camera <- 
      st_read(
        "https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/occurrences_biomonitoreo_camera.csv",
        options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")
      ) %>%
      st_set_crs(4326) %>%
      select(Genus, Species, Number.of.Animals, Sampling.Event, Longitude, Latitude, Photo.Date, Photo.time) %>%
      rename( # rename to DwC terms
        individualCount = Number.of.Animals,
        locality = Sampling.Event,
        decimalLongitude = Longitude,
        decimalLatitude = Latitude,
        eventDate = Photo.Date,
        eventTime = Photo.time
      ) %>%
      mutate(scientificName = paste(Genus, Species)) %>%
      mutate(vernacularName = "") %>%          
      mutate(eventDate = as.Date(eventDate, format = "%Y-%m-%d")) %>%
      mutate(individualCount = as.integer(individualCount)) %>%
      mutate(stateProvince = "") %>%          
      mutate(decimalLongitude = as.double(decimalLongitude)) %>%
      mutate(decimalLatitude = as.double(decimalLatitude)) %>%
      mutate(year = as.integer(format(as.Date(eventDate), format = "%Y"))) %>%
      mutate(month = as.integer(format(as.Date(eventDate), format = "%m"))) %>%
      mutate(collectionCode = "Biomonitoreo participativo - cámaras") %>%
      subset(scientificName %in% df_indicators$scientificName) # INCLUDE ONLY INDICATORS!!!
    
    # Rearrange columns
    sf_occurrences_biomonitoreo_camera <- 
      sf_occurrences_biomonitoreo_camera[
        c("scientificName", "vernacularName", "individualCount",
          "stateProvince", "locality", "decimalLongitude",
          "decimalLatitude", "eventDate", "eventTime",
          "geometry", "year", "month",
          "collectionCode"
        )
      ]     

    # Merge with other occurrences datasets
    sf_occurrences <-
      rbind(
        sf_occurrences_ebird_user,
        sf_occurrences_gbif,
        sf_occurrences_biomonitoreo_app,
        sf_occurrences_biomonitoreo_camera
      )
    
    # Summarize individuals by species
    df_speciesCount <-
      sf_occurrences %>%
      group_by(scientificName) %>%
      summarize(individualCount = sum(individualCount, na.rm = TRUE))    
  } else {
    # Nothing in GBIF list
    sf_occurrences <- sf_occurrences_ebird_user
  }
  
  # Order occurrences
  sf_occurrences <-
    sf_occurrences %>%
    arrange(eventDate, eventTime)        
  
  # Get vectors with coordinates
  locations_coords <- do.call(rbind, st_geometry(sf_locations))
  occurrences_coords <- do.call(rbind, st_geometry(sf_occurrences))
  
  # Get closest location (monitoring site) for each locality
  closest <- nn2(locations_coords, occurrences_coords, k = 1, searchtype = "radius", radius = RADIUS)
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
  
  # Save merged occurrences
  if (SAVE_MERGED_OCCURRENCES) {
    st_write(sf_occurrences, "occurrences-merged.csv")
  }
  
}


# List of scientific names in occurrences
choices_scientific_name <- unique(sf_occurrences$scientificName)
choices_scientific_name <- sort(choices_scientific_name)
choices_scientific_name <- insert(choices_scientific_name, 1, "Todos")

# List of monitoring sites in occurrences
choices_location <- unique(sf_occurrences$location)
choices_location <- sort(choices_location)
choices_location <- insert(choices_location, 1, "Todos")

# List of collections in occurrences
choices_collection_code <- unique(sf_occurrences$collectionCode)
choices_collection_code <- sort(choices_collection_code)
choices_collection_code <- insert(choices_collection_code, 1, "Todos")
