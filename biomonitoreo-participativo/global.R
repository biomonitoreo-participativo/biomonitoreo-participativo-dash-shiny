library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(sp)
library(dplyr)


options(stringsAsFactors = FALSE)


df_ebird_user_data <- 
  read.csv("https://raw.githubusercontent.com/biomonitoreo-participativo/biomonitoreo-participativo-datos/master/ebird-data-user-biomonitoreo_aclap.csv")

df_ebird_user_data <-
  df_ebird_user_data %>%
  select(Scientific.Name, Count, Location, Longitude, Latitude, Date, Time)


choices_scientific_name <- 
  unique(df_ebird_user_data$Scientific.Name)

choices_scientific_name <- sort(choices_scientific_name)
