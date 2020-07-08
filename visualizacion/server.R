shinyServer(function(input, output) {
    
    filterOccurrences <- reactive({
        filteredOccurrences <- st_drop_geometry(sf_occurrences)
        
        # Filter by scientific name
        if (input$select_scientific_name != "Todos") {
            filteredOccurrences <-
                subset(
                    st_drop_geometry(sf_occurrences), 
                    scientificName == input$select_scientific_name
                ) %>%
                arrange(eventDate, eventTime)
            
            FACTOR_INDIVIDUALS <- 1000
        }
        
        # Filter by location (monitoring site)
        if (input$select_location != "Todos") {
            filteredOccurrences <-
                subset(
                    filteredOccurrences,
                    location == input$select_location
                ) %>%
                arrange(eventDate, eventTime)
        }
        
        # Filter by collection code
        if (input$select_collection_code != "Todos") {
            filteredOccurrences <-
                subset(
                    filteredOccurrences,
                    collectionCode == input$select_collection_code
                ) %>%
                arrange(eventDate, eventTime)
        }        

        return(filteredOccurrences)
    })
    
    output$lf_occurrences <- renderLeaflet({
        occurrences <- filterOccurrences()
        
        locations <-
            sf_locations %>%
            subset(location %in% occurrences$location) # INCLUDE ONLY LOCATIONS PRESENT IN FILTERED OCCURRENCES!!!
        
        locationsGrpByIndividualCount <-
            occurrences %>%
            group_by(locationID) %>%
            summarize(individualCount = sum(individualCount, na.rm = TRUE)) %>%
            left_join(
                select(st_drop_geometry(sf_locations), location, locationID, decimalLongitude, decimalLatitude),
                by = c("locationID")
            )
        
        # Color palettes
        palIndividualCount <- 
            colorNumeric('Blues', locationsGrpByIndividualCount$individualCount)
        
        leaflet(sf_grid) %>%
            addTiles() %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%  
            addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
            addCircles(
                lng = locationsGrpByIndividualCount$decimalLongitude,
                lat = locationsGrpByIndividualCount$decimalLatitude,
                radius = locationsGrpByIndividualCount$individualCount*FACTOR_INDIVIDUALS,
                weight = 1,
                color = palIndividualCount(locationsGrpByIndividualCount$individualCount),
                fillColor = palIndividualCount(locationsGrpByIndividualCount$individualCount),
                fillOpacity = 0.7,
                label = paste0(
                            occurrences$location,
                            " (", as.character(locationsGrpByIndividualCount$individualCount), " individuos )"
                        ),
                group = "Individuos en sitios"
            ) %>%
            addCircleMarkers(
                data = occurrences,
                lng = ~decimalLongitude,
                lat = ~decimalLatitude,
                stroke = F,
                radius = 8,
                fillColor = 'red',
                fillOpacity = 1,
                label = paste0(occurrences$scientificName 
                               ),
                popup = paste0(
                            "<strong>Registro de presencia de especie</strong>", "<br>",
                            "<br>",
                            "<strong>Nombre científico:</strong> ", occurrences$scientificName, "<br>",
                            "<strong>Nombre común:</strong> ", occurrences$vernacularName, "<br>",
                            "<br>",
                            "<strong>Localidad:</strong> ", occurrences$locality, "<br>",
                            paste0(
                                "<strong>Longitud:</strong> ", occurrences$decimalLongitude, " ",
                                "<strong>Latitud:</strong> ", occurrences$decimalLatitude,
                                "<br>"
                            ),  
                            "<br>",
                            "<strong>Sitio de monitoreo asociado:</strong> ", occurrences$location, "<br>",
                            "<br>",
                            paste0(
                                "<strong>Fecha:</strong> ", occurrences$eventDate, " ",
                                "<strong>Hora:</strong> ", occurrences$eventTime 
                            )
                        ),
                group = "Registros de presencia"
            ) %>%
            addMarkers(
                lng = locations$decimalLongitude,
                lat = locations$decimalLatitude,
                label = locations$location,
                popup = paste0(
                    "<strong>Sitio de monitoreo</strong>", "<br>",
                    "<br>",
                    "<strong>Nombre:</strong> ", locations$location, "<br>",
                    "<br>",
                    paste0(
                        "<strong>Longitud:</strong> ", locations$decimalLongitude, " ",
                        "<strong>Latitud:</strong> ", locations$decimalLatitude,
                        "<br>"
                    )
                ),                
                group = "Sitios de monitoreo"
            ) %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap", "Stamen Toner Lite", "ESRI World Imagery"),
                overlayGroups = c("Individuos en sitios",
                                  "Registros de presencia",
                                  "Sitios de monitoreo"
                                  ),
                options = layersControlOptions(collapsed = F)
            ) %>%
            addLegend(
                title = "Individuos en sitios",
                pal = palIndividualCount,
                values = locationsGrpByIndividualCount$individualCount,
                group = "Individuos en sitios"
            ) %>%
            addMiniMap(
                position = "bottomleft",
                toggleDisplay = TRUE,
                tiles = providers$OpenStreetMap.Mapnik
            ) %>%
            addScaleBar(
                position = "bottomright", 
                options = scaleBarOptions()
            ) %>%
            addMeasure(
                position = "bottomright",
                primaryLengthUnit = "meters",
                secondaryLengthUnit = "kilometers",
                localization = "es"
            ) %>%
            hideGroup("Individuos en sitios")
    })     

    output$dt_occurrences <- renderDT({
        data <- 
            filterOccurrences() %>%
            select(scientificName, vernacularName, individualCount, collectionCode,
                   location, locality, decimalLongitude,
                   decimalLatitude, eventDate, eventTime
                   )
        
        datatable(
            data, 
            rownames = FALSE,
            colnames = c("Nombre científico", "Nombre común", "Cantidad", "Conjunto de datos",
                         "Sitio de monitoreo", "Localidad", "Longitud",
                         "Latitud", "Fecha", "Hora"
                         ),            
            options = list(searchHighlight = TRUE,
                           pageLength = 15,
                           language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")
                           )
        )
    })
    
    output$plot_occurrences_by_year <- renderPlotly({
        data <- 
            filterOccurrences() %>%
            group_by(year) %>%
            summarize(individualCount = sum(individualCount, na.rm = TRUE)) %>%
            mutate (year = as.character(year))

        plot_ly(
            type = 'bar',
            orientation = 'v',
            x = data$year,
            y = data$individualCount
        ) %>%
        layout(
            title = "Cantidad de individuos observados por año",
            yaxis = list(title = "Cantidad de individuos"),
            xaxis = list(title = "Año"),
            hovermode = "compare"
        )
        
    })
    
    output$plot_occurrences_by_month <- renderPlotly({
        data <- 
            filterOccurrences() %>%
            group_by(month) %>%
            summarize(individualCount = sum(individualCount, na.rm = TRUE)) %>%
            mutate (month = as.character(month))
        
        plot_ly(
            type = 'bar',
            orientation = 'v',
            x = data$month,
            y = data$individualCount
        ) %>%
        layout(
            title = "Cantidad de individuos observados por mes",
            yaxis = list(title = "Cantidad de individuos"),
            xaxis = list(title = "Mes"),
            hovermode = "compare"
        )
    })     
    
    output$plot_occurrences_by_location <- renderPlotly({
        data <- 
            filterOccurrences() %>%
            group_by(location) %>%
            summarize(individualCount = sum(individualCount, na.rm = TRUE))
        
        plot_ly(
            type = 'bar',
            orientation = 'v',
            x = data$location,
            y = data$individualCount
        ) %>%
            layout(
                title = "Cantidad de individuos observados por sitio de monitoreo",
                yaxis = list(title = "Cantidad de individuos"),
                xaxis = list(title = "Sitio de monitoreo"),
                hovermode = "compare"
            )
        
    })    
    
    output$plot_species_by_location <- renderPlotly({
        data <- 
            filterOccurrences() %>%
            group_by(location) %>%
            mutate(unique_species = n_distinct(scientificName))
        
        plot_ly(
            type = 'bar',
            orientation = 'v',
            x = data$location,
            y = data$unique_species
        ) %>%
            layout(
                title = "Cantidad de especies observadas por sitio de monitoreo",
                yaxis = list(title = "Cantidad de especies"),
                xaxis = list(title = "Sitio de monitoreo"),
                hovermode = "compare"
            )
        
    })        
    
})
