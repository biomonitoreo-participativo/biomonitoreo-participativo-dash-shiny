shinyServer(function(input, output) {
    
    filterOccurrences <- reactive({
        if (input$select_scientific_name == "Todos") {
            return (st_drop_geometry(sf_occurrences))
        }
        
        return(
            subset(
                st_drop_geometry(sf_occurrences), 
                scientificName == input$select_scientific_name
            ) %>%
            arrange(eventDate, eventTime)
        )
    })
    
    output$lf_occurrences <- renderLeaflet({
        occurrences <- filterOccurrences()
        
        locations <-
            sf_locations %>%
            subset(location %in% occurrences$location) # INCLUDE ONLY LOCATIONS PRESENT IN FILTERED OCCURRENCES!!!
        
        leaflet() %>%
            addTiles() %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%  
            addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
            addCircleMarkers(
                data = occurrences,
                lng = ~decimalLongitude,
                lat = ~decimalLatitude,
                stroke = F,
                radius = 8,
                fillColor = 'red',
                fillOpacity = 1,
                label = paste0(occurrences$scientificName, " (", occurrences$vernacularName, ")"),
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
                overlayGroups = c("Registros de presencia", "Sitios de monitoreo"),
                options = layersControlOptions(collapsed = F)
            ) %>%
            addMiniMap(
                position = "bottomleft",
                toggleDisplay = TRUE,
                tiles = providers$OpenStreetMap.Mapnik
            ) %>%
            addScaleBar(
                position = "bottomright", 
                options = scaleBarOptions()
            )
    })     

    output$dt_occurrences <- renderDT({
        data <- 
            filterOccurrences() %>%
            select(scientificName, vernacularName, individualCount,
                   location, locality, decimalLongitude,
                   decimalLatitude, eventDate, eventTime
                   )
        
        datatable(
            data, 
            rownames = FALSE,
            colnames = c("Nombre científico", "Nombre común", "Cantidad",
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
            filterOccurrences()
        
        data <-
            data %>%
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
})
