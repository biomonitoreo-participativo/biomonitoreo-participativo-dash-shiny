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
        
        # Filter by date range
        filteredOccurrences <-
            subset(
                filteredOccurrences,
                eventDate >= as.Date(input$select_date_range[1], origin = "1970-01-01") & eventDate <= as.Date(input$select_date_range[2], origin = "1970-01-01")
            ) %>%
            arrange(eventDate, eventTime)

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
            setView(-83.3068622, 9.22145746, 10) %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
            addPolygons(
                data = sf_protected_areas,
                color = "Black",
                fillColor = "transparent",
                stroke = TRUE,
                weight = 4.0,
                label = paste0(sf_protected_areas$siglas_cat, " ", sf_protected_areas$nombre_asp),
                popup = paste0(sf_protected_areas$siglas_cat, " ", sf_protected_areas$nombre_asp),
                group = "Áreas protegidas"
            ) %>%
            addPolygons(
                data = sf_biological_corridors,
                color = "Purple",
                fillColor = "transparent",
                stroke = TRUE,
                weight = 4.0,
                label = paste0(sf_biological_corridors$nombre_cb),
                popup = paste0(sf_biological_corridors$nombre_cb),
                group = "Corredores biológicos"
            ) %>%                        
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
                            "<strong>Observación</strong>", "<br>",
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
                group = "Observaciones"
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
                baseGroups = c("OpenStreetMap", "Stamen Toner Lite", "CartoDB Dark Matter", "Imágenes de ESRI"),
                overlayGroups = c("Áreas protegidas",
                                  "Corredores biológicos",
                                  "Individuos en sitios",
                                  "Observaciones",
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
            hideGroup("Áreas protegidas") %>%
            hideGroup("Corredores biológicos") %>%
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
            extensions = c("Buttons"),
            options = list(searchHighlight = TRUE,
                           lengthMenu = list(c(10, 15, 25, 50, 100, -1), c(10, 15, 25, 50, 100, "Todos")),
                           dom = 'Bfrtlip',
                           language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
                           buttons = list(list(extend='copy', text='Copiar'),
                                          list(extend='csv', text='CSV'),
                                          list(extend='csv', text='Excel'),
                                          list(extend='csv', text='PDF')
                                          )
                           )
        )
    })
    
    output$plot_occurrences_individuals_scientificNames_by_date <- renderPlotly({
        data <-
            filterOccurrences() %>%
            group_by(eventDate) %>%
            summarize(
                occurrences_count = n(), 
                individualCount_sum = sum(individualCount, na.rm = TRUE), 
                scientificName_count = n_distinct(scientificName, na.rm = TRUE)
            )
        
        data$eventDate <- as.Date(data$eventDate, "%Y-%m-%d")
        
        plot_ly(data = data,
                x = ~ eventDate,
                y = ~ occurrences_count, 
                name = 'Observaciones', 
                type = 'scatter',
                mode = 'lines',
                line = list(color = "blue")) %>%
            add_trace(y = ~ individualCount_sum,
                      name = 'Individuos',
                      mode = 'lines',
                      line = list(color = "black")) %>%
            add_trace(y = ~ scientificName_count,
                      name = 'Especies',
                      mode = 'lines',
                      line = list(color = "green")) %>%
            layout(title = "Cantidad de observaciones, individuos y especies por fecha",
                   yaxis = list(title = "Observaciones, individuos, especies"),
                   xaxis = list(title = "Fecha"),
                   legend = list(x = 0.1, y = 0.9),
                   hovermode = "compare") %>%
            config(locale = 'es')
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
    
    output$plot_individualCountBySpecies <- renderPlotly({
        data <- 
            filterOccurrences() %>%
            group_by(scientificName) %>%
            summarize(individualCount = sum(individualCount, na.rm = TRUE))  %>%
            arrange(desc(individualCount))
        
        data %>%
            mutate(scientificName = factor(scientificName, levels = scientificName)) %>%
            top_n(n = 10, wt = individualCount) %>%  
            plot_ly(x = ~ scientificName, 
                    y = ~ individualCount, 
                    type = "bar", 
                    text = ~ individualCount,
                    textposition = 'auto',
                    marker = list(color = 'blue')
            ) %>%
            layout(yaxis = list(title = "Cantidad de individuos"),
                   xaxis = list(title = ""),
                   margin = list(l = 10,
                                 r = 10,
                                 b = 10,
                                 t = 10,
                                 pad = 2
                   )
            ) 
        
    })        
    
})
