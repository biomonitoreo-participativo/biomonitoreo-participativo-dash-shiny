shinyServer(function(input, output) {
    
    filteredOccurrencesEbirdUser <- reactive({
        filteredData <- 
            subset(df_occurrences_ebird_user, scientificName == input$select_scientific_name_ebird_user) %>%
            arrange(eventDate, eventTime)
        
        return(filteredData)
    })

    output$dt_occurrences_ebird_user <- renderDT({
        data <- filteredOccurrencesEbirdUser()
        
        datatable(
            data, 
            rownames = FALSE,
            colnames = c("Nombre científico", "Nombre común", "Cantidad", "Provincia", "Localidad", "Longitud", "Latitud", "Fecha", "Hora"),            
            options = list(searchHighlight = TRUE,
                           pageLength = 15,
                           language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")
                           )
        )
    })
    
    output$plot_occurrences_ebird_user_by_year <- renderPlotly({
        data <- 
            filteredOccurrencesEbirdUser()
        
        data <-
            data %>%
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
    
    output$lf_occurrences_ebird_user <- renderLeaflet({
        data <- filteredOccurrencesEbirdUser()
        
        leaflet() %>%
            addTiles() %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%  
            addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
            addCircleMarkers(
                data = data,
                lng = ~decimalLongitude,
                lat = ~data$decimalLatitude,
                group = "Registros de presencia",
                stroke = F,
                radius = 8,
                fillColor = 'red',
                fillOpacity = 1,
                popup = paste(data$scientificName, data$locality, data$eventDate, sep = '<br/>')
            ) %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap", "Stamen Toner Lite", "ESRI World Imagery"),
                overlayGroups = c("Registros de presencia"),
                options = layersControlOptions(collapsed = F)
            ) %>%
            addMiniMap(
                position = "bottomleft",
                toggleDisplay = TRUE,
                tiles = providers$OpenStreetMap.Mapnik
            )            
    })     
    
})
