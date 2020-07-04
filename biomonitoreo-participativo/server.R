shinyServer(function(input, output) {
    
    occurrenceData <- reactive({
        
        filteredData <- subset(df_ebird_user_data, Scientific.Name == input$select_scientific_name)
        
        return(filteredData)
        
    })

    output$datatable_occurrences <- renderDT({
        data <- occurrenceData()
        
        datatable(
            data, 
            rownames = FALSE,
            options = list(searchHighlight = TRUE,
                           language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                           )
        )
    })

})
