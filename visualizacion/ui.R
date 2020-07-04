dashboardPage(
    dashboardHeader(title = "Biomonitoreo participativo"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = "Registros en cuenta de eBird", 
                startExpanded = TRUE,
                menuSubItem(text = "Tabla", tabName = "tab_dt_occurrences_ebird_user"),
                menuSubItem(text = "Gráficos", tabName = "tab_plots_occurrences_ebird_user"),
                menuSubItem(text = "Mapa", tabName = "tab_lf_occurrences_ebird_user")
            ),        
            selectInput(
                inputId = "select_scientific_name_ebird_user",
                label = "Nombre científico",
                choices = choices_scientific_name_ebird_user
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "tab_dt_occurrences_ebird_user",
                DTOutput(outputId = "dt_occurrences_ebird_user")
            ),
            tabItem(
                tabName = "tab_plots_occurrences_ebird_user",
                plotlyOutput(outputId = "plot_occurrences_ebird_user_by_year")
            ),            
            tabItem(
                tabName = "tab_lf_occurrences_ebird_user",
                tags$style(type = 'text/css', '#lf_occurrences_ebird_user {height: calc(100vh - 80px) !important;}'),
                leafletOutput(outputId = "lf_occurrences_ebird_user")
            )            
        )
    )
)
