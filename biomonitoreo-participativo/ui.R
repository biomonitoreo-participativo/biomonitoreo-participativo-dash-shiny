dashboardPage(
    dashboardHeader(title = "Otro"),
    dashboardSidebar(sidebarMenu(
        menuItem(text = "Player Data", 
                 startExpanded = TRUE,
                 menuSubItem(text = "Data", tabName = "playerData")
        ),        
        selectInput(
            inputId = "select_scientific_name",
            label = "Nombre científico",
            choices = choices_scientific_name
        )
    )),
    dashboardBody(tabItems(tabItem(
        tabName = "playerData",
        DTOutput(outputId = "datatable_occurrences")
    )))
)
