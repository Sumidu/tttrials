## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(scales)
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

ui <- dashboardPage(
    # Dashboard Header ----
    dashboardHeader(title = "Nobel Prize Laureates"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Laureates", tabName = "laureates", icon = icon("user-graduate")),
            menuItem("Publications", tabName = "publications", icon = icon("book"))
        )
    ),
    # Dashboard Body ----
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "laureates",
                # Menü Seite 1 Nobel Laureates ----
                fluidRow(
                    column(width = 3,
                        box(width = 12, 
                            # Kategorien auswahl ----
                            checkboxGroupInput("prize_cat",
                                label = "Please select Nobel prize",
                                choices = nobel_winners$category %>% unique(),
                                selected = nobel_winners$category %>% unique()
                            )
                        ),
                        box(width = 12, 
                            # Infoboxen ----
                            infoBoxOutput("approvalBox", width = 12)
                         # Zweite Box, erste Spalte
                        )
                    ),
                    column(width = 9,
                           box(width = 12,
                               dataTableOutput("nobel_laureates")
                           )
                           
                        )
                )
                # Menü 1
            ),
            tabItem(
                tabName = "publications", "TEST"
                # Menü 2
            )
        ),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        )
    )
)


# Serverfunktion ----
server <- function(input, output) {
    
    nobel_selection <- nobel_winners %>% 
        select(full_name, organization_country, prize_year, category, prize, prize_share, laureate_type, gender) %>% 
        arrange(desc(prize_year))

    # Datatable output ----
    output$nobel_laureates <- renderDataTable({
            nobel_selection %>% filter(category %in% input$prize_cat)
        })
    
    # Approvalbox output ----
    output$approvalBox <- renderInfoBox({
        
        gender_table <- nobel_selection %>% filter(category %in% input$prize_cat) %>% select(gender) %>% table()
        if(dim(gender_table) == 0){
            percent_val <- 0.5
        } else {
            percent_val <- gender_table[1] / gender_table[2]
        }
        male_icon <- icon("male")
        female_icon <- icon("female")
        
        if(percent_val > 0.1) {
            selected_icon <- male_icon
        } else {
            selected_icon <- female_icon
        }
         
        percent_output <- percent(percent_val, accuracy = .01)
        
        valueBox(
            "Gender", percent_output, icon = selected_icon,
            color = "blue",
        )
    })
    
}

shinyApp(ui, server)