#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(gghighlight)

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

country_names <- student_ratio$country %>% unique() %>% sort()
continents <- country_names[str_starts(country_names, "Africa")]



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Teacher Ratio"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel(
            "Teacher Ratio by Continent", 
            sidebarLayout(
                sidebarPanel(
                    selectInput("xachse", "Select the Variable for the x-axis", choices = names(student_ratio)),
                    sliderInput("select_year", label="Select a year", 
                                min = min(student_ratio$year),
                                max = max(student_ratio$year), 
                                value = 2015),
                    checkboxGroupInput("country_sel", label="Select the countries", choices = country_names, selected = c(country_names[1], "Germany"))
                ),
                mainPanel(wellPanel(
                    plotOutput("mainplot")
                )
                          )
            )
        ),
        tabPanel(
            "Teacher Ratio by Country", 
            sidebarLayout(
                sidebarPanel(
                    "Test"
                ),
                mainPanel("Test"
                )
            )
                
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$mainplot <- renderPlot({
        student_ratio %>%
            filter(country %in% input$country_sel) %>% 
            #filter(year == input$select_year) %>% 
            ggplot() + aes_string(x = input$xachse, y = "student_ratio") + 
            geom_point() + 
            coord_flip()
            #gghighlight(student_ratio > 30) +
        # facet_wrap(indicator)
                
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
