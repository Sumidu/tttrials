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
library(DT)
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
write_rds(tidy_anime, "temp.rds")

tidy_anime <- read_rds("temp.rds")



anime <- tidy_anime %>% select(animeID, name, title_english, genre, episodes, rating, score, scored_by, rank, popularity) %>% 
  mutate(rating = factor(rating))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Anime Data"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(3,
      wellPanel(
        checkboxInput("r17", "R - 17+ (violence & profanity) -", FALSE),
        checkboxInput("pg13", "PG-13 - Teens 13 or older", FALSE),
        checkboxInput("pg", "PG - Children ", FALSE),
        checkboxInput("rplus", "R+ - Mild Nudity ", FALSE),
        checkboxInput("g", "G - All Ages", FALSE)
        )
     ),
     column(9,
      # Show a plot of the generated distribution
        plotOutput("rankingplot", brush = "plot_brush"),
        DT::DTOutput("tabledata")
      )
   
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$tabledata <- renderDT(brushedPoints(anime, input$plot_brush))
   
   
   
   output$rankingplot <- renderPlot({
    
     temp <- anime %>% filter(rating == "HALLO")
     if(input$r17){
       temp <- anime %>% filter(rating == "R - 17+ (violence & profanity)") %>% bind_rows(temp)
     }
     if(input$pg13){
       temp <- anime %>% filter(rating == "PG-13 - Teens 13 or older") %>% bind_rows(temp)
     }
     
     temp %>% ggplot() + 
       aes(scored_by, score, color = genre) + 
       geom_point(alpha = 0.2) +
       labs(title="Anime Ratings over Popularity")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

