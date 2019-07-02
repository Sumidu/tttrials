#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyBS)
library(shinythemes)

library(ggplot2)
library(dplyr)
library(gridExtra)
library(dggridR)
library(grid)
library(RColorBrewer)
library(ggrepel)
library(ggthemes)
library(viridis)
library(ggjoy)
library(leaflet)
library(rgeos)
library(rworldmap)

df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")


centroids <- gCentroid(getMap(resolution="high"), byid=TRUE) %>% as.data.frame() %>% rownames_to_column()
regions <- centroids %>% pull(rowname) %>% unique() %>% sort()

ui <- dashboardPage(
    dashboardHeader(title = "Ufo Sightings"),
    dashboardSidebar( sidebarMenu(id = "mainnavigation",
        menuItem("Start", tabName = "start", icon = icon("home")),
        menuItem("Where are Ufos", tabName = "where", icon = icon("location-arrow")),
        menuItem("Explore your homearea", tabName = "myhome", icon = icon("user"))
    )),
    dashboardBody(
        tabItems(
            # First tab content ----
            tabItem(tabName = "start", 
                    fluidRow(
                        box(width = 12,
                            h1("Welcome to UFO Sightings"),
                            p("On this website you will find information about possible UFO sightings around the world."),
                            p("Have you ever seen a UFO? Or have you wondered what affects where UFOs are seen? Or when?"),
                            div(style="text-align:right;",
                                actionButton("gotolocation", "Continue", icon = icon("arrow-circle-right"))
                            )
                            
                                         
                        )
                        
                    )
            ),
            
            # Second tab content ----
            tabItem(tabName = "where",
                    fluidRow(
                        box(width = 12,
                            h1("UFO Sightings over the world"),
                            p("This page shows where on earth UFOs where sighted."),
                            plotOutput("ufosightings2", height = "600px") %>% withSpinner(),
                            br(),
                            div(style="text-align:right;",
                                actionButton("gotomyhome", "Continue", icon = icon("arrow-circle-right"))
                            )
                            
                            
                        )
                        
                    )
            ),
            # Third tab ----
            tabItem(tabName = "myhome",
                    fluidRow(
                        box(width = 12,
                            h1("UFO Sightings near your area"),
                            p("This page shows where near your home UFOs where sighted."),
                            leafletOutput("ufosightings") %>% withSpinner(),
                            selectInput("countryselect", "Select your country", choices = regions),
                            sliderInput("binrange", "Group area for circle size", min=3, max=500, value = 50),
                            br(),
                            div(style="text-align:center;",
                                actionButton("gotohome", "Start over", icon = icon("arrow-circle-left"))
                            )
                            
                            
                        )
                        
                    )
            )
            
        )
    )
)

server <- function(input, output, session) {

    
    
    
    # plot outputs ----
    
    output$ufosightings2 <- renderPlot({
        countries_map <- map_data("world")
        world_map <- ggplot() + 
            geom_map(data = countries_map, 
                     map = countries_map,aes(x = long, y = lat, map_id = region, group = group),
                     fill = "white", color = "black", size = 0.1)
        
        world_map + geom_point(data=df,aes(x=longitude,y=latitude),alpha=.5,size=.25) + theme_fivethirtyeight() + ggtitle('Location of UFO sightings')
    })
    
    output$ufosightings <- renderLeaflet({
        
        
        sel_country <- input$countryselect
        #sel_country <- "Germany"
        lat_pos <- centroids %>% filter(rowname == sel_country) %>% pull(y)
        long_pos <- centroids %>% filter(rowname == sel_country) %>% pull(x)
        
        
        
        dggs          <- dgconstruct(spacing=input$binrange, metric=FALSE, resround='down')
        sample_map <- df %>% select("lat" = latitude, "lon" = longitude,ufo_shape) %>% filter(!is.na(lat)) 
        sample_map$cell <- dgGEO_to_SEQNUM(dggs,sample_map$lon,sample_map$lat)$seqnum
        
        cellcenters   <- as_tibble(dgSEQNUM_to_GEO(dggs,sample_map$cell))
        sample_map$cen_lat <- cellcenters$lat_deg
        sample_map$cen_lon <- cellcenters$lon_deg
        
        cell_data <- sample_map %>% select(cell, long = cen_lon, lat = cen_lat) %>% unique() %>% arrange(cell)
        
        counts <- sample_map %>% group_by(cell) %>% summarise(count = n())
        
        cell_data <- cell_data %>% inner_join(counts)
        
    
        
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = FALSE)
            ) %>%
            leaflet::addCircles(data = cell_data, weight = 1, radius = ~count*100) %>% 
            setView(lng = long_pos,lat = lat_pos, zoom = 5)
        #map_plot + geom_point(data=sel_map,aes(x=longitude,y=latitude),alpha=.5,size=.25) + theme_fivethirtyeight() + ggtitle('Location of UFO sightings')
    })
    
    
    
   
    # action buttons (weiterklicken) -----
    observeEvent(input$gotohome, {
        updateTabsetPanel(session, "mainnavigation",
                          selected = "start")
    })
    
     observeEvent(input$gotomyhome, {
        updateTabsetPanel(session, "mainnavigation",
                          selected = "myhome")
    })
    
    
    observeEvent(input$gotolocation, {
        updateTabsetPanel(session, "mainnavigation",
                          selected = "where")
    })
    
}

shinyApp(ui, server)