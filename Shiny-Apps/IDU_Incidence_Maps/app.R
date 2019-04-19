#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(urbnmapr)
require(tidyverse)
require(shiny)


incidenceData <- read.csv(here("Active-data-sets/incidenceMapData.csv"))
mapCreateData <- left_join(tidyDiagData, states, by = "state_name")
    

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("HIV Incidence due to Injection Drug Use"),

    # Sidebar with a drop down list for which year to look at 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "year", choices = unique(incidenceData$Year), label = "Select Year", selected = 2008)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderPlot({
        mapCreateData %>%
            filter(., Year == input$year) %>%
            # first layer plots states 
            ggplot(data = states, mapping = aes(long, lat, group=group)) + 
            # center the map view on the US
            coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
            # black boarder and grey fill for all states
            geom_polygon(color = "black", fill = NA) +
            # outline for counties, I commented it out because it looked too busy 
            # geom_polygon(data = counties, fill = NA, color = "white") +
            # adding the IDU as the density fill per county
            geom_polygon(data = mapData, aes(fill = log(per_IDU))) +
            #change gradient for scale bar -- I wanted darker color to be higher IDU density. 
            scale_fill_gradientn( colors = c("white", "blue"),
                                  guide = guide_colorbar(title.position = "top"))+
            # re plot the black boarder lines
            geom_polygon(color = "black", fill = NA)
            })
}

# Run the application 
shinyApp(ui = ui, server = server)
