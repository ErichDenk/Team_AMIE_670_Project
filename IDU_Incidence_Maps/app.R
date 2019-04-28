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
require(viridis)
require(here)

incidenceData <- read.csv(here("Active-data-sets/incidenceDat.csv")) %>%
    select(., state_name = State, Year = Year1, NewDiag = "New.Diagnoses.State.Cases") %>%
    mutate(state_name = as.character(state_name))
mapCreateData <- left_join(incidenceData, states, by = "state_name")
    

theme_map <- function(...) {
    theme_minimal() +
        theme(
            text = element_text(family = "Arial", color = "#22211d"),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_text(size=10),
            legend.text=element_text(size=8),
            legend.position = "bottom",
            # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "#f5f5f2", color = NA), 
            panel.background = element_rect(fill = "#f5f5f2", color = NA), 
            legend.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.border = element_blank(),
            ...
        )
}

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
            geom_polygon(data = mapCreateData, aes(fill = log(NewDiag))) +
            #change gradient for scale bar -- I wanted darker color to be higher IDU density. 
            
            scale_fill_viridis(option = "magma", direction = -1, 
                               name = "IDU Incidence",
                               guide = guide_colorbar(
                                   direction = "horizontal",
                                   barheight = unit(2, units = "mm"),
                                   barwidth = unit(50, units = "mm"),
                                   draw.ulim = F,
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = 0.5)) +
            
                               
            #scale_fill_gradientn( colors = c("white", "blue"),
                             
                               #   guide = guide_colorbar(title.position = "top"))+
            # re plot the black boarder lines
            geom_polygon(color = "black", fill = NA)
            })
}

# Run the application 
shinyApp(ui = ui, server = server)
