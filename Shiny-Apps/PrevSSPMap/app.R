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
#devtools::install_github("dreamRs/shinyWidgets")
require(shinyWidgets)
require(here)



prevalenceData <- read.csv(here("Active-data-sets/2015-Prevalence-IDU-data.csv")) %>%
    mutate(County = as.character(County))%>%
    select(., county_name = County, State,PercentIDU)

sspLocation <- read.csv(here("Active-data-sets/sspLocationLatLong.csv")) %>%
    select(., state, latitude, longitude) 

mapData <- left_join(tidyData, counties, by = "county_name")

West <- c("WA", "OR", "CA", "NV", "UT", "ID", "MT", "WY", "CO")

Southwest <- c("TX", "AZ", "NM", "OK")

Midwest <- c("ND", "SD", "NE", "KS", "MO", "IL", "IA", "MN", "WI", "MI", "OH", "IN")

Southeast <- c("AR", "LA", "AL", "MS", "GA", "FL", "SC", "NC", "TN", "KY", "VA", "DE", "MD", "WV")

Northeast <- c("PA", "NJ", "CT", "NY", "RI", "VT", "NH", "MA", "ME")

USA <- c(West, Southwest, Midwest, Southeast, Northeast) 


regions = data.frame("West" = c("WA", "OR", "CA", "NV", "UT", "ID", "MT", "WY", "CO", NA, NA, NA, NA, NA), Southwest = c("TX", "AZ", "NM", "OK", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA), Midwest = c("ND", "SD", "NE", "KS", "MO", "IL", "IA", "MN", "WI", "MI", "OH", "IN", NA, NA), Southeast = c("AR", "LA", "AL", "MS", "GA", "FL", "SC", "NC", "TN", "KY", "VA", "DE", "MD", "WV"), Northeast = c("PA", "NJ", "CT", "NY", "RI", "VT", "NH", "MA", "ME", NA,NA,NA,NA,NA))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("HIV Prevalence due to Injection Drug Use by State and Syringe Services Program Locations"),
    
    # Button which can hopefully be used to toggle SSP location points on/off
  #  sidebarLayout(
   #     sidebarPanel(
    #        materialSwitch(inputId = "id", label = "SSP Location", status = "default" )
            #checkboxInput(inputId = "sspOnOff", label = "Turn on SSP Location Plot", value = T)
            #selectInput(inputId = "year", choices = unique(incidenceData$Year), label = "Select Year", selected = 2008)
      sidebarLayout(
          sidebarPanel(
              selectInput(inputId = "region", choices = c( "West", "Southwest", "Midwest", "Southeast", "Northeast", "USA"), label = "Select Region", selected = "USA")
      ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("map")
        )
    )
)





server <- function(input, output) {
    
   # observe({
    #    xy <-  c(input$id)
     #   if (xy) {
            ## find index of the closest point
      #      sspMap <- sspMap + geom_point(data=sspLocation, mapping =aes(x=longitude, y=latitude), inherit.aes = FALSE)
            
       # }
    #})
    
    
    output$map <- renderPlot({
        data <- switch(input$region, 
                       "West" = regions$West,
                       "Southwest" = regions$Southwest,
                       "Midwest" = regions$Midwest,
                       "Southeast" = regions$Southeast,
                       "Northeast" = regions$Northeast, 
                       "USA" = c(as.character(regions$West), as.character(regions$Southwest), as.character(regions$Southeast), as.character(regions$Northeast), as.character(regions$Midwest)))
    
        mapData %>%
            # first layer plots states 
            ggplot(data = states[states$state_abbv %in% data,], mapping = aes(long, lat, group=group)) + 
            # center the map view on the US
            coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
            # black boarder and grey fill for all states
            geom_polygon(color = "black", fill = "grey") +
            # adding the IDU as the density fill per county
            geom_polygon(data = mapData[mapData$state_abbv %in% data,], aes(fill = PercentIDU)) +
            #change gradient for scale bar -- I wanted darker color to be higher IDU density. 
            scale_fill_gradientn( colors = c("white", "blue"),
                                  guide = guide_colorbar(title.position = "top"))+
            # re plot the black boarder lines
            geom_polygon(color = "black", fill = NA) +
            geom_point(data=sspLocation[sspLocation$state %in% data,], mapping =aes(x=longitude, y=latitude), inherit.aes = FALSE)
    })
    }


# Run the application 
shinyApp(ui = ui, server = server)
