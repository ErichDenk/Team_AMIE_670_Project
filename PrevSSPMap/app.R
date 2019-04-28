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
require(here)
require(viridis)



prevalenceData <- read.csv(here("Active-data-sets/2015-Prevalence-IDU-data.csv")) %>%
    mutate(County = as.character(County))%>%
    select(., county_name = County, State,PercentIDU)

sspLocation <- read.csv(here("Active-data-sets/sspLocationLatLong.csv")) %>%
    select(., state, latitude, longitude) 

mapDataPrev <- left_join(prevalenceData, counties, by = "county_name") %>%
    na.omit()




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

theme_chart <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "#22211d"),
      legend.title = element_text(size=10),
      legend.text=element_text(size=8),
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





prevAve <-  read.csv(here("Active-data-sets/2015-Prevalence-IDU-data.csv")) %>%
    select(., State, CountyPrevalence, TotalIDU) %>%
    mutate(State = as.character(State)) %>%
    na.omit() %>%
    group_by(State) %>%
    summarize(., totalIDU = sum(TotalIDU)) 

sspDensityDat <- read.csv(here("Active-data-sets/sspDensity.csv"))




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
              selectInput(inputId = "region", choices = c("USA", "West", "Southwest", "Midwest", "Southeast", "Northeast"), label = "Select Region", selected = "USA"),
              sliderInput("SSPdot", "SSP Location Dot Size", min = 0, max = 5, value = 3)
      ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("map"),
            plotOutput("sspDensity"),
            plotOutput("legalStatus")
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
                       "USA" = c(as.character(regions$West), "PR", "HI", "AK", as.character(regions$Southwest), as.character(regions$Southeast), as.character(regions$Northeast), as.character(regions$Midwest)))
    
        mapDataPrev %>%
            # first layer plots states 
            ggplot(data = states[states$state_abbv %in% data,], mapping = aes(long, lat, group=group)) + 
            # center the map view on the US
            coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
            # black  boarder and white fill for all states (NA space will be white)
            geom_polygon(color = "black", fill = "white") +
            # adding the IDU as the density fill per county
            geom_polygon(data = mapDataPrev[mapDataPrev$state_abbv %in% data,], aes(fill = PercentIDU)) +
            #change gradient for scale bar -- I wanted darker color to be higher IDU density. 
          # re plot the black boarder lines
          geom_polygon(color = "black", fill = NA) +
          geom_point(data=sspLocation[sspLocation$state %in% data,], mapping =aes(x=longitude, y=latitude), inherit.aes = FALSE, size = input$SSPdot, alpha = 2/3) +
                            scale_fill_viridis(option = "magma", direction = -1, 
                               name = "IDU Prevalence",
                               guide = guide_colorbar(
                                   direction = "horizontal",
                                   barheight = unit(2, units = "mm"),
                                   barwidth = unit(50, units = "mm"),
                                   draw.ulim = F,
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = 0.5)) +
          theme(legend.key.width = unit(.5, "in"),
                legend.position = "bottom") +
          labs(x = NULL, 
               y = NULL, 
               title = "HIV prevalence due to injection drug use transmission and syringe service program locations", 
               fill = "HIV Prevalence due to IDU Transmission as a Percent of all HIV Cases") +
          theme_map() 
    })
    
    output$sspDensity <- renderPlot({
        data <- switch(input$region, 
                       "West" = regions$West,
                       "Southwest" = regions$Southwest,
                       "Midwest" = regions$Midwest,
                       "Southeast" = regions$Southeast,
                       "Northeast" = regions$Northeast, 
                       "USA" = c(as.character(regions$West), as.character(regions$Southwest), "PR", "HI", "AK", as.character(regions$Southeast), as.character(regions$Northeast), as.character(regions$Midwest)))

        
        
        sspDensityDat %>% 
          .[.$State %in% data,] %>%
          top_n(., n = 15, sspBurden) %>%
          ggplot(., aes(reorder(State, sspBurden), sspBurden, fill = factor(Legal), alpha = factor(sspExist))) +
          geom_col() +
          scale_alpha_manual(values = c("No SSPs in state"=0.3, "At least one SSP in state"=1), guide = 'none') +
          scale_fill_viridis(discrete=TRUE, direction = -1) + 
          coord_flip() +
          theme(legend.key.width = unit(.5, "in"),
                legend.position = "right") +
          labs(title = "The Burden of SSPs by State",
               subtitle="Does the prevalence of SSPs in states match the need due to IDU transmission?",
               caption = "Transparent columns represent the unmet need in states with zero reported SSPs. Some states do not release IDU prevalence data.",
               fill = "Legality of SSPs", 
               x = "SSP Burden (IDU Transmission Cases/Total SSPs in State)", 
               y = "State") + 
          theme_chart()
        
       # ggplot(sspDensityDat[sspDensityDat$State %in% data,], aes(State, sspBurden, fill = factor(Legal), alpha = factor(sspExist))) +
        #    geom_col() +
         # scale_alpha_manual(values = c("No SSPs in state"=0.3, "At least one SSP in state"=1)) +
          #scale_fill_viridis(discrete=TRUE, direction = -1) + 
          #coord_flip() 
          

    })
   
  #  output$legalStatus <- renderPlot({
   #     data <- switch(input$region, 
    #                   "West" = regions$West,
     #                  "Southwest" = regions$Southwest,
      #                 "Midwest" = regions$Midwest,
       #                "Southeast" = regions$Southeast,
        #               "Northeast" = regions$Northeast, 
         #              "USA" = c(as.character(regions$West), as.character(regions$Southwest), as.character(regions$Southeast), as.character(regions$Northeast), as.character(regions$Midwest)))
        
    #    ggplot(LawDat[LawDat$State %in% data,], aes(State, Legal)) +
    #    geom_col()
    #})
    
    }


# Run the application 
shinyApp(ui = ui, server = server)
