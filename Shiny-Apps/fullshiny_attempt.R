library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
#library(spDataLarge)   # load larger geographic data
library(dplyr)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)   # for web applications
require(cartogram)
require(lubridate)
require(ggplot2)
#setwd("~/ivyxd94/datasci group project/Active-data-sets")
require(tidyverse)
require(shiny)
require(here)
require(urbnmapr)
library(urbnthemes)
library(markdown)
library(DT)
require(viridis)


#load all datasets



regions = data.frame("West" = c("WA", "OR", "CA", "NV", "UT", "ID", "MT", "WY", "CO", NA, NA, NA, NA, NA), Southwest = c("TX", "AZ", "NM", "OK", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA), Midwest = c("ND", "SD", "NE", "KS", "MO", "IL", "IA", "MN", "WI", "MI", "OH", "IN", NA, NA), Southeast = c("AR", "LA", "AL", "MS", "GA", "FL", "SC", "NC", "TN", "KY", "VA", "DE", "MD", "WV"), Northeast = c("PA", "NJ", "CT", "NY", "RI", "VT", "NH", "MA", "ME", NA,NA,NA,NA,NA))


#ui

ui <- navbarPage("Navbar!",
           tabPanel("State PrEP Users and HIV Prevalence",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(inputId = "year",choices = unique(tidydata$year),
                                    label = "Select Year",selected = 2012)
                      ),
                      mainPanel(
                        plotOutput("map1"),
                        plotOutput("map2")
                      )
                    )
           ),
           tabPanel(("HIV Prevalence due to Injection Drug Use by State and Syringe Services Program Locations"),
                    
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
                        plotOutput("map3"),
                        plotOutput("sspDensity"),
                        plotOutput("legalStatus")
                      )
                    )
                   ),
           navbarMenu("Data Tables",
                      tabPanel("State Prep Users and HIV Prevalence",
                               DT::dataTableOutput("table1")
                      ),
                      tabPanel("HIV Prevalence due to Injection Drug Use by State and Syringe Services Program Locations",
                               DT::dataTableOutput("sspiduprev")
                      )
                    )
                  )


#server.R 

server <-function(input, output, session) {
  output$map1 <- renderPlot({
    mapdata %>% 
      filter(., year == input$year) %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = prepusers)) +
      geom_polygon(color = "#ffffff", size = .25) +
      scale_fill_viridis(option = "magma", direction = -1,
                         name = "State PrEP Users",
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(50, units = "mm"),
                           draw.ulim = F,
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5)) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.key.width = unit(.5, "in"),
            legend.position = "bottom") +
      labs(x = NULL, 
           y = NULL, 
           title = "U.S. PrEP Users by State", 
           fill = "State PrEP Users") +
      theme_map()})
  
  output$map2 <- renderPlot({
    mapCreateData %>%
      filter(., Year == input$year) %>%
      ggplot(mapping = aes(long, lat, group = group, fill = log(NewDiag))) +
      geom_polygon(color = "#ffffff", size = .25) +
      scale_fill_viridis(option = "magma", direction = -1,
                         name = "HIV New Diagnosis",
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(50, units = "mm"),
                           draw.ulim = F,
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5)) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.key.width = unit(.5, "in"),
            legend.position = "bottom") +
      labs(x = NULL, 
           y = NULL, 
           title = "U.S. HIV New Diagnosis by State", 
           fill = "State New Diagnosis") +
      theme_map()})
  
  output$map3 <- renderPlot({
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
  
  output$table1 <- DT::renderDataTable({
    DT::datatable(table1)
    
    })
  output$sspiduprev <- DT::renderDataTable({
    DT::datatable(sspiduprev)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
  
  
  
  

  
           
           
           
  