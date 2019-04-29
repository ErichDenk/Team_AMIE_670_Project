library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data


library(dplyr)

library(ggplot2) # tidyverse vis package

require(ggplot2)

require(tidyverse)
require(shiny)
require(here)
require(urbnmapr)
library(urbnthemes)
library(markdown)
library(DT)
require(viridis)


#load all datasets
prep <- read.csv("activedata/prep.csv") %>%
  dplyr::select(Year, State, State.PrEP.Users)

incidenceData <- read.csv("activedata/incidenceDat.csv") %>%
  select(., state_name = State, Year = Year1, NewDiag = "New.Diagnoses.State.Cases") %>%
  mutate(state_name = as.character(state_name))
mapCreateData <- left_join(incidenceData, states, by = "state_name")

prep1 <- prep %>%
  rename("state_name" = State)

newincidencedata <- subset(incidenceData, Year >= 2012 )%>%
  mutate(Year = as.numeric(Year))

table1 <- full_join(newincidencedata,prep1,
                    by=c('state_name','Year')
)%>%
  rename(`New Diagnosis` = NewDiag) %>%
  rename(`State` = state_name)


prevalenceData <- read.csv("activedata/2015-Prevalence-IDU-data.csv") %>%
  mutate(County = as.character(County))%>%
  select(., county_name = County, State,PercentIDU)

sspLocation <- read.csv("activedata/sspLocationLatLong.csv") %>%
  select(., state, latitude, longitude) 

mapDataPrev <- left_join(prevalenceData, counties, by = "county_name") %>%
  na.omit()

tidydata <- prep %>%
  mutate(state = as.character(State))%>%
  mutate(year = as.integer(Year)) %>%
  mutate(prepusers = as.numeric(State.PrEP.Users))%>%
  dplyr::select(., state_name = state,year,prepusers)
mapdata <- left_join(tidydata, states, by = "state_name")
na.omit(mapdata)

prevAve <-  read.csv("activedata/2015-Prevalence-IDU-data.csv") %>%
  select(., State, CountyPrevalence, TotalIDU) %>%
  mutate(State = as.character(State)) %>%
  na.omit() %>%
  group_by(State) %>%
  summarize(., totalIDU = sum(TotalIDU)) 

sspDensityDat <- read.csv("activedata/sspDensity.csv")

sspidu <- right_join(prevAve,sspDensityDat, by = "State")
sspidu2 <- select(sspidu, -X)
sspiduprev <- full_join (sspidu2, prevalenceData) %>%
  rename(`County` = county_name) %>%
  rename(`Percent HIV Cases due to IDU` = PercentIDU) %>%
  rename(`State Total IDU` = totalIDU) %>%
  rename(`SSP Exist` = sspExist) %>%
  rename(`State SSP Burden`= sspBurden) 



regions = data.frame("West" = c("WA", "OR", "CA", "NV", "UT", "ID", "MT", "WY", "CO", NA, NA, NA, NA, NA), Southwest = c("TX", "AZ", "NM", "OK", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA), Midwest = c("ND", "SD", "NE", "KS", "MO", "IL", "IA", "MN", "WI", "MI", "OH", "IN", NA, NA), Southeast = c("AR", "LA", "AL", "MS", "GA", "FL", "SC", "NC", "TN", "KY", "VA", "DE", "MD", "WV"), Northeast = c("PA", "NJ", "CT", "NY", "RI", "VT", "NH", "MA", "ME", NA,NA,NA,NA,NA))



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
      plot.caption = element_text(size = 8, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184"),
      ...
    )
}


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
      legend.text=element_text(size=7),
      legend.position = "bottom",
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      plot.caption = element_text(size = 8, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184"),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      ...
    )
}


regions = data.frame("West" = c("WA", "OR", "CA", "NV", "UT", "ID", "MT", "WY", "CO", NA, NA, NA, NA, NA), Southwest = c("TX", "AZ", "NM", "OK", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA), Midwest = c("ND", "SD", "NE", "KS", "MO", "IL", "IA", "MN", "WI", "MI", "OH", "IN", NA, NA), Southeast = c("AR", "LA", "AL", "MS", "GA", "FL", "SC", "NC", "TN", "KY", "VA", "DE", "MD", "WV"), Northeast = c("PA", "NJ", "CT", "NY", "RI", "VT", "NH", "MA", "ME", NA,NA,NA,NA,NA))

library(rsconnect)

library(RGraphics)


#ui

ui <- navbarPage("TEAM AMIE",
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
           tabPanel(("HIV, Injection Drug Use (IDU), and Syringe Service Program (SSP)"),
                    
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
                      tabPanel("HIV, Injection Drug Use (IDU), and Syringe Service Program (SSP)",
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
      scale_fill_viridis(limits = c(0, 13000), option = "magma", direction = -1,
                         name = "Number of PrEP Users",
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
           title = "PrEP Users by State", 
           subtitle = "The number of persons who had at least one day of prescribed oral TDF/FTC for PrEP in a year",
           caption = "Author: Team AMIE; Geometries: Urban Institute; Data: ACS",
           fill = "State PrEP Users") +
      theme_map() })
  
  output$map2 <- renderPlot({
    mapCreateData %>%
      filter(., Year == input$year) %>%
      ggplot(mapping = aes(long, lat, group = group, fill = NewDiag)) +
      geom_polygon(color = "#ffffff", size = .25) +
      scale_fill_viridis(limits = c(0, 13000), option = "magma", direction = -1,
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
           title = "HIV New Diagnosis by State", 
           caption = "Author: Team AMIE; Geometries: Urban Institute; Data: CDC",
           subtitle = "The number of persons newly diagnosed with HIV infection during a given 1-year time period.",
           fill = "State New Diagnosis") +
      theme_map() })
  
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
      geom_polygon(color = "#241e64", fill = "#c6c2c2", size = 0.50) +
      # adding the IDU as the density fill per county
      geom_polygon(data = mapDataPrev[mapDataPrev$state_abbv %in% data,], aes(fill = PercentIDU)) +
      #change gradient for scale bar -- I wanted darker color to be higher IDU density. 
      # re plot the black boarder lines
      geom_polygon(color = "#241e64", fill = NA, size = 0.50) +
      geom_point(data=sspLocation[sspLocation$state %in% data,], mapping =aes(x=longitude, y=latitude), inherit.aes = FALSE, size = input$SSPdot, alpha = 2/3) +
      scale_fill_viridis(option = "magma", direction = -1, 
                         name = "Percent of all HIV Cases due to IDU Transimission",
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(90, units = "mm"),
                           draw.ulim = F,
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5)) +
      theme(legend.key.width = unit(.5, "in"),
            legend.position = "bottom") +
      labs(x = NULL, 
           y = NULL, 
           title = "HIV, Injection Drug Use (IDU), and Syringe Service Program (SSP)", 
           subtitle = "Percent of all HIV Cases due to IDU and Locations of SSPs",
           caption = "Author: Team AMIE; Geometries: Urban Institute; Data: CDC, NASEN",
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
  
  
  
  

  
           
           
           
  