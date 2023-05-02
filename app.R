
library(tidyverse)
library(readxl)
library(janitor)
library(leaflet)
library(geojsonio)
library(sp)
library(scales)
library(shiny)

#Importing data

sheets <- excel_sheets(here::here("Project1-STAT431", "generation_monthly.xlsx"))
sheets <- sheets[-18]

#Splitting data by the year (2012) where there were sheet formatting changes made
sheets_2011 <- sheets[0:5]
sheets_2023 <- sheets[6:17]
energy_data_2011 <- map_df(sheets_2011, ~ read_excel(here::here("Project1-STAT431", "generation_monthly.xlsx"), sheet = .x))
energy_data_2023 <- map_df(sheets_2023, ~ read_excel(here::here("Project1-STAT431", "generation_monthly.xlsx"), sheet = .x, skip = 4))

#Excel sheet formatting error, merging data from two columns
energy_data_2023 <- energy_data_2023 |>
  mutate(`GENERATION (Megawatthours)` = coalesce(`GENERATION (Megawatthours)`, `GENERATION\r\n(Megawatthours)`)) |>
  select(-"GENERATION\r\n(Megawatthours)")

#rejoining the split data now that formatting issues have been resolved
energy_data <- rbind(energy_data_2011, energy_data_2023)

#cleaning data by renaming columns, changing state abbreviations to names, etc.
energy_data <- energy_data %>%
  rename(name = STATE,
         gen = `GENERATION (Megawatthours)`,
         producer = `TYPE OF PRODUCER`,
         energy = `ENERGY SOURCE`) %>%
  filter(!(name %in% c("US-Total", "DC"))) %>%
  mutate(name = state.name[match(name, state.abb)],
         gen = as.numeric(gen),
         MONTH = month.name[as.numeric(MONTH)],
         YEAR = as.numeric(YEAR))
##############################################################################################
# Reading Spatial dataframe for states
states <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
##############################################################################################


# modeling and functions

model <- lm(gen ~ ., data = energy_data) #basis for predictive linear model

makeSpatialStates <- function(df) { #helper function to make spatial states data that is readable by leaflet 
  allstates <- data.frame(name = levels(as.factor(energy_data$name)))
  df <- merge(df, allstates, all=TRUE)
  
  df <- merge(states, df, by="name", all=FALSE)
  
  return(df)
}

makePredict <- function(y, m, p, e) { #make a linear prediction, given historic data, on what energy generaion will be given user inputs
  
  map_data <- data.frame(name = levels(as.factor(energy_data$name)), 
                         YEAR = y, 
                         MONTH = m,
                         producer = p,
                         energy = e) 
  
  map_data$gen = predict(model, newdata = map_data)
  
  map_data <- select(map_data, name, gen)
  return(makeSpatialStates(map_data))
}

makeObserve <- function(y, m, p, e) { #given user input, alter the graph to show correct historic data of that time of year, producer, and energy type
  map_data <- energy_data %>%
    filter(YEAR == y,
           MONTH == m,
           producer == p,
           energy == e) %>%
    select(name, gen)
  
  return(makeSpatialStates(map_data))
}

#---------------------------APP STARTS HERE-----------------------------------

##############################################################################

ui <- fluidPage(
  titlePanel("Energy Output Per State (in megawatthours)"),
  
  radioButtons("fordat",
                "What would you like to do?",
                choices = c("Look at Past Data" = "past", 
                            "Make a Forecast" = "forecast")),

  uiOutput("select_ui"),
  
  leafletOutput("MyMap"),
  
  textOutput("Warning"),

)

################################################################################
server <- function(input, output) {

  output$select_ui <- renderUI({


    if(input$fordat == "past") {
      
      tagList( #user inputs on year, month, producer, and energy type
        sliderInput(inputId = "year",
                    label = "Choose Year",
                    value = 2022, min = 2001, max = 2023, sep=""),
        
        selectInput(inputId = "month",
                  label = "Choose Month",
                  choices = c("January", "February", "March", "April", "May", "June",
                              "July", "August", "September", "November", "December")),
        
        selectInput(inputId = "prod",
                  label = "Choose Type of Producer",
                  choices = levels(as.factor(energy_data$producer))),

        selectInput(inputId = "energy",
                  label = "Choose Source of Energy",
                  choices = levels(as.factor(energy_data$energy)),
                  selected = "Total")
      )
    } else {
      
      tagList( #user inputs on year, month, producer, and energy type
        sliderInput(inputId = "year",
                    label = "Choose Year",
                    value = 2024, min = 2023, max = 2050, sep=""),
        
        selectInput(inputId = "month",
                    label = "Choose Month",
                    choices = c("January", "February", "March", "April", "May", "June",
                                "July", "August", "September", "November", "December")),
        
        selectInput(inputId = "prod",
                    label = "Choose Type of Producer",
                    choices = levels(as.factor(energy_data$producer))),
        
        selectInput(inputId = "energy",
                    label = "Choose Source of Energy",
                    choices = levels(as.factor(energy_data$energy)),
                    selected = "Total")
        
      )
      
    }
  }) %>%
    bindEvent(input$fordat)
  
  map_data <- reactive({ #respond to user input of whether making forecast or looking at past data
    if(input$fordat == "past") {
      makeObserve(input$year, input$month, input$prod, input$energy)
    }
    else {
      makePredict(input$year, input$month, input$prod, input$energy)
    }
  })

  output$Warning <- renderText(
    ifelse(sum(is.na(map_data()$gen)) == 0, "", "WARNING: States that don't have data for certain energy types will be listed as NA and grayed out graphically")
  )  
  
  output$MyMap <- renderLeaflet({ #generates leaflet map 
        bins <- c(-Inf, 0, 2000000, 4000000, 6000000, 8000000, 10000000, Inf)
        pal <- colorBin("Blues", domain = map_data()$gen, bins = bins)

        g <- map(map_data()$gen, comma_format(digits = 12))
        n <- map_data()$name
        labels <- map2(n, g, \(n, g) paste(n, ": ", g, ifelse(is.na(g), "", " megawatthours"), sep="")) %>%
          lapply(htmltools::HTML)

        leaflet(map_data()) %>%
          setView(-96, 37.8, 4) %>%
          addProviderTiles("MapBox", options = providerTileOptions(
            id = "mapbox.light",
            accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'),
            minZoom = 3, #gives barrier to how far out you can go
            maxZoom = 6 #gives barrier to how far in you can go
          )) %>%
          addPolygons(fillColor = ~pal(gen),
                      weight = 2,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 0.7,
                      highlightOptions = highlightOptions(
                        weight = 5,
                        color = "#700",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(textsize = "13px")) %>%
          addLegend(pal = pal, values = ~gen, opacity = 0.7, title = "In Megawatthours",
                    position = "bottomright")
        })

}

shinyApp(ui = ui, server = server)
