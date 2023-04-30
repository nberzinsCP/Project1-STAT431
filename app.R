################################################################################
# Libraries 
library(shiny)
library(tidyverse)
library(leaflet)
library(readxl)

################################################################################
# Reading excel file
sheets <- excel_sheets(here::here("generation_monthly.xlsx"))
sheets <- sheets[-18]
sheets_2012 <- sheets[0:6]
sheets_2023 <- sheets[7:17]

energy_data_2012 <- map_df(sheets_2012, ~ read_excel(here::here( "generation_monthly.xlsx"), sheet = .x))
energy_data_2023 <- map_df(sheets_2023, ~ read_excel(here::here( "generation_monthly.xlsx"), sheet = .x, skip = 4))

################################################################################
# Data Cleaning
energy_data_2012_fhalf <- energy_data_2012 %>%
  filter(is.na(YEAR)) %>%
  select(7:12) %>%
  slice(-1:-3) %>%
  janitor::row_to_names(row_number = 1)

energy_data_2012_lhalf <- energy_data_2012 %>%
  filter(!is.na(YEAR)) %>%
  select(1:6)

energy_data_2023_clean <- energy_data_2023 %>%
  mutate(`GENERATION (Megawatthours)` = coalesce(`GENERATION (Megawatthours)`, `GENERATION\r\n(Megawatthours)`)) %>%
  select(1:6)

energy_data <- rbind(energy_data_2012_fhalf, energy_data_2012_lhalf, energy_data_2023_clean)

energy_data <- energy_data %>%
  rename(name = STATE, 
         gen = `GENERATION (Megawatthours)`, 
         producer = `TYPE OF PRODUCER`,
         energy = `ENERGY SOURCE`) %>%
  filter(!(name %in% c("US-TOTAL", "DC"))) %>%
  mutate(name = state.name[match(name, state.abb)],
         gen = as.numeric(gen),
         MONTH = month.name[as.numeric(MONTH)],
         YEAR = as.numeric(YEAR)) 

################################################################################
# Reading Spatial dataframe for states 
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

################################################################################
# modeling and functions
model <- lm(gen ~ ., data = energy_data)

makeSpatialStates <- function(df) {
  allstates <- data.frame(name = levels(as.factor(energy_data$name))) 
  df <- merge(df, allstates, all=TRUE)
  
  df <- sp::merge(states, df, by="name", all=FALSE)
  
  return(df)
}

makePredict <- function(y, m, p, e) {

  map_data <- data.frame(name = levels(as.factor(energy_data$name)), 
                         YEAR = y, 
                         MONTH = m,
                         producer = p,
                         energy = e) 
  
  map_data$gen = predict(model, newdata = map_data)
  
  map_data <- select(map_data, name, gen)
  return(makeSpatialStates(map_data))
}

makeObserve <- function(y, m, p, e) {
  map_data <- energy_data %>%
    filter(YEAR == y, 
           MONTH == m, 
           producer == p, 
           energy == e) %>%
    select(name, gen)
  
  return(makeSpatialStates(map_data))
}

################################################################################
#Start of App
ui <- fluidPage(
  
  titlePanel("Energy Output Per State (in megawatthours)"),
  
  leafletOutput("MyMap"),
  
  textOutput("Warning"),
  
  sliderInput(inputId = "year",
              label = "Choose Year",
              value = 2022, min = 2001, max = 2023, sep=""),
  
  checkboxInput(inputId = "lm",
                label = "Predictive Modeling?",
                value = FALSE),
  
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

################################################################################
server <- function(input, output, session) {
  
  observe(
    if(input$lm) {
      updateSliderInput(session, "year", min = 2023, max = 2050)
    }
    else {
      updateSliderInput(session, "year", min = 2001, max = 2023)
    }
  )

  map_data <- reactive({
    if(input$lm) {
      makePredict(input$year, input$month, input$prod, input$energy)
    }
    else {
      makeObserve(input$year, input$month, input$prod, input$energy)
    }
  })
  
  output$Warning <- renderText(
    ifelse(sum(is.na(map_data()$gen)) == 0, "", "WARNING: States that don't have data for certain energy types will be listed as NA")
    )
  
  output$MyMap <- renderLeaflet({
    bins <- c(-Inf, 0, 2000000, 4000000, 6000000, 8000000, 10000000, Inf)
    pal <- colorBin("Blues", domain = map_data()$gen, bins = bins)
    
    g <- map(map_data()$gen, scales::comma_format(digits = 12))
    n <- map_data()$name
    labels <- map2(n, g, \(n, g) paste(n, ": ", g, ifelse(is.na(g), "", " megawatthours"), sep="")) %>%
      lapply(htmltools::HTML)
    
    leaflet(map_data()) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'),
        minZoom = 3, 
        maxZoom = 6 
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
    }
    )
}


shinyApp(ui = ui, server = server)
