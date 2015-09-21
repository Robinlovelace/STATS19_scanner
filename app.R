library(shiny) ; library(shinydashboard) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; 
library(RColorBrewer) ; library(ggvis)

# load the casualty data
data <- readRDS(file="casualties_2005-14.Rda")
data$year <- as.factor(data$year)
data$severity <- factor(data$severity, levels= c("Fatal", "Serious", "Slight"), ordered = TRUE)
data$day <- factor(data$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered=T)
data$hour <- factor(data$hour)
data$light <- ordered(data$light, levels = c("Dark", "Daylight"))
boroughs <-  readOGR("boroughs.geojson", "OGRGeoJSON")


header <- dashboardHeader(
  title = "STATS19 scanner"
)

body <- dashboardBody(
  fluidRow(
      column(width = 8,
             box(width = NULL,
                 valueBoxOutput("casualtyBox"),
                 valueBoxOutput("KSIBox"),
                 valueBoxOutput("collisionBox"),
                 leafletOutput("map", height = 800))
      ),
      column(width = 4,
             box(h4("Inputs"), width = NULL,
                 br(),
                 sliderInput("year",
                             label = "Select a year:",
                             min = 2005,
                             max = 2014,
                             value = 2014,
                             sep = "", ticks = FALSE),
                 selectInput(inputId = "mode",
                              label = "Select a mode of travel:",
                              choices = c(levels(data$mode)),
                              selected = "Pedal Cycle"),
                 selectInput(inputId = "severity",
                             label = "Select severity:",
                             choices = c(levels(data$severity)),
                             selected = c(levels(data$severity)), multiple=T),
                 hr(),
                 br(),
                 h4("Outputs"),
                 br(),
                 tabBox( width = NULL,
                     tabPanel("Boroughs",
                              h5("Casualties by borough", style = "color:black", align = "center"),
                              ggvisOutput("borough_count")),
                     tabPanel("Months",
                              h5("Casualties by month and gender", style = "color:black", align = "center"),
                              ggvisOutput("timeband_month")),
                     tabPanel("Hours",
                              h5("Casualties by hour and severity", style = "color:black", align = "center"),
                              ggvisOutput("timeband_hour")),
                     tabPanel("Demographics",
                              h5("Casualties by ageband and gender", style = "color:black", align = "center"),
                              ggvisOutput("ageband_gender")),
                     tabPanel("About", br(),
                     p("STATS19 collision data for Greater London are available from",
                       a("Transport for London",
                         href = "https://www.tfl.gov.uk/corporate/publications-and-reports/road-safety"),
                       "and information about collision data can be found",
                       a("here.",
                         href = "https://www.tfl.gov.uk/cdn/static/cms/documents/collision-data-guide.pdf")),
                     br(),
                     p("Repo here: ",
                     a(href = "https://github.com/hpartridge/STATS19_scanner", icon("github"), target = "_blank")
                     ))
                 )))))

ui <- shinyUI(dashboardPage(header, dashboardSidebar(disable = TRUE), body, skin="black"))
  

server <- function(input, output, session) {
  
  # reactive data
  
  casualties <- reactive({data %>% filter(year %in% input$year &
                                            mode %in% input$mode &
                                            severity %in% input$severity)})
  
  
  # reactive data in bounds
  
  dataInBounds <- reactive({
    df <- casualties()
    if (is.null(input$map_bounds))
      return(df[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(df,
           lat >= latRng[1] & lat <= latRng[2] &
             long >= lngRng[1] & long <= lngRng[2])
  })
  
  # Value boxes
  
  output$casualtyBox <- renderValueBox({
    df <- dataInBounds()
    valueBox(
      format(nrow(df), format="d", big.mark=","), "Casualties", icon = NULL,
    color = "black")
})
  
  output$KSIBox <- renderValueBox({
    df <- dataInBounds()
    df <- df %>% filter(severity == "Fatal" | severity == "Serious")
    valueBox(
      format(nrow(df), format="d",big.mark=","), "Killed or Seriously Injured", icon = NULL,
      color = "black")
  })
  
  output$collisionBox <- renderValueBox({
    df <- dataInBounds()
    df <- df %>% distinct(AREFNO)
    valueBox(
      format(nrow(df), format="d",big.mark=","), "Collisions", icon = NULL,
      color = "black")
  })
  
  
  ## Map ##
  
  output$map <- renderLeaflet({
    leaflet(data) %>%  addProviderTiles("CartoDB.DarkMatter") %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addLegend(position = "bottomleft", colors = c("#b10026", "#fd8d3c", "#ffeda0"), 
                labels = c("Fatal", "Serious", "Slight"), opacity = 1, title = "Severity")
  })
  
  observe({
    pal <- colorFactor(c("#b10026", "#fd8d3c", "#ffeda0"), domain = c("Fatal", "Serious", "Slight"), ordered = TRUE)
    leafletProxy("map", data = casualties()) %>%
      clearShapes() %>% 
      addPolygons(data = boroughs, fill = F, color = "white", weight = 1.5, group = "Boroughs") %>% 
      addLayersControl(
        overlayGroups = "Boroughs",
        position = "topright",
        options = layersControlOptions(collapsed = FALSE)) %>% 
      addCircles(~long, ~lat, radius = 6, color = ~pal(severity), fillOpacity = 0.5, popup = ~text)
  })
  
  
  ## Boroughs ## 
  
  # Casualties by borough
  borough_count <- reactive({ 
    df <- dataInBounds()
    df %>%
      group_by(borough) %>%
      summarise(count = n()) %>% 
      ggvis(~borough, ~count, fill := "black") %>%
      mutate(borough = reorder(borough, -count)) %>%
      layer_bars(stroke := "white") %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      hide_legend("fill") %>% 
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  borough_count %>% bind_shiny("borough_count")  

  
  ## Months ##
  
  # Casualties by month and severity
  timeband_month <- reactive({ 
    df <- dataInBounds()
    df %>%
      group_by(severity, month) %>%
      summarise(count = n()) %>% 
      ggvis(~month, ~count, fill = ~severity) %>%
      layer_bars(stroke := "white") %>%
      scale_nominal("fill", range = c("#b10026", "#fd8d3c", "#ffeda0")) %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      add_legend("fill", title = "Severity") %>%  
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  timeband_month %>% bind_shiny("timeband_month")
  
  ## Hours ##
  
  # Casualties by hour and light conditions
  timeband_hour <- reactive({ 
    df <- dataInBounds()
    df %>%
      group_by(light, hour) %>%
      summarise(count = n()) %>% 
      ggvis(~hour, ~count, fill = ~light) %>%
      layer_bars(stroke := "white") %>%
      scale_nominal("fill", range = c("midnightblue", "yellow")) %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      add_legend("fill", title = "Light conditions") %>% 
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  timeband_hour %>% bind_shiny("timeband_hour")
  
  ## Demographics ##  
  
  # Casualties by ageband and gender
  ageband_gender <- reactive({ 
    df <- dataInBounds()
    df %>%
      group_by(sex, ageband) %>%
      summarise(count = n()) %>% 
      ggvis(~ageband, ~count, fill = ~sex) %>%
      layer_bars(stroke := "white") %>%
      scale_nominal("fill", range = c("steelblue", "purple"), label=c("Male", "Female")) %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>% 
      add_legend("fill", title = "Gender") %>% 
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  ageband_gender %>% bind_shiny("ageband_gender") 

  }

shinyApp(ui, server)