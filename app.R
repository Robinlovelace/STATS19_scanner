library(shiny) ; library(shinydashboard) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; 
library(RColorBrewer) ; library(ggvis) ; library(scales) ; library(DT)

# load the casualty data
data <- readRDS(file="casualties_2005-14.Rda")
data$year <- as.factor(data$year)
data$severity <- factor(data$severity, levels= c("Fatal", "Serious", "Slight"), ordered = TRUE)
data$day <- factor(data$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered=T)
data$hour <- factor(data$hour)
data$light <- ordered(data$light, levels = c("Dark", "Daylight"))
boroughs <-  readOGR("boroughs.geojson", "OGRGeoJSON")


ui <- shinyUI(dashboardPage(skin = "black",
                            dashboardHeader(title = "STATS19 scanner"),
                            dashboardSidebar(
                              sidebarMenu(
                                sliderInput("year",
                                            label = "Select a year:",
                                            min = 2005,
                                            max = 2014,
                                            value = 2014,
                                            sep = "", ticks = FALSE),
                                radioButtons(inputId = "mode",
                                             label = "Select a mode of travel:",
                                             choices = c(levels(data$mode)),
                                             selected = "Pedal Cycle"),
                                selectInput(inputId = "severity",
                                            label = "Select severity:",
                                            choices = c(levels(data$severity)),
                                            selected = c(levels(data$severity)), multiple=T),
                                hr(),
                                menuItem("Casualty map", tabName = "map", icon = icon("fa fa-map-marker")),
                                menuItem("Borough statistics", tabName = "boroughs", icon = icon("fa fa-bar-chart")),
                                menuItem("Temporal profile", tabName = "temporal", icon = icon("fa fa-clock-o")),
                                menuItem("Demographics", tabName = "demographics", icon = icon("fa fa-users")),
                                menuItem("Raw data", tabName = "data", icon = icon("th")),
                                menuItem("GitHub", icon = icon("fa fa-github-square"), 
                                         href = "https://github.com/hpartridge/STATS19_scanner"))),
                            dashboardBody(
                              tabItems(
                                tabItem(tabName = "map",
                                        fluidRow(
                                          title = "Map", width = 12,
                                          leafletOutput("map", height = "1000"))
                                ),
                                tabItem(tabName = "boroughs",
                                        fluidRow(
                                          box(title = "Top 10 boroughs", height = "auto", width = 3, solidHeader=TRUE, collapsible=TRUE,
                                              tableOutput("borough_stats")),
                                          box(title = "Casualties by borough", width = 9, solidHeader=TRUE, collapsible=TRUE,
                                              ggvisOutput("borough_count")))
                                        
                                ),
                                tabItem(tabName = "temporal",
                                        fluidRow(
                                          box(title = "Casualties by month and severity", width = 5, solidHeader=TRUE, collapsible=TRUE,
                                              ggvisOutput("timeband_month")),
                                          box(title = "Casualties by hour and light conditions", width = 7, solidHeader=TRUE, collapsible=TRUE,
                                              ggvisOutput("timeband_hour")))
                                ),
                                tabItem(tabName = "demographics",
                                        fluidRow(
                                          box(title = "Casualties by ageband and gender", width = 6, solidHeader=TRUE, collapsible=TRUE,
                                              ggvisOutput("ageband_gender")),
                                          box(title = "Casualties by ageband and severity", width = 6, solidHeader=TRUE, collapsible=TRUE,
                                              ggvisOutput("ageband_severity")))
                                ),
                                tabItem(tabName = "data",
                                        fluidRow(box(width = 12, DT::dataTableOutput("table"))))
                              ))))


server <- function(input, output, session) {
  
  casualties <- reactive({data %>% filter(year %in% input$year &
                                            mode %in% input$mode &
                                            severity %in% input$severity)})
  
  ## CASUALTY MAP ##
  
  output$map <- renderLeaflet({
    leaflet(data) %>%  addProviderTiles("CartoDB.DarkMatter") %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) 
  })
  
  observe({
    pal <- colorFactor(c("#b10026", "#fd8d3c", "#ffeda0"), domain = c("Fatal", "Serious", "Slight"), ordered = TRUE)
    leafletProxy("map", data = casualties()) %>%
      clearShapes() %>% 
      addPolygons(data = boroughs, fill = F, color = "white", weight = 1.5, group = "London Boroughs") %>% 
      addLayersControl(
        overlayGroups = "London Boroughs",
        options = layersControlOptions(collapsed = FALSE)) %>% 
      addCircles(~long, ~lat, radius = ~ifelse(severity == "Fatal" | severity == "Serious", 10, 6), color = ~pal(severity), fillOpacity = 0.5, popup = ~text)
  })
  
  
  ## BOROUGH STATISTICS ## 
  
  # Casualties by borough
  borough_count <- reactive({ 
    casualties() %>%
      group_by(borough) %>%
      summarise(count = n()) %>% 
      ggvis(~borough, ~count, fill := "steelblue") %>%
      mutate(borough = reorder(borough, -count)) %>%
      layer_bars(stroke := "white") %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      hide_legend("fill") %>% 
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  borough_count %>% bind_shiny("borough_count")  
  
  # Borough statistics
  output$borough_stats <- renderTable({
    dataset <- casualties()
    dataset %>% 
      group_by(borough) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      arrange(desc(count)) %>%
      mutate(percent = paste0(round(100 * count/sum(count), 1), "%")) %>%
      select(Borough = borough, Count = count, Percent = percent) %>% 
      head(10)
  }, include.rownames=FALSE)
  
  
  ## TEMPORAL PROFILE ##
  
  # Casualties by month and severity
  timeband_month <- reactive({ 
    casualties() %>%
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
  
  # Casualties by hour and light conditions
  timeband_hour <- reactive({ 
    casualties() %>%
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
  
  ## DEMOGRAPHICS ##  
  
  # Casualties by ageband and gender
  ageband_gender <- reactive({ 
    casualties() %>%
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
  
  # Casualties by ageband and gender
  ageband_severity <- reactive({ 
    casualties() %>%
      group_by(ageband, severity) %>%
      summarise(count = n()) %>% 
      ggvis(~ageband, ~count, fill = ~severity) %>%
      layer_bars(stroke := "white") %>%
      scale_nominal("fill", range = c("#b10026", "#fd8d3c", "#ffeda0"), label=c("Fatal", "Serious", "Slight")) %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>% 
      add_legend("fill", title = "Severity") %>% 
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  ageband_severity %>% bind_shiny("ageband_severity") 
  
  
  # RAW DATA ##
  
  output$table <- DT::renderDataTable({
    casualties() %>% select(AREFNO, date, mode, severity, sex, ageband, borough, long, lat)
    
  }) }

shinyApp(ui, server)