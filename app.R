library(shiny) ; library(shinydashboard) ; library(dplyr) ; library(leaflet) ; 
library(RColorBrewer) ; library(ggplot2) ; library(scales) ; library(DT)

# load the casualty data
data <- readRDS(file="casualties_2005-14.Rda")
data$year <- as.factor(data$year)
data$agegroup <- as.factor(data$agegroup)
data$severity <- factor(data$severity, levels= c("Fatal", "Serious", "Slight"), ordered = TRUE)
data$day <- factor(data$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered=T)
data$hour <- factor(data$hour)
data$light <- ordered(data$light, levels = c("Dark", "Daylight"))

ui <- shinyUI(dashboardPage(skin = "blue",
  dashboardHeader(title = "STATS19 scanner"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("year", 
                  label = "Select year:",
                  choices = c(levels(data$year)),
                  selected = 2014),
    radioButtons(inputId = "mode",
                 label = "Select mode of travel:",
                 choices = c(levels(data$mode)),
                 selected = "Pedal Cycle"),
    selectInput(inputId = "severity",
                label = "Select severity:",
                choices = c(levels(data$severity)),
                selected = c(levels(data$severity)), multiple=T),
    hr(),
    menuItem("Casualty map", tabName = "map", icon = icon("fa fa-map-marker")),
    menuItem("Borough statistics", tabName = "boroughs", icon = icon("fa fa-users")),
    menuItem("Temporal profile", tabName = "temporal", icon = icon("fa fa-clock-o")),
    menuItem("Demographics", tabName = "demographics", icon = icon("fa fa-users")),
    menuItem("Raw data", tabName = "data", icon = icon("th")),
    menuItem("About", tabName = "about", icon = icon("fa fa-question")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                title = "Map", width = 12,
                    leafletOutput("map", height = "650"))
      ),
      tabItem(tabName = "boroughs",
              fluidRow(
                box(title = "Casualties by borough and severity", width = 12, solidHeader=TRUE, collapsible=TRUE,
                    plotOutput("borough")))
      ),
      tabItem(tabName = "temporal",
              fluidRow(
                box(title = "Casualties by month and severity", width = 5, solidHeader=TRUE, collapsible=TRUE,
                    plotOutput("timeband_month")),
                box(title = "Casualties by hour and light conditions", width = 7, solidHeader=TRUE, collapsible=TRUE,
                    plotOutput("timeband_hour")))
      ),
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Casualties by ageband and gender", width = 6, solidHeader=TRUE, collapsible=TRUE,
                    plotOutput("ageband_gender")),
                box(title = "Casualties by ageband and severity", width = 6, solidHeader=TRUE, collapsible=TRUE,
                    plotOutput("ageband_severity")))
      ),
      tabItem(tabName = "data",
              fluidRow(box(width = 12, DT::dataTableOutput("table")))),
      tabItem(tabName = "about",
              fluidRow(
                column(12,
                       h4(strong("About")),
                       h5("This application is designed to allow the user to interrogate road traffic 
                         collisions recorded in Greater London between 2005 and 2014."),
                       h4(strong("How to use")),
                       h5("The filter panel allows the user to plot road traffic collisions involving injury by year, mode and severity onto the map. 
                           Details of each collision can be obtained by clicking on any of the points. 
                           Information on the demographic and temporal profile of casualties are provided under the relevant tabs."),
                       h4(strong("Data sources")),
                       h5("STATS19 road traffic collision data for Greater London are available from",
                         a("Transport for London", 
                           href = "https://www.tfl.gov.uk/corporate/publications-and-reports/road-safety"),
                         "and information about collision data can be found",
                         a("here.", 
                           href = "https://www.tfl.gov.uk/cdn/static/cms/documents/collision-data-guide.pdf")),
                       h4(strong("Code")),
                       h5("The R code used to create this application can be found on ",
                          a("GitHub", 
                            href = "https://github.com/hpartridge/collision_mapper")))))
              ))))


server <- function(input, output, session) {

casualties <- reactive({data %>% filter(year %in% input$year &
                                         mode %in% input$mode &
                                         severity %in% input$severity)})

## CASUALTY MAP ##

  output$map <- renderLeaflet({
leaflet(casualties()) %>%  addProviderTiles("CartoDB.Positron") %>%
  fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
   
  observe({
    pal <- colorFactor(c("black", "red", "orange"), domain = c("Fatal", "Serious", "Slight"), ordered = FALSE)
        leafletProxy("map", data = casualties()) %>%
          addProviderTiles("CartoDB.Positron") %>% 
          addCircles(~long, ~lat, radius = 6, color = ~pal(severity), fillOpacity = 0.3, opacity = 0.5, popup = ~text)
  })

## BOROUGH STATISTICS ##
 
   output$borough <- renderPlot({
    borough_count <- casualties() %>%
      group_by(severity, borough) %>%
      summarise(count = n())
    ggplot(borough_count, aes(x=reorder(borough, count, FUN=sum), y = count, fill=severity)) +
      geom_bar(color="white", stat="identity") +
      scale_y_continuous(breaks= pretty_breaks()) +
      scale_fill_manual(values=c("black", "red", "orange"), name="") +
      ylab("") + xlab("") +
      theme_bw() +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = NA),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid.major.x = element_line(colour=NA)) +
      coord_flip()
  })
  
## TEMPORAL PROFILE ##
  
  # Casualties by month and severity
  output$timeband_month <- renderPlot({
    timeband_month <- casualties() %>%
      group_by(severity, month) %>%
      summarise(count = n())
    ggplot(timeband_month, aes(x=month, y=count, fill=severity)) +
      geom_bar(stat="identity") +
      scale_y_continuous(breaks= pretty_breaks()) +
      scale_fill_manual(values=c("black", "red", "orange"), name="") +
      ylab("") + xlab("") +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = NA),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid.major.x = element_line(colour=NA))
  })
  
  # Casualties by hour and light conditions
  output$timeband_hour <- renderPlot({
    timeband_hour <- casualties() %>%
      group_by(light, hour) %>%
      summarise(count = n())
    ggplot(timeband_hour, aes(x=hour, y=count, fill=light)) +
      geom_bar(stat="identity") +
      scale_y_continuous(breaks= pretty_breaks()) +
      scale_fill_manual(values=c("midnightblue", "yellow"), name="") +
      ylab("") + xlab("") +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = NA),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid.major.x = element_line(colour=NA))
  })  
  
## DEMOGRAPHICS ##  
  
# Casualties by ageband and gender
output$ageband_gender <- renderPlot({
    ageband_gender <- casualties() %>%
      group_by(sex, ageband) %>%
      summarise(count = n())
     ggplot(ageband_gender, aes(x=ageband, y=count, fill=sex)) +
      geom_bar(position="dodge", stat="identity") +
       scale_y_continuous(breaks= pretty_breaks()) +
       scale_fill_manual(values=c("steelblue", "mediumpurple1"), name="", labels=c("Male", "Female")) +
       ylab("") + xlab("") +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = NA),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid.major.x = element_line(colour=NA))
  })

# Casualties by ageband and gender
output$ageband_severity <- renderPlot({
  ageband_severity <- casualties() %>%
    group_by(ageband, severity) %>%
    summarise(count = n())
  ggplot(ageband_severity, aes(x=ageband, y=count, fill=severity)) +
    geom_bar(stat="identity") +
    scale_y_continuous(breaks= pretty_breaks()) +
    scale_fill_manual(values=c("black", "red", "orange"), name="", labels=c("Fatal", "Serious", "Slight")) +
    ylab("") + xlab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = NA),
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_line(colour=NA))
})

# RAW DATA ##

  output$table <- DT::renderDataTable({
    casualties() %>% select(AREFNO, date, mode, severity, agegroup, borough, long, lat)
    
  }) }

shinyApp(ui, server)
