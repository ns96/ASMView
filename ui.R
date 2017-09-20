#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(
    title = "LeafLift Portal v0.2.1",
    dropdownMenuOutput("notificationMenu")
  ),
  
  dashboardSidebar(
    textInput(inputId = "otherLocation",
              label = "Enter Location:",
              value = ""),
    
    actionButton("setLocation", "Find Location"),
    
    selectInput("selectLocation", "Select Location:", 
                choices = list("Location 1" = "L1", "Location 2" = "L2",
                               "Location 3" = "L3", "Other" = "Other"), selected = "L1"),
    
    selectInput("selectNode", "Select Node:", 
                choices = list("ALL" = 'N0', "Node 1" = 'N1',
                               "Node 2" = 'N2', "Node 3" = 'N3'), selected = 'N1'),
    
    selectInput("selectDays", "Select Number Of Days:", 
                choices = list("7 Days" = 7, "30 Days" = 30,
                               "60 Days" = 60), selected = 7),
    
    # add the menu for vewing the various sensors
    sidebarMenu(
      menuItem("Atmospheric", tabName = "atmospheric", icon = icon("cloud")),
      menuItem("Lighting", tabName = "lighting", icon = icon("lightbulb-o")),
      menuItem("Reservoir", tabName = "reservoir", icon = icon("life-ring")),
      menuItem("Plant/Soil", tabName = "plant", icon = icon("leaf")),
      br(),
      br(),
      img(src="logo.png", width=225, height=71, align="center")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "atmospheric",
              h4("Atmospheric Sensors"),
              
              fluidRow(
                infoBoxOutput("asTemperature"),
                infoBoxOutput("asHumidity"),
                infoBoxOutput("asCO2")
              ),
              
              fluidRow(
                tabBox(
                  title = "Last 24 Hours",
                  id = "atmosphericTab1", height = "500px",
                  tabPanel("Temperature", plotlyOutput("temperaturePlot1")),
                  tabPanel("Humidity", plotlyOutput("humidityPlot1")),
                  tabPanel("CO2", plotlyOutput("co2Plot1"))
                ),
                
                tabBox(
                  title = textOutput("atmosphericTabBoxTitle"),
                  id = "atmosphericTab2", height = "500px",
                  tabPanel("Temperature",  plotlyOutput("temperaturePlot2")),
                  tabPanel("Humidity", plotlyOutput("humidityPlot2")),
                  tabPanel("CO2", plotlyOutput("co2Plot2"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Temperature/Humdity Last 24 Hours", 
                  width = "100%",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("temperatureHumidityPlot1")
                )
              ),
              
              fluidRow(
                box(
                  title = textOutput("boxTitle1"), 
                  width = "100%",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("temperatureHumidityPlot2")
                )
              )
      ),
      
      tabItem(tabName = "lighting",
              h4("Light Sensors"),
              
              fluidRow(
                infoBoxOutput("lsLUX"),
                infoBoxOutput("lsPAR"),
                infoBoxOutput("lsRGB")
              ),
              
              fluidRow(
                tabBox(
                  title = "Last 24 Hours",
                  id = "lightTab1", height = "500px",
                  tabPanel("LUX", plotlyOutput("luxPlot1")),
                  tabPanel("PAR", plotlyOutput("parPlot1")),
                  tabPanel("RGB", plotlyOutput("rgbPlot1"))
                ),
                
                tabBox(
                  title = textOutput("lightTabBoxTitle"),
                  id = "lightTab2", height = "500px",
                  tabPanel("LUX",  plotlyOutput("luxPlot2")),
                  tabPanel("PAR", plotlyOutput("parPlot2")),
                  tabPanel("RGB", plotlyOutput("rgbPlot2"))
                )
              )
      ),
      
      tabItem(tabName = "reservoir",
              h4("Reservoir Sensors"),
              
              fluidRow(
                infoBoxOutput("rsTemperature"),
                infoBoxOutput("rsPH"),
                infoBoxOutput("rsPPM")
              ),
              
              fluidRow(
                tabBox(
                  title = "Last 24 Hours",
                  id = "reservoirTab1", height = "500px",
                  tabPanel("Temperature", plotlyOutput("rtempPlot1")),
                  tabPanel("PH", plotlyOutput("rphPlot1")),
                  tabPanel("PPM", plotlyOutput("rppmPlot1"))
                ),
                
                tabBox(
                  title = textOutput("reservoirTabBoxTitle"),
                  id = "reservoirTab2", height = "500px",
                  tabPanel("Temperature",  plotlyOutput("rtempPlot2")),
                  tabPanel("PH", plotlyOutput("rphPlot2")),
                  tabPanel("PPM", plotlyOutput("rppmPlot2"))
                )
              )
      ),
      
      tabItem(tabName = "plant",
              h4("Soil Moisture / Weight Sensors"),
              
              fluidRow(
                infoBoxOutput("psMoisture"),
                infoBoxOutput("psGrowth")
              ),
              
              fluidRow(
                tabBox(
                  title = "Last 24 Hours",
                  id = "plantTab1", height = "500px",
                  tabPanel("Soil Moisture", plotlyOutput("moisturePlot1")),
                  tabPanel("Growth", plotlyOutput("growthPlot1"))
                ),
                
                tabBox(
                  title = textOutput("plantTabBoxTitle"),
                  id = "plantTab2", height = "500px",
                  tabPanel("Soil Moisture", plotlyOutput("moisturePlot2")),
                  tabPanel("Growth", plotlyOutput("growthPlot2"))
                )
              )
      )
    )
  )
)