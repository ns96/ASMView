#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

source('helpers.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # output any notification in the header
  output$notificationMenu <- renderMenu({
    messageData <- getNotifications(input$selectLocation, input$otherLocation)
    
    msgs <- lapply(messageData, function(msg) {
      notificationItem(text = msg,
                              icon = icon("exclamation-triangle"), 
                              status = "warning")
    })
    
    dropdownMenu(type = "notifications", .list = msgs)
  })
  
  # based on the selected location load the correct node names
  observe({
    location = input$selectLocation
    print(paste("Location Selected >>", location))
    
    nodeList = demoNodes
    default = 'N1'
    if(location == 'Piru') {
      nodeList = piruNodes
      default = 'ICE'
    }
    
    updateSelectInput(session, "selectNode",
                      choices = nodeList,
                      selected = default)
  })
  
  # handle the search button being pressed
  observeEvent(input$setLocation, {
    location = input$otherLocation
    valid = checkLocation(location)
    
    showModal(modalDialog(
      title = "Location Search",
      size = "s",
      paste("Location:", input$otherLocation, 'Found >>>', valid)
    ))
    
    if(valid) {
      updateSelectInput(session, "selectLocation", selected = "Other")
    }
  })
  
  #
  # update the info boxes and plots for the atmospheric sensor tab
  #
  output$asTemperature <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'temp')
    
    infoBox("Temperature (F)", info, icon = icon("thermometer-half"))
  })
  
  output$asHumidity <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'humidity')
    
    infoBox("% Humidity", info, icon = icon("tint"))
  })
  
  output$asCO2 <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'co2', 0)
    
    infoBox("CO2 (ppm)", info, icon = icon("cloud"))
  })
  
  output$temperaturePlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'temp', 1, useCache = TRUE)
    
    getPlot(df, 50, 100)
  })
  
  output$humidityPlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'humidity', 1, useCache = TRUE)
    
    getPlot(df, 20, 100)
  })
  
  output$co2Plot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'co2', 1, useCache = TRUE)
    
    getPlot(df, 200, 1800)
  })
  
  output$temperaturePlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$atmosphericTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'temp', days)
    
    getPlot(df, 50, 100)
  })
  
  output$humidityPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$atmosphericTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'humidity', days)
    
    getPlot(df, 20, 100)
  })
  
  output$co2Plot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$atmosphericTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'co2', days)
    
    getPlot(df, 200, 1800)
  })
  
  output$temperatureHumidityPlot1 <- renderPlotly({
    df = loadDataByLocationCombine(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'temp', 'humidity', 1)
    
    getTemperatureHumidityPlot(df, 0, 100, 'Temperature', 'Humidity')
  })
  
  output$temperatureHumidityPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$boxTitle1 <- renderText({
      paste('Temperature/Humidity Last', days, 'Days')
    })
    
    df = loadDataByLocationCombine(input$selectLocation, 
                                   input$otherLocation, 
                                   input$selectNode, 'temp', 'humidity', days)
    
    getTemperatureHumidityPlot(df, 0, 100, 'Temperature', 'Humidity')
  })
  
  #
  # update the plots for the light sensor tab
  #
  output$lsLUX <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'lux', 0)
    
    infoBox("LUX", info, icon = icon("lightbulb-o"))
  })
  
  output$lsPAR <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'par', 0)
    
    infoBox("PAR", info, icon = icon("bolt"))
  })
  
  output$lsRGB <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'rgb', 0)
    
    infoBox("RGB Levels", info, icon = icon("cubes"))
  })
  
  output$luxPlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'lux', 1, useCache = TRUE)
    
    getPlot(df, 0, 110)
  })
  
  output$parPlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'par', 1, useCache = TRUE)
    
    getPlot(df, 0, 800)
  })
  
  output$rgbPlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'rgb', 1, useCache = TRUE)
    
    getPlot(df, 0, 300)
  })
  
  output$luxPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$lightTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'lux', days)
    
    getPlot(df, 0, 110)
  })
  
  output$parPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$lightTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'par', days)
    
    getPlot(df, 0, 800)
  })
  
  output$rgbPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$lightTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'rgb', days)
    
    getPlot(df, 0, 300)
  })
  
  #
  # update the plots for the reservoir sensor tab
  #
  output$rsTemperature <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'rtemp')
    
    infoBox("Temperature", info, icon = icon("thermometer-half"))
  })
  
  output$rsPH <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'ph')
    
    infoBox("Reservoir pH", info, icon = icon("flask"))
  })
  
  output$rsPPM <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'ppm', 0)
    
    infoBox("Reservoir PPM", info, icon = icon("eyedropper"))
  })
  
  output$rtempPlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'rtemp', 1, useCache = TRUE)
    
    getPlot(df, 50, 100)
  })
  
  output$rphPlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'ph', 1, useCache = TRUE)
    
    getPlot(df, 4, 8)
  })
  
  output$rppmPlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'ppm', 1, useCache = TRUE)
    
    getPlot(df, 500, 1500)
  })
  
  output$rtempPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$reservoirTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'rtemp', days)
    
    getPlot(df, 50, 100)
  })
  
  output$rphPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$reservoirTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'ph', days)
    
    getPlot(df, 4, 8)
  })
  
  output$rppmPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$reservoirTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'ppm', days)
    
    getPlot(df, 500, 1500)
  })
  
  #
  # update the plots for the soil moisture and plant growth tab
  #
  output$psMoisture <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'smoist')
    
    infoBox("Soil Moisture", 36, icon = icon("tint"))
  })
  
  output$psGrowth <- renderInfoBox({
    info <- getDataInformation(input$selectLocation, 
                               input$otherLocation, 
                               input$selectNode, 'pgrowth')
    
    infoBox("Plant Weight", info, icon = icon("balance-scale"))
  })
  
  output$moisturePlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'smoist', 1, useCache = TRUE)
    
    getPlot(df, 25, 45)
  })
  
  output$growthPlot1 <- renderPlotly({
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'pgrowth', 1, useCache = TRUE)
    
    getPlot(df, 10, 60)
  })
  
  output$moisturePlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$plantTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'smoist', days)
    
    getPlot(df, 25, 45)
  })
  
  output$growthPlot2 <- renderPlotly({
    days <- as.integer(input$selectDays)
    
    output$plantTabBoxTitle <- renderText({
      paste('Last', days, 'Days')
    })
    
    df = loadDataByLocation(input$selectLocation, 
                            input$otherLocation, 
                            input$selectNode, 'pgrowth', days)
    
    getPlot(df, 10, 60)
  })
})