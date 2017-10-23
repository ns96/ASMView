#
# Script to load json data files
#

library(jsonlite)
library(anytime)
library(plotly)

# font for the plots
f <- list(
  family = "Courier New, monospace",
  size = 14,
  color = "#7f7f7f"
)

# store a vector of remote locations
rlocations <- c('Piru')

piruNodes <- list('WestGR1' = 'piruWestGR1', 'WestGR2' = 'piruWestGR2', 
                  'NorthGR3a' = 'piruNorthGR3a', 'NorthGR3b' = 'piruNorthGR3b',
                  'NorthGR3c' = 'piruNorthGR3c', 'NorthUrbanGR1' = 'piruNorthUrbanGR1',
                  'NorthUrbanGR2' = 'piruNorthUrbanGR2','Greenhouse' = 'PiruGreenhouse',
                  'FarmOne' = 'FarmOne', 'ICE' = 'ICE', 'EastVillage' = 'EastVillage')

demoNodes <- list("ALL" = 'N0', "Node 1" = 'N1',
                  "Node 2" = 'N2', "Node 3" = 'N3')

# the base url
baseURL = "http://api-quadroponic.rhcloud.com/v1/report/chart2/"

# function to return a plot of with either 1, 2, 3 y columns plotted etc
getPlot = function(df, miny, maxy, ytitle = 'Y') {
  cns = colnames(df)
  
  if("y3" %in% cns) {
    return(getY3Plot(df, miny, maxy, ytitle))
  } else if("y2" %in% cns) {
    return(getY2Plot(df, miny, maxy, ytitle))
  } else {
    return(getY1Plot(df, miny, maxy, ytitle))
  }
}

# function to return a plot with only 1 y column
getY1Plot = function(df, miny, maxy, ytitle = 'Y') {
  plt <- plot_ly(df, x = ~x) %>%
    add_trace(y = ~y, mode = 'lines') %>%
    add_trace(y = ~y_min, dash = 'dash') %>%
    add_trace(y = ~y_max, dash = 'dash') %>%
    layout(yaxis = list(range = c(miny, maxy), title = ytitle, font = f),
           xaxis = list(title = 'Timestamp', font = f),
           showlegend = FALSE)
  
  return(plt)
}

# function to return a plot with only 2 y columns
getY2Plot = function(df, miny, maxy, ytitle = 'Y') {
  plt <- plot_ly(df, x = ~x) %>%
    add_trace(y = ~y, mode = 'lines') %>%
    add_trace(y = ~y2, mode = 'lines') %>%
    add_trace(y = ~y_min, dash = 'dash') %>%
    add_trace(y = ~y_max, dash = 'dash') %>%
    layout(yaxis = list(range = c(miny, maxy), title = ytitle, font = f),
           xaxis = list(title = 'Timestamp', font = f),
           showlegend = FALSE)
  
  return(plt)
}

#function to return a plot with only 3 y columns
getY3Plot = function(df, miny, maxy, ytitle= 'Y') {
  plt <- plot_ly(df, x = ~x) %>%
    add_trace(y = ~y, mode = 'lines') %>%
    add_trace(y = ~y2, mode = 'lines') %>%
    add_trace(y = ~y3, mode = 'lines') %>%
    add_trace(y = ~y_min, dash = 'dash') %>%
    add_trace(y = ~y_max, dash = 'dash') %>%
    layout(yaxis = list(range = c(miny, maxy), title = ytitle, font = f),
           xaxis = list(title = 'Timestamp', font = f),
           showlegend = FALSE)
  
  return(plt)
}

# function to return a plot
getTemperatureHumidityPlot = function(df, miny, maxy, y1name, y2name, ytitle = 'Y') {
  cns <- colnames(df)
  
  if("y1B" %in% cns) {
    plt <- plot_ly(df, x = ~x) %>%
      add_trace(y = ~y1A, mode = 'lines', name = y1name) %>%
      add_trace(y = ~y1B, mode = 'lines', name = y1name) %>%
      add_trace(y = ~y2A, mode = 'lines', name = y2name) %>%
      add_trace(y = ~y2B, mode = 'lines', name = y2name) %>%
      layout(yaxis = list(range = c(miny, maxy), title = ytitle, font = f),
            xaxis = list(title = 'Timestamp', font = f),
            showlegend = TRUE)
  
    return(plt)
  } else {
    plt <- plot_ly(df, x = ~x) %>%
      add_trace(y = ~y1A, mode = 'lines', name = y1name) %>%
      add_trace(y = ~y2A, mode = 'lines', name = y2name) %>%
      layout(yaxis = list(range = c(miny, maxy), title = ytitle, font = f),
             xaxis = list(title = 'Timestamp', font = f),
             showlegend = TRUE)
    
    return(plt)  
  }
}

# function to get dummy data
getDummyData = function(days, freq, value, sp_min, sp_max) {
  ds = days * 86400 # get total seconds from days
  fs = freq * 60 # get seconds from freq which is in minuetes
  
  now_ct <- as.POSIXct(Sys.time())
  tm <- seq(0, ds, by = fs)
  x <- now_ct - tm
  y <- rnorm(length(x), mean = value, sd = 2)
  y2 <- rnorm(length(x), mean = value, sd = 2)
  y_min <- rep(sp_min, length(x))
  y_max <- rep(sp_max, length(x))
  
  df <- data.frame(x, y, y2, y_min, y_max)
  
  return(df)
}

# function to combine two data frames with same x, and two y
combineData = function(df1, df2) {
  cns <- colnames(df1)
  
  x <- df1$x
  y1A <- df1$y
  y2A <- df2$y
  
  if("y2" %in% cns) {
    y1B <- df1$y2
    y2B <- df2$y2
  
    df <- data.frame(x, y1A, y1B, y2A, y2B)
    return(df)
  } else {
    df <- data.frame(x, y1A, y2A)
    return(df)
  }
}

# functopn load data into data frame and convert
# the x axis to data time objects
loadData = function(filename) {
  jdata <- fromJSON(filename)
  df <- data.frame(jdata)
  df$x <-anytime(df$x)
  
  return(df)
} 

# load data by location and node and number of days
loadDataByLocation = function(loc, oloc, node, prefix, days) {
  location = getLocation(loc, oloc)
  
  if(location %in% rlocations) {
    # get the sensor name based on file prefix
    sensor = getSensorName(prefix)
    
    if(sensor != 'unknown') {
      dataURL = paste0(baseURL, location, '/', node, '/', sensor, '/', days)
      print(paste("Loading URL data:", location, node, dataURL))
    
      df = loadData(dataURL)
      return(df)
    } else {
      return(NULL)
    }
  } else {
    filename = paste0('data/', prefix, '_', days, 'D.json')
    print(paste("Loading data:", location, node, filename))
    
    df = loadData(filename)
    
    if (location == 'Demo') {
      if(node == 'N1') {
        return(df)
      } else { # add some random jitter to data
        return(addJitterToData(df))
      }
    } else { 
      return(df)
    }
  }
}

# load data by location and node and number for 1 day then get the info
# right now just the last data point to reflect current conditions
getDataInformation = function(loc, oloc, node, prefix, d = 1) {
  df = loadDataByLocation(loc, oloc, node, prefix, 1)
  
  if(is.null(df)) {
    return('N/A')
  }
  
  ylast = tail(df$y, n = 1)
  info = round(ylast, digits = d)
  
  if("y2" %in% colnames(df)) {
    y2last = tail(df$y2, n = 1)
    info = paste(info, '/', round(y2last, digits = d))
  }
  
  if("y3" %in% colnames(df)) {
    y3last = tail(df$y3, n = 1)
    info = paste(info, '/', round(y3last, digits = d))
  }

  return(info)
}

# load data by location and node and number of days
loadDataByLocationCombine = function(loc, oloc, node, prefix1, prefix2, days) {
  location = getLocation(loc, oloc)
  
  if(location %in% rlocations) {
    sensor1 = getSensorName(prefix1)
    sensor2 = getSensorName(prefix2)
    url1 = paste0(baseURL, location, '/', node, '/', sensor1, '/', days)
    url2 = paste0(baseURL, location, '/', node, '/', sensor2, '/', days)
    
    print(paste("Loading 2 URL data:", location, node, url1, url2))
    
    df1 = loadData(url1)
    df2 = loadData(url2)
    
    df = combineData(df1, df2)
    return(df)
  } else {
    filename1 = paste0('data/', prefix1, '_', days, 'D.json')
    filename2 = paste0('data/', prefix2, '_', days, 'D.json')
  
    print(paste("Loading data:", location, node, filename1, filename2))
  
    df1 = loadData(filename1)
    df2 = loadData(filename2)
  
    df = combineData(df1, df2)
    if(location == 'L1' && node == 'N1') {
      return(df)
    } else { # add some random jitter to data
      return(addJitterToData(df))
    }
  }
}

# return the location by checking if location or otherl ocation
# is valid
getLocation = function(location, otherLocation) {
  if(location == 'Other') {
    return(otherLocation)
  } else {
    return(location)
  }
}

# check to see if location enter is valid or not
checkLocation = function(location) {
  vloc = c('L10', 'L20', 'L30')
  valid = location %in% vloc
  return(valid)
}

# geth the sensor name based on file prefix
getSensorName = function(prefix) {
  if(prefix == 'temp') {
    return('dhtTemperature_f')
  } else if(prefix == 'humidity') {
    return('dhtHumidity')
  } else if(prefix == 'lux') {
    return('lux')
  } else {
    return('unknown')
  }
}

# get messages for a particular location
getNotifications = function(location, otherLocation) {
  location = getLocation(location, otherLocation)
  
  msgs <- list()
  for(i in 1:7) {
    msgs[[i]] = paste("Sensor @", location, 'out of range...')
  }
  
  return(msgs)
} 

# add noise to all the y axis. use to simulate data nodes
addJitterToData = function(df) {
  dfc <- cbind(df)
  
  dfc$y <- jitter(dfc$y, factor=10, amount=1)
  
  if("y2" %in% colnames(dfc)) {
    dfc$y2 <- jitter(df$y2, factor=10, amount=1)
  }
  
  if("y3" %in% colnames(dfc)) {
    dfc$y3 <- jitter(df$y3, factor=10, amount=1)
  }
  
  if("y1A" %in% colnames(dfc)) {
    dfc$y1A <- jitter(df$y1A, factor=10, amount=1)
  }
  
  if("y1B" %in% colnames(dfc)) {
    dfc$y1B <- jitter(df$y1B, factor=10, amount=1)
  }
  
  if("y2A" %in% colnames(dfc)) {
    dfc$y2A <- jitter(df$y2A, factor=10, amount=1)
  }
  
  if("y2B" %in% colnames(dfc)) {
    dfc$y2B <- jitter(df$y2B, factor=10, amount=1)
  }
  
  return(dfc)  
}


#
# Test code
#

dfU <- loadDataByLocation('Piru', '', 'piruWestGR1', 'temp', 10)
#getPlot(dfU, 50, 100, ytitle = 'Y')