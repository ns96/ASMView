#
# Script to load json data files
#

library(jsonlite)
library(anytime)
library(plotly)

# function to return a plot
getPlot = function(df, miny, maxy) {
  plt <- plot_ly(df, x = ~x) %>%
    add_trace(y = ~y, mode = 'lines') %>%
    add_trace(y = ~y_min, dash = 'dash') %>%
    add_trace(y = ~y_max, dash = 'dash') %>%
    layout(yaxis = list(range = c(miny, maxy)), 
           showlegend = FALSE)
  
  return(plt)
}

# function to return a plot
getY2Plot = function(df, miny, maxy) {
  plt <- plot_ly(df, x = ~x) %>%
    add_trace(y = ~y, mode = 'lines') %>%
    add_trace(y = ~y2, mode = 'lines') %>%
    add_trace(y = ~y_min, dash = 'dash') %>%
    add_trace(y = ~y_max, dash = 'dash') %>%
    layout(yaxis = list(range = c(miny, maxy)), 
           showlegend = FALSE)
  
  return(plt)
}

# function to return a plot
getY3Plot = function(df, miny, maxy) {
  plt <- plot_ly(df, x = ~x) %>%
    add_trace(y = ~y, mode = 'lines') %>%
    add_trace(y = ~y2, mode = 'lines') %>%
    add_trace(y = ~y3, mode = 'lines') %>%
    add_trace(y = ~y_min, dash = 'dash') %>%
    add_trace(y = ~y_max, dash = 'dash') %>%
    layout(yaxis = list(range = c(miny, maxy)), 
           showlegend = FALSE)
  
  return(plt)
}

# function to return a plot
getTemperatureHumidityPlot = function(df, miny, maxy, y1name, y2name) {
  plt <- plot_ly(df, x = ~x) %>%
    add_trace(y = ~y1A, mode = 'lines', name = y1name) %>%
    add_trace(y = ~y1B, mode = 'lines', name = y1name) %>%
    add_trace(y = ~y2A, mode = 'lines', name = y2name) %>%
    add_trace(y = ~y2B, mode = 'lines', name = y2name) %>%
    layout(yaxis = list(range = c(miny, maxy)), 
           showlegend = TRUE)
  
  return(plt)
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
  x <- df1$x
  y1A <- df1$y
  y1B <- df1$y2
  y2A <- df2$y
  y2B <- df2$y2
  
  df <- data.frame(x, y1A, y1B, y2A, y2B)
  return(df)
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
  filename = paste0('data/', prefix, '_', days, 'D.json')
  
  print(paste("Loading data:", location, node, filename))
  
  df = loadData(filename)
  if(location == 'L1' && node == 'N1') {
    return(df)
  } else { # add some random jitter to data
    return(addJitterToData(df))
  }
}

# load data by location and node and number for 1 day then get the info
# right now just the last data point to reflect current conditions
getDataInformation = function(loc, oloc, node, prefix) {
  df = loadDataByLocation(loc, oloc, node, prefix, 1)
  
  ylast = tail(df$y, n = 1)
  info = round(ylast, digits = 1)
  
  if("y2" %in% colnames(df)) {
    y2last = tail(df$y2, n = 1)
    info = paste(info, '/', round(y2last, digits = 1))
  }
  
  if("y3" %in% colnames(df)) {
    y3last = tail(df$y3, n = 1)
    info = paste(info, '/', round(y3last, digits = 1))
  }

  return(info)
}

# load data by location and node and number of days
loadDataByLocationCombine = function(loc, oloc, node, prefix1, prefix2, days) {
  location = getLocation(loc, oloc)
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

# get messages for a particular location
getNotifications = function(location, otherLocation) {
  location = getLocation(location, otherLocation)
  
  msgs <- list()
  for(i in 1:7) {
    msgs[[i]] = paste("Sensor @", location, 'out of range...')
  }
  
  return(msgs)
} 


msgsTest <- getNotifications("Test", "Test")
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
