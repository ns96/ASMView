#
# Script to load json data files
#

library(jsonlite)
library(anytime)
library(plotly)
library(scales)

# font for the plots
f <- list(
  family = "Courier New, monospace",
  size = 14,
  color = "#7f7f7f"
)

# store a vector of remote locations
rlocations <- c('DEV')

demoNodes <- list("ALL" = 'N0', "Node 1" = 'N1',
                  "Node 2" = 'N2', "Node 3" = 'N3')

devNodes <- list("Node 1" = 'N1');

# the base url
baseURL = "http://api-quadroponic.rhcloud.com/v1/report/chart2/"
devURL = "http://34.230.80.186:5000/data/"
#devURL = "http://localhost:5000/data/"

# used to catch dataframe to keep from making back to back calls to API
dataList <- list()

# function to check a dataframe is not null or empty
checkDataframe = function(df) {
  if(is.null(df)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# get a dummy data with only one row
getEmptyPlot = function() {
  x <- c(0,1)
  y <- c(0,0)
  y_min <- c(0,0)
  y_max <- c(0,0)
  
  df <- data.frame(x, y, y_min, y_max)
  return(getY1Plot(df, 0, 100, 'No Data ...')) 
}

# function to return a plot of with either 1, 2, 3 y columns plotted etc
getPlot = function(df, miny, maxy, ytitle = 'Y') {
  if(checkDataframe(df)) {
    cns = colnames(df)
  
    if("y3" %in% cns) {
      return(getY3Plot(df, miny, maxy, ytitle))
    } else if("y2" %in% cns) {
      return(getY2Plot(df, miny, maxy, ytitle))
    } else {
      return(getY1Plot(df, miny, maxy, ytitle))
    }
  } else {
    return(getEmptyPlot())
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
  if(checkDataframe(df)) {
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
  } else {
    return(getEmptyPlot())
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

# function to return the cache data or null if it doesn't exit
getCachedData = function(filename) {
  if(filename %in% names(dataList)) {
    return(dataList[[filename]])
  } else {
    return(NULL)
  }  
}

# functopn load data into data frame and convert
# the x axis to data time objects
loadData = function(filename, rs=FALSE, medfilt=TRUE, useCache=FALSE) {
  if(useCache) {
    #return(getCachedData(filename))
  }
  
  jdata <- fromJSON(filename)
  df <- data.frame(jdata)
  
  if(nrow(df) == 0) {
    print(paste("Empty Dataframe:", filename))
    return(NULL)
  }
  
  df$x <- anytime(df$x)
  
  # See if to rescale the y data. 
  # This is only used for lux and rgb data for now
  if(rs) {
    cns <- colnames(df)
    
    df$y <- rescale(df$y, to = c(0, 100))
    df$y_min <- rep(10, nrow(df))
    df$y_max <- rep(90, nrow(df))
    
    if("y2" %in% cns) {
      df$y2 <- rescale(df$y2, to = c(0, 100))  
    }
    
    if("y3" %in% cns) {
      df$y3 <- rescale(df$y3, to = c(0, 100))  
    }
  }
  
  # see if to do mediam filtering of data
  if(medfilt) {
    cns <- colnames(df)
    
    df$y <- runmed(df$y, 5)
    
    if("y2" %in% cns) {
      df$y2 <- runmed(df$y2, 5)  
    }
    
    if("y3" %in% cns) {
      df$y3 <- runmed(df$y3, 5)  
    }
  }
  
  # store the data in the cache
  dataList[[filename]] <<- df
  
  return(df)
} 

# check to see if this is a valid location
checkNode = function(location, node) {
  nodes = list()
  
  if(location == 'Piru') {
    nodes = unlist(piruNodes, use.names = FALSE)
  } else if(location == 'DEV') {
    nodes = unlist(devNodes, use.names = FALSE)
  }
  
  return(node %in% nodes)
}

# load data by location and node and number of days
loadDataByLocation = function(loc, oloc, node, prefix, days, useCache = FALSE) {
  location = getLocation(loc, oloc)
  
  rs = FALSE
  #if(prefix == 'lux') {
  #  rs = TRUE
  #}
  
  if(location %in% rlocations) {
    # set the base url to dev if needed
    if(location == 'DEV') {
      baseURL = devURL
    }
    
    # get the sensor name based on file prefix
    sensor = getSensorName(prefix, location)
    
    # used to prevent costly calls to api with invalid nodes
    validNode = checkNode(location, node)
    
    if(sensor != 'unknown' && validNode) {
      dataURL = paste0(baseURL, location, '/', node, '/', sensor, '/', days)
      print(paste("Loading URL data:", location, node, dataURL))
    
      df = loadData(dataURL, rs, useCache)
      return(df)
    } else {
      return(NULL)
    }
  } else {
    filename = paste0('data/', prefix, '_', days, 'D.json')
    print(paste("Loading data:", location, node, filename))
    
    df = loadData(filename, rs, useCache)
    
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
    # set the base url to dev if needed
    if(location == 'DEV') {
      baseURL = devURL
    }
    
    sensor1 = getSensorName(prefix1, location)
    sensor2 = getSensorName(prefix2, location)
    url1 = paste0(baseURL, location, '/', node, '/', sensor1, '/', days)
    url2 = paste0(baseURL, location, '/', node, '/', sensor2, '/', days)
    
    print(paste("Loading 2 URL data:", location, node, url1, url2))
    
    # used to prevent costly calls to api with invalid nodes
    validNode = checkNode(location, node)
    
    if(validNode) {
      df1 = loadData(url1, useCache = TRUE)
      if(days == 1) {
        df2 = loadData(url2, useCache = TRUE)
      } else {
        df2 = loadData(url2, useCache = FALSE)
      }
      
      df = combineData(df1, df2)
      return(df)
    } else {
      return(NULL)
    }
  } else {
    filename1 = paste0('data/', prefix1, '_', days, 'D.json')
    filename2 = paste0('data/', prefix2, '_', days, 'D.json')
  
    print(paste("Loading data:", location, node, filename1, filename2))
  
    df1 = loadData(filename1, useCache = TRUE)
    if(days == 1) {
      df2 = loadData(filename2, useCache = TRUE)
    } else {
      df2 = loadData(filename2, useCache = FALSE)
    }
  
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
getSensorName = function(prefix, location) {
  if(location == 'DEV') {
    return(prefix)
  }
  
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
# ***TEST CODE***
#

#dfU <- loadDataByLocation('Piru', '', 'ICE', 'temp', 1)
#getPlot(dfU, 50, 100, ytitle = 'Y')
#dfU2 <- loadDataByLocation('Piru', '', 'ICE', 'temp', 1, useCache = TRUE)
#df22 = loadData('http://localhost:5000/data/DEV/N1/temp/7')