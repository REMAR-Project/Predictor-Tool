### read data

CrabDateRead = function(file = NULL) {
  
  library(jsonlite)
  library(httr)
  
  # Preliminaries -----------------------------------------------------------
  
  # if the file is not specified, use the file picker to select it
  if(is.null(file) == TRUE) {
    cat("Select the file to read in with the file picker (may be under this window) \n")
    cat("Waiting...\n")
    file = file.choose()
  }
  
  # Read in and format the data ---------------------------------------------
  
  # Rread in the header information from the file
  ID.numb = read.table(file = file, header = FALSE, sep=":", nrows = 1, skip = 1)
  ID.numb = as.character(ID.numb[,2])
  ID.name = read.table(file = file, header = FALSE, sep=":", nrows = 1, skip = 2)
  ID.name = as.character(ID.name[,2])
  clock.offset = read.table(file = file, header = FALSE, sep=":", nrows = 1, skip = 5)
  clock.offset = as.numeric(clock.offset[,2])
  
  # Read in the latitude and longitude accounting for strange encoding of the file
  latitude = read.table(file = file, header = FALSE, sep="", nrows = 1, skip = 3)
  if (grepl("Â", latitude[1], fixed=TRUE)){latitude[1] = str_replace(latitude[1], "Â", "")}
  longitude = read.table(file = file, header = FALSE, sep="", nrows = 1, skip = 4)
  if (grepl("Â", longitude[1], fixed=TRUE)){longitude[1] = str_replace(longitude[1], "Â", "")}
  templat = paste(gsub('LATITUDE:', '', latitude[1]), latitude[2], "''", latitude[3])
  templng = paste(gsub('LONGITUDE:', '', longitude[1]), longitude[2], "''", longitude[3])
  ID.latlng = paste(gsub(' ', '', templat), " ", gsub(' ', '', templng))
  latitude = as.character(unlist(latitude))
  longitude = as.character(unlist(longitude))
  
  latitude.text = latitude
  longitude.text = longitude
  
  
  
  # Modify the data to covert to decimals
  latitude = gsub('º', '', latitude)
  longitude = gsub('º', '', longitude)
  latitude = gsub('\'', '', latitude)
  longitude = gsub('\'', '', longitude)
  latitude = gsub('LATITUDE:', '', latitude)
  longitude = gsub('LONGITUDE:', '', longitude)
  latitude.dec = as.numeric(latitude[1]) +  as.numeric(latitude[2]) / 60
  longitude.dec = as.numeric(longitude[1]) +  as.numeric(longitude[2]) / 60
  if(latitude[3] == "S")  latitude.dec = -latitude.dec
  if(longitude[3] == "W")  longitude.dec = -longitude.dec
  
  # Read in the tide data
  data = read.table(file = file, header = TRUE, sep="", skip = 14, fill = TRUE)
  
  # Report some information to the console
  cat("Working on station number",ID.numb,"(",ID.name,")","\n")
  cat(latitude.text,"(",latitude.dec,")","\n") 
  cat(longitude.text,"(",longitude.dec,")","\n") 
  cat("file:",basename(file),"\n") 
  map.link = paste("https://maps.google.com/?q=",latitude.dec,",",longitude.dec,sep = "")
  cat("google maps link:", map.link)
  cat("\n")
  cat("reading the data:")
  
  # Loop through all the rows in the data and build a table
  
  # Create empty data containers
  tide.day.number = NULL
  tide.number = NULL
  tide.time = as.data.frame(ISOdatetime(1970, 1, 1, 1, 0, 0, tz="UTC")) # to begin with this is local time rather than UTC
  names(tide.time) = "time"
  tide.level = NULL
  
  # The text file has ten possible tide entries each day
  for (i in 1:length(data[,1])) {
    
    # report progress to the screen because this might take some time
    if(round(i/365) == i/365) cat(" ",round(100*i/length(data[,1]),1), "%", sep = "")
    
    tide.day.number = rbind(tide.day.number, i)
    
    date = as.character(data[i,1])
    day = as.numeric(substr(date, 1, 2))
    month = as.numeric(substr(date, 4, 5))
    year = as.numeric(substr(date, 7, 10))
    
    n = 1
    while(n <= 10) {
      # nth possible tide on this day
      
      # Only add if there is data there  
      if(is.na(as.character(data[i,2*n+1])) == FALSE) {
        tide.number = rbind(tide.number, n)
        time = as.character(data[i,2*n])
        height = as.numeric(data[i,2*n+1])
        hour = as.numeric(substr(time, 1, 2))
        minute = as.numeric(substr(time, 4, 5))
        # the time is read in and converted to actual UTC
        full.time = as.data.frame(ISOdatetime(year, month, day, hour, minute, 0, tz="UTC") + clock.offset*60*60)
        names(full.time) = "time"
        tide.time = rbind.data.frame(tide.time, full.time)
        tide.level = rbind(tide.level, height)
        
      } else { n = 100 } # For speed: the first NA tide on a day means there are no more to read
      # end of if
      
      n = n + 1   
    } # End of n while loop
  } # End of i loop
  
  cat(" finished! \n")
  
  # Remove the first row value of time (a dummy that was there to maintain the values in date format)
  tide.time.utc = tide.time[-1,]
  
  # Prepare the dataframe and return it -------------------------------------
  
  clock.offset = -clock.offset # change this to the usual time zone form
  
  
  # Get State -------------------------------
  
  stations = c('10650', '20520', '10525', '30540', '30825', '40240', '40263', '60135', '60245')
  states = c('AP', 'PA2', 'PA1', 'PB', 'SE', 'BA-ES', 'ES', 'PR', 'SC')
  
  if (ID.numb %in% stations){
    
    # If REMAR site, no need to use google maps
    state = states[match(ID.numb, stations)]
    
  } else {
    
    # Get state from google maps
    state <- ""
    url = paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=", latitude.dec, ",", longitude.dec, "&key=AIzaSyCKfzrRm8UtGzJo9Z-ek0gX4l2eeRtfzUk")
    google.raw <- GET(url=url)
    json <- fromJSON(rawToChar(google.raw$content))
    for (i in 1:length(json$results$address_components))
    {
      if (state != "") {break}
      for (j in 1:length(json$results$address_components[[i]]$types))
      {
        if ("administrative_area_level_1" %in% json[["results"]][["address_components"]][[i]][["types"]][[j]])
        {
          state <- json$results$address_components[[i]]$short_name[j]
          break
        }
      }  
    }
  }
  
  
  ID.state = state
  
  
  
  # Finish building data ---------------------------------
  
  station = list(ID.numb, ID.name, ID.state, clock.offset, latitude.dec, longitude.dec, map.link, basename(file))
  names(station) = c("ID.numb", "ID.name", "ID.state", "clock.offset", "latitude", "longitude", "map.link", "file")
  tide = data.frame(tide.time.utc, tide.level)
  rownames(tide) = NULL
  
  output = list(station, tide)
  names(output) = c("station", "tide")
  
  return (output)
  
} # end of function

