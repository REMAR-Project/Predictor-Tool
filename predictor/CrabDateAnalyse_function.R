
#####################
# Analysis Function #
#####################


CrabDateAnalyse = function(data = NULL, dayswindow = 3, andada = as.Date(c("2000-01-01", "2000-03-31")), period = NULL, plot = TRUE, flow.hours = 12, window.days = c(0,3), svg = FALSE, plotNumber = 0, n=1) {
  
  
  # Preliminaries -----------------------------------------------------------
  
  # Tools
  library(tools)
  # API libraries to get state from Google Maps based on lat/lng
  library(httr)
  # Check the required package is loaded and if not load it  
  require(oce)
  
  # Make sure that the correct andada values are specified
  
  if (length(andada) != 2) 
    stop("andada must be in the form c(first day, last day)")
  
  if (as.numeric(andada[2] - andada[1]) > 365)
    stop("andada must be less than a year long")
  
  if (as.numeric(andada[2] - andada[1]) < 0)
    stop("andada must be more than a day long")
  
  # Make sure that the correct data are specified
  
  if (length(data) != 2) 
    stop("data must be the complete and exact form from the CrabDateRead function")
  
  if (length(data$station) != 8) 
    stop("data must be the complete and exact form from the CrabDateRead function")
  
  if (length(data$tide) != 2) 
    stop("data must be the complete and exact form from the CrabDateRead function")
  
  # Rebuild local time (without daylight savings). Note this is not actually UTC even though it is encoded as UTC
  data$tide$tide.time.local = data$tide$tide.time.utc + data$station$clock.offset*60*60
  data$tide$tide.local.time.hour = format(data$tide$tide.time.local,"%H:%M:%S")
  data$tide$tide.local.time.day = format(data$tide$tide.time.local,"%Y-%m-%d")
  
  # If the period is not specified, set it to the dates covered in the data
  if(is.null (period)) 
    period = c(min(as.Date(data$tide$tide.local.time.day)), max(as.Date(data$tide$tide.local.time.day)))
  
  # Make sure that the correct period values are specified
  
  if (length(period) != 2) 
    stop("period must be in the form c(first day, last day)")
  
  if (as.numeric(period[2] - period[1]) < 0)
    stop("period must be more than a day long")
  
  # Limit the period to what is actually in the data
  period[1] = max(period[1], min(as.Date(data$tide$tide.local.time.day)))
  period[2] = min(period[2], max(as.Date(data$tide$tide.local.time.day)))
  
  # Make sure that a correct number of flow hours is specified
  
  #if (flow.hours < 0) 
  #stop("flow.hours must be a positive number")
  
  # Make sure that a correct window of days is are specified
  
  if (length(window.days) != 2) 
    stop("window.days must be in the form c(days before, days after)")
  
  days.before = window.days[1]
  days.after = window.days[2]
  
  if (min(days.before, days.after) < 0) 
    stop("window.days must be two positive numbers")
  if (max(days.before, days.after) > 15) 
    stop("window.days must be two numbers, both less than 15")
  
  # Select only the period of interest
  data$tide = data$tide[which((as.Date(data$tide$tide.time.utc + data$station$clock.offset*60*60)) <= (period[2])),]
  data$tide = data$tide[which((as.Date(data$tide$tide.time.utc + data$station$clock.offset*60*60)) >= (period[1])),]
  
  
  # Organise the variables --------------------------------------------------
  # This is not memory efficient, but it is convenient for shorter code
  clock.offset = data$station$clock.offset
  tide.time = data$tide$tide.time.utc
  longitude.dec = data$station$longitude
  latitude.dec = data$station$latitude
  ID.name = data$station$ID.name
  ID.numb = data$station$ID.numb
  state = data$station$ID.state
  ID.latlng = data$station$ID.latlng
  file = data$station$file
  tide.local.time.day = data$tide$tide.local.time.day
  tide.local.time.hour = data$tide$tide.local.time.hour
  tide.level = data$tide$tide.level
  tide.local.time = data$tide$tide.time.local 
  
  # Report some information to the console
  cat("Report for station", state, " - ", ID.numb, paste0("(",ID.name,")"),"\n")
  cat("Latitude","(",latitude.dec,")","\n") 
  cat("Longitude","(",longitude.dec,")","\n") 
  map.link = paste("https://maps.google.com/?q=",latitude.dec,",",longitude.dec,sep = "")
  cat("google maps link:", map.link)
  cat("\n")
  cat("Analysing between", as.character(period[1]), "and", as.character(period[2]))
  
  # Calculate moon information (this is done on the UTC)
  moon = moonAngle(as.numeric(tide.time), longitude.dec, latitude.dec, useRefraction = FALSE)
  moon.fraction = moon$illuminatedFraction
  moon.distance = moon$distance
  moon.azimuth = moon$azimuth
  moon.altitude = moon$altitude
  moon.phase = moon$phase
  moon.phase.number = floor(moon.phase)
  moon.phase.round = floor((moon.phase-floor(moon.phase)) * 4) / 3
  
  # Calculate sun information (this is done on the UTC)
  sun = sunAngle(as.numeric(tide.time), longitude.dec, latitude.dec, useRefraction = FALSE)
  sun.distance = sun$distance
  
  # Get moon phase information and convert to local time
  moon.phase.times = MoonPhase()
  new.moons = moon.phase.times$phase.new + clock.offset*60*60
  full.moons = moon.phase.times$phase.full + clock.offset*60*60
  
  moon = moonAngle(as.numeric(new.moons), longitude.dec, latitude.dec, useRefraction = FALSE)
  new.moons.distance = (moon$distance - 356815.2)/(406690.9 - 356815.2)
  
  moon = moonAngle(as.numeric(full.moons), longitude.dec, latitude.dec, useRefraction = FALSE)
  full.moons.distance = (moon$distance - 356815.2)/(406690.9 - 356815.2)
  
  
  # Analysis
  
  # Calculate some useful data
  
  tide.interval = diff(as.numeric(tide.time)) / (60*60)
  tide.range = diff(tide.level)
  
  tide.level.high = tide.level[which(tide.range < 0)]
  tide.level.low = tide.level[which(tide.range > 0)]
  tide.interval.ebb = tide.interval[which(tide.range > 0)]
  tide.interval.flow = tide.interval[which(tide.range < 0)]
  tide.range.ebb = tide.range[which(tide.range > 0)]
  tide.range.flow = tide.range[which(tide.range < 0)]
  
  tide.local.time.high = tide.local.time[which(tide.range < 0)]
  tide.local.time.low = tide.local.time[which(tide.range > 0)]
  
  mean.tide.interval.ebb = mean(tide.interval.ebb)
  mean.tide.range.ebb = mean(tide.range.ebb) 
  mean.tide.interval.flow = mean(tide.interval.flow)
  mean.tide.range.flow = mean(tide.range.flow)
  
  median.tide.interval.ebb = median(tide.interval.ebb)
  median.tide.range.ebb = median(tide.range.ebb) 
  median.tide.interval.flow = median(tide.interval.flow)
  median.tide.range.flow = median(tide.range.flow)
  
  # Calculate Andada information
  andada.day.start = as.numeric(format(andada[1],"%d"))
  andada.day.end = as.numeric(format(andada[2],"%d"))
  andada.month.start = as.numeric(format(andada[1],"%m"))
  andada.month.end = as.numeric(format(andada[2],"%m"))
  andada.year.diff = as.numeric(format(andada[2],"%Y")) - as.numeric(format(andada[1],"%Y"))
  
  
  # Do interpolation of tides
  func.interpolate.high = approxfun(x =  tide.local.time.high, y = tide.level.high, method="linear",  ties = mean) 
  func.interpolate.low = approxfun(x =  tide.local.time.low, y = tide.level.low, method="linear",  ties = mean) 
  
  tide.range.interpolated = func.interpolate.high(tide.local.time) - func.interpolate.low(tide.local.time)
  
  # look at the tide flow over the next period of hours specified by flow.hours
  
  tide.flow = tide.level
  
  for(i in 1 : length(tide.local.time)) {
    
    if (flow.hours < 0)
    {
      window.local.time.from = tide.local.time[i] + flow.hours*60*60
      window.local.time.to = tide.local.time[i] 
    }
    else
    {
      if (state == "PA" | state == "PA1" | state == "PA2"){
        window.local.time.from = tide.local.time[i] - (12*60*60)
      } else {
        window.local.time.from = tide.local.time[i]
      }
      window.local.time.to = tide.local.time[i] + flow.hours*60*60
    }
    
    
    tide.flow[i] = tide.level[i] - min(tide.level[which(tide.local.time >= window.local.time.from & tide.local.time <= window.local.time.to)])  
    
  }
  
  # A sequence of days
  days = seq(min(tide.local.time), max(tide.local.time), "day")
  middays = trunc(days, "days") + 60*60*12
  days = format(days,"%Y-%m-%d")
  
  tide.local.time.days = format(tide.local.time,"%Y-%m-%d")
  day.flow.max = NULL
  
  for(i in 1 : length(days)) {
    day.flow.max[i] = max(tide.flow[which(tide.local.time.days == days[i])])  
    
  }
  
  midday.moon =  moonAngle(as.numeric(middays), longitude.dec, latitude.dec, useRefraction = FALSE)
  midday.moon = midday.moon$illuminatedFraction
  
  new.moon.midday = middays[which(midday.moon < 0.02)]
  new.moon.day.flow.max = day.flow.max[which(midday.moon < 0.02)]
  full.moon.midday = middays[which(midday.moon > 0.98)]
  full.moon.day.flow.max =day.flow.max[which(midday.moon > 0.98)]
  
  spline.new.moon.flow = smooth.spline(as.numeric(new.moon.midday), new.moon.day.flow.max, spar = 0.5)
  spline.full.moon.flow = smooth.spline(as.numeric(full.moon.midday), full.moon.day.flow.max, spar = 0.5)
  
  # discard what is out of range of the dates of interest
  new.moons.analyse = new.moons[which(new.moons >= min(tide.local.time) & new.moons <= max(tide.local.time))]
  full.moons.analyse = full.moons[which(full.moons >= min(tide.local.time) & full.moons <= max(tide.local.time))]
  
  
  new.moon.flow.max = NA
  full.moon.flow.max = NA
  
  ## PLOT ##
  
  
  if(isTRUE(plot)) {
    
    #stations = c('10650','10620','20533','20520','10525','10572','30120','30149','30110','30114','30140','30225','30337','30407','30445','30443','30461','30540','30645','30685','30725','30810','30825','40118','40135','40140','40145','40240','40263','40250','50116','50156','50140','50145','50165','50170','50210','50225','60139', '60132','60130','60135','60220','60230', '60245', '60250', '60370')
    #states = c('AP','AP','PA','PA2','PA1','PA','MA','MA','MA','MA','MA','PI','CE','RN','RN','RN','RN','PB','PE','PE','AL','SE','SE','BA','BA','BA','BA','BA-ES','ES','ES','RJ','RJ','RJ','RJ','RJ','RJ','SP','SP','PR','PR','PR','PR','SC','SC','SC', 'SC','RS')
    

    
    # Time series plots
    # A sequence of months for all plots
    months = seq(min(tide.local.time), (max(tide.local.time) + 60*60*24), "month")
    
    
    
    year.start = as.numeric(format(tide.local.time[1],"%Y"))
    year.end = as.numeric(format(tide.local.time[length(tide.local.time)],"%Y"))
    year.start <<- year.start
    year.end <<- year.end
    
    if(isTRUE(svg)) {
      #svg(paste(ID.name, year.start, year.end,".svg", sep=""), width=10, height=7)
      setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path)))
      dir.create("export", showWarnings = FALSE)
      setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export"))
      name = paste0(plotNumber, ". ", state, "-", ID.numb, ".png")
      png(filename=name, width = 1150, height = 780)
    }
    
    months2 = c(format(as.Date.POSIXct(months), "%b"))
    
    latlong = paste0(round(data$station$latitude, 2), ", ", round(data$station$longitude, 2))
    
    # Tide range and time
    par(mar=c(3,3,5,2))
    #temp.xlim = c(ISOdate(2016,1,1), ISOdate(2019,1,1))
    plot(tide.local.time, tide.range.interpolated, type="l", xlab = "", ylab = "Tide range in metres", ylim = c(-1,7), col = "grey75", yaxt = "n")
    title(paste("Estação: ", ID.numb, "\nNome: ", toTitleCase(tolower(ID.name)), "\n"), adj=0,  cex.main=0.8, line=2)
    title(paste("Estado:", state, "\nLat/Long:", latlong, "\n"), adj=1, cex.main=0.8, line=2)
    axis(3, as.numeric(as.POSIXct.Date(as.Date.POSIXct(months)+15)), months2, cex.axis=0.7, col="white")
    axis(3, months, labels=rep(c(""), times=length(months)) , cex.axis=0.7)
    axis(2, c(0,1,2,3,4,5,6), las=1)
    abline(v = full.moons, col = "grey75", lty = 1)
    abline(v = new.moons, col = "grey75", lty = 3)
    rect(min(new.moons) , 6.25, max(new.moons), 7.25, col = "white", border = NA)
    points(x = new.moons, y = (0.5*new.moons.distance+6.5), pch = 16, cex = 1.5, col = "black")
    points(x = full.moons, y = (0.5*full.moons.distance+6.5), pch = 16, cex = 1.5, col = "gold")
    
    lines(middays, day.flow.max, col = "blue")
    
    # points(new.moon.midday, new.moon.day.flow.max, col = "grey75")
    # points(full.moon.midday, full.moon.day.flow.max)
    
    lines(predict(spline.new.moon.flow, as.numeric(middays)), col = "black", lwd = 2)
    lines(predict(spline.full.moon.flow, as.numeric(middays)), col = "gold", lwd = 2)
    
    abline(h = 0)
    
    # ANDADA BLOCK

    if(!is.null(state) & ! length(state) == 0)
    {
      if (state == "RJ" | state == "SP" | state == "PR" | state == "SC" | state == "RS")
      {
        andada = as.Date(c("2000-11-01", "2001-01-31"))
        andada.day.start = as.numeric(format(andada[1],"%d"))
        andada.day.end = as.numeric(format(andada[2],"%d"))
        andada.month.start = as.numeric(format(andada[1],"%m"))
        andada.month.end = as.numeric(format(andada[2],"%m"))
        andada.year.diff = as.numeric(format(andada[2],"%Y")) - as.numeric(format(andada[1],"%Y"))
      }
    }

    
    for(y in (year.start - 1):(year.end + 1)) {
      andada1 = ISOdatetime(y, andada.month.start, andada.day.start, 0, 0, 1, tz="UTC")
      andada2 = ISOdatetime(y + andada.year.diff, andada.month.end, andada.day.end, 23, 59, 59, tz="UTC")
      
      lines(c(andada1, andada1, andada2, andada2, andada1), c(0, 6, 6, 0, 0), col = "black", lwd = 2, lty = 2 )
    } # END OF ANDADA BLOCK
    
    

    
    
    ## Intersection points ##
    
    fullmoon <- predict(spline.full.moon.flow, as.numeric(middays))
    newmoon <- predict(spline.new.moon.flow, as.numeric(middays))
    
    x1 <- fullmoon$y
    x2 <- newmoon$y
    
    # Find points where x1 is above x2.
    above <- x1 > x2
    
    # Points always intersect when above=TRUE, then FALSE or reverse
    intersect.points <- which(diff(above) != 0)
    
    # Find the slopes for each line segment.
    x1.slopes <- x1[intersect.points+1] - x1[intersect.points]
    x2.slopes <- x2[intersect.points+1] - x2[intersect.points]
    
    # Find the intersection for each segment.
    x.points <- intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes-x2.slopes))
    y.points <- x1[intersect.points] + (x1.slopes*(x.points-intersect.points))
    
    # Joint points
    joint.points <- which(x1 == x2)
    x.points <- c(x.points, joint.points)
    y.points <- c(y.points, x1[joint.points])
    
    poi.dec <- x.points - floor(x.points)
    poi.x <- ((((fullmoon$x[x.points+1] - fullmoon$x[x.points])*poi.dec) + fullmoon$x[x.points]) + (((newmoon$x[x.points+1] - newmoon$x[x.points])*poi.dec) + newmoon$x[x.points])) / 2
    poi.y <- y.points
    
    # Plot points
    points(x=poi.x, y=poi.y, col="red", pch=19)
    
    poi.dates.dan <<- as.Date.POSIXct(poi.x)
    
    ## end of intersection ##
    
    
    if(isTRUE(svg)) {
      dev.off()
    }  
    
    options(warn=-1)
    
    ## Transition period CSV ##
    
    # Add station data to vector
    csvContent <- c(ID.numb, toTitleCase(tolower(ID.name)), state, paste0(round(latitude.dec, 2), ", ", round(longitude.dec, 2)))
    
    for (i in 1:length(poi.x)){
      csvContent <- c(csvContent, toString(format(as.Date.POSIXct(poi.x[i]))))
    }
    
    if (length(csvContent) < length(csv.data$headers))
    {
      diff = length(csv.data$headers) - length(csvContent)
      for (i in 1:diff){
        csvContent <- c(csvContent, "")
      }
    }
    
    csv.data <<- cbind(csv.data, data.frame(station=csvContent))
    
    ## End CSV
    
    
    ######## prep file #######
    
    
    
    
    f1 <- vector()
    d1 <- vector()
    m1 <- vector()
    
    for(i in 1:length(spline.full.moon.flow$x)){
      for(j in 1:length(moon.full.dates)){
        if (as.Date.POSIXct(spline.full.moon.flow$x[i]) == as.Date.POSIXct(moon.full.dates[j])){
          f1 <- c(f1, spline.full.moon.flow$x[i])
          d1 <- c(d1, "FM")
          m1 <- c(m1, spline.full.moon.flow$y[i])
        }
      }
    }
    
    f2 <- vector()
    d2 <- vector()
    m2 <- vector()
    
    for(i in 1:length(spline.new.moon.flow$x)){
      for(j in 1:length(moon.new.dates)){
        if (as.Date.POSIXct(spline.new.moon.flow$x[i]) == as.Date.POSIXct(moon.new.dates[j])){
          f2 <- c(f2, spline.new.moon.flow$x[i])
          d2 <- c(d2, "NM")
          m2 <- c(m2, spline.new.moon.flow$y[i])
        }
      }
    }
    
    f <- vector()
    d <- vector()
    m <- vector()
    
    if (as.Date.POSIXct(spline.new.moon.flow$x[1]) > as.Date.POSIXct(spline.full.moon.flow$x[1])){
      fm = TRUE
    } else {
      fm = FALSE
    }
    
    for (i in 1:min(c(length(f1), length(f2)))){
      if(fm){
        f <- c(f, f1[i], f2[i])
        d <- c(d, d1[i], d2[i])
        m <- c(m, m1[i], m2[i])
      } else {
        f <- c(f, f2[i], f1[i])
        d <- c(d, d2[i], d1[i])
        m <- c(m, m2[i], m1[i])
      }
    }
    
    daniel <- data.frame(as.Date.POSIXct(f),d,m)
    names(daniel) <- c("Date", "Phase", "MTA")

    ## Advice with poi ##
    
    advice <- rep("", length(daniel$Date))
    rule <- rep("", length(daniel$Date))
    
    # Go through each point of intersection (poi)
    for (i in 1:length(poi.dates.dan)){
      
      # Extract year of current poi
      poi.year = as.numeric(format(poi.dates.dan[i], "%Y"))
      
      # Go through each moon in daniel file
      for (j in 1:(length(daniel$Date)-1)){
        
        # Extract year and month from each date in daniel file
        moon.year = as.numeric(format(daniel$Date[j], "%Y"))
        moon.month = as.numeric(format(daniel$Date[j], "%m"))
        
        # RULE 2: Transition in last week of November or first three weeks December
        
        if (transitionPoints(poi.dates.dan[i]) == "Rule 2"){
          
          # if rule 2, advice has to be applied to the following Jan, Feb, Mar and Apr
          year.apply = poi.year + 1
          
          # Ensure the advice is only applied to Ja, Feb, March and Apr
          if (moon.year == year.apply & (moon.month == 1 | moon.month == 2 | moon.month == 3 | moon.month == 4)){
            
            # Calculate advice based on mta
            if (daniel$MTA[j] > daniel$MTA[j+1]){
              advice[j] = daniel$Phase[j]
            } else {
              advice[j] = daniel$Phase[j+1]
            }
            
            rule[j] = "Rule 2"
          }
        } # end of rule 2
        
        # Rule 3A: transition in last week of December or fisrt 3 weeks of January (1/1/1)
        
        if (transitionPoints(poi.dates.dan[i]) == "Rule 3A"){
          
          year.apply = poi.year
          
          # Ensure the advice is only applied to Ja, Feb, March and Apr
          if (moon.year == year.apply & (moon.month == 1 | moon.month == 2 | moon.month == 3 | moon.month == 4)){
            
            # Calculate advice based on mta
            if (daniel$MTA[j] > daniel$MTA[j+1]){
              advice[j] = daniel$Phase[j]
            } else {
              advice[j] = daniel$Phase[j+1]
            }
            
            rule[j] = "Rule 3A"
          }
        } # end of rule 3A
        
        
        # Rule 3B: transition in last week January and first 3 weeks of February (2/1/1)
        
        if (transitionPoints(poi.dates.dan[i]) == "Rule 3B"){
          
          year.apply = poi.year
          
          # Apply the advice "both moons" to Jan
          if (moon.year == year.apply & moon.month == 1){
            advice[j] = "Both"
            rule[j] = "Rule 3B"
          }
          
          # Apply mta advice to Feb, Mar and Apr
          else if (moon.year == year.apply & (moon.month == 2 | moon.month == 3 | moon.month == 4)){
            
            # Calculate advice based on mta
            if (daniel$MTA[j] > daniel$MTA[j+1]){
              advice[j] = daniel$Phase[j]
            } else {
              advice[j] = daniel$Phase[j+1]
            }
            
            rule[j] = "Rule 3B"
          }
        } # end of rule 3B
        
        
        # Rule 3C: Transition in last week of February or first 3 weeks of March (1/2/2)
        
        if (transitionPoints(poi.dates.dan[i]) == "Rule 3C"){
          
          year.apply = poi.year
          
          # Apply the advice "both moons" to Feb and Mar
          if (moon.year == year.apply & (moon.month == 2 | moon.month == 3)){
            advice[j] = "Both"
            rule[j] = "Rule 3C"
          }
          
          # Apply mta advice to Jan and Apr
          else if (moon.year == year.apply & (moon.month == 1 | moon.month == 4)){
            
            # Calculate advice based on mta
            if (daniel$MTA[j] > daniel$MTA[j+1]){
              advice[j] = daniel$Phase[j]
            } else {
              advice[j] = daniel$Phase[j+1]
            }
            
            rule[j] = "Rule 3C"
          }
        } # end of rule 3C
        
        
        # Rule 4: Transition in last week March or anytime in April (1/1/2)
        
        if (transitionPoints(poi.dates.dan[i]) == "Rule 4"){
          
          year.apply = poi.year
          
          # Apply the advice "both moons" to Mar
          if (moon.year == year.apply & moon.month == 3){
            advice[j] = "Both"
            rule[j] = "Rule 4"
          }
          
          # Apply mta advice to Jan, Feb and Apr
          else if (moon.year == year.apply & (moon.month == 1 | moon.month == 2 | moon.month == 4)){
            
            # Calculate advice based on mta
            if (daniel$MTA[j] > daniel$MTA[j+1]){
              advice[j] = daniel$Phase[j]
            } else {
              advice[j] = daniel$Phase[j+1]
            }
            
            rule[j] = "Rule 4"
          }
        } # end of rule 4
        
        
        # Apply rule 1 everywhere else
        else {
          
          year.apply = poi.year
          
          # Ensure the advice is only applied to Ja, Feb, March and Apr
          if (moon.year == year.apply){
            
            # Check that there is no advice
            if (advice[j] == ""){
              # Calculate advice based on mta
              if (daniel$MTA[j] > daniel$MTA[j+1]){
                advice[j] = daniel$Phase[j]
              } else {
                advice[j] = daniel$Phase[j+1]
              }
              rule[j] = "Rule 1"
            }
          }
          
        } # End rule 1
        
      } # End for loop daniel
      
    } # End for loop poi
    
    
    daniel <- cbind(daniel, advice1=advice)
    
    
    ## Get andada advice
    
    andada <- rep("", length(daniel$Date))
    
    for (i in 1:length(daniel$Date)){
      
      if (daniel$advice1[i] == "Both"){
        andada[i] = "Yes"
      } else if (daniel$advice1[i] == daniel$Phase[i]){
        andada[i] = "Yes"
      } else {
        andada[i] = "No"
      }
      
    }
    
    daniel <- cbind(daniel, andada1=andada)
    
    ##############################
    ###### SECOND POI CHECK ######
    ##############################
    
    if(state=="RJ" | state=="SP" | state=="PR" | state=="SC"){
       daniel <- cbind(daniel, advice2=south.pred(daniel, poi.dates.dan))
       cat("\n", "Running south.pred", "\n")
    } else {
       daniel <- cbind(daniel, advice2=north.pred(daniel, poi.dates.dan))
       cat("\n", "Running north.pred", "\n")
    }
    
    
    
    #daniel <- cbind(daniel, advice2=ba.pred(daniel, poi.dates.dan))
    
    
    ## Get andada advice
    
    andada <- rep("", length(daniel$Date))
    
    for (i in 1:length(daniel$Date)){
      
      if (daniel$advice2[i] == "Both"){
        andada[i] = "Yes"
      } else if (daniel$advice2[i] == daniel$Phase[i]){
        andada[i] = "Yes"
      } else {
        andada[i] = "No"
      }
      
    }
    
    
    ## End second POI check
    
    daniel <- cbind(daniel, andada2=andada, rule)
    
    temp.date <- vector()
    temp.phase <- vector()
    temp.maxta <- vector()
    temp.advice1 <- vector()
    temp.andada1 <- vector()
    temp.advice2 <- vector()
    temp.andada2 <- vector()
    temp.rule <- vector()
    
    
    ## Remove months outside november-april range
    for (i in 1:length(daniel$Phase)){
      tempM = format(daniel$Date[i], "%m")
      if (tempM == "11" | tempM == "12" |tempM == "01" |tempM == "02" |tempM == "03" |tempM == "04"){
        temp.date <- c(temp.date, daniel$Date[i])
        temp.phase <- c(temp.phase, daniel$Phase[i])
        temp.maxta <- c(temp.maxta, daniel$MTA[i])
        temp.advice1 <- c(temp.advice1, daniel$advice1[i])
        temp.andada1 <- c(temp.andada1, daniel$andada1[i])
        temp.advice2 <- c(temp.advice2, daniel$advice2[i])
        temp.andada2 <- c(temp.andada2, daniel$andada2[i])
        temp.rule <- c(temp.rule, daniel$rule[i])
      }
    }
    
    daniel <- data.frame(as.Date(as.POSIXct.Date(temp.date)), temp.phase, temp.maxta, temp.advice1, temp.andada1, temp.advice2, temp.andada2, temp.rule)
    names(daniel) <- c("Date", "Phase", "Max TA", "Advice 1", "Andada 1", "Advice 2", "Andada 2", "Rule")
    stations = c('10650', '20520', '10525', '30540', '30825', '40240', '40263', '60135', '60245', '10566')
    
    if(state == "BA-ES"){state = "ES"}
    
  if (n==1){  
    if (ID.numb %in% stations){
      cat("\n", blue("Dan"), "\n")
      csv.accuracy.content <<- c(csv.accuracy.content, state, "")
      daniel <- accuracyCheck(daniel$`Andada 1`, daniel, state, 1, "Andadas.csv")
      daniel <- accuracyCheck(daniel$`Andada 1`, daniel, state, 1, "Andadas3.csv")
      daniel <- accuracyCheck(daniel$`Andada 2`, daniel, state, 1, "Andadas.csv")
      daniel <- accuracyCheck(daniel$`Andada 2`, daniel, state, 1, "Andadas3.csv")
      daniel <- accuracyCheck(daniel$`Andada 1`, daniel, state, 2, "Andadas.csv")
      daniel <- accuracyCheck(daniel$`Andada 1`, daniel, state, 2, "Andadas3.csv")
      names(daniel) <- c("Date", "Phase", "Max TA", "Advice 1", "Andada 1", "Advice 2", "Andada 2", "Rule", "Accuracy 1 (obs1)", "Accuracy 1 (obs2)", "Accuracy 2 (obs1)", "Accuracy 2 (obs2)", "Observation", "Prediction")
    }
  
    
    daniel <<- daniel
    
    tempd <- data.frame(daniel[[1]], daniel[[2]], daniel[[3]], daniel[[6]], daniel[[7]], daniel[[8]], daniel[[11]], daniel[[13]])
    names(tempd) <- c("Date", "Phase", "Max TA", "Advice (algo)", "Andada (algo)", "Rule", "Correctness (algo vs obs)", "Observations")

    write.table(tempd,paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/daniel-",state,".csv"), sep=",", row.names = FALSE,  col.names=TRUE)
  }
    
    temp.date <- vector()
    temp.phase <- vector()
    temp.maxta <- vector()
    temp.advice2 <- vector()
    temp.andada2 <- vector()
    temp.rule <- vector()
    temp.obs1 <- vector()
    temp.obs2 <- vector()
    
    
    ## Remove months outside november-april range
    for (i in 1:length(daniel$Phase)){
      tempM = format(daniel$Date[i], "%m")
      if (tempM == "11" | tempM == "12" |tempM == "01" |tempM == "02" |tempM == "03" |tempM == "04"){
        temp.date <- c(temp.date, daniel$Date[i])
        temp.phase <- c(temp.phase, daniel$Phase[i])
        temp.maxta <- c(temp.maxta, daniel$`Max TA`[i])
        temp.advice2 <- c(temp.advice2, daniel$`Advice 2`[i])
        temp.andada2 <- c(temp.andada2, daniel$`Andada 2`[i])
        temp.rule <- c(temp.rule, daniel$Rule[i])
        temp.obs1 <- c(temp.obs1, daniel$Observation)
        temp.obs2 <- c(temp.obs2, daniel$Prediction)
      }
    }
    
    daniel2 <- data.frame(as.Date(as.POSIXct.Date(temp.date)), temp.phase, temp.maxta, temp.advice2, temp.andada2, temp.rule)
    names(daniel2) <- c("Date", "Phase", paste0(plotNumber, ". Max TA"), paste0(plotNumber, ". Advice"), paste0(plotNumber, ". Andada"), paste0(plotNumber, ". Rule"))
    
    if (n==2){
      # if (plotNumber==1){
      #   csv.all <<- data.frame(Date=daniel2[[1]], Phase=daniel2[[2]], "1. Max TA"=daniel2[[3]], "1. Advice"=daniel2[[4]], "1. Andada"=daniel2[[5]], "1. Rule"=daniel2[[6]], check.names = FALSE)
      # } else {
      #   if (length(daniel2[3]) < length(csv.all$Date)){
      #     temp3 = c(daniel2[[3]], rep("", length(csv.all$Date)-length(daniel2[[3]])))
      #     temp4 = c(daniel2[[4]], rep("", length(csv.all$Date)-length(daniel2[[3]])))
      #     temp5 = c(daniel2[[5]], rep("", length(csv.all$Date)-length(daniel2[[3]])))
      #     temp6 = c(daniel2[[6]], rep("", length(csv.all$Date)-length(daniel2[[3]])))
      #     csv.temp <- data.frame(temp3, temp4, temp5, temp6)
      #     names(csv.temp) <- c(paste0(plotNumber, ". Max TA"), paste0(plotNumber, ". Advice"), paste0(plotNumber, ". Andada"), paste0(plotNumber, ". Rule"))
      #     csv.all <<- cbind(csv.all, csv.temp)
      #   } else {
      #     csv.all <<- cbind(csv.all, daniel2[3], daniel2[4], daniel2[5], daniel2[6])
      #   }
      #   
      # }
      
      tempf <- rep(plotNumber, length(daniel2$Date))
      tempstation <- rep(ID.numb, length(daniel2$Date))
      tempstate <- rep(state, length(daniel2$Date))
      
      names(daniel2) <- c("Date", "Phase", "Max TA", "Advice", "Andada", "Rule")
      latlng = paste0(round(data$station$latitude, 2), ", ", round(data$station$longitude, 2))
      
      if (plotNumber==1){
        csv.all.vertical <- data.frame(tempf, tempstation, tempstate, daniel2, latlng)
      } else {
        csv.temp2 <- data.frame(tempf, tempstation, tempstate, daniel2, latlng)
        csv.all.vertical <- rbind(csv.all.vertical, csv.temp2)
      }
      
      csv.all.vertical <<- csv.all.vertical
    }
    
    daniel2 <<- daniel2
    
    
    write.table(daniel2,paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/", plotNumber, ". ", state, "-", ID.numb," (Dan2).csv"), sep=",", row.names = FALSE,  col.names=TRUE)
    
    # indexes = which(daniel2$Date >= as.Date(ISOdate(2016,1,1)) & daniel2$Date <= as.Date(ISOdate(2019,1,1)))
    # dt <- data.frame(Date=daniel2$Date[indexes], Phase=daniel2$Phase[indexes], "Max TA"=daniel2$`Max TA`[indexes], Advice=daniel2$Advice[indexes], Andada=daniel2$Andada[indexes], Rule=daniel2$Rule[indexes])
    #  
    # write.table(dt,paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/", plotNumber, ". ", state, "-", ID.numb," (Dan2).csv"), sep=",", row.names = FALSE,  col.names=TRUE)
    
    
    
  }
  
  
  return (1) 
  
} # end of function

