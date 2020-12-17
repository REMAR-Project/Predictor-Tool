prep_analysis = function(fileNumber=0, tides=NULL, svg=FALSE, n=1){
  
  if (n==2){
    cat(yellow("\nInfo: Prep Analysis has not run (n=2)\n"))
    return(1)
  }
  
  library(ggplot2)
  
  cat(yellow("\nStarting Prep_Analysis.R\n"))
  
  state = tides$station$ID.state
  
  # Convert UTC time to local time
  tides$tide$tide.time.utc = tides$tide$tide.time.utc + tides$station$clock.offset*60*60
  
  # Used to separate high and low tides
  tide.range = diff(tides$tide$tide.level)
  
  # Separate high and low tides (tide level)
  tide.level.high = tides$tide$tide.level[which(tide.range < 0)]
  tide.level.low = tides$tide$tide.level[which(tide.range > 0)]
  
  # Separate high and low tides (time points)
  tide.local.time.high = tides$tide$tide.time.utc[which(tide.range < 0)]
  tide.local.time.low = tides$tide$tide.time.utc[which(tide.range > 0)]
  
  # Do interpolation of tides
  func.interpolate.high = approxfun(x =  tide.local.time.high, y = tide.level.high, method="linear",  ties = mean) 
  func.interpolate.low = approxfun(x =  tide.local.time.low, y = tide.level.low, method="linear",  ties = mean) 
  
  # Better to plot
  tide.range.interpolated = func.interpolate.high(tides$tide$tide.time.utc) - func.interpolate.low(tides$tide$tide.time.utc)
  
  
  # Plot tidal amplitude over time (interpolated)
  # plot(tides$tide$tide.time.utc, tide.range.interpolated, type="l", col="grey30", ylab="Tidal amplitude", main=paste(tides$station$ID.numb, "-", tides$station$ID.name))
  
  # Get start and end year from data
  year.start = as.numeric(format(tides$tide$tide.time.utc[1],"%Y"))
  if (as.numeric(format(tides$tide$tide.time.utc[1],"%m")) != 1){year.start = year.start + 1}
  year.end = as.numeric(format(tides$tide$tide.time.utc[length(tides$tide$tide.time.utc)],"%Y"))
  if (year.end > 2024){year.end = 2024}
  
  # Only keep moons for months present in the data
  new.moons <- moon.new.dates[which(format(as.Date.POSIXct(moon.new.dates),"%Y") >= year.start & format(as.Date.POSIXct(moon.new.dates),"%Y") <= year.end)]
  
  full.moons <- moon.full.dates[which(format(as.Date.POSIXct(moon.full.dates),"%Y") >= year.start & format(as.Date.POSIXct(moon.full.dates),"%Y") <= year.end)]
  
  # Find out which date is the earliest
  # e.g if new moon = 14/01/06 and full moon = 29/01/06 then temp1 is new moon
  # this ensures dates are sorted ascendingly, also helps getting the current phase into the data frame
  
  if (full.moons[1] < new.moons[1]){
    temp1 <- full.moons
    temp2 <- new.moons
    phase <- c("FM", "NM")
  } else {
    temp1 <- new.moons
    temp2 <- full.moons
    phase <- c("NM", "FM")
  }
  
  moons <- vector()
  phases <- vector()
  
  for (i in 1:length(temp1)){
    moons <- c(moons, temp1[i])
    phases <- c(phases, phase[1])
    moons <- c(moons, temp2[i])
    phases <- c(phases, phase[2])
  }
  
  if (NA %in% moons){
    if (match(NA, moons) == length(moons))
    {
      moons <- moons[!is.na(moons)]
      phases <- head(phases, -1)
    }
  }
  
  # data frame that will be exported
  prep.analysis <- data.frame(as.Date.POSIXct(moons), phases)
  
  # Get all mtas, std dev, std error and max tidal amplitude for temp1
  mta1 <- numeric()
  stdev1 <- numeric()
  sterror1 <- numeric()
  maxta1 <- numeric()

  if (state == "PA" | state == "PA1" | state == "PA2"){
    minus = 1
  } else {
    minus = 0
  }
  plus = 0

  for (i in 1:length(temp1))
  {
    index.high <- which(as.Date(tide.local.time.high) >= (as.Date.POSIXct(temp1[i])-minus) & as.Date(tide.local.time.high) <= (as.Date.POSIXct(temp1[i])+plus))
    index.low <- which(as.Date(tide.local.time.low) >= (as.Date.POSIXct(temp1[i])-minus) & as.Date(tide.local.time.low) <= (as.Date.POSIXct(temp1[i])+plus))
    amplitudes <- numeric()
    
    if (length(index.high) != 0 & length(index.low) != 0){
      for (j in 1:min(c(length(index.high), length(index.low))))
      {
        amplitudes <- c(amplitudes, tide.level.high[index.high[j]] - tide.level.low[index.low[j]])
      }
      mta1 <- c(mta1, mean(amplitudes))
      stdev1 <- c(stdev1, sd(amplitudes))
      sterror1 <- c(sterror1, sd(amplitudes)/sqrt(length(amplitudes)))
      maxta1 <- c(maxta1, max(amplitudes))
    }
    else{
      mta1 <- c(mta1, NA)
      stdev1 <- c(stdev1, NA)
      sterror1 <- c(sterror1, NA)
      maxta1 <- c(maxta1, NA)
    }
  }

  
  # Get all mtas, std dev, std error and max tidal amplitude for temp2
  mta2 <- numeric()
  stdev2 <- numeric()
  sterror2 <- numeric()
  maxta2 <- numeric()
  
  for (i in 1:length(temp2))
  {
    index.high <- which(as.Date(tide.local.time.high) >= (as.Date.POSIXct(temp2[i])-minus) & as.Date(tide.local.time.high) <= (as.Date.POSIXct(temp2[i])+plus))
    index.low <- which(as.Date(tide.local.time.low) >= (as.Date.POSIXct(temp2[i])-minus) & as.Date(tide.local.time.low) <= (as.Date.POSIXct(temp2[i])+plus))
    amplitudes <- numeric()
    
    if (length(index.high) != 0 & length(index.low) != 0){
      for (j in 1:min(c(length(index.high), length(index.low))))
      {
        amplitudes <- c(amplitudes, tide.level.high[index.high[j]] - tide.level.low[index.low[j]])
      }
      mta2 <- c(mta2, mean(amplitudes))
      stdev2 <- c(stdev2, sd(amplitudes))
      sterror2 <- c(sterror2, sd(amplitudes)/sqrt(length(amplitudes)))
      maxta2 <- c(maxta2, max(amplitudes))
    } else {
      mta2 <- c(mta2, NA)
      stdev2 <- c(stdev2, NA)
      sterror2 <- c(sterror2, NA)
      maxta2 <- c(maxta2, NA)
    }
  }
  
  # Make one big array of both temp1 and temp2 mtas, std dev, std error and max ta
  mta <- numeric()
  stdev <- numeric()
  sterror <- numeric()
  maxta <- numeric()
  
  for (i in 1:length(temp1)){
    mta <- c(mta, mta1[i], mta2[i])
    stdev <- c(stdev, stdev1[i], stdev2[i])
    sterror <- c(sterror, sterror1[i], sterror2[i])
    maxta <- c(maxta, maxta1[i], maxta2[i])
  }
  
  # if (NA %in% mta & NA %in% stdev & NA %in% sterror & NA %in% maxta){
  #   if (match(NA, mta) == length(mta))
  #   {
  #     mta <- mta[!is.na(mta)]
  #     stdev <- stdev[!is.na(stdev)]
  #     sterror <- sterror[!is.na(sterror)]
  #     maxta <- maxta[!is.na(maxta)]
  #   }
  # }
  
  if (is.na(mta[length(mta)])){
    mta <- mta[1:(length(mta)-1)]
    stdev <- stdev[1:(length(stdev)-1)]
    sterror <- sterror[1:(length(sterror)-1)]
    maxta <- maxta[1:(length(maxta)-1)]
  }
  
  prep.analysis <- cbind(prep.analysis, round(mta,2), round(stdev, 2), round(sterror, 2), round(maxta,2))
  names(prep.analysis) <- c("Date", "Phase", "MTA", "Std Dev", "Std Error", "Max TA")
  
    
    
    
  
  # plot
  
  if(isTRUE(svg)) {
    setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path)))
    dir.create("export", showWarnings = FALSE)
    setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export"))
    name = paste0(fileNumber, ". ", substr(tides$station$file, 1, nchar(tides$station$file)-4), " (Prep Analysis).png")
    png(filename=name, width = 1150, height = 780)
  }
  
  par(mar=c(3,3,5,2)) #reset margins
  
  if (prep.analysis$Phase[1] == "FM"){
    temp1.type = "FM"
    temp1 = smooth.spline(spline(as.Date.POSIXct(full.moons), mta1, ties=mean))
    temp2 = smooth.spline(spline(as.Date.POSIXct(new.moons), mta2, ties=mean))
    plot(temp1, type="l", lwd=2, col="darkorange2", ylab="Mean Tidal Amplitude (m)", ylim=c(0,6.5), xaxt='n', yaxt='n')
    lines(temp2, lwd=2, col="black")
  } else {
    temp1.type = "NM"
    temp1 = smooth.spline(spline(as.Date.POSIXct(new.moons), mta1, ties=mean))
    temp2 = smooth.spline(spline(as.Date.POSIXct(full.moons), mta2, ties=mean))
    plot(temp1, type="l", lwd=2, col="black", ylab="Mean Tidal Amplitude (m)", ylim=c(0,6.5), xaxt='n', yaxt='n')
    lines(temp2, lwd=2, col="darkorange2")
  }
  
  # make temp1 and temp2 have the same length
  if (length(temp1$x) > length(temp2$x)){
    temp1$x = temp1$x[seq(1, length(temp2$x))]
    temp1$y = temp1$y[seq(1, length(temp2$y))]
  } else {
    temp2$x = temp2$x[seq(1, length(temp1$x))]
    temp2$y = temp2$y[seq(1, length(temp1$y))]
  }
  
  points(as.Date.POSIXct(full.moons), rep(6.5, length(full.moons)), col="darkorange2", pch=19, cex=1.2)
  points(as.Date.POSIXct(new.moons), rep(6.5, length(new.moons)), col="black", pch=19, cex=1.2)

  # Andada block
  
  state = tides$station$ID.state
  
  if (state == "RJ" | state == "SP" | state == "PR" | state == "SC" | state == "RS"){
    for (i in (year.start - 1):(year.end + 1)){
      rect(as.Date.POSIXct(ISOdate((i-1),11,1)), 0, as.Date.POSIXct(ISOdate(i,1,31)), 6, col=rgb(1,1,1,0), lty=2, lwd=2)
    } 
  }else {
      for (i in (year.start - 1):(year.end + 1)){
        rect(as.Date.POSIXct(ISOdate(i,1,1)), 0, as.Date.POSIXct(ISOdate(i,3,31)), 6, col=rgb(1,1,1,0), lty=2, lwd=2)
  }
  }
  
  months <- vector()
  months.labels <- vector()
  years <- numeric()
  for(i in (year.start):(year.end+1)){
    for (j in 1:12){
      months <- c(months, ISOdate(i, j, 1)) 
    }
    months.labels <- c(months.labels, c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
    years <- c(years, ISOdate(i,1,1))
  }
  
  for(i in seq(1, length(months), by=2)){
    rect(as.Date.POSIXct(months[i]), -1, as.Date.POSIXct(months[i+1]), 7, col=rgb(0.01,0.01,0.01,0.1), lty=0)
  }
  
 
  
  abline(v = as.Date.POSIXct(months), col = "grey75", lty = 3)
  axis(3, at=as.Date.POSIXct(months)+15, labels=months.labels, cex.axis=0.7, col="white")
  axis(3, at=as.Date.POSIXct(months), labels=rep(c(""), times=length(months)) , cex.axis=0.7)
  axis(2, seq(0,6), seq(0,6), las=1)
  axis(1, as.Date.POSIXct(years), format(as.Date.POSIXct(years),"%Y"))
  
  title(paste("Estação: ", tides$station$ID.numb, "\nNome: ", toTitleCase(tolower(tides$station$ID.name)), "\n"), adj=0,  cex.main=0.8, line=2)
  title(paste("Estado:", state, "\nLat/Long:", paste0(round(tides$station$latitude, 2), ", ", round(tides$station$longitude, 2)), "\n"), adj=1, cex.main=0.8, line=2)
  
  
  # Intersection points #
  
  poi.x <- numeric()
  poi.y <- numeric()
  
  for (i in seq(1, length(temp1$x), by=8)){
    result = tryCatch({
      t1 <- spline(temp1$x[seq(i,i+30)], temp1$y[seq(i,i+30)])
      t2 <- spline(temp2$x[seq(i,i+30)], temp2$y[seq(i,i+30)])
      poi.x <- c(poi.x, curve_intersect(t1, t2)$x)
      poi.y <- c(poi.y, curve_intersect(t1, t2)$y)
    }, error = function(e) {})
  }
  
  #points(poi.x, poi.y, pch=19, col="red")
  
  poi.dates <<- as.Date.POSIXct(as.POSIXct.Date(poi.x))
  
  poi.x <- poi.x[!duplicated(poi.dates)]
  poi.y <- poi.y[!duplicated(poi.dates)]
  poi.dates <<- poi.dates[!duplicated(poi.dates)]
  
  points(poi.x, poi.y, pch=19, col="red")
  
  if(isTRUE(svg)) {
    dev.off()
    #fileName = paste0(fileNumber, ". ", substr(tides$station$file, 1, nchar(tides$station$file)-4), " (Prep Analysis).csv")
    #write.table(prep.analysis,paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/", fileName), sep=",", row.names = FALSE,  col.names=TRUE)
  }
  
  
  
  
  
  ## Advice with poi ##
  
  advice <- rep("", length(prep.analysis$Date))
  rule <- rep("", length(prep.analysis$Date))
  
  # Go through each point of intersection (poi)
  for (i in 1:length(poi.dates)){
    
    # Extract year of current poi
    poi.year = as.numeric(format(poi.dates[i], "%Y"))
    
    # Go through each moon in prep.analysis file
    for (j in 1:(length(prep.analysis$Date)-1)){
      
      # Extract year and month from each date in prep.analysis file
      moon.year = as.numeric(format(prep.analysis$Date[j], "%Y"))
      moon.month = as.numeric(format(prep.analysis$Date[j], "%m"))
      
      # RULE 2: Transition in last week of November or first three weeks December
      
      if (transitionPoints(poi.dates[i]) == "Rule 2"){
        
        # if rule 2, advice has to be applied to the following Jan, Feb, Mar and Apr
        year.apply = poi.year + 1
        
        # Ensure the advice is only applied to Ja, Feb, March and Apr
        if (moon.year == year.apply & (moon.month == 1 | moon.month == 2 | moon.month == 3 | moon.month == 4)){
          
          # Calculate advice based on mta
          if (prep.analysis$MTA[j] > prep.analysis$MTA[j+1]){
            advice[j] = prep.analysis$Phase[j]
          } else {
            advice[j] = prep.analysis$Phase[j+1]
          }
          
          rule[j] = "Rule 2"
        }
      } # end of rule 2
      
      # Rule 3A: transition in last week of December or fisrt 3 weeks of January (1/1/1)
      
      if (transitionPoints(poi.dates[i]) == "Rule 3A"){
        
        year.apply = poi.year
        
        # Ensure the advice is only applied to Ja, Feb, March and Apr
        if (moon.year == year.apply & (moon.month == 1 | moon.month == 2 | moon.month == 3 | moon.month == 4)){
          
          # Calculate advice based on mta
          if (prep.analysis$MTA[j] > prep.analysis$MTA[j+1]){
            advice[j] = prep.analysis$Phase[j]
          } else {
            advice[j] = prep.analysis$Phase[j+1]
          }
          
          rule[j] = "Rule 3A"
        }
      } # end of rule 3A
      
      
      # Rule 3B: transition in last week January and first 3 weeks of February (2/1/1)
      
      if (transitionPoints(poi.dates[i]) == "Rule 3B"){
        
        year.apply = poi.year
        
        # Apply the advice "both moons" to Jan
        if (moon.year == year.apply & moon.month == 1){
          advice[j] = "Both"
          rule[j] = "Rule 3B"
        }
        
        # Apply mta advice to Feb, Mar and Apr
        else if (moon.year == year.apply & (moon.month == 2 | moon.month == 3 | moon.month == 4)){
          
          # Calculate advice based on mta
          if (prep.analysis$MTA[j] > prep.analysis$MTA[j+1]){
            advice[j] = prep.analysis$Phase[j]
          } else {
            advice[j] = prep.analysis$Phase[j+1]
          }
          
          rule[j] = "Rule 3B"
        }
      } # end of rule 3B
      
      
      # Rule 3C: Transition in last week of February or first 3 weeks of March (1/2/2)
      
      if (transitionPoints(poi.dates[i]) == "Rule 3C"){
        
        year.apply = poi.year
        
        # Apply the advice "both moons" to Feb and Mar
        if (moon.year == year.apply & (moon.month == 2 | moon.month == 3)){
          advice[j] = "Both"
          rule[j] = "Rule 3C"
        }
        
        # Apply mta advice to Jan and Apr
        else if (moon.year == year.apply & (moon.month == 1 | moon.month == 4)){
          
          # Calculate advice based on mta
          if (prep.analysis$MTA[j] > prep.analysis$MTA[j+1]){
            advice[j] = prep.analysis$Phase[j]
          } else {
            advice[j] = prep.analysis$Phase[j+1]
          }
          
          rule[j] = "Rule 3C"
        }
      } # end of rule 3C
      
      
      # Rule 4: Transition in last week March or anytime in April (1/1/2)
      
      if (transitionPoints(poi.dates[i]) == "Rule 4"){
        
        year.apply = poi.year
        
        # Apply the advice "both moons" to Mar
        if (moon.year == year.apply & moon.month == 3){
          advice[j] = "Both"
          rule[j] = "Rule 4"
        }
        
        # Apply mta advice to Jan, Feb and Apr
        else if (moon.year == year.apply & (moon.month == 1 | moon.month == 2 | moon.month == 4)){
          
          # Calculate advice based on mta
          if (prep.analysis$MTA[j] > prep.analysis$MTA[j+1]){
            advice[j] = prep.analysis$Phase[j]
          } else {
            advice[j] = prep.analysis$Phase[j+1]
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
            if (prep.analysis$MTA[j] > prep.analysis$MTA[j+1]){
              advice[j] = prep.analysis$Phase[j]
            } else {
              advice[j] = prep.analysis$Phase[j+1]
            }
            rule[j] = "Rule 1"
          }
        }
        
      } # End rule 1
      
    } # End for loop prep.analysis
    
  } # End for loop poi
  
  
  prep.analysis <- cbind(prep.analysis, advice)
  names(prep.analysis) <- c("Date", "Phase", "MTA", "Std Dev", "Std Error", "Max TA", "Advice Next Phase (Algo)")
  
  
  ## Get andada advice
  
  andada <- rep("", length(prep.analysis$Date))
  
  for (i in 1:length(prep.analysis$Date)){
    
    if (prep.analysis$`Advice Next Phase (Algo)`[i] == "Both"){
      andada[i] = "Yes"
    } else if (prep.analysis$`Advice Next Phase (Algo)`[i] == prep.analysis$Phase[i]){
      andada[i] = "Yes"
    } else {
      andada[i] = "No"
    }
      
  }
  
  prep.analysis <- cbind(prep.analysis, andada)
  names(prep.analysis) <- c("Date", "Phase", "MTA", "Std Dev", "Std Error", "Max TA", "Advice Next Phase (Algo)", "Andada Next Phase (Algo)")
  

  ##################################################
  ### Checking next month instead of next phase ####
  ##################################################
  
  if(state=="AP" | state=="MA" | state=="PI" | state=="PA" | state=="PA1" | state=="PA2"){
    prep.analysis <- cbind(prep.analysis, south.pred(prep.analysis, poi.dates))
  } else {
    prep.analysis <- cbind(prep.analysis, south.pred(prep.analysis, poi.dates))
  }

  names(prep.analysis) <- c("Date", "Phase", "MTA", "Std Dev", "Std Error", "Max TA", "Advice Next Phase (Algo)","Andada Next Phase (Algo)", "Advice Next Month (Algo)")
  
  
  ## Get andada advice
  
  andada <- rep("", length(prep.analysis$Date))
  
  for (i in 1:length(prep.analysis$Date)){
    
    if (prep.analysis$`Advice Next Month (Algo)`[i] == "Both"){
      andada[i] = "Yes"
    } else if (prep.analysis$`Advice Next Month (Algo)`[i] == prep.analysis$Phase[i]){
      andada[i] = "Yes"
    } else {
      andada[i] = "No"
    }
    
  }
  
  
  prep.analysis <- cbind(prep.analysis, andada, rule)
  names(prep.analysis) <- c("Date", "Phase", "MTA", "Std Dev", "Std Error", "Max TA", "Advice Next Phase (Algo)","Andada Next Phase (Algo)", "Advice Next Month (Algo)", "Andada Next Month (Algo)", "Rule")

  
  
  
  
  ### GGPLOT TEST
  
  x1 <- numeric()
  x2 <- numeric()
  y1 <- numeric()
  y2 <- numeric()
  color <- vector()
  border <- vector()
  
  t1 <- data.frame(x=temp1$x, y=temp1$y)
  yyy <- vector()
  mmm <- vector()
  ddd <- vector()
  former.year = ""
  former.month = ""
  
  for(i in 1:length(t1$x)){
    yyy <- c(yyy, format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%Y"))
    mmm <- c(mmm, format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%b"))
    ddd <- c(ddd, as.POSIXlt.Date(t1$x[i])$yday)
    if (format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%Y") == former.year & format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%m") == "01"){
      x1 = c(x1, 0)
      x2 = c(x2, 90)
      y1 = c(y1, 0)
      y2 = c(y2, 6)
      color <- c(color, NA)
      border <- c(border, "#000000")
    } else if (format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%Y") == former.year & format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%m") == former.month){
      x1 = c(x1, 0)
      x2 = c(x2, 0)
      y1 = c(y1, 0)
      y2 = c(y2, 0)
      color <- c(color, NA)
      border <- c(border, NA)
    } else {
      x1 = c(x1, (as.numeric(format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%m"))-1)*30)
      x2 = c(x2, (as.numeric(format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%m")) * 30))
      y1 = c(y1, -Inf)
      y2 = c(y2, Inf)
      
      if((as.numeric(format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%m")) %% 2) == 0){
        color <- c(color, "#FFFFFF")
      } else {
        color <- c(color, "#CECECE")
      }
      border <- c(border, NA)
    }
    former.year = format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%Y")
    former.month = format(as.Date.POSIXct(as.POSIXct.Date(t1$x[i])), "%m")
    }

  t1 <- cbind(t1, year=yyy, month=mmm, day=ddd, x1=x1, x2=x2, y1=y1, y2=y2, color=color, border=border)
  
  t2 <- data.frame(x=temp2$x, y=temp2$y)
  yyy <- vector()
  for(i in 1:length(t2$x)){yyy <- c(yyy, format(as.Date.POSIXct(as.POSIXct.Date(t2$x[i])), "%Y"))}
  t2 <- cbind(t2, year=yyy)
  mmm <- vector()
  for(i in 1:length(t2$x)){mmm <- c(mmm, format(as.Date.POSIXct(as.POSIXct.Date(t2$x[i])), "%b"))}
  t2 <- cbind(t2, month=mmm)
  ddd <- vector()
  for(i in 1:length(t2$x)){ddd <- c(ddd, as.POSIXlt.Date(t2$x[i])$yday)}
  t2 <- cbind(t2, day=ddd)
  
  
  
  poix<- NA[seq(1, length(t1$x))]
  poiy <- NA[seq(1, length(t1$x))]
  
  for(i in 1:length(poi.x)){
    py = format(as.Date.POSIXct(as.POSIXct.Date(poi.x[i])), "%Y")
    pm = format(as.Date.POSIXct(as.POSIXct.Date(poi.x[i])), "%m")
    
    for (j in 1:length(t1$x)){
      dy = format(as.Date.POSIXct(as.POSIXct.Date(t1$x[j])), "%Y")
      dm = format(as.Date.POSIXct(as.POSIXct.Date(t1$x[j])), "%m")
      if (py == dy & pm == dm){
        poix[j] = as.POSIXlt.Date(poi.x[i])$yday
        poiy[j] = poi.y[i]
      }
    }
  }
  
  t1 <- cbind(t1, poix=poix, poiy=poiy)
  
  if (temp1.type == "FM"){
    line1.color = "darkorange1"
    line2.color = "black"
  } else {
    line1.color = "black"
    line2.color = "darkorange1"
  }
  
  
  title = paste("Estação: ", tides$station$ID.numb, " ", "-", " ", "Nome: ", toTitleCase(tolower(tides$station$ID.name)), " ", "-", " ", "Estado:", state,  " ", "-", " ", "Lat/Long:", paste0(round(tides$station$latitude, 2), round(tides$station$longitude, 2)))

  if(isTRUE(svg)) {
    setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path)))
    dir.create("export", showWarnings = FALSE)
    setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export"))
    name = paste0(fileNumber, ". ", substr(tides$station$file, 1, nchar(tides$station$file)-4), " (multiples).png")
    png(filename=name, width = 1150, height = 780)
  }
  
  plot <- ggplot(t1, aes(day, y), col=c("Full", "New")) + 
    geom_rect(data=t1, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=color, color=border, alpha=0.3, linetype = "dashed") +
    geom_line(aes(day,y), size=1.2, color=line1.color) +  
    geom_line(aes(t2$day, t2$y), size=1.2, color=line2.color) +
    geom_point(aes(poix, poiy), size=2.5, color="red") +
    facet_wrap(~year) + 
    theme_bw() +
    xlab("") + ylab("MTA") + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    scale_x_continuous(limits=c(0,364), minor_breaks = seq(0 , 364, 30), breaks = seq(0, 364, 30)) +
    scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1)) +
    ggtitle(title) + 
    theme(plot.title = element_text(size=10, face="bold", hjust=0.5),  axis.title=element_text(size=9)) 
  

  options(warn=-1)
  print(plot)
  

  
  temp.date <- vector()
  temp.phase <- vector()
  temp.mta <- vector()
  temp.std <- vector()
  temp.ste <- vector()
  temp.maxta <- vector()
  temp.advice1 <- vector()
  temp.advice2 <- vector()
  temp.andada1 <- vector()
  temp.andada2 <- vector()
  temp.rule <- vector()
  
  
  ## Remove months outside november-april range
  for (i in 1:length(prep.analysis$Phase)){
    tempM = format(prep.analysis$Date[i], "%m")
    if (tempM == "11" | tempM == "12" |tempM == "01" |tempM == "02" |tempM == "03" |tempM == "04"){
      temp.date <- c(temp.date, prep.analysis$Date[i])
      temp.phase <- c(temp.phase, prep.analysis$Phase[i])
      temp.mta <- c(temp.mta, prep.analysis$MTA[i])
      temp.std <- c(temp.std, prep.analysis$`Std Dev`[i])
      temp.ste <- c(temp.ste, prep.analysis$`Std Error`[i])
      temp.maxta <- c(temp.maxta, prep.analysis$`Max TA`[i])
      temp.advice1 <- c(temp.advice1, prep.analysis$`Advice Next Phase (Algo)`[i])
      temp.advice2 <- c(temp.advice2, prep.analysis$`Advice Next Month (Algo)`[i])
      temp.andada1 <- c(temp.andada1, prep.analysis$`Andada Next Phase (Algo)`[i])
      temp.andada2 <- c(temp.andada2, prep.analysis$`Andada Next Month (Algo)`[i])
      temp.rule <- c(temp.rule, prep.analysis$Rule[i])
    }
  }
  
  prep.analysis.short <- data.frame(as.Date(as.POSIXct.Date(temp.date)), temp.phase, temp.mta, temp.std, temp.ste, temp.maxta, temp.advice1, temp.andada1, temp.advice2, temp.andada2, temp.rule)
  names(prep.analysis.short) <- c("Date", "Phase", "MTA", "Std Dev", "Std Error", "Max TA", "Advice Next Phase (Algo)","Andada Next Phase (Algo)", "Advice Next Month (Algo)", "Andada Next Month (Algo)", "Rule")
  
  stations = c('20520', '10525', '30540', '30825', '40240', '40263', '60135', '60245', '10566')
  
    if (tides$station$ID.numb %in% stations){
      prep.analysis.short <- accuracyCheck(prep.analysis.short$`Andada Next Phase (Algo)`, prep.analysis.short, state, 1, "Andadas.csv")
      prep.analysis.short <- accuracyCheck(prep.analysis.short$`Andada Next Phase (Algo)`, prep.analysis.short, state, 1, "Andadas3.csv")
      prep.analysis.short <- accuracyCheck(prep.analysis.short$`Andada Next Month (Algo)`, prep.analysis.short, state, 1, "Andadas.csv")
      prep.analysis.short <- accuracyCheck(prep.analysis.short$`Andada Next Month (Algo)`, prep.analysis.short, state, 1, "Andadas3.csv")
      prep.analysis.short <- accuracyCheck(prep.analysis.short$`Andada Next Month (Algo)`, prep.analysis.short, state, 2, "Andadas.csv")
      prep.analysis.short <- accuracyCheck(prep.analysis.short$`Andada Next Month (Algo)`, prep.analysis.short, state, 2, "Andadas3.csv")
      accuracyREMAR(state, 1)
      accuracyREMAR(state, 2)
      names(prep.analysis.short) <- c("Date", "Phase", "MTA", "Std Dev", "Std Error", "Max TA", "Advice 1","Andada 1", "Advice 2", "Andada 2", "Rule", "Accuracy 1 (obs)", "Accuracy 1 (obs2)", "Accuracy 2 (obs)", "Accuracy 2 (obs2)","Observation", "Prediction")
    }


  

  prep.analysis.short <<- prep.analysis.short

  
  prep.analysis <<- prep.analysis
    
    if(isTRUE(svg)) {
      dev.off()
      #fileName = paste0(fileNumber, ". ", substr(tides$station$file, 1, nchar(tides$station$file)-4), " (Prep Analysis - Short).csv")
      fileName = paste0("maxime-", state, ".csv")
      write.table(prep.analysis.short,paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/", fileName), sep=",", row.names = FALSE,  col.names=TRUE)
    }
  
  options(warn=0)

}




transitionPoints = function(poi){
  
  tempD = format(poi, "%d")
  tempM = format(poi, "%m")
  tempY = format(poi, "%Y")
  
  # RULE 1:  No transition between last week of November to April
  if(tempM != "12" & tempM != "01" & tempM != "02" & tempM != "03" & tempM != "04" & !(tempM=="11" & as.numeric(tempD) >= 24)){
    return("Rule 1")
  } 
  
  # RULE 2: Transition in last week of November or first three weeks December
  else if ((tempM == "12" & (as.numeric(tempD) < 24)) | (tempM == "11" & (as.numeric(tempD) >= 24))){
    return("Rule 2")
  }
  
  # Rule 3A: transition in last week of December or fisrt 3 weeks of January (1/1/1)
  else if ((tempM == "01" & as.numeric(tempD) < 24) | (tempM == "12" & as.numeric(tempD) >= 24)) {
    return("Rule 3A")
  }
  
  # Rule 3B: transition in last week January and first 3 weeks of February (2/1/1)
  else if ((tempM == "01" & as.numeric(tempD) >= 24) | (tempM == "02" & as.numeric(tempD) < 22)) {
    return("Rule 3B")
  }
  
  # Rule 3C: Transition in last week of February or first 3 weeks of March (1/2/2)
  else if ((tempM == "02" & as.numeric(tempD) >= 22) | (tempM == "03" & as.numeric(tempD) < 25)) {
    return("Rule 3C")
  }
  
  # Rule 4: Transition in last week March or anytime in April (1/1/2)
  else if ((tempM == "03" & as.numeric(tempD) >= 24) | (tempM == "04")) {
    return("Rule 4")
  }
}

transitionPointsSouth = function(poi){
  
  tempD = format(poi, "%d")
  tempM = format(poi, "%m")
  tempY = format(poi, "%Y")
  
  # RULE 1:  No transition between last week of November to April
  if(tempM != "10" & tempM != "11" & tempM != "12" & tempM != "01" & tempM != "02" & !(tempM=="9" & as.numeric(tempD) >= 24)){
    return("Rule 1")
  } 
  
  # RULE 2: Transition in last week of November or first three weeks December
  else if ((tempM == "10" & (as.numeric(tempD) < 24)) | (tempM == "9" & (as.numeric(tempD) >= 24))){
    return("Rule 2")
  }
  
  # Rule 3A: transition in last week of December or fisrt 3 weeks of January (1/1/1)
  else if ((tempM == "11" & as.numeric(tempD) < 24) | (tempM == "10" & as.numeric(tempD) >= 24)) {
    return("Rule 3A")
  }
  
  # Rule 3B: transition in last week January and first 3 weeks of February (2/1/1)
  else if ((tempM == "11" & as.numeric(tempD) >= 24) | (tempM == "12" & as.numeric(tempD) < 22)) {
    return("Rule 3B")
  }
  
  # Rule 3C: Transition in last week of February or first 3 weeks of March (1/2/2)
  else if ((tempM == "12" & as.numeric(tempD) >= 22) | (tempM == "01" & as.numeric(tempD) < 25)) {
    return("Rule 3C")
  }
  
  # Rule 4: Transition in last week March or anytime in April (1/1/2)
  else if ((tempM == "01" & as.numeric(tempD) >= 24) | (tempM == "02")) {
    return("Rule 4")
  }
}


