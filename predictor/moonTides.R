moonTides = function(){
  
  # get only high tides from data
  
  tempX <- vector()
  tempY <- vector()
  tides.high <- list()
  
  if (tides$tide$tide.level[1] > tides$tide$tide.level[2]){
    from = 1
  } else {
    from = 2
  }
  
  
  for (i in seq(from=from, to=length(tides$tide$tide.time.utc), by=2))
  {
    tempX <- c(tempX, tides$tide$tide.time.utc[i])
    tempY <- c(tempY, tides$tide$tide.level[i])
  }
  
  tides.high <- list(tempX, tempY)
  names(tides.high) <- c("Time", "Tide")
  
  plot(as.Date.POSIXct(tides.high$Time), tides.high$Tide, type="l", col="grey70")
  
  
  # get new moon tides
  
  moon.new.tides <- numeric()
  
  for (i in 1:length(moon.new.dates))
  {
    tempTide = 0
    for (j in 1:length(tides.high$Time))
    {
      if (as.Date.POSIXct(tides.high$Time[j]) == as.Date.POSIXct(moon.new.dates[i]))
      {
        if (tides.high$Tide[j] > tempTide)
        {
          tempTide <- tides.high$Tide[j]
        }
      }
    }
    moon.new.tides <- c(moon.new.tides, tempTide)
  }
  
  lines(as.Date.POSIXct(moon.new.dates), moon.new.tides, col="black", lwd=3)
  
  
  
  # get black moon tides
  
  moon.full.tides <- numeric()
  
  for (i in 1:length(moon.full.dates))
  {
    tempTide = 0
    for (j in 1:length(tides.high$Time))
    {
      if (as.Date.POSIXct(tides.high$Time[j]) == as.Date.POSIXct(moon.full.dates[i]))
      {
        if (tides.high$Tide[j] > tempTide)
        {
          tempTide <- tides.high$Tide[j]
        }
      }
    }
    moon.full.tides <- c(moon.full.tides, tempTide)
  }
  
  
  
  lines(as.Date.POSIXct(moon.full.dates), moon.full.tides, col="gold1", lwd=3)
  
  
  # ANDADA BLOCK
  
  year.start = as.numeric(format(as.Date.POSIXct(tides.high$Time[1]),"%Y"))
  year.end = as.numeric(format(as.Date.POSIXct(tides.high$Time[length(tides.high$Time)]),"%Y"))
  
  for (i in (year.start - 1):(year.end + 1))
  {
    rect(as.Date.POSIXct(ISOdate(i,1,1)), min(tides.high$Tide), as.Date.POSIXct(ISOdate(i,3,11)), max(tides.high$Tide), col=rgb(1,1,1,0), lty=2, lwd=2)
  }
  
  
  title(paste(tides$station$ID.numb, "-", tides$station$ID.name))
  
}