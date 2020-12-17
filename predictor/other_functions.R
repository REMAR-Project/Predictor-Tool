
library(crayon)
library(rworldmap)
library(ggplot2)
library(jsonlite)
library(stringr)

# curve intersect

curve_intersect <- function(curve1, curve2, empirical=TRUE, domain=NULL) {
  if (!empirical & missing(domain)) {
    stop("'domain' must be provided with non-empirical curves")
  }
  
  if (!empirical & (length(domain) != 2 | !is.numeric(domain))) {
    stop("'domain' must be a two-value numeric vector, like c(0, 10)")
  }
  
  if (empirical) {
    # Approximate the functional form of both curves
    curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
    curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)
    
    # Calculate the intersection of curve 1 and curve 2 along the x-axis
    point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x),
                       c(min(curve1$x), max(curve1$x)))$root
    
    # Find where point_x is in curve 2
    point_y <- curve2_f(point_x)
  } else {
    # Calculate the intersection of curve 1 and curve 2 along the x-axis
    # within the given domain
    point_x <- uniroot(function(x) curve1(x) - curve2(x), domain)$root
    
    # Find where point_x is in curve 2
    point_y <- curve2(point_x)
  }
  
  return(list(x = point_x, y = point_y))
}





# Function to draw legend #

plotLegend = function(hours, svg = FALSE) {
  if(isTRUE(svg)) {
    #svg(paste(ID.name, year.start, year.end,".svg", sep=""), width=10, height=7)
    setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export"))
    png(filename="0. Legend.png", width = 705, height = 480)
  }
  if (hours < 0)
  {
    hours.text = paste("last", abs(hours))
  }
  else
  {
    hours.text = paste("next", hours)
  }
  plot(x=1, y=1, xaxt = "n", yaxt = "n", xlab="", ylab="", bty="n")
  legend("center", legend = c("Tide range", paste("Max tide flow over the", hours.text, "hours"), "Andada period", "New moon tide flow maximums",  "Full moon tide flow maximums", "Transition points"), cex = 1.4, col = c("grey75", "blue", "black","black", "goldenrod3", "red"), lty = c(1,1,2,1,1,0), lwd = c(2,2,3,3,3,1), pch = c(NA, NA, NA, NA, NA, 20))
  if(isTRUE(svg)) {
    dev.off()
  }  
}

# end of function



# Function to produce graphs 1 by 1, out put in R studio #

readOne = function(hours, svg=FALSE)
{
  csv.accuracy.headers <- c("", "", "Dan (Max TA)", "", "Dan 2", "", "Max (MTA)", "", "Max (Max TA)", "", "REMAR", "")
  csv.accuracy.content <<- vector()
  csv.accuracy <<- data.frame(csv.accuracy.headers)
  
  tempH <- vector ()
  for (i in 1:42){tempH<- c(tempH, paste0("T", i))}
  csvHeader <- c("Station", "Name", "State", tempH)
  csv.data <<- data.frame(headers=csvHeader)
  tides <<- CrabDateRead()
  CrabDateAnalyse(tides, svg=svg, flow.hours = hours)
  prep_analysis(tides=tides, svg=svg)
  
  csv.accuracy <<- cbind(csv.accuracy, csv.accuracy.content)
  #plotLegend(hours)
  library(crayon)
  cat(green("\nDone.\n"))
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
# end of function




# Read All Files in a Folder #
# and produce a png output of each graph #

readAll = function(hours, read=TRUE, n=1){
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  csv.accuracy.headers <- c("", "", "Dan (Max TA)", "", "Dan 2", "", "Max (MTA)", "", "Max (Max TA)", "", "REMAR", "")
  csv.accuracy.content <<- vector()
  csv.accuracy <<- data.frame(csv.accuracy.headers)
  
  tempH <- vector ()
  for (i in 1:42){tempH<- c(tempH, paste0("T", i))}
  csvHeader <- c("Station", "Name", "State", "Lat/Lng", tempH)
  csv.data <<- data.frame(headers = csvHeader)

  if(read==TRUE){
    
    # Code for bulk processing files in a directory ---------------------------
    file = file.choose() # use the file picker to select any of the input files
    # make a list of all the txt files in this directory
    list <- list.files(path = dirname(file), pattern="*.txt", full.names = TRUE)
    done = 0
    total = length(list)
    
    # if a file was selected, delete export directory
    unlink("export", recursive=TRUE)
    
    error <<- vector()
    tides.list <<- list()
    longs <- numeric()
    lats <- numeric()
    sites <- vector()
    
    for(i in 1:length(list)){
      result = tryCatch({
        data.tides = CrabDateRead(list[i])
        tides.list <<- c(tides.list, list(data.tides))
        done = done + 1
        
        longs <- c(longs, data.tides$station$longitude)
        lats <- c(lats, data.tides$station$latitude)
        sites <- c(sites, data.tides$station$ID.numb)
        
      }, error = function(e) {
        fileName = strsplit(list[i], "/")[[1]]
        cat(red(paste("\nError reading file", fileName[length(fileName)], "\n")))
        print(e)
        error <<- c(error, paste("\nError reading file", fileName[length(fileName)], "\n"))
      }, finally = {
        
      })
      
      # Print progress
      
      cat(green(paste0("Read: ", done, "/", total, "\n\n")))
    }
    sortData()
    list.backup <<- list
    longs.backup <<- longs
    lats.backup <<- lats
    sites.backup <<- sites
  }

  
  done = 0
  
  if(read==FALSE){
    unlink("export", recursive=TRUE)
  }
  
  for (i in 1:length(tides.list))
  {
    result = tryCatch({
      list <- list.backup
      total = length(list)
      longs <- longs.backup
      lats <- lats.backup
      sites <- sites.backup
      
      #remar.analysis2 <- remar(tides.list[[i]], i, n)
      
      csv.accuracy.content <<- vector()
      CrabDateAnalyse(tides.list[[i]], svg = TRUE, flow.hours = hours, plotNumber=i, n=n)
      
      # c1 = daniel$`Andada 2`
      # c2 = daniel$`Accuracy 2 (obs1)`
      # 
      # ##
      # 
      # remar.analysis2$dan2 = c1
      # remar.analysis2$dan2acc = c2
      # names(remar.analysis2) <- c("Date", "Phase", "MTA", "Difference", "Percent", "Probability", "Andada (algo)", "Acc (remar pred)", "REMAR Pred", "Acc (remar obs)", "REMAR Obs", "REMAR Accuracy", "Dan2 (Andada)", "Dan2 (Acc)")
      # 
      # write.table(remar.analysis2, paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/", i, ". ", tides.list[[i]]$station$ID.state, "-", tides.list[[i]]$station$ID.numb," (Remar Method) - SHORT.csv"), sep=",", row.names = FALSE,  col.names=TRUE)
      # 
      # ##
      # 
      # remar.analysis2 <<- remar.analysis2
      
      prep_analysis(fileNumber=i, tides=tides.list[[i]], svg=TRUE,n=n)
       if (n==1){
         csv.accuracy <<- cbind(csv.accuracy, csv.accuracy.content)
       }
      done = done + 1
      cat(green(paste0("\nAnalysis : ", done, "/", total, "\n\n")))
      
    }, error = function(e) {
      fileName = strsplit(list[i], "/")[[1]]
      cat(red(paste("\nError Analysing file", fileName[length(fileName)], "\n")))
      print(e)
      error <<- c(error, paste("\nError Analysing file", fileName[length(fileName)], "\n", str(e$call)))
    }, finally = {
      
    })
  }
  
  plotLegend(hours, svg=TRUE)
  #makeCSV(vertical=FALSE)
  makeCSV()
  numberOfDays(csv.data)
  
  # error file
  con <- file(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/errors.txt"))
  writeLines(paste(error, collapse=""), con)
  close.connection(con)
  
  makeMap(longs, lats, sites, svg=TRUE)
  makeMap(longs, lats, sites)
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  # Save accuracy.csv
  if (n==1){
    write.table(csv.accuracy, paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/", "accuracy.csv"), sep=",", row.names = FALSE,  col.names=FALSE)
  }
  
  # Save All(Dan2).csv, now called All-Sites.csv
  if (n==2){
    
    names(csv.all.vertical) <- c("File Number", "Station", "State", "Date", "Phase", "Max TA", "Advice", "Andada", "Rule", "LatLng")
    write.table(csv.all.vertical, paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/", "All-Sites.csv"), sep=",", row.names = FALSE,  col.names=TRUE)
    
  }
  cat(green(paste("Finished at", format(Sys.time(), "%H:%M:%S"), "\n")))
  options(warn=0)
  
}
# end of function



### Function to make csv file sorted from north to south ###

makeCSV = function(vertical=TRUE){
  

  if (vertical)
  {
    write.table(csv.data,paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/export-vertical.csv"), sep=",", row.names = FALSE,  col.names=FALSE)
  }
  else
  {
    csv.data2 <<- data.frame(matrix(ncol = 43, nrow = 0))
    
      for (j in 2:length(csv.data))
      {
          csv.data2 <<- rbind(csv.data2, csv.data[j]$station)
      }

    colnames(csv.data2) <<- csv.data[1]$headers
    write.table(csv.data2,paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/export-horizontal.csv"), sep=",", row.names = FALSE,  col.names=TRUE)
  }
  
}

### end of function

# Get difference in days between two transition points

numberOfDays = function(csv.data){
  days <- c(paste0("Station", "\t\t", "Difference in Days", "\n"))
  for (j in 2:length(csv.data))
  {
    diff <- numeric()
    for (i in 5:length(csv.data[j]$station))
    {
      if ((csv.data[j]$station[i+1] != "") & (!is.na(csv.data[j]$station[i+1])))
      {
        diff <- c(diff, as.numeric(difftime(as.Date(csv.data[j]$station[i+1]), as.Date(csv.data[j]$station[i]))))
      }
    }
    days <- c(days, paste(paste0(csv.data[j]$station[1], ' (', csv.data[j]$station[3], ')', ":\t")), paste0(diff, collapse=", "), "\n")
  }
  #cat(paste(days, collapse=""))
  con <- file(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export/difference-in-days.txt"))
  writeLines(paste(days, collapse=""), con)
  close.connection(con)
}


# Sort data from north to south using latitude

sortData = function(){
  
  count = 0
  while(1) {
    count_swaps = 0
    for (i in 1 : (length(tides.list) - 1 - count)) {
      if (tides.list[[i]]$station$latitude < tides.list[[i+1]]$station$latitude) {
        temp = tides.list[[i]]
        tides.list[[i]] <<- tides.list[[i+1]]
        tides.list[[i+1]] <<- temp
        count_swaps = count_swaps + 1
      }
    }
    count = count + 1
    if(count_swaps == 0) break
  }
  
}




makeMap = function(longs, lats, sites, svg=FALSE){
  
  if(isTRUE(svg)) {
    setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export"))
    png(filename="Map.png", width = 1200, height = 780)
    fontsize = 0.9
  } else {
    fontsize = 0.6
  }
  
  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = c(-85,5), ylim = c(-20,-10), asp = 1)
  points(longs, lats, col = "red", cex = 1, pch = 16)
  text(x= longs, y = lats, sites, pos = 4, cex = fontsize, col = "red")
  
  if(isTRUE(svg)) {
    dev.off()
  } 
  
}
