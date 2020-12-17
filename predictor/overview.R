library(xlsx)

overview = function(){
  
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/export"))
  
fileName = "Masterfile-R.xlsx"

overview <- data.frame()

if (file.exists(fileName)){
  file.remove(fileName)
}

years = c(2020,2021,2022,2023,2024)
#years = seq(year.start, year.end)

fileNumber = length(tides.list)
  
  
  for (i in 1:length(years)){
    
    months = c("Jan", "Feb", "Mar")
    
    overview.headers <- c("", years[i], "NM", "FM", "Both", "Total Stations", "", "% NM", "%FM", "% Both", "", "")
    
    temp.overview <- data.frame(overview.headers)
    
    for (j in 1:length(months)){
      
      count.nm = 0
      count.fm = 0
      count.both = 0
      
      file <- vector()
      station <- vector()
      state <- vector()
      month <- vector()
      year <- vector()
      rule <- vector()
      advice <- vector()
      latlng <- vector()
      rule <- vector()
      
      
      
      for (k in 1:fileNumber){
        
        indexes = which(format(csv.all.vertical$Date, "%b-%Y") == paste0(months[j], "-",years[i]) & csv.all.vertical$tempf == k)
        
        if (length(indexes) == 2){
          
          file <- c(file, csv.all.vertical$tempf[indexes[1]])
          station <- c(station, csv.all.vertical$tempstation[indexes[1]])
          state <- c(state, csv.all.vertical$tempstate[indexes[1]])
          month <- c(month, months[j])
          year <- c(year, years[i])
          latlng <- c(latlng, csv.all.vertical$latlng[indexes[1]])
          rule <- c(rule, csv.all.vertical$Rule[indexes[1]])
          
          if (csv.all.vertical$Advice[indexes[1]] == csv.all.vertical$Advice[indexes[2]]){
            
            if (csv.all.vertical$Advice[indexes[1]] == "NM"){
              count.nm = count.nm + 1
              advice <- c(advice, "NM")
            } else if (csv.all.vertical$Advice[indexes[1]] == "FM"){
              count.fm = count.fm + 1
              advice <- c(advice, "FM")
            } else {
              count.both = count.both + 1
              advice <- c(advice, "Both")
            }
            
          } else {
            count.both = count.both + 1
            advice <- c(advice, "Both")
          }
          
        } else {
          
          # if (length(indexes) == 3){
          #   
          #   cat("More than two moons in", months[j], years[i], "checking with", as.character(csv.all.vertical$Date[indexes[3]]), "and", as.character(csv.all.vertical$Date[indexes[3]+1]), "\n")
          #   
          #   file <- c(file, csv.all.vertical$tempf[indexes[3]])
          #   station <- c(station, csv.all.vertical$tempstation[indexes[3]])
          #   state <- c(state, csv.all.vertical$tempstate[indexes[3]])
          #   month <- c(month, months[j+1])
          #   year <- c(year, years[i])
          #   latlng <- c(latlng, csv.all.vertical$latlng[indexes[3]])
          #   rule <- c(rule, csv.all.vertical$Rule[indexes[3]])
          #   
          #   if (csv.all.vertical$Advice[indexes[3]] == csv.all.vertical$Advice[indexes[3]+1]){
          #     
          #     if (csv.all.vertical$Advice[indexes[3]] == "NM"){
          #       count.nm = count.nm + 1
          #       advice <- c(advice, "NM")
          #     } else if (csv.all.vertical$Advice[indexes[3]] == "FM"){
          #       count.fm = count.fm + 1
          #       advice <- c(advice, "FM")
          #     } else {
          #       count.both = count.both + 1
          #       advice <- c(advice, "Both")
          #     }
          #     
          #   } else {
          #     count.both = count.both + 1
          #     advice <- c(advice, "Both")
          #   }
          #   
          # }
          
        }
        
        

      }
      
      if (length(advice) != fileNumber){advice <- c(advice, rep("", (fileNumber-length(advice))))}
      
      sheetName = paste(months[j], years[i])
      temp.frame <- data.frame(file, station, state, month, year, advice, rule, latlng)
      write.xlsx(x=temp.frame, file=fileName, sheetName=sheetName, col.names=TRUE, row.names=FALSE, append=TRUE)
      total = count.fm + count.nm + count.both

      overview.content <- c("", months[j], count.nm, count.fm, count.both, total, "", round((count.nm*100)/total,1), round((count.fm*100)/total,1), round((count.both*100)/total,1), "", "")
      temp.overview <- cbind(temp.overview, overview.content)
      
    }
    
    overview <- rbind(overview, temp.overview)
    
  }

  write.xlsx(x=overview, file=fileName, sheetName="Overview", col.names=FALSE, row.names=FALSE, append=TRUE)
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  cat(green(paste("Overview File Produced (", format(Sys.time(), "%H:%M:%S"), ")\n")))

}

