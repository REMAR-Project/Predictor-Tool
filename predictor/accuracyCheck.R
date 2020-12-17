accuracyCheck = function(andada, prep.analysis.short, state, n=1, file){
  
  # Set current working directory
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
    if (state == "BA-ES"){state = "BA"}
  else if (state == "PA"){state="PA1"}

    # load csv file into data frame
    actual <<- read.csv(file.path(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/input files/"), file), stringsAsFactors=FALSE)
    
    
    data = actual[match(state, names(actual))]
    
    if (n==2){
        prep.analysis.short <- cbind(prep.analysis.short, data=data[[1]][1:length(prep.analysis.short$Date)])
        return(prep.analysis.short)
    }
    
    accuracy <- rep("", length(prep.analysis.short$Date))
    
    for (i in 1:length(prep.analysis.short$Date)){
      
      for (j in 1:length(actual$Phase)){
        
        date = paste0(strsplit(actual$ï..Date[j], "/")[[1]][3], "-", strsplit(actual$ï..Date[j], "/")[[1]][2], "-", strsplit(actual$ï..Date[j], "/")[[1]][1])
        
        if (prep.analysis.short$Date[i] == date & data[[1]][j] != ""){
          
          if (tolower(data[[1]][j]) == "precautionary" & tolower(andada[i]) != ""){
            accuracy[i] = "Correct (Prec)"
          } else if (tolower(andada[i]) == tolower(data[[1]][j])){
            accuracy[i] = "Correct"
          } else if (tolower(andada[i]) == ""){
            accuracy[i] = ""
          } else {
            accuracy[i] = "Wrong"
          }
          
        }
        
      }
      
    }
    
    
    
    prep.analysis.short <- cbind(prep.analysis.short, accuracy)
    
    
    wrong = 0
    right = 0
    
    for (i in 1:length(accuracy)){
      if (accuracy[i] == "Correct"){
        right = right + 1
      } else if (accuracy[i] == "Correct (Prec)"){
        right = right + 1
      } else if (accuracy[i] == "Wrong"){
        wrong = wrong + 1
      }
    }
    
    csv.accuracy.content <<- c(csv.accuracy.content, paste0(paste0("Correct: ", right, " (", round((100*right)/(right+wrong), 2), "%)"), paste0(" - Wrong: ", wrong, " (", round((100*wrong)/(right+wrong), 2), "%)")))
    
    cat(paste0(file, ": ", paste0("Correct: ", right, " (", round((100*right)/(right+wrong), 2), "%)"), " - ", paste0("Wrong: ", wrong, " (", round((100*wrong)/(right+wrong), 2), "%)\n")))
    
    return(prep.analysis.short)

  
}


accuracyREMAR = function(state, file){
  
  if (state=="BA-ES"){state="BA"}
  else if (state=="PA"){state="PA1"}
  
  if (file==1){file="accuracy-remar.csv"}
  if (file==2){file="accuracy-remar2.csv"}
  
  remar <- read.csv(file.path(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/input files/"), file), stringsAsFactors=FALSE)
  for(i in 1:length(remar[[1]])){
    if(remar[[1]][i] == state){
      csv.accuracy.content <<- c(csv.accuracy.content, paste0("Correct: ", remar[[2]][i], " (", remar[[3]][i], "%) - Wrong: ", remar[[4]][i], " (", remar[[5]][i], "%)"))
    }
  }
  
}
