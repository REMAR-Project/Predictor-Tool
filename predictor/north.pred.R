
##############################
###### SECOND POI CHECK ######
##############################

north.pred = function(daniel, poi.dates.dam){
  
  ## Advice with poi ##
  
  advice <- rep("", length(daniel$Date))
  rule <- rep("", length(daniel$Date))
  
  # Go through each point of intersection (poi)
  for (i in 1:length(poi.dates.dan)){
    
    # Extract year of current poi
    poi.year = as.numeric(format(poi.dates.dan[i], "%Y"))
    poi.month = as.numeric(format(poi.dates.dan[i], "%m"))
    poi.day = as.numeric(format(poi.dates.dan[i], "%d"))
    
    # Go through each moon in daniel file
    for (j in 1:(length(daniel$Date)-3)){
      
      # Extract year and month from each date in daniel file
      moon.year = as.numeric(format(daniel$Date[j], "%Y"))
      moon.month = as.numeric(format(daniel$Date[j], "%m"))
      moon.day = as.numeric(format(daniel$Date[j], "%d"))
      
      # RULE 2: Transition in last week of November or first three weeks December
      
      if (transitionPoints(poi.dates.dan[i]) == "Rule 2"){
        
        # if rule 2, advice has to be applied to the following Jan, Feb, Mar and Apr
        year.apply = poi.year + 1
        
        # Ensure the advice is only applied to Ja, Feb, March and Apr
        if (moon.year == year.apply & (moon.month == 1 | moon.month == 2 | moon.month == 3 | moon.month == 4)){
          
          # Calculate advice based on mta
          if (daniel$MTA[j+2] > daniel$MTA[j+3]){
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
          if (daniel$MTA[j+2] > daniel$MTA[j+3]){
            advice[j] = daniel$Phase[j]
          } else {
            advice[j] = daniel$Phase[j+1]
          }
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
          if (daniel$MTA[j+2] > daniel$MTA[j+3]){
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
        # if (moon.year == year.apply & (moon.month == 2)){
        #   if (poi.day < 16 & moon.day < 16){
        #     advice[j] = "Both"
        #     rule[j] = "Rule 3C"
        #   } else if (poi.day >= 16 & moon.day >= 16){
        #     advice[j] = "Both"
        #     rule[j] = "Rule 3C"
        #   } else {
        #     # Calculate advice based on mta
        #     if (daniel$MTA[j+2] > daniel$MTA[j+3]){
        #       advice[j] = daniel$Phase[j]
        #     } else {
        #       advice[j] = daniel$Phase[j+1]
        #     }
        #     rule[j] = "Rule 3C"
        #   }
        # }
        
        # Apply the advice "both moons" to Feb
        if (moon.year == year.apply & (moon.month == 2)){
          advice[j] = "Both"
          rule[j] = "Rule 3C"
        }
        
        # Apply mta advice to Jan and Apr
        else if (moon.year == year.apply & (moon.month == 1 | moon.month == 3 | moon.month == 4)){
          
          # Calculate advice based on mta
          if (daniel$MTA[j+2] > daniel$MTA[j+3]){
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
          if (daniel$MTA[j+2] > daniel$MTA[j+3]){
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
            if (daniel$MTA[j+2] > daniel$MTA[j+3]){
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
  
  
  return(advice)
  
}



## End second POI check