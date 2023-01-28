
  
original_fillEncounter = function(FLOOR_HIST_myEncounter,cernerStartMonth,cernerEndMonth) {
  
  cernerStartMonthS = as.character(cernerStartMonth);cernerEndMonthS = as.character(cernerEndMonth)
  
  # Case 1 : Pday - simplely fill the month with the 1st floor
  if(FLOOR_HIST_myEncounter$pdayFlag) {
    fillFloor = FLOOR_HIST_myEncounter$patientHistory[[1]]$floor
    for(ii in 2:(length(FLOOR_HIST_myEncounter$patientHistory)-1)) {
      FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor = fillFloor
      FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType = "P_f"			
      FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorTime = 0			
    }
    return(FLOOR_HIST_myEncounter)
  }
  
  
  # Case 2 : Either a valid admit or discharge exists but not both
  #           -> If discharge only, get the first floor logic then stick on at 1st
  #           -> If admit only, get the last floor and stick on at end.
  if(is.null(FLOOR_HIST_myEncounter$admDate) || FLOOR_HIST_myEncounter$admDate==0) {
    # We will insert an F_a (filled admit) in this case on cernerStartMonth
    for(ii in 1:length(FLOOR_HIST_myEncounter$patientHistory) ) {
      if(!is.null(FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor)) {
        fillFloor = FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor[1]
        FLOOR_HIST_myEncounter$patientHistory[[1]]$floor=c(FLOOR_HIST_myEncounter$patientHistory[[1]]$floor,fillFloor)
        FLOOR_HIST_myEncounter$patientHistory[[1]]$floorType=c(FLOOR_HIST_myEncounter$patientHistory[[1]]$floorType,"F_A")
        FLOOR_HIST_myEncounter$patientHistory[[1]]$floorTime=c(FLOOR_HIST_myEncounter$patientHistory[[1]]$floorTime,0)
        FLOOR_HIST_myEncounter$admDate = cernerStartMonthS
        break
      }
    }
  }
  
  if(is.null(FLOOR_HIST_myEncounter$disDate) || FLOOR_HIST_myEncounter$disDate==0) {
    # We will insert and F_d (filled disch) in this case on cernerEndMonth
    admDateOffset = as.numeric(as.Date(FLOOR_HIST_myEncounter$admDate) - cernerStartMonth) + 1
    # Just run the "getDailyMaxFloor" function up to end of the month and then fill it.
    fillFloor = NULL
    
    for(ii in admDateOffset:(length(FLOOR_HIST_myEncounter$patientHistory))) {
      FLOOR_HIST_myEncounter_ii = FLOOR_HIST_myEncounter$patientHistory[[ii]]
      if(!is.null(getDailyMaxFloor(FLOOR_HIST_myEncounter_ii))) {
        fillFloor = getDailyMaxFloor(FLOOR_HIST_myEncounter_ii)
      }
    }
    
    FLOOR_HIST_myEncounter$patientHistory[[length(FLOOR_HIST_myEncounter$patientHistory)]]$floor=c(FLOOR_HIST_myEncounter$patientHistory[[length(FLOOR_HIST_myEncounter$patientHistory)]]$floor,fillFloor)
    FLOOR_HIST_myEncounter$patientHistory[[length(FLOOR_HIST_myEncounter$patientHistory)]]$floorType=c(FLOOR_HIST_myEncounter$patientHistory[[length(FLOOR_HIST_myEncounter$patientHistory)]]$floorType,"F_D")
    FLOOR_HIST_myEncounter$patientHistory[[length(FLOOR_HIST_myEncounter$patientHistory)]]$floorTime=c(FLOOR_HIST_myEncounter$patientHistory[[length(FLOOR_HIST_myEncounter$patientHistory)]]$floorTime,0)
    FLOOR_HIST_myEncounter$disDate = cernerEndMonthS		
  }
  
  # Case 3 : No admit or discharge exists and not a pday, this implies just a mid month transfer
  if(!is.null(FLOOR_HIST_myEncounter$admDate) && !is.null(FLOOR_HIST_myEncounter$disDate)) {
    admDate = as.Date(FLOOR_HIST_myEncounter$admDate)
    disDate = as.Date(FLOOR_HIST_myEncounter$disDate)
    
    admOffset = as.numeric(admDate-cernerStartMonth)+1
    disOffset = as.numeric(disDate-cernerStartMonth)+1
    
    fillFloor=NULL
    
    for(ii in admOffset:disOffset) {
      
      FLOOR_HIST_myEncounter_ii = FLOOR_HIST_myEncounter$patientHistory[[ii]]
      
      # This needs to be filled with fillFloor
      if(is.null(FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor)) {
        FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor=fillFloor
        FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType="F"			
        FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorTime=0	
      } else {
        fillFloor = getDailyMaxFloor(FLOOR_HIST_myEncounter_ii)
      }
    }
    
    return(FLOOR_HIST_myEncounter)
  }
  
  
  # At end create a vector of # delimited floors
  return ("ERROR") # This should never happen
}

