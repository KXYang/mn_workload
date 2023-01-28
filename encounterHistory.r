

createSparseHistory = function(CERNER_DAT_myEncounter,cernerStartMonth,cernerEndMonth,TIME_CUTOFF_L,TIME_CUTOFF_U) {
  
  pdayFlag = sum(CERNER_DAT_myEncounter$TRANS=="pday")==length(CERNER_DAT_myEncounter$TRANS) # Pday Only
  
  # Default Values
  disDate = 0; admDate = 0; transFlag = 0; admFloor = "START"; discFloor = "END"
  
  persisted = 0 # If we persist any type of record on this set to 1
  
  dateRange = seq(from=cernerStartMonth,to=cernerEndMonth,by=1)
  
  patientHistory = vector(mode="list",length=length(dateRange))
  names(patientHistory) = dateRange
  
  # On each date for this encounter will have floor, time and type in parallel	
  for(ii in 1:length(patientHistory)) {
    patientHistory[[ii]] = vector(mode="list",length=4)
    names(patientHistory[[ii]]) = c("floor","floorType","floorTime", "setback")
  }
  
  # No adjustments need to be applied to a pure pday record
  if(pdayFlag == TRUE) {
    
    pdayFloor = CERNER_DAT_myEncounter$NURSE_UNIT[1] # Take the first one only
    
    cernerStartMonthS = as.character(cernerStartMonth);cernerEndMonthS = as.character(cernerEndMonth)
    
    last <- tail(CERNER_DAT_myEncounter, n=1)
    
    same <- trim(substr(last$INFO, 23, 25)) == trim(substr(last$INFO, 4, 6))
    
    if(same == FALSE){
      patientHistory[[cernerStartMonthS]]$floor = c(patientHistory[[cernerStartMonthS]]$floor, pdayFloor)
      patientHistory[[cernerStartMonthS]]$floorType = c(patientHistory[[cernerStartMonthS]]$floorType, "P")
      patientHistory[[cernerStartMonthS]]$floorTime = c(patientHistory[[cernerStartMonthS]]$floorTime, 0)	
      
      patientHistory[[cernerEndMonthS]]$floor = c(patientHistory[[cernerEndMonthS]]$floor, pdayFloor)
      patientHistory[[cernerEndMonthS]]$floorType = c(patientHistory[[cernerEndMonthS]]$floorType, "P")
      patientHistory[[cernerEndMonthS]]$floorTime = c(patientHistory[[cernerEndMonthS]]$floorTime, 0)
      
      persisted = 1
    } 
    
    if(same == TRUE) {
      pdayFlag <- FALSE
      
      patientHistory[[cernerStartMonthS]]$floor = c(patientHistory[[cernerStartMonthS]]$floor, pdayFloor)
      patientHistory[[cernerStartMonthS]]$floorType = c(patientHistory[[cernerStartMonthS]]$floorType, "P")
      patientHistory[[cernerStartMonthS]]$floorTime = c(patientHistory[[cernerStartMonthS]]$floorTime, 0)	
      
      disDate <- as.Date(trim(substr(CERNER_DAT_myEncounter$INFO,20,28)),"%d-%b-%y")
      recTime <- trim(substr(CERNER_DAT_myEncounter$INFO,30,34))
      recTime = as.numeric(gsub(':','',recTime))
      
      if (isTRUE(recTime <= TIME_CUTOFF_L)) {
        disDate <- disDate-1
        recTime <- 2359
        patientHistory[[as.character(disDate)]]$setback <- 1
      }
      
      disDateS <- as.character(disDate)
      patientHistory[[disDateS]]$floor = c(patientHistory[[disDateS]]$floor,pdayFloor)
      patientHistory[[disDateS]]$floorTime = c(patientHistory[[disDateS]]$floorTime,recTime)
      patientHistory[[disDateS]]$floorType = c(patientHistory[[disDateS]]$floorType,"D")
      
      persisted = 1
    }
    
  } else {
    
    # For non pday records parse and apply any fixes to the date and time where necessary
    tmp = CERNER_DAT_myEncounter[CERNER_DAT_myEncounter$TRANS=="adm",]
    if(nrow(tmp) > 0) {
      
      admDate = tmp$DATE
      recTime = as.numeric(tmp$TIME)
      
      if(isTRUE(recTime <= TIME_CUTOFF_L)) { 
        admDate = admDate-1
        recTime = 2359
        patientHistory[[as.character(admDate)]]$setback <- 1
      }
      
      if(admDate >= cernerStartMonth) {
        admDate = as.character(admDate)
        patientHistory[[admDate]]$floor = c(patientHistory[[admDate]]$floor,as.character(tmp$NURSE_UNIT))
        patientHistory[[admDate]]$floorTime = c(patientHistory[[admDate]]$floorTime,recTime)
        patientHistory[[admDate]]$floorType = c(patientHistory[[admDate]]$floorType,"A")
        persisted=1
      } else { # This case represents a record on first of month prior to 4am
        admDate = NULL
      }
    }
    
    tmp = CERNER_DAT_myEncounter[CERNER_DAT_myEncounter$TRANS=="dec",]		
    if(nrow(tmp) > 0) {
      
      discFloor = as.character(tmp$NURSE_UNIT)
      disDate = tmp$DATE
      recTime = as.numeric(tmp$TIME)			
      
      if(isTRUE(recTime <= TIME_CUTOFF_L)) { 
        disDate = disDate-1
        recTime = 2359
        patientHistory[[as.character(disDate)]]$setback <- 1
      }
      
      if(disDate >= cernerStartMonth) {
        disDate = as.character(disDate)
        patientHistory[[disDate]]$floor = c(patientHistory[[disDate]]$floor,as.character(tmp$NURSE_UNIT))
        patientHistory[[disDate]]$floorTime = c(patientHistory[[disDate]]$floorTime,recTime)
        patientHistory[[disDate]]$floorType = c(patientHistory[[disDate]]$floorType,"D")
        persisted=1
      } else {
        disDate = NULL
      }
    }
    
    tmp = CERNER_DAT_myEncounter[CERNER_DAT_myEncounter$TRANS=="dis",]		
    if(nrow(tmp) > 0) {
      
      disFloor = as.character(tmp$NURSE_UNIT)
      disDate = tmp$DATE
      recTime = as.numeric(tmp$TIME)			
      
      if(isTRUE(recTime <= TIME_CUTOFF_L)) { 
        disDate = disDate-1
        recTime = 2359
        patientHistory[[as.character(disDate)]]$setback <- 1
      }
      
      if(disDate >= cernerStartMonth) {
        persisted=1
        disDate = as.character(disDate)
        patientHistory[[disDate]]$floor = c(patientHistory[[disDate]]$floor,as.character(tmp$NURSE_UNIT))
        patientHistory[[disDate]]$floorTime = c(patientHistory[[disDate]]$floorTime,recTime)
        patientHistory[[disDate]]$floorType = c(patientHistory[[disDate]]$floorType,"D")
      } else {
        # Update 20180118
        disDate = NULL
      }
    }
    
    
    # Adjust discharge time if the last row is 'pday' record
    last <- tail(CERNER_DAT_myEncounter,1)
    same <- trim(substr(last$INFO, 23, 25)) == trim(substr(last$INFO, 4, 6))
    if (isTRUE(as.numeric(nrow(CERNER_DAT_myEncounter)) > 1) && last$TRANS == 'pday' && same == TRUE){

      tmpDate <- trim(substr(last$INFO, 20, 28))
      disDate = as.Date(tmpDate,"%d-%b-%y")
      
      recTime = trim(substr(last$INFO, 30, 34))
      recTime = as.numeric(gsub(':','',recTime))
      
      if(isTRUE(recTime <= TIME_CUTOFF_L)) { 
        disDate = disDate-1
        recTime = 2359
        patientHistory[[as.character(disDate)]]$setback <- 1
      }
      
      if(isTRUE(disDate >= cernerStartMonth)) {
        persisted=1
        disDate = as.character(disDate)
        patientHistory[[disDate]]$floor = c(patientHistory[[disDate]]$floor,as.character(last$NURSE_UNIT))
        patientHistory[[disDate]]$floorTime = c(patientHistory[[disDate]]$floorTime,recTime)
        patientHistory[[disDate]]$floorType = c(patientHistory[[disDate]]$floorType,"D")
      } else {
        disDate = NULL
      }
      
    }
    
    
    # Parse each record delimited by ->- record workload to both records on that day?
    tmp = CERNER_DAT_myEncounter[CERNER_DAT_myEncounter$TRANS=="trano",]
    if(nrow(tmp) > 0) {
      # There may be multiple transfers 
      for(ii in 1:nrow(tmp)) {
        
        transFlag = 1
        
        # Parse the from and to floor
        floorSplit = strsplit(as.character(tmp[ii,'INFO']),TRANO_FLOOR_DELIM)
        floorFrom = trim(floorSplit[[1]][1])
        floorTo = trim(floorSplit[[1]][2])
        
        transDate = tmp[ii,'DATE']
        transTime = as.numeric(tmp[ii,'TIME'])
        ValidTransTo = 1
        
        if(isTRUE(transTime <= TIME_CUTOFF_L)){ 
          transDate = transDate-1
          transTime = 2359
          patientHistory[[as.character(transDate)]]$setback <- 1
        } else if(transTime > TIME_CUTOFF_L & transTime < TIME_CUTOFF_U) {
          ValidTransTo = 0
        }
        
        if(transDate >= cernerStartMonth) {
          transDate = as.character(transDate)
          
          
          # V1.10 : Add the from always too - we just disallow mwkd
          # Add the From if not in window

          patientHistory[[transDate]]$floor = c(patientHistory[[transDate]]$floor,floorFrom)
          patientHistory[[transDate]]$floorTime = c(patientHistory[[transDate]]$floorTime,transTime)
          patientHistory[[transDate]]$floorType = c(patientHistory[[transDate]]$floorType,"T_f")
          
          # Always add the to
          patientHistory[[transDate]]$floor = c(patientHistory[[transDate]]$floor,floorTo)
          patientHistory[[transDate]]$floorTime = c(patientHistory[[transDate]]$floorTime,transTime)
          patientHistory[[transDate]]$floorType = c(patientHistory[[transDate]]$floorType,"T_t")
          
          persisted=1
        } else {
          transFlag = 0
        }
      }
    }	
  }
  
  if(persisted) {
    # Results represented as list
    return(list(pdayFlag=pdayFlag,
                transFlag=transFlag,
                disDate=disDate,
                admDate=admDate,
                patientHistory=patientHistory))
  } else {
    return (NULL)
  }
}

fillEncounter = function(FLOOR_HIST_myEncounter,cernerStartMonth,cernerEndMonth) {
  
  cernerStartMonthS = as.character(cernerStartMonth);cernerEndMonthS = as.character(cernerEndMonth)
  
  # Case 1 : Pday simple fill the month with the 1st floor
  if(!is.null(FLOOR_HIST_myEncounter$pdayFlag) && FLOOR_HIST_myEncounter$pdayFlag == TRUE) {
    fillFloor = FLOOR_HIST_myEncounter$patientHistory[[1]]$floor
    for(ii in 2:(length(FLOOR_HIST_myEncounter$patientHistory)-1)) {
      FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor = fillFloor
      FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType = "P_f"			
      FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorTime = 0			
    }
    return(FLOOR_HIST_myEncounter)
  }
  
  
  # Case 2 : Either a valid admit or discharge exists but not both
  #           -> If discharge only, get first floor logic then stick on at 1st
  #           -> If admit only, get last floor and stick on at end.
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
    # Just run the max floor algo. up to end of month and then fill it.
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
    
    for(ii in 1:length(FLOOR_HIST_myEncounter$patientHistory)) {
      if (isTRUE(FLOOR_HIST_myEncounter$patientHistory[[ii]]$setback == 1) && 
          isTRUE(match('T_f',FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType) > 0)){
       
         FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor <- FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor[1]
      
        }
    }
    
    for(ii in 1:length(FLOOR_HIST_myEncounter$patientHistory)) {
      if (isTRUE(FLOOR_HIST_myEncounter$patientHistory[[ii]]$setback == 1) && 
          isTRUE(length(FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType) == 1) &&
          isTRUE(FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType == 'A')){
        
        FLOOR_HIST_myEncounter$patientHistory[[ii]] = vector(mode="list",length=4)
        names(FLOOR_HIST_myEncounter$patientHistory[[ii]]) = c("floor","floorType","floorTime", "setback")
        
      }
    }
    
    return(FLOOR_HIST_myEncounter)
  }
  
  # At end create a vector of # delimited floors
  return ("ERROR") # This should never happen
}


##### New Function - Updated Jan 19, 2018 ------------------------------------
checkDuration = function(FILLED_FLOOR_HIST_myEncounter) {
  
  mtrans_1 <- data.frame(c("T_f", "T_t", "T_f", "T_t"))
  colnames(mtrans_1) <- 'type'
  mtrans_2 <- data.frame(c("A", "T_f", "T_t"))
  colnames(mtrans_2) <- 'type'
  
  for(ii in 1:length(FILLED_FLOOR_HIST_myEncounter$patientHistory)) {
    
    if(!is.null(FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType) &&
       isTRUE(length(FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType) >= 3) &&
       isTRUE(length(FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor) > 1)) {
      
      type <- data.frame(FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorType)
      colnames(type) <- 'type'
      
      # SCENARIO 1
      if (nrow(type) == 4 && identical(type, mtrans_1)){
        time <- data.frame(FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorTime)
        colnames(time) <- 'time'
        check <- data.frame(type, time)
        
        for(tt in 1:nrow(time)){
          
          if (isTRUE(nchar(time[tt,]) < 4)){
            check$time[tt] <- paste(paste(rep(0,4-nchar(time[tt,])),collapse = ""), time[tt,], sep = "")
          }
          check$time[tt] <- paste(substr(check$time[tt],1,2), ":", substr(check$time[tt],3,4), ":00",collapse = "")
        }
        
        require(chron)
        check$time <- gsub(" ", "", check$time) 
        check$time <- times(check$time)
        timestamp <- check$time[3] - check$time[2]
        
        if (isTRUE(60 * hours(timestamp) + minutes(timestamp) < 60)) {
          FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor = FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor[c(1,4)]
        }
      }
      
      # SCENARIO 2
      if (nrow(type) == 3 && identical(type, mtrans_2)){
        time <- data.frame(FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floorTime)
        colnames(time) <- 'time'
        check <- data.frame(type, time)
        
        for(tt in 1:nrow(time)){
          
          if (isTRUE(nchar(time[tt,]) < 4)){
            check$time[tt] <- paste(paste(rep(0,4-nchar(time[tt,])),collapse = ""), time[tt,], sep = "")
          } 
          check$time[tt] <- paste(substr(check$time[tt],1,2), ":", substr(check$time[tt],3,4), ":00",collapse = "")
          
        }
        
        require(chron)
        check$time <- gsub(" ", "", check$time) 
        check$time <- times(check$time)
        timestamp <- check$time[2] - check$time[1]
        
        if (isTRUE(60 * hours(timestamp) + minutes(timestamp) < 60)){
          FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor = FILLED_FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor[3]
          
        }
      }
    }
  }
  return(FILLED_FLOOR_HIST_myEncounter)
}
##### ---------------------------------------------------------

condenseFilledEncounter = function(FLOOR_HIST_myEncounter,cernerStartMonth,cernerEndMonth) {
  
  floorHistory = vector(mode="list",length=length(FLOOR_HIST_myEncounter$patientHistory))
  names(floorHistory) = names(FLOOR_HIST_myEncounter$patientHistory)
  
  for(ii in 1:length(FLOOR_HIST_myEncounter$patientHistory)) {
    
    #Updated Jan 18, 2018

    if(!is.null(FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor)) {
      for(jj in 1: length(FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor)) {
        if (FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor[[jj]] == '11LM') {
          FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor[[jj]] <- '11S'
        }
      }
      uniqueFloor = unique(FLOOR_HIST_myEncounter$patientHistory[[ii]]$floor)
      if(!is.null(uniqueFloor)) {
        floorHistory[[ii]] = uniqueFloor
      }
    }
  }
  return(floorHistory)
}

getDailyMaxFloor = function(FLOOR_HIST_myEncounter_ii) {
	# We take the max here incase more than 1 record exists at max , i.e. T_f, T_t case
	# Based on insertion this will always take the T_t
	if(is.null(FLOOR_HIST_myEncounter_ii$floorTime)) { return (NULL) }
	
	maxIndex = max(which(FLOOR_HIST_myEncounter_ii$floorTime == max(FLOOR_HIST_myEncounter_ii$floorTime)))
	return(FLOOR_HIST_myEncounter_ii$floor[maxIndex])
}

getDailyMinFloor = function(FLOOR_HIST_myEncounter_ii) {

	if(is.null(FLOOR_HIST_myEncounter_ii$floorTime)) { return (NULL) }

	# We take min here as above in case two records exist at max
	minIndex = min(which(FLOOR_HIST_myEncounter_ii$floorTime == min(FLOOR_HIST_myEncounter_ii$floorTime)))
	return(FLOOR_HIST_myEncounter_ii$floor[minIndex])
}

#encountersWorkload is condensed list of cerner places.
#NURSING_WKLD 
applyWorkload = function(encountersWorkload,NURSING_WKLD,GRASP_NURSING_MAP,myEncounter_MWKD_Exclusions) {

	WORKLOAD_NOTAPPLIED = NULL

	myEncounter = unique(as.character(NURSING_WKLD$encounter))

	# At least 1 record exists to apply workload to
	if(!is.null(NURSING_WKLD) && nrow(NURSING_WKLD) > 0) {

		# Iterator over NURSNIG_WKLD nrow
		for(jj in 1:nrow(NURSING_WKLD)) {

			nursingWorkload_jj = trim(as.character(NURSING_WKLD[jj,]$floor)) # Nursing Floors on date jj
			cernerFloors_jj = encountersWorkload[[NURSING_WKLD[jj,]$date]]   # Cerner floors on date of nursing jj
		
			exclusions_jj = NULL
			if(!is.null(myEncounter_MWKD_Exclusions)) {
				exclusions_jj = as.character(myEncounter_MWKD_Exclusions[myEncounter_MWKD_Exclusions$DATE==NURSING_WKLD[jj,]$date,]$FLOOR)
			}
		
			# Null impliers
			if(!is.null(cernerFloors_jj)) {
		
				# We use this counter on a day to track removals
				numRecordsRemoved = 0
		
				# Traverse all Cerner Floors on date jj checking for first matching pair.
				for(kk in 1:length(cernerFloors_jj)) {
				
					# Check for Exclusions 
					if(!is.null(exclusions_jj) && !is.na(match(cernerFloors_jj[kk],exclusions_jj))) {
					
						encountersWorkload[[NURSING_WKLD[jj,]$date]] = encountersWorkload[[NURSING_WKLD[jj,]$date]][-(kk-numRecordsRemoved)]

						if(length(encountersWorkload[[NURSING_WKLD[jj,]$date]])==0) {
							encountersWorkload[NURSING_WKLD[jj,]$date] = list(NULL)
						}
						
						numRecordsRemoved = numRecordsRemoved + 1
						
					} else if(isValidGraspCernerPair(GRASP_NURSING_MAP,nursingWorkload_jj,cernerFloors_jj[kk])) {
				
						# Valid pair was found, remove the record and break as this nursing floor can only update 1 record
					  
					  encountersWorkload[[NURSING_WKLD[jj,]$date]] = encountersWorkload[[NURSING_WKLD[jj,]$date]][-(kk-numRecordsRemoved)]
					  if(length(encountersWorkload[[NURSING_WKLD[jj,]$date]])==0) {
					    encountersWorkload[NURSING_WKLD[jj,]$date] = list(NULL)
					  }
					  
					  #encountersWorkload[NURSING_WKLD[jj,]$date] = list(NULL)

						break
					} else if(length(cernerFloors_jj)==1){
						# We don't want to give false positives, so just log single cases where workload could be applied but wasn't
						WORKLOAD_NOTAPPLIED = rbind(WORKLOAD_NOTAPPLIED,NURSING_WKLD[jj,])
					}
				}
			}
		}
	}
	
	return(list(workloadApplied = encountersWorkload, workloadNotApplied = WORKLOAD_NOTAPPLIED))
}

updateWorkloadStats = function(WORKLOAD_STATS,
							   encounterTotalWorkload,
							   encounterMissingWorkload) {

							   
	flatWkld = unlist(encounterTotalWorkload)
	flatMWkld = unlist(encounterMissingWorkload)
	
	if(!is.null(flatWkld)) {
		for(jj in 1:length(flatWkld)) {
			if(is.null(WORKLOAD_STATS$WORKLOAD[flatWkld[jj]]) || is.na(WORKLOAD_STATS$WORKLOAD[flatWkld[jj]]) ) {
				WORKLOAD_STATS$WORKLOAD[flatWkld[jj]] = 1
			} else {
				WORKLOAD_STATS$WORKLOAD[flatWkld[jj]] = WORKLOAD_STATS$WORKLOAD[flatWkld[jj]] + 1
			}
		}
	}
	
	if(!is.null(flatMWkld)) {
		for(jj in 1:length(flatMWkld)) {
			if(is.null(WORKLOAD_STATS$MWORKLOAD[flatMWkld[jj]]) || is.na(WORKLOAD_STATS$MWORKLOAD[flatMWkld[jj]]) ) {
				WORKLOAD_STATS$MWORKLOAD[flatMWkld[jj]] = 1
			} else {
				WORKLOAD_STATS$MWORKLOAD[flatMWkld[jj]] = WORKLOAD_STATS$MWORKLOAD[flatMWkld[jj]] + 1
			}
		}
	}
	
	return(WORKLOAD_STATS)
}

#
populateWorkloadStats = function(WORKLOAD_STATS) {
	# Now we will convert statistics to matrix, wkld is superset of mwkld
	wkd = as.matrix(unlist(WORKLOAD_STATS$WORKLOAD))
	mwkd = unlist(WORKLOAD_STATS$MWORKLOAD)
	lookup = match(rownames(wkd),names(mwkd))
	wkd = cbind(as.matrix(mwkd[lookup]),wkd)
	wkd[is.na(wkd)] = 0
	
	percent <- as.numeric(wkd[,1] / wkd[,2])
	wkd = cbind(wkd,paste(round(percent*100,0), "%", sep=""))
	colnames(wkd) = c("MissingWorkload","Workload","PercentMissing")
	wkd = wkd[sort(rownames(wkd)),]
	wkd = data.frame(wkd)
	return(wkd)
}

getMWKDExclusions = function(FILLED_FLOOR_HIST_myEncounter, cernerStartMonth, cernerEndMonth) {

	EXCLUSIONS = NULL

	for(jj in 1:length(FILLED_FLOOR_HIST_myEncounter$patientHistory)) {
	
		flag=0
		if(length(FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorType) > 0) {
		
			for(kk in 1:length(FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorType)) {
				if(FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorType[kk]=="T_f" && 
				   FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorTime[kk] >= TIME_CUTOFF_L &&
				   FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorTime[kk] <= TIME_CUTOFF_U)
				{
					exclusion_jj = c(names(FILLED_FLOOR_HIST_myEncounter$patientHistory)[jj],
									FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floor[kk],
									1)
					EXCLUSIONS = rbind(EXCLUSIONS,exclusion_jj)
				}
			}
		}
	}

	if(!is.null(EXCLUSIONS)) {
		colnames(EXCLUSIONS) = c("DATE","FLOOR","FLAG")
		EXCLUSIONS = unique(EXCLUSIONS)
		rownames(EXCLUSIONS) = 1:nrow(EXCLUSIONS)
		return(data.frame(EXCLUSIONS))
	}

	return(EXCLUSIONS)
}


applyFloorExclusions = function(missingWorkload,FLOOR_EXCLUSIONS) {
	for(kk in 1:length(missingWorkload)) {
		cernerFloors_kk = missingWorkload[[kk]]
		
		for(mm in 1:length(cernerFloors_kk)) {
			index = sum(match(FLOOR_EXCLUSIONS,cernerFloors_kk[mm]),na.rm=T)
			if(index > 0) {
						
				missingWorkload[[kk]] = missingWorkload[[kk]][-mm]
									
				if(is.na(missingWorkload[[kk]][mm])) {
					missingWorkload[kk] = list(NULL)
				}
			}
		}
	}
	
	return(missingWorkload)
}


