
missingWorkloadDriver = function(cernerAuditFile, 
								nursingFile, 
								graspCernerAliasFile,
								workloadNotAppliedLog,
								workloadLogFileXLS,
								workloadLogFileTXT,
								persistToExcel)
{

	# Load all Cerner Data ordered on VISIT_ID, DATE, TIME with adm, pday, dis, trano, dec records only
	CERNER_DAT = parseCernerFile(cernerAuditFile)
	cernerStartMonth = min(CERNER_DAT$DATE); cernerEndMonth = max(CERNER_DAT$DATE)
	print(paste(format(Sys.time(), "%a %b %d %Y %X"),": Cerner File Loaded, ", cernerStartMonth, " to ", cernerEndMonth, " with ", nrow(CERNER_DAT), " records.", sep=""))

	# Create list per encounter storing necessary information : type, time, floor, etc.
	cernerEncounters = unique(CERNER_DAT$VISIT_ID)
	FLOOR_HISTORY = vector(mode="list",length=length(cernerEncounters))
	names(FLOOR_HISTORY) = cernerEncounters

	# Read the nursing workload file
	NURSING_DAT = parseNursingFile(nursingFile)
	print(paste(format(Sys.time(), "%a %b %d %Y %X"),": Nursing File Loaded, ", min(NURSING_DAT$date), " to ", max(NURSING_DAT$date), " with ", nrow(NURSING_DAT), " records.", sep=""))
	
	# Parse the cerner to nursing logic file
	GRASP_NURSING_MAP = readGraspCernerAliasFile(graspCernerAliasFile)
	print(paste(format(Sys.time(), "%a %b %d %Y %X"),": Grasp Mapping File Loaded, with ", nrow(GRASP_NURSING_MAP), " records.", sep=""))

	# Read Floor Exclusions - ER
	FLOOR_EXCLUSIONS = as.character(as.matrix(read.table(file=floorNoMissingWorkloadFile,sep="\t",header=T)))
	
	WORKLOAD_STATS = NULL  # This will store workload, missing workload numbers

	print(paste(format(Sys.time(), "%a %b %d %Y %X"),": Starting Missing Workload calculation", sep=""))
	
	pb <- winProgressBar(title = "Calculating Missing Workload", min = 0, max = length(cernerEncounters), width = 300)

	WORKLOAD_NOTAPPLIED = NULL 
	
	
	# Per encounter create a sparse history of the patient which we will 
	# later fill in and calculate missing workload on based on nursing records
	for(ii in 1:length(cernerEncounters)) {

		setWinProgressBar(pb, ii, title=paste( round(ii/length(cernerEncounters)*100, 0), "% done"))
		
			CERNER_DAT_myEncounter = CERNER_DAT[CERNER_DAT$VISIT_ID==cernerEncounters[ii],]
			
			if ((CERNER_DAT_myEncounter[1,]$TRANS == 'trano')&&(CERNER_DAT_myEncounter[1,]$NURSE_UNIT == 'Emerg Admit')&&
			    (CERNER_DAT_myEncounter[1,]$TIME <= TIME_CUTOFF_L) ){
			  
			  CERNER_DAT_myEncounter <- CERNER_DAT_myEncounter[-1,]
			  
			}

			FLOOR_HIST_myEncounter = createSparseHistory(CERNER_DAT_myEncounter,cernerStartMonth,cernerEndMonth,TIME_CUTOFF_L,TIME_CUTOFF_U)

			if(!is.null(FLOOR_HIST_myEncounter)) {
				# Fill in the encounter for workload and simplify workload
				# i.e. repetitive floors get only 1 workload
				FILLED_FLOOR_HIST_myEncounter = fillEncounter(FLOOR_HIST_myEncounter,cernerStartMonth,cernerEndMonth)


				for(jj in 1:length(FILLED_FLOOR_HIST_myEncounter$patientHistory)) {
				  
				  if(length(FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorType) == 2){
				    trans <- data.frame(FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorType)
				    colnames(trans) <- 'type'
				    trans1 <- data.frame(c("T_f", "T_t"))
				    colnames(trans1) <- 'type'
				    
				    if(identical(trans, trans1)) {
				      
				      for(kk in 1:length(FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorType)) {
				        if(FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorType[kk]=="T_f" && 
				           FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorTime[kk] >= TIME_CUTOFF_L &&
				           FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floorTime[kk] <= TIME_CUTOFF_U)
				        {
				          FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floor[kk] <- FILLED_FLOOR_HIST_myEncounter$patientHistory[[jj]]$floor[-kk]
				          
				        }
				      }
				    }
				  }
				}


				for(kk in 1:length(FILLED_FLOOR_HIST_myEncounter[[5]])) {
				  FILLED_FLOOR_HIST_myEncounter[[5]][[kk]][["setback"]] <- NULL
				}
				
				FILLED_FLOOR_HIST_myEncounter <- checkDuration(FILLED_FLOOR_HIST_myEncounter)

				
				# Get any logical exclusions based on raw data
				myEncounter_MWKD_Exclusions = getMWKDExclusions(FLOOR_HIST_myEncounter,cernerStartMonth,cernerEndMonth)

				encountersWorkload = condenseFilledEncounter(FILLED_FLOOR_HIST_myEncounter,cernerStartMonth,cernerEndMonth)
				
				# Calculate total workload
				original_FILLED_FLOOR_HIST_myEncounter <- original_fillEncounter(FLOOR_HIST_myEncounter,cernerStartMonth,cernerEndMonth)
				   
				original_encountersWorkload <- condenseFilledEncounter(original_FILLED_FLOOR_HIST_myEncounter,cernerStartMonth,cernerEndMonth)
				
				FILLED_FLOOR_HIST_myEncounter$totalWorkload <- original_encountersWorkload
				
				# Use Condensed History with Nursing WOrkload to fill in missing workload
				NURSING_WKLD = NURSING_DAT[NURSING_DAT$encounter == cernerEncounters[ii],]
				
					workloadStatus = applyWorkload(encountersWorkload,NURSING_WKLD,GRASP_NURSING_MAP,myEncounter_MWKD_Exclusions)
					FILLED_FLOOR_HIST_myEncounter$missingWorkload = workloadStatus$workloadApplied
					FILLED_FLOOR_HIST_myEncounter$missingWorkload = applyFloorExclusions(FILLED_FLOOR_HIST_myEncounter$missingWorkload,FLOOR_EXCLUSIONS)
					WORKLOAD_NOTAPPLIED = rbind(WORKLOAD_NOTAPPLIED,workloadStatus$workloadNotApplied)

				
				if(length(unique(CERNER_DAT_myEncounter$NAME)) > 1) {
					print(paste("NAME",cernerEncounters[ii]))
				}
				
				if(length(unique(CERNER_DAT_myEncounter$MRN)) > 1) {
					print(paste("mrn-",cernerEncounters[ii]))
				}
				
				FILLED_FLOOR_HIST_myEncounter$name = trim(as.character(unique(CERNER_DAT_myEncounter$NAME)[1])) 
				FILLED_FLOOR_HIST_myEncounter$mrn = trim(as.character(unique(CERNER_DAT_myEncounter$MRN)[1]))

				# This encounter has both workload, missingworkload so updateTotals
				WORKLOAD_STATS = updateWorkloadStats(WORKLOAD_STATS,
													 FILLED_FLOOR_HIST_myEncounter$totalWorkload,
													 FILLED_FLOOR_HIST_myEncounter$missingWorkload)

				# Persist
				FLOOR_HISTORY[[ii]] = FILLED_FLOOR_HIST_myEncounter
			} else {
				print(paste(format(Sys.time(), "%a %b %d %Y %X"),": Record ignored, insufficient data for Encounter ", cernerEncounters[ii], sep=""))
			}
			
	}

	write.table(WORKLOAD_NOTAPPLIED,file=workloadNotAppliedLog,sep="\t",col.names=T,row.names=F,quote=F)
	
	# Done all calculations - output necessary results
	close(pb)

	wkd = populateWorkloadStats(WORKLOAD_STATS)

	if(persistToExcel == 1) {
		
		sheetName = format(cernerStartMonth,"%Y%m%d")
		odbcPersistResult = odbcPersistHistoricalDataFrame(wkd, workloadLogFileXLS, sheetName)
		
		# The worksheet already exists - user must delete to resolve
		if(odbcPersistResult == 0) {
			print(paste(format(Sys.time(), "%a %b %d %Y %X"),": WARNING : Missing Workload Not Written to XLS - ", sheetName, " already exists!", sep=""))
			return(0)
		}	
	} else {
		write.table(file=workloadLogFileTXT, wkd, col.names=T, row.names=T)
	}
	
	suppressWarnings(logFloorHistory(FLOOR_HISTORY, missingWorkloadLogFile))
	
	print(paste(format(Sys.time(), "%a %b %d %Y %X"),": Completed Missing Workload calculation", sep=""))
}
