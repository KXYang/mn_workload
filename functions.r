
### Define 6 functions:
# trim
# readGraspCernerAliasFile
# isValidGraspCernerPair
# parseNursingFile
# parseCernerFile
# logFloorHistory

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# To import mapping file
readGraspCernerAliasFile <- function(graspCernerAliasFile) {
	DAT = read.table(file=graspCernerAliasFile, sep="\t", header=TRUE)
	return(DAT)
}

# Grasp refers to nursing workload file, Cerner refers to monthly audit file
isValidGraspCernerPair <- function(GRASP_NURSING_MAP, graspFloor, cernerFloor) {

	MAP_SUBSET = GRASP_NURSING_MAP[GRASP_NURSING_MAP$GRASP_Name==graspFloor,]

	if(nrow(MAP_SUBSET)>0) {

		# This effectively queries on the cernerFloor for this given graspFloor
		MAP_SUB_SUBSET = MAP_SUBSET[MAP_SUBSET$CERNER_Alias==cernerFloor,]

		# A direct match or wildcard match
		if(nrow(MAP_SUB_SUBSET)>0) { return (1) }
		if(sum(MAP_SUBSET$CERNER_Alias == "*") > 0) { return (1) }
	}

	return (0) # No match
}

# Parse nursing workload file
parseNursingFile <- function(nursingFile) {

	filler1=77
	dateLen=8
	filler2=6
	encounterLen=10
	floorLen=7

	# Read only date, encounter, floor and create unique entries
	nursingDAT = read.fwf(file=nursingFile, widths=c(filler1,dateLen,filler2,encounterLen,-1,floorLen))
	colnames(nursingDAT)=c('filler1','date','filler2','encounter','floor')
	nursingDAT = nursingDAT[,-c(1,3)]
	nursingDAT = unique(nursingDAT)
	nursingDAT$floor = trim(nursingDAT$floor)
	nursingDAT$date = as.character(as.Date(as.character(nursingDAT$date),"%Y%m%d"))
	
	return (nursingDAT)
}

# Parse audit file 
parseCernerFile <- function(cernerAuditFile) {

	# Field Offsets and Lengths
	costCenterLen = 11
	transLen = 5                   
	visitIdLen = 15                
	medServiceLen = 15
	nurseUnitLen = 10              
	nameLen = 25
	mrnLen = 15
	transactionDTLen = 17          
	transactionDLen = 11
	infoLen = 50                   
	encounterLen = 14
	ipAdultLen = 11
	ipPedLen = 11
	ipNewLen = 11
	mhAdultLen = 11
	mhPedLen = 11

	# Read header only
	cernerHeader = read.table(file=cernerAuditFile,sep="|",nrows=1)
	cernerHeader = as.matrix(cernerHeader[-length(cernerHeader)]) # remove the last redundant element

	# Read all data
	cernerDAT = read.fwf(file=cernerAuditFile, widths=c(costCenterLen,-1,
	                                                    transLen,-1,
	                                                    visitIdLen,-1,
	                                                    medServiceLen,-1,
	                                                    nurseUnitLen,-1,
	                                                    nameLen,-1,
	                                                    mrnLen,-1,
	                                                    transactionDTLen,-1,
	                                                    infoLen,-1,
	                                                    encounterLen,-1,
	                                                    ipAdultLen,-1,
	                                                    ipPedLen,-1,
	                                                    ipNewLen,-1,
	                                                    mhAdultLen,-1,
	                                                    mhPedLen), skip = 1)

	colnames(cernerDAT) = cernerHeader
	colnames(cernerDAT) = trim(colnames(cernerDAT))
	cernerDATSubset = cernerDAT[,c('TRANS','VISIT_ID','NURSE_UNIT','TRANSACTION_DT_TM','INFO','NAME','MRN')]

	# Trim fields accordingly and remove tday, TRANS, trani records as we won't need them
	cernerDATSubset$VISIT_ID = trim(gsub('-','',cernerDATSubset$VISIT_ID))
	cernerDATSubset$NURSE_UNIT = trim(cernerDATSubset$NURSE_UNIT)
	cernerDATSubset$NURSE_UNIT = gsub('Emerg Admi','Emerg Admit',cernerDATSubset$NURSE_UNIT)
	cernerDATSubset$TRANS = trim(cernerDATSubset$TRANS)
	cernerDATSubset = cernerDATSubset[(cernerDATSubset$TRANS!="tday" & cernerDATSubset$TRANS!="TRANS" & cernerDATSubset$TRANS!="trani"),]

	# We want to break TRANSACTION_DT_TM into two variables: date and time prior to sorting
	tmpDate = as.Date(trim(substr(cernerDATSubset$TRANSACTION_DT_TM,0,transactionDLen)),"%d-%b-%Y")
	tmpTime = trim(substr(cernerDATSubset$TRANSACTION_DT_TM,transactionDLen+1,transactionDTLen))
	tmpTime = as.matrix(gsub(':','',tmpTime))
	tmpTime = apply(tmpTime,c(1,2),as.numeric)

	cernerDATSubset$DATE = tmpDate
	cernerDATSubset$TIME = tmpTime
	
	# Update location 11LM to 11S - updated on Jan 23, 2018
	cernerDATSubset[cernerDATSubset$NURSE_UNIT == "11LM",]$NURSE_UNIT <- "11S"

	# Convert the dates to an R date object so sorting is meaningful
	cernerDATSubset = cernerDATSubset[with(cernerDATSubset, order(VISIT_ID,DATE,TIME)), ]	
	
}


# Log the filled data 

logFloorHistory = function(FLOOR_HISTORY, missingWorkloadLogFile) {
  
	allEncounters = names(FLOOR_HISTORY)

	FLOOR_HIST = NULL
	FLOOR_HIST_DETAILED = NULL
	MWKLD_HIST = NULL
	
	for(ii in 1:length(allEncounters)) {
		encounterii = allEncounters[[ii]]
		workload = rep(NA,length(FLOOR_HISTORY[[ii]]$totalWorkload))
		workloadHistory = rep(NA,length(FLOOR_HISTORY[[ii]]$totalWorkload))
		names(workload) = names(FLOOR_HISTORY[[ii]]$totalWorkload)
		names(workloadHistory) = names(FLOOR_HISTORY[[ii]]$totalWorkload)
		
		for(jj in 1:length(FLOOR_HISTORY[[ii]]$totalWorkload)) {
			if(!is.null(FLOOR_HISTORY[[ii]]) && !is.null(FLOOR_HISTORY[[ii]]$totalWorkload[[jj]])) {
				workload[jj] = paste(FLOOR_HISTORY[[ii]]$totalWorkload[[jj]],collapse="#")
				
				workloadHistory[jj] = paste(FLOOR_HISTORY[[ii]]$patientHistory[[jj]]$floor,
											FLOOR_HISTORY[[ii]]$patientHistory[[jj]]$floorType,
											FLOOR_HISTORY[[ii]]$patientHistory[[jj]]$floorTime,
											sep="|",collapse="#")

				workload = as.matrix(workload)
				workloadHistory = as.matrix(workloadHistory)
				colnames(workload) = encounterii
				colnames(workloadHistory) = encounterii
			}
		}
		
		for(jj in 1:length(FLOOR_HISTORY[[ii]]$missingWorkload)) {
			if(!is.null(FLOOR_HISTORY[[ii]]) && !is.null(FLOOR_HISTORY[[ii]]$missingWorkload[[jj]])) {

				# Possible to have more htan 1 mwkld per day so account for this
				mwkFloor = FLOOR_HISTORY[[ii]]$missingWorkload[[jj]]
				
				rec = c(encounterii,
				        names(FLOOR_HISTORY[[ii]]$missingWorkload)[jj],
						"FILL",
						FLOOR_HISTORY[[ii]]$mrn,
						FLOOR_HISTORY[[ii]]$name)
				
				rec = t(as.matrix(rec))
				
				if(length(mwkFloor) > 1) {
					rec = rec[rep(1,length(mwkFloor)),]
				}
				
				rec[,3] = mwkFloor

				if(is.null(MWKLD_HIST)) { 
					MWKLD_HIST = rec
				} else {
					MWKLD_HIST = rbind(MWKLD_HIST,rec)
				}	
			}
		}

	}
	
	# Per missing workload record we want to output the visit number, date of missing workload, nursing unit, chart number and name of patient
	colnames(MWKLD_HIST) = c("Encounter","Date","NursingUnit","ChartNumber","Name")
	
	write.table(file=missingWorkloadLogFile,MWKLD_HIST,sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)

}

