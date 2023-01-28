
############################# RUN SETTINGS #############################
TIME_MAX = 2359                      # Time that a turnback gets set to
TIME_CUTOFF_L = 400                  # Lower range for turnback cutoff
TIME_CUTOFF_U = 730                  # Upper range for turnback cutoff
TRANO_FLOOR_DELIM = '-->--'          # Delimiter for transfers
logToScreen = 0
persistToExcel = 1                   # If this is 1 we append to an excel file, o.w. text

######################### INPUT FILES #############################

# GRASP nursing workload file
inputDirList = list.files(paste(baseDir,"/input",sep=""))
utlIndex = grep("utl",inputDirList)
nursingFile = paste(baseDir,"/input/",inputDirList[utlIndex[1]],sep="")

# Floor names of Cerner Floor that should receive no missing workload
floorNoMissingWorkloadFile = paste(baseDir,"/input/floorExclusions.txt",sep="") 

# Cerner monthly audit file
cernerAuditFile = paste(baseDir,"/input/msh_moh_audit.csv",sep="")

# Mapping file
graspCernerAliasFile = paste(baseDir,"/input/graspCernerAlias.txt",sep="")

######################### OUTPUT FILES #############################

missingWorkloadLogFile = paste(baseDir,"/output/missingWorkloadLog.txt",sep="")
workloadLogFileXLS = paste(baseDir,"/output/MWKD_HISTORICAL_SUMMARY.xls",sep="")
workloadLogFileTXT = paste(baseDir,"/output/MWKD_STATS.txt",sep="")

# Log fileS
runStatusLogFile = paste(baseDir,"/logs/log.txt",sep="")
workloadNotAppliedLog = paste(baseDir,"/logs/workloadNotApplied.txt",sep="")


