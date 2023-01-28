
#install.packages("chron")
#install.packages("xlsx")

rm(list=ls())
baseDir = getwd()
setwd(baseDir)

source('functions.r')
source('totalWkld.r')
source('encounterHistory.r')
source('missingWorkloadDriver.r')
source('missingWorkloadSettings.r')
source('excelCode.R')

	
if(!logToScreen) { sink(file=runStatusLogFile) }
	
missingWorkloadDriver(cernerAuditFile, 
					  nursingFile, 
					  graspCernerAliasFile,
					  workloadNotAppliedLog,
					  workloadLogFileXLS,
					  workloadLogFileTXT,
					  persistToExcel)
						  
if(!logToScreen) { sink(file=NULL) }
