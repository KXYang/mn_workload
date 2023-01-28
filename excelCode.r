
odbcPersistHistoricalDataFrame = function(DAT, xlsFile, sheetName) {
  
  require("xlsx")
  
  fileExist = 0
  
  if(file.exists(xlsFile)) {
    fileExist = 1
  }
  
  write.xlsx(DAT, xlsFile, sheetName = sheetName, 
             row.names=TRUE, append=FALSE)
  
  return(1)
}


