dataPop <- function(excelInput){
      function(){
      restrResults <- queryKQIs()

      #Main KQIs
      kqiInfo <- list()
      kqiInfo <- excelInput$decomSheet("KQI_Overview_Raw")
      LogMsg("Populating data of  KQI_Overview_Raw...")
      populateOldRes(kqiInfo, 3:15, 16)
      populateNewResForKqi(kqiInfo, restrResults$KQI[,1], 16) #Insert row 16 with new data

      #TCP App Indicators
      tcpAppInfo <- list()
      tcpAppInfo <- excelInput$decomSheet("TCP_App_Raw")
      LogMsg("Populating data of  TCP_App_Raw...")
      populateOldRes(tcpAppInfo, 3:15, 16)
      populateNewRes(tcpAppInfo, restrResults$TCP_APP_TRANS[,1], 16) #Insert row 16 with new data

      #TCP RAT interface
      tcpRatInfo <- list()
      tcpRatInfo <- excelInput$decomSheet("TCP_RAT_Raw")
      LogMsg("Populating data of  TCP_RAT_Raw...")
      populateOldRes(tcpRatInfo, 3:15, 16)
      populateNewRes(tcpRatInfo, restrResults$TCP_RAT_TRANS[,1], 16) #Insert row 16 with new data

      #TCP BSC interface
      tcpBscInfo <- list()
      tcpBscInfo <- excelInput$decomSheet("TCP_BSC_Raw")
      LogMsg("Populating data of  TCP_BSC_Raw...")
      populateOldRes(tcpBscInfo, 3:15, 16)
      populateNewRes(tcpBscInfo, restrResults$TCP_BSC_TRANS[,1], 16) #Insert last row with new data

      #TCP RNC interface
      tcpRncInfo <- list()
      tcpRncInfo <- excelInput$decomSheet("TCP_RNC_Raw")
      LogMsg("Populating data of  TCP_RNC_Raw...")
      populateOldRes(tcpRncInfo, 3:15, 16)
      populateNewRes(tcpRncInfo, restrResults$TCP_RNC_TRANS[,1], 16) #Insert last row with new data

      #TCP SGSN interface
      tcpSgsnInfo <- list()
      tcpSgsnInfo <- excelInput$decomSheet("TCP_SGSN_Raw")
      LogMsg("Populating data of  TCP_SGSN_Raw...")
      populateOldRes(tcpSgsnInfo, 3:15, 16)
      populateNewRes(tcpSgsnInfo, restrResults$TCP_SGSN_TRANS[,1], 16) #Insert last row with new data

      #TCP GGSN interface
      tcpGgsnInfo <- list()
      tcpGgsnInfo <- excelInput$decomSheet("TCP_GGSN_Raw")
      LogMsg("Populating data of  TCP_GGSN_Raw...")
      populateOldRes(tcpGgsnInfo, 3:15, 16)
      populateNewRes(tcpGgsnInfo, restrResults$TCP_GGSN_TRANS[,1], 16) #Insert last row with new data

      #TCP SGW interface
      tcpSgwInfo <- list()
      tcpSgwInfo <- excelInput$decomSheet("TCP_SGW_Raw")
      LogMsg("Populating data of  TCP_SGW_Raw...")
      populateOldRes(tcpSgwInfo, 3:15, 16)
      populateNewRes(tcpSgwInfo, restrResults$TCP_SGW_TRANS[,1], 16) #Insert last row with new data

      #TCP PGW interface
      tcpPgwInfo <- list()
      tcpPgwInfo <- excelInput$decomSheet("TCP_PGW_Raw")
      LogMsg("Populating data of  TCP_PGW_Raw...")
      populateOldRes(tcpPgwInfo, 3:15, 16)
      populateNewRes(tcpPgwInfo, restrResults$TCP_PGW_TRANS[,1], 16) #Insert last row with new data

      # Web failure
      failureInfo <- list()
      failureInfo <- excelInput$decomSheet("Web_Failure")
      LogMsg("Populating data of  Web_Failure...")
      populateNewResInFailure(failureInfo, restrResults$FAILURE)
      }
}

populateNewRes <- function(excelInput, newValues, rowIndex){

      rows <- excelInput$rows[rowIndex]
      cell <- getCells(rows, 1)

      mapply(setCellValue, cell, paste0(Sys.Date() - dateBack_Start)) # date changed
      # set cell range for populating new values
      rows <- excelInput$rows[rowIndex]
      cells <- getCells(rows , 2:length(getCells(rows)))
      # set cells with new values
      mapply(setCellValue, cells, newValues)
}

populateOldRes <- function(excelInput, rowRangeForOldData, lastRowIndex){
      # set cell range for populating old values
      rows      <- excelInput$rows[rowRangeForOldData]
      lastRow <- excelInput$rows[lastRowIndex]
      cells     <- getCells(rows, 1:length(getCells(lastRow)))
      # get old values to be populated
      startCell <- length(excelInput$values)-13*length(getCells(lastRow))+1
      endCell   <- length(excelInput$values)
      oldValues <- excelInput$values[startCell:endCell]
      # set cells with old valules
      mapply(setCellValue, cells, oldValues)
}

populateNewResForKqi <- function(excelInput, newValues, rowIndex){
      rows <- excelInput$rows[rowIndex]
      cell <- getCells(rows, 1)

      mapply(setCellValue, cell, paste0("Network"))

      rows <- excelInput$rows[rowIndex]
      cell <- getCells(rows, 2)

      mapply(setCellValue, cell, paste0("All"))

      rows <- excelInput$rows[rowIndex]
      cell <- getCells(rows, 3)

      mapply(setCellValue, cell, paste0(Sys.Date() - dateBack_Start)) # date changed

      rows <- excelInput$rows[rowIndex]

      cells <- getCells(rows , 4:length(getCells(rows)))

      mapply(setCellValue, cells, newValues)
}

populateNewResInFailure <- function(excelInput, newValues){
      for(i in 1:ncol(newValues)) {
            rows <- excelInput$rows[2:(nrow(newValues)+1)]
            cells <- getCells(rows , i)
            mapply(setCellValue, cells, newValues[,i])
      }
}