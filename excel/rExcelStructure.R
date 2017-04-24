decomExcel <- function() {
   excel      <- list()
   excel$xlsx <- list.files(getwd(), paste("^[a-z|A-Z].*", "KQI Daily Report", ".*xlsx$", sep = ""))[1]
   excel$wb   <- loadWorkbook(file.path(".", excel$xlsx))
   excel$ws   <- getSheets(excel$wb)

   excel$decomSheet <- function(sheet = character()){
         temp       <- list()

         temp$sheet <- excel$ws[[sheet]]

         temp$rows  <- getRows(temp$sheet)

         temp$cells <- getCells(temp$rows)

         temp$values<- lapply(temp$cells, getCellValue)

         temp
   }

   excel$newData <- dataPop(excel)

   excel$save  <- function() {saveWorkbook(excel$wb, gsub('20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]'
                                                      , paste0(Sys.Date() - dateBack_Start) # date changed
                                                      , excel$xlsx)
                                         )
         }

   excel$delete <- function() { if (file.exists(excel$xlsx)) file.remove(excel$xlsx)
         }

   excel
}