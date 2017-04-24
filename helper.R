# calculate days from unix Time in number
days <- function(date = Sys.Date()){
      numDay <- as.numeric(difftime(Sys.Date() - dateBack_Start, unixTime, units = "day")) # date changed
      round(numDay)
}

# calculate months from unix Time in number
months <- function(date = Sys.Date()){
      numMonth <- as.numeric(difftime(Sys.Date() - dateBack_Start, unixTime, units = "weeks")) / 4.351864 # date changed
      #a <- 567
      round(numMonth)
}

# replace pattern in SQL script
replace_date <- function(sql, st_date, end_date){
      if(class(st_date) == class(numeric()) && class(end_date) == class(numeric())){
            sql <- gsub(pattern = "%st_date" , replacement = st_date , sql)
            sql <- gsub(pattern = "%end_date", replacement = end_date, sql)
            sql
      }else{
            LogMsg("Start or end date is not a number")
      }
}

replace_level <- function(sql, days = character(), months = character()){
      sql <- gsub(pattern = "%months", replacement = months, sql)
      sql <- gsub(pattern = "%days", replacement = days, sql)
      sql
}

# detect consecutive decrement of data
consecDown <- function(dataSeries) {
      sumDown = 0

      for (i in 1:length(diff(dataSeries)<0)-1) {
            sumDown <- ifelse ((diff(dataSeries)<0)[i+1] == (diff(dataSeries)<0)[i] && (diff(dataSeries)<0)[i+1] == 'TRUE',
                               sumDown +1, 0
            )
      }
      sumDown + 1
}

# detect consecutive increment of data
consecUp <- function(dataSeries) {
      sumDown = 0

      for (i in 1:length(diff(dataSeries)>0)-1) {
            sumDown <- ifelse ((diff(dataSeries)>0)[i+1] == (diff(dataSeries)>0)[i] && (diff(dataSeries)>0)[i+1] == 'TRUE',
                               sumDown +1, 0
            )
      }
      sumDown + 1
}

# extract column values of one KQI
colValue <- function(worksheetName, kqiList) {
      lapply(kqiList, function(nList) {as.numeric(excel$decomSheet(worksheetName)$values[
                        c(seq(from = match(nList,excel$decomSheet(worksheetName)$values)+excel$decomSheet(worksheetName)$rows$'16'$getPhysicalNumberOfCells()
                              , to = match(nList,excel$decomSheet(worksheetName)$values)+excel$decomSheet(worksheetName)$rows$'16'$getPhysicalNumberOfCells()*14
                              , by = excel$decomSheet(worksheetName)$rows$'16'$getPhysicalNumberOfCells()))
                        ]
                  )
            }
      )
}