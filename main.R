###########################################################################################
### This program is designed for automatically emailing database report.
###
### Preliminary: to convert pdf to image, ImageMagick and GhostScript need to be installed.
###
### 'main' loads packages, sources codes and implements functions to refresh excel worsheets.
### 'rExcelStructure' decomposits excel structure and runs nested functions.
### 'rDBConnection' setups database connection and provides a query function.
### 'rSQL' contains mutiple customized SQL scripts with subsitute patterns.
### 'rQuery' replaces subsitutes, runs muti-queries in list and reshapes query results.
### 'rDataPopulation' inserts query results in parsed worksheet cells.
### 'rMail' uses Outlook APIs to attach and generate comments and graphs from excel in HTML.
###########################################################################################
package <- c("xlsx", "RDCOMClient", "animation")
lapply(package, require, character.only = TRUE)

source("./cfg.R")
source("./drv/rDBConnection.R")
source("./excel/rExcelStructure.R")
source("./query/rSQL.R")
source("./query/rQuery.R")
source("./excel/rDataPopulation.R")
source("./helper.R")
source("./mail/rMail.R")

excel <- decomExcel()

excel$newData()

excel$save()

excel$delete()

outlookSend()