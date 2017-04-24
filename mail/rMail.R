## convert excel to pdf
excelToPdf <- function(){
      # init com api for excel
      excelApp <- COMCreate("Excel.Application")
      # get report directory
      dirReport <- dir(getwd(), paste("^[a-z|A-Z].*", "KQI Daily", ".*xlsx$", sep = ""), full.names = TRUE)
      # open excel report
      reportWb <- excelApp$workbooks()$Open(dirReport)
      # pointer to 2nd worksheet
      reportWs_KQI_Overview <- reportWb$Worksheets()$Item(2)
      # display worksheet name
      #reportWs[["Name"]]
      # select 2nd worksheet
      reportWs_KQI_Overview$Select()
      # export excel as pdf format
      excelApp[["ActiveSheet"]]$ExportAsFixedFormat(Type = 0, Filename = "KQI_Overview_Graph.pdf")
      # pointer to 4th worksheet
      reportWs_TCP_App <- reportWb$Worksheets()$Item(4)
      reportWs_TCP_App$Select()
      excelApp[["ActiveSheet"]]$ExportAsFixedFormat(Type = 0, Filename = "TCP_App_Graph.pdf")
      # save workbook
      excelApp[["ActiveWorkbook"]]$Save()
      # close excel
      excelApp$Quit()
}

pdfToPng <- function(){
      # copy pdf file from default path to R project path
      file.copy(from = paste0("C:/Users/j00264597/Documents/","KQI_Overview_Graph.pdf"), to = paste0(getwd(),"/","KQI_Overview_Graph.pdf"), overwrite = TRUE)
      file.copy(from = paste0("C:/Users/j00264597/Documents/","TCP_App_Graph.pdf"), to = paste0(getwd(),"/","TCP_App_Graph.pdf"), overwrite = TRUE)
      # don NOT open output automatically
      ani.options(autobrowse = FALSE)
      # convert pdf to png by using ImageMagick
      im.convert("KQI_Overview_Graph.pdf", output = "KQI_Overview_Graph.png", extra.opts="-density 130")
      im.convert("TCP_App_Graph.pdf", output = "TCP_App_Graph.png", extra.opts="-density 130")
}

alertList <- function() {
      # create KQI list
      kqiList                 <- list()
      kqiList$pageResp        <- 'Page Response Success Rate(Top 10 Webpages)'
      kqiList$pageBrow        <- 'Page Browsing Success Rate(Top 10 Webpages)'
      kqiList$pageRespDelay   <- 'Page Response Delay(Top 10 Webpages)'
      kqiList$pageBrowDelay   <- 'Page Browsing Delay(Top 10 Webpages)'
      kqiList$voiceCallMo     <- 'Perceived Voice Call Setup Success Rate MO'
      kqiList$voiceCallMt     <- 'Perceived Voice Call Setup Success Rate MT'
      kqiList$voiceDrop       <- 'Perceived Voice Call Drop Rate(MO&MT)'
      kqiList$dns             <- 'DNS Query Success Rate(%)'
      kqiList$tcp             <- 'TCP Connection Success Rate(%)'

      tcpRatList              <- list()
      #tcpRatList$GSM         <- '1_2G'
      tcpRatList$UMTS         <- '1_3G'
      tcpRatList$LTE          <- '1_4G'

      tcpRncList              <- list()
      tcpRncList$ABRNC4       <- '1_ABRNC4'
      tcpRncList$ALBRNC4      <- '1_ALBRNC4'
      tcpRncList$ARRNC4       <- '1_ARRNC4'
      tcpRncList$KDRNC4       <- '1_KDRNC4'
      tcpRncList$KHRNC2       <- '1_KHRNC2'
      tcpRncList$ODRNC2       <- '1_ODRNC2'
      tcpRncList$VIRRNC2      <- '1_VIRRNC2'

      tcpSgsnList             <- list()
      tcpSgsnList$KDMME1      <- '1_KDMME1'
      tcpSgsnList$KHMME2      <- '1_KHMME2'

      tcpGgsnList             <- list()
      tcpGgsnList$ALBEPG2_Gn  <- '1_ALBEPG2(Gn)'
      tcpGgsnList$AREPG2_Gn   <- '1_AREPG2(Gn)'
      tcpGgsnList$ARGGSN10_Gn <- '1_ARGGSN10(Gn)'
      tcpGgsnList$ARGGSN11_Gn <- '1_ARGGSN11(Gn)'
      tcpGgsnList$KHEPG1_Gn   <- '1_KHEPG1(Gn)'

      tcpSgwList              <- list()
      tcpSgwList$ALBEPG2_S11  <- '1_ALBEPG2(S11)'
      tcpSgwList$AREPG2_S11   <- '1_AREPG2(S11)'
      tcpSgwList$ARGGSN10_S11 <- '1_ARGGSN10(S11)'
      tcpSgwList$ARGGSN11_S11 <- '1_ARGGSN11(S11)'
      tcpSgwList$KHEPG1_S11   <- '1_KHEPG1(S11)'

      tcpPgwList              <- list()
      tcpPgwList$ALBEPG2_Gi   <- '1_ALBEPG2(Gi)'
      tcpPgwList$AREPG2_Gi    <- '1_AREPG2(Gi)'
      tcpPgwList$ARGGSN10_Gi  <- '1_ARGGSN10(Gi)'
      tcpPgwList$ARGGSN11_Gi  <- '1_ARGGSN11(Gi)'
      tcpPgwList$KHEPG1_Gi    <- '1_KHEPG1(Gi)'

      # extract values for each KQI list
      alist                   <- list()
      alist$kqi               <- colValue("KQI_Overview_Raw", kqiList)
      alist$tcpRat            <- colValue("TCP_RAT_Raw", tcpRatList)
      alist$tcpRnc            <- colValue("TCP_RNC_Raw", tcpRncList)
      alist$tcpSgsn           <- colValue("TCP_SGSN_Raw", tcpSgsnList)
      alist$tcpGgsn           <- colValue("TCP_GGSN_Raw", tcpGgsnList)
      alist$tcpSgw            <- colValue("TCP_SGW_Raw", tcpSgwList)
      alist$tcpPgw            <- colValue("TCP_PGW_Raw", tcpPgwList)

      alist
}

# generate alert comments by detecting consecutive up/down changes
alertComment <- function(al){
      LogMsg("Comments and alters are generating...")
      down_PageResp    <<- ifelse(consecDown(al$kqi$pageResp)>1,paste0('Page Response(%) dropped in last ',consecDown(al$kqi$pageResp),' days!','<p></p>'),'')

      down_PageBrow    <<- ifelse(consecDown(al$kqi$pageBrow)>1,paste0('Page Browsing(%) dropped in last ',consecDown(al$kqi$pageBrow),' days!','<p></p>'),'')

      up_pageRespDelay <<- ifelse(consecUp(al$kqi$pageRespDelay)>1,paste0('Page Response Delay(ms) arised in last ',consecUp(al$kqi$pageRespDelay),' days!','<p></p>'),'')

      up_PageBrowDelay <<- ifelse(consecUp(al$kqi$pageBrowDelay)>1,paste0('Page Browsing Delay(ms) arised in last ',consecUp(al$kqi$pageBrowDelay),' days!','<p></p>'),'')

      down_VoiceCallMo <<- ifelse(consecDown(al$kqi$voiceCallMo)>1,paste0('Voice Call Setup MO(%) dropped in last ',consecDown(al$kqi$voiceCallMo),' days!','<p></p>'),'')

      down_VoiceCallMt <<- ifelse(consecDown(al$kqi$voiceCallMt)>1,paste0('Voice Call Setup MT(%) dropped in last ',consecDown(al$kqi$voiceCallMt),' days!','<p></p>'),'')

      up_VoiceDrop     <<- ifelse(consecUp(al$kqi$voiceDrop)>1,paste0('Voice Call Drop(%) arised in last ',consecUp(al$kqi$voiceDrop),' days!','<p></p>'),'')

      down_DNS         <<- ifelse(consecDown(al$kqi$dns)>1,paste0('DNS Connection(%) dropped in last ',consecDown(al$kqi$dns),' days!','<p></p>'),'')

      down_TCP         <<- ifelse(consecDown(al$kqi$tcp)>1,paste0('TCP Connection(%) dropped in last ',consecDown(al$kqi$tcp),' days!','<p></p>'),'')

      TCP_Demarcation  <<- ifelse(consecDown(al$kqi$tcp)>1,paste0('<br/>','TCP Demarcation ','<p></p>'),'')

      down_tcpRat      <<- ifelse(consecDown(al$kqi$tcp)>1 && any(lapply(al$tcpRat, consecDown)>1),paste0('Worst RAT is: ',names(which(lapply(lapply(al$tcpRat, consecDown),function(x){x>1})=='TRUE')),'<p></p>',collapse = ''),'')

      down_tcpRnc      <<- ifelse(consecDown(al$kqi$tcp)>1 && any(lapply(al$tcpRnc, consecDown)>1),paste0('Worst RAT is: ',names(which(lapply(lapply(al$tcpRnc, consecDown),function(x){x>1})=='TRUE')),'<p></p>',collapse = ''),'')

      down_tcpSgsn     <<- ifelse(consecDown(al$kqi$tcp)>1 && any(lapply(al$tcpSgsn, consecDown)>1),paste0('Worst SGSN is: ',names(which(lapply(lapply(al$tcpSgsn, consecDown),function(x){x>1})=='TRUE')),'<p></p>',collapse = ''),'')

      down_tcpGgsn     <<- ifelse(consecDown(al$kqi$tcp)>1 && any(lapply(al$tcpGgsn, consecDown)>1),paste0('Worst GGSN is: ',names(which(lapply(lapply(al$tcpGgsn, consecDown),function(x){x>1})=='TRUE')),'<p></p>',collapse = ''),'')

      down_tcpSgw      <<- ifelse(consecDown(al$kqi$tcp)>1 && any(lapply(al$tcpSgw, consecDown)>1),paste0('Worst SGW is: ',names(which(lapply(lapply(al$tcpSgw, consecDown),function(x){x>1})=='TRUE')),'<p></p>',collapse = ''),'')

      down_tcpPgw      <<- ifelse(consecDown(al$kqi$tcp)>1 && any(lapply(al$tcpPgw, consecDown)>1),paste0('Worst PGW is: ',names(which(lapply(lapply(al$tcpPgw, consecDown),function(x){x>1})=='TRUE')),'<p></p>',collapse = ''),'')
}

# create an email with attchments and embeded HTLM contents
attachAll <- function(){
      ## init com api for outlook
      outApp <- COMCreate("Outlook.Application")
      ## create an email
      outMail = outApp$CreateItem(0)

      ## configure  email parameter
      outMail[["To"]] = "anson.liang@huawei.com;Hng.Chun.Hua@huawei.com;Erik.Gade.Nielsen@huawei.com;shanni.gao@huawei.com;yelin.wang@huawei.com;lyubcho.toshev@huawei.com;"
      outMail[["Cc"]] = "suyu@huawei.com;heqibing.he@huawei.com;Anders.Hansson@huawei.com;edvardas.semenas@huawei.com;Piotr.Kalert@huawei.com;tdcpm@huawei.com"
      #outMail[["To"]] = "anson.liang@huawei.com"

      outMail[["subject"]] = paste0("*Internal* KQI Daily Report ", Sys.Date() - dateBack_Start)

      # get outlook signiture
      outMail$GetInspector()
      signature = outMail[["HTMLBody"]]

      # attach a picture
      dirPic_KQI_Overview <- dir(getwd(), paste("^KQI.*", ".*png$", sep = ""), full.names = TRUE)
      dirPic_TCP_App      <- dir(getwd(), paste("^TCP_A.*", ".*png$", sep = ""), full.names = TRUE)
      outMail[["Attachments"]]$Add(dirPic_KQI_Overview)
      outMail[["Attachments"]]$Add(dirPic_TCP_App)

      # translate to html
      picture_KQI_Overview <- paste0("<img src= 'cid:",basename(dirPic_KQI_Overview),"' >")
      picture_TCP_App      <- paste0("<img src= 'cid:",basename(dirPic_TCP_App),"' >")

      # html body
      outMail[["HTMLBody"]] = paste0(
            '<p>Hej,</p>',
            '<p>Please check KQI performance on ',Sys.Date() - dateBack_Start, '</p>',
            '<p><span style="font-size: 15px"><b>1) KQI Overview with Performance Alerts</b></span></p>',
            down_PageResp,
            down_PageBrow,
            up_pageRespDelay,
            up_PageBrowDelay,
            down_VoiceCallMo,
            down_VoiceCallMt,
            up_VoiceDrop,
            down_DNS,
            down_TCP,
            TCP_Demarcation,
            down_tcpRat,
            down_tcpRnc,
            down_tcpSgsn,
            down_tcpGgsn,
            down_tcpSgw,
            down_tcpPgw,
            picture_KQI_Overview,
            '<p><span style="font-size: 15px"><b>2) TCP Performance of 12 popular apps</b></span></p>',
            picture_TCP_App,
            signature
      )

      # attach the excel report via its directory
      dirReport <- dir(getwd(), paste("^[a-z|A-Z].*", "KQI Daily", ".*xlsx$", sep = ""), full.names = TRUE)
      outMail[["Attachments"]]$Add(dirReport)

      #outMail$Display()

      ## send email
      LogMsg(
            tryCatch({
                  outMail$Send()
                  "Email Sent!" # Log
            },
            warning = function (w){
                  paste0("email sent warning: ", w) # Log
            },
            error = function (e) {
                  paste0("email sent error: "  , e) # Log
            }
            )
      )
}

outlookSend <- function(){

      excelToPdf()
      LogMsg("Excel to PDF is done!")

      pdfToPng()
      LogMsg("PDF to PNG is done!")

      alertComment(al = alertList())

      attachAll()
}