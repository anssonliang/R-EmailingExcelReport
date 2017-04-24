# replace patterns in SQL script
#substituteFun <- function(sql, st_date = Sys.Date() - 1, end_date = Sys.Date() - 0){ # change date heres
substituteFun <- function(sql, st_date_input, end_date_input){
      st_date  <- as.numeric(difftime(st_date_input , unixTime, units = "sec"))
      end_date <- as.numeric(difftime(end_date_input, unixTime, units = "sec"))

      sql <- replace_date (sql, st_date, end_date )
      sql <- replace_level(sql, months = months(), days = days() )
      print(sql)
      sql
}

# run SQL queries and restructure query results
queryKQIs <-  function(){

      # nested query functions for each sql element
      queryResult <- lapply(sql, function(sql) {getIqData(substituteFun(sql(), Sys.Date() - dateBack_Start, Sys.Date() - dateBack_End ))}) # date changed

      # restructure the query result to fit excel template
      df <- list()

      df$KQI <- data.frame(matrix(NA, ncol = 1, nrow = 14))

      ## KQIs
      # WEB
      a <- colSums(queryResult$WEB)
      df$KQI[1, 1]      <- as.numeric(      (a['FST_PAGE_ACK_NUM']   / a['FST_PAGE_REQ_NUM']*100))
      df$KQI[2, 1]      <- as.numeric(       a['FST_PAGE_ACK_NUM'])
      df$KQI[3, 1]      <- as.numeric(       a['FST_PAGE_REQ_NUM'])
      df$KQI[4, 1]      <- as.numeric(       a['PAGE_SR_DELAY_MSEL'] / a['FST_PAGE_ACK_NUM']  )
      df$KQI[5, 1]      <- as.numeric(      (a['PAGE_SUCCEED_TIMES'] / a['FST_PAGE_REQ_NUM']*100))
      df$KQI[6, 1]      <- as.numeric(       a['PAGE_SUCCEED_TIMES'])
      df$KQI[7, 1]      <- as.numeric(       a['FST_PAGE_REQ_NUM'])
      df$KQI[8, 1]      <- as.numeric(       a['PAGE_DELAY_MSEL']    / a['FST_PAGE_ACK_NUM'])

      # FACEBOOK
      df$KQI[9, 1]      <- as.numeric(queryResult$FACEBOOK[1, 1])

      # INSTAGRAM
      df$KQI[10, 1]     <- as.numeric(queryResult$INSTAGRAM[1, 1])

      # SNAPCHAT
      df$KQI[11, 1]     <- as.numeric(queryResult$SNAPCHAT[1, 1])

      # YOUTUBE
      df$KQI[12, 1]     <- as.numeric(queryResult$YOUTUBE[1, 1])

      # NETFLIX
      df$KQI[13, 1]     <- as.numeric(queryResult$NETFLIX[1, 1])

      # YOUSEE
      df$KQI[14, 1]     <- as.numeric(queryResult$YOUSEE[1, 1])

      # HBO
      df$KQI[15, 1]     <- as.numeric(queryResult$HBO[1, 1])

      # VIAPLAY
      df$KQI[16, 1]     <- as.numeric(queryResult$VIAPLAY[1, 1])

      # TV2
      df$KQI[17, 1]     <- as.numeric(queryResult$TV2[1, 1])

      # DR
      df$KQI[18, 1]     <- as.numeric(queryResult$DR[1, 1])

      # APPSTORE_FILEACCESS
      df$KQI[19, 1]     <- as.numeric(queryResult$APPSTORE_FILEACCESS[1, 1])

      # GOOGLEPLAY_FILEACCESS
      df$KQI[20, 1]     <- as.numeric(queryResult$GOOGLEPLAY_FILEACCESS[1, 1])

      # AMAZONS3_FILEACCESS
      df$KQI[21, 1]     <- as.numeric(queryResult$AMAZONS3_FILEACCESS[1, 1])

      # WINDOWS_FILEACCESS
      df$KQI[22, 1]     <- as.numeric(queryResult$WINDOWS_FILEACCESS[1, 1])

      # APPLEICLOUD_FILEACCESS
      df$KQI[23, 1]     <- as.numeric(queryResult$APPLEICLOUD_FILEACCESS[1, 1])

      # Voice all
      a <- colSums(queryResult$VOICE)
      df$KQI[24, 1]     <- as.numeric((a['MOALERTCOUNT']         / a['MOCCHIREDCOUNT']) * 100)
      df$KQI[25, 1]     <- as.numeric(a['MOALERTCOUNT'])
      df$KQI[26, 1]     <- as.numeric(a['MOCCHIREDCOUNT'])
      df$KQI[27, 1]     <- as.numeric((a['MTALERTCOUNT']         / a['MTSETUPCOUNT']) * 100)
      df$KQI[28, 1]     <- as.numeric(a['MTALERTCOUNT'])
      df$KQI[29, 1]     <- as.numeric(a['MTSETUPCOUNT'])
      df$KQI[30, 1]     <- as.numeric((a['CONACKRADIODROPCOUNT'] / a['CONACKCOUNT']) * 100)
      df$KQI[31, 1]     <- as.numeric(a['CONACKRADIODROPCOUNT'])
      df$KQI[32, 1]     <- as.numeric(a['CONACKCOUNT'])
      df$KQI[33, 1]     <- as.numeric((a['E2EALERTDELAY']        / a['CALLPROCEEDCOUNT'] ) /1000 )

      # Retain 3G
      df$KQI[34, 1]     <- as.numeric(queryResult$Retain3G[1, 1])

      # TRAFFIC 2G
      df$KQI[35, 1]     <- as.numeric(queryResult$TRAFFIC2G[1, 1])

      # TRAFFIC 3G
      df$KQI[36, 1]     <- as.numeric(queryResult$TRAFFIC3G[1, 1])

      # DNS
      a <- colSums(queryResult$DNS)
      df$KQI[37, 1]    <- as.numeric(      (a['MS_DNS_SUCCEED_TIMES']   / a['MS_DNS_REQ_TIMES']*100))
      df$KQI[38, 1]    <- as.numeric(       a['MS_DNS_SUCCEED_TIMES'])
      df$KQI[39, 1]    <- as.numeric(       a['MS_DNS_REQ_TIMES'])

      # TCP
      a <- colSums(queryResult$TCP)
      df$KQI[40, 1]    <- as.numeric(      (a['TCPCONNSUCCCOUNT'] / a['TCPCONNCOUNT']*100))
      df$KQI[41, 1]    <- as.numeric(       a['TCPCONNSUCCCOUNT'])
      df$KQI[42, 1]    <- as.numeric(       a['TCPCONNCOUNT'])

      # FAILURES
      b <- queryResult$FAILURES

      df$FAILURE <- data.frame(matrix(NA, ncol = 7, nrow = nrow(b)))

      for(i in 1:nrow(b)) {
            df$FAILURE[i, 1]    <- as.character( b[i,'DATE_TIME'], format = "%Y-%m-%d")
            df$FAILURE[i, 2]    <- as.character( b[i,'HOST'])
            df$FAILURE[i, 3]    <- as.character( b[i,'CAUSE_CATEGORY'])
            df$FAILURE[i, 4]    <- as.character( b[i,'SCENARIO'])
            df$FAILURE[i, 5]    <- as.character( b[i,'PROTOCOL'])
            df$FAILURE[i, 6]    <- as.character( b[i,'FAILURE_CAUSE'])
            df$FAILURE[i, 7]    <- as.numeric( b[i,'FAILURE_TIMES'])
      }

      # TCP apps
      c <- queryResult$TCP_APP

      df$TCP_APP <- data.frame(matrix(NA, ncol = 15, nrow = 12))

      df$TCP_APP_TRANS<- data.frame(matrix(NA, ncol = 1, nrow = 15*12))
      for(i in 1:12) {
            df$TCP_APP[i, 1]     <- as.numeric(      (c[i,'COUNTER_11']   / c[i,'COUNTER_10']*100))
            df$TCP_APP[i, 2]     <- as.numeric(      (c[i,'COUNTER_7'] + c[i,'COUNTER_5'])/1024/1024/1024)
            df$TCP_APP[i, 3]     <- as.numeric(      (c[i,'COUNTER_19']   / c[i,'COUNTER_18']*100))
            df$TCP_APP[i, 4]     <- as.numeric(      (c[i,'COUNTER_14']   / c[i,'COUNTER_13']*100))
            df$TCP_APP[i, 5]     <- as.numeric(      (c[i,'COUNTER_17']   / c[i,'COUNTER_6']*100))
            df$TCP_APP[i, 6]     <- as.numeric(      (c[i,'COUNTER_12']   / c[i,'COUNTER_4']*100))
            df$TCP_APP[i, 7]     <- as.numeric(       c[i,'COUNTER_15']   / c[i,'COUNTER_11'])
            df$TCP_APP[i, 8]     <- as.numeric(       c[i,'COUNTER_2']    / c[i,'COUNTER_20'])
            df$TCP_APP[i, 9]     <- as.numeric(       c[i,'COUNTER_1']    / c[i,'COUNTER_3'])
            df$TCP_APP[i, 10]    <- as.numeric(      (c[i,'COUNTER_9']    / c[i,'COUNTER_18']*100))
            df$TCP_APP[i, 11]    <- as.numeric(      (c[i,'COUNTER_8']    / c[i,'COUNTER_13']*100))
            df$TCP_APP[i, 12]    <- as.numeric(      (c[i,'COUNTER_22']   / c[i,'COUNTER_18']*100))
            df$TCP_APP[i, 13]    <- as.numeric(      (c[i,'COUNTER_21']   / c[i,'COUNTER_13']*100))
            df$TCP_APP[i, 14]    <- as.numeric(      (c[i,'COUNTER_16']   / c[i,'COUNTER_11']*100))
            df$TCP_APP[i, 15]    <- as.numeric(      (c[i,'COUNTER_15'] - c[i,'COUNTER_16'])  / c[i,'COUNTER_11'])
      }

      # Transpose data frame of TCP apps
      for(j in 1:15) {
            df$TCP_APP_TRANS[1+(j-1)*12,1]   <- df$TCP_APP[1,j]
            df$TCP_APP_TRANS[2+(j-1)*12,1]   <- df$TCP_APP[2,j]
            df$TCP_APP_TRANS[3+(j-1)*12,1]   <- df$TCP_APP[3,j]
            df$TCP_APP_TRANS[4+(j-1)*12,1]   <- df$TCP_APP[4,j]
            df$TCP_APP_TRANS[5+(j-1)*12,1]   <- df$TCP_APP[5,j]
            df$TCP_APP_TRANS[6+(j-1)*12,1]   <- df$TCP_APP[6,j]
            df$TCP_APP_TRANS[7+(j-1)*12,1]   <- df$TCP_APP[7,j]
            df$TCP_APP_TRANS[8+(j-1)*12,1]   <- df$TCP_APP[8,j]
            df$TCP_APP_TRANS[9+(j-1)*12,1]   <- df$TCP_APP[9,j]
            df$TCP_APP_TRANS[10+(j-1)*12,1]  <- df$TCP_APP[10,j]
            df$TCP_APP_TRANS[11+(j-1)*12,1]  <- df$TCP_APP[11,j]
            df$TCP_APP_TRANS[12+(j-1)*12,1]  <- df$TCP_APP[12,j]
      }

      ## TCP demarcation
      # TCP RAT interface
      d <- queryResult$TCP_RAT

      df$TCP_RAT <- data.frame(matrix(NA, ncol = 15, nrow = 3))

      df$TCP_RAT_TRANS<- data.frame(matrix(NA, ncol = 1, nrow = 15*3))
      for(i in 1:3) {
            df$TCP_RAT[i, 1]     <- as.numeric(      (d[i,'COUNTER_11']   / d[i,'COUNTER_10']*100))
            df$TCP_RAT[i, 2]     <- as.numeric(      (d[i,'COUNTER_7'] + d[i,'COUNTER_5'])/1024/1024/1024)
            df$TCP_RAT[i, 3]     <- as.numeric(      (d[i,'COUNTER_19']   / d[i,'COUNTER_18']*100))
            df$TCP_RAT[i, 4]     <- as.numeric(      (d[i,'COUNTER_14']   / d[i,'COUNTER_13']*100))
            df$TCP_RAT[i, 5]     <- as.numeric(      (d[i,'COUNTER_17']   / d[i,'COUNTER_6']*100))
            df$TCP_RAT[i, 6]     <- as.numeric(      (d[i,'COUNTER_12']   / d[i,'COUNTER_4']*100))
            df$TCP_RAT[i, 7]     <- as.numeric(       d[i,'COUNTER_15']   / d[i,'COUNTER_11'])
            df$TCP_RAT[i, 8]     <- as.numeric(       d[i,'COUNTER_2']    / d[i,'COUNTER_20'])
            df$TCP_RAT[i, 9]     <- as.numeric(       d[i,'COUNTER_1']    / d[i,'COUNTER_3'])
            df$TCP_RAT[i, 10]    <- as.numeric(      (d[i,'COUNTER_9']    / d[i,'COUNTER_18']*100))
            df$TCP_RAT[i, 11]    <- as.numeric(      (d[i,'COUNTER_8']    / d[i,'COUNTER_13']*100))
            df$TCP_RAT[i, 12]    <- as.numeric(      (d[i,'COUNTER_22']   / d[i,'COUNTER_18']*100))
            df$TCP_RAT[i, 13]    <- as.numeric(      (d[i,'COUNTER_21']   / d[i,'COUNTER_13']*100))
            df$TCP_RAT[i, 14]    <- as.numeric(      (d[i,'COUNTER_16']   / d[i,'COUNTER_11']*100))
            df$TCP_RAT[i, 15]    <- as.numeric(      (d[i,'COUNTER_15'] - d[i,'COUNTER_16'])  / d[i,'COUNTER_11'])
      }

      # Transpose data frame of TCP RAT
      for(j in 1:15) {
            df$TCP_RAT_TRANS[1+(j-1)*3,1]   <- df$TCP_RAT[1,j]
            df$TCP_RAT_TRANS[2+(j-1)*3,1]   <- df$TCP_RAT[2,j]
            df$TCP_RAT_TRANS[3+(j-1)*3,1]   <- df$TCP_RAT[3,j]
      }

      # TCP BSC interface
      e <- queryResult$TCP_BSC

      df$TCP_BSC <- data.frame(matrix(NA, ncol = 15, nrow = 7))

      df$TCP_BSC_TRANS<- data.frame(matrix(NA, ncol = 1, nrow = 15*7))
      for(i in 1:7) {
            df$TCP_BSC[i, 1]     <- as.numeric(      (e[i,'COUNTER_11']   / e[i,'COUNTER_10']*100))
            df$TCP_BSC[i, 2]     <- as.numeric(      (e[i,'COUNTER_7'] + e[i,'COUNTER_5'])/1024/1024/1024)
            df$TCP_BSC[i, 3]     <- as.numeric(      (e[i,'COUNTER_19']   / e[i,'COUNTER_18']*100))
            df$TCP_BSC[i, 4]     <- as.numeric(      (e[i,'COUNTER_14']   / e[i,'COUNTER_13']*100))
            df$TCP_BSC[i, 5]     <- as.numeric(      (e[i,'COUNTER_17']   / e[i,'COUNTER_6']*100))
            df$TCP_BSC[i, 6]     <- as.numeric(      (e[i,'COUNTER_12']   / e[i,'COUNTER_4']*100))
            df$TCP_BSC[i, 7]     <- as.numeric(       e[i,'COUNTER_15']   / e[i,'COUNTER_11'])
            df$TCP_BSC[i, 8]     <- as.numeric(       e[i,'COUNTER_2']    / e[i,'COUNTER_20'])
            df$TCP_BSC[i, 9]     <- as.numeric(       e[i,'COUNTER_1']    / e[i,'COUNTER_3'])
            df$TCP_BSC[i, 10]    <- as.numeric(      (e[i,'COUNTER_9']    / e[i,'COUNTER_18']*100))
            df$TCP_BSC[i, 11]    <- as.numeric(      (e[i,'COUNTER_8']    / e[i,'COUNTER_13']*100))
            df$TCP_BSC[i, 12]    <- as.numeric(      (e[i,'COUNTER_22']   / e[i,'COUNTER_18']*100))
            df$TCP_BSC[i, 13]    <- as.numeric(      (e[i,'COUNTER_21']   / e[i,'COUNTER_13']*100))
            df$TCP_BSC[i, 14]    <- as.numeric(      (e[i,'COUNTER_16']   / e[i,'COUNTER_11']*100))
            df$TCP_BSC[i, 15]    <- as.numeric(      (e[i,'COUNTER_15'] - e[i,'COUNTER_16'])  / e[i,'COUNTER_11'])
      }

      # Transpose data frame of TCP BSC
      for(j in 1:15) {
            df$TCP_BSC_TRANS[1+(j-1)*7,1]   <- df$TCP_BSC[1,j]
            df$TCP_BSC_TRANS[2+(j-1)*7,1]   <- df$TCP_BSC[2,j]
            df$TCP_BSC_TRANS[3+(j-1)*7,1]   <- df$TCP_BSC[3,j]
            df$TCP_BSC_TRANS[4+(j-1)*7,1]   <- df$TCP_BSC[4,j]
            df$TCP_BSC_TRANS[5+(j-1)*7,1]   <- df$TCP_BSC[5,j]
            df$TCP_BSC_TRANS[6+(j-1)*7,1]   <- df$TCP_BSC[6,j]
            df$TCP_BSC_TRANS[7+(j-1)*7,1]   <- df$TCP_BSC[7,j]
      }

      # TCP RNC interface
      f <- queryResult$TCP_RNC

      df$TCP_RNC <- data.frame(matrix(NA, ncol = 15, nrow = 7))

      df$TCP_RNC_TRANS<- data.frame(matrix(NA, ncol = 1, nrow = 15*7))
      for(i in 1:7) {
            df$TCP_RNC[i, 1]     <- as.numeric(      (f[i,'COUNTER_11']   / f[i,'COUNTER_10']*100))
            df$TCP_RNC[i, 2]     <- as.numeric(      (f[i,'COUNTER_7'] + f[i,'COUNTER_5'])/1024/1024/1024)
            df$TCP_RNC[i, 3]     <- as.numeric(      (f[i,'COUNTER_19']   / f[i,'COUNTER_18']*100))
            df$TCP_RNC[i, 4]     <- as.numeric(      (f[i,'COUNTER_14']   / f[i,'COUNTER_13']*100))
            df$TCP_RNC[i, 5]     <- as.numeric(      (f[i,'COUNTER_17']   / f[i,'COUNTER_6']*100))
            df$TCP_RNC[i, 6]     <- as.numeric(      (f[i,'COUNTER_12']   / f[i,'COUNTER_4']*100))
            df$TCP_RNC[i, 7]     <- as.numeric(       f[i,'COUNTER_15']   / f[i,'COUNTER_11'])
            df$TCP_RNC[i, 8]     <- as.numeric(       f[i,'COUNTER_2']    / f[i,'COUNTER_20'])
            df$TCP_RNC[i, 9]     <- as.numeric(       f[i,'COUNTER_1']    / f[i,'COUNTER_3'])
            df$TCP_RNC[i, 10]    <- as.numeric(      (f[i,'COUNTER_9']    / f[i,'COUNTER_18']*100))
            df$TCP_RNC[i, 11]    <- as.numeric(      (f[i,'COUNTER_8']    / f[i,'COUNTER_13']*100))
            df$TCP_RNC[i, 12]    <- as.numeric(      (f[i,'COUNTER_22']   / f[i,'COUNTER_18']*100))
            df$TCP_RNC[i, 13]    <- as.numeric(      (f[i,'COUNTER_21']   / f[i,'COUNTER_13']*100))
            df$TCP_RNC[i, 14]    <- as.numeric(      (f[i,'COUNTER_16']   / f[i,'COUNTER_11']*100))
            df$TCP_RNC[i, 15]    <- as.numeric(      (f[i,'COUNTER_15'] - f[i,'COUNTER_16'])  / f[i,'COUNTER_11'])
      }

      # Transpose data frame of TCP RNC
      for(j in 1:15) {
            df$TCP_RNC_TRANS[1+(j-1)*7,1]   <- df$TCP_RNC[1,j]
            df$TCP_RNC_TRANS[2+(j-1)*7,1]   <- df$TCP_RNC[2,j]
            df$TCP_RNC_TRANS[3+(j-1)*7,1]   <- df$TCP_RNC[3,j]
            df$TCP_RNC_TRANS[4+(j-1)*7,1]   <- df$TCP_RNC[4,j]
            df$TCP_RNC_TRANS[5+(j-1)*7,1]   <- df$TCP_RNC[5,j]
            df$TCP_RNC_TRANS[6+(j-1)*7,1]   <- df$TCP_RNC[6,j]
            df$TCP_RNC_TRANS[7+(j-1)*7,1]   <- df$TCP_RNC[7,j]
      }

      # TCP SGSN interface
      g <- queryResult$TCP_SGSN

      df$TCP_SGSN <- data.frame(matrix(NA, ncol = 15, nrow = 2))

      df$TCP_SGSN_TRANS<- data.frame(matrix(NA, ncol = 1, nrow = 15*2))
      for(i in 1:2) {
            df$TCP_SGSN[i, 1]     <- as.numeric(      (g[i,'COUNTER_11']   / g[i,'COUNTER_10']*100))
            df$TCP_SGSN[i, 2]     <- as.numeric(      (g[i,'COUNTER_7'] + g[i,'COUNTER_5'])/1024/1024/1024)
            df$TCP_SGSN[i, 3]     <- as.numeric(      (g[i,'COUNTER_19']   / g[i,'COUNTER_18']*100))
            df$TCP_SGSN[i, 4]     <- as.numeric(      (g[i,'COUNTER_14']   / g[i,'COUNTER_13']*100))
            df$TCP_SGSN[i, 5]     <- as.numeric(      (g[i,'COUNTER_17']   / g[i,'COUNTER_6']*100))
            df$TCP_SGSN[i, 6]     <- as.numeric(      (g[i,'COUNTER_12']   / g[i,'COUNTER_4']*100))
            df$TCP_SGSN[i, 7]     <- as.numeric(       g[i,'COUNTER_15']   / g[i,'COUNTER_11'])
            df$TCP_SGSN[i, 8]     <- as.numeric(       g[i,'COUNTER_2']    / g[i,'COUNTER_20'])
            df$TCP_SGSN[i, 9]     <- as.numeric(       g[i,'COUNTER_1']    / g[i,'COUNTER_3'])
            df$TCP_SGSN[i, 10]    <- as.numeric(      (g[i,'COUNTER_9']    / g[i,'COUNTER_18']*100))
            df$TCP_SGSN[i, 11]    <- as.numeric(      (g[i,'COUNTER_8']    / g[i,'COUNTER_13']*100))
            df$TCP_SGSN[i, 12]    <- as.numeric(      (g[i,'COUNTER_22']   / g[i,'COUNTER_18']*100))
            df$TCP_SGSN[i, 13]    <- as.numeric(      (g[i,'COUNTER_21']   / g[i,'COUNTER_13']*100))
            df$TCP_SGSN[i, 14]    <- as.numeric(      (g[i,'COUNTER_16']   / g[i,'COUNTER_11']*100))
            df$TCP_SGSN[i, 15]    <- as.numeric(      (g[i,'COUNTER_15'] - g[i,'COUNTER_16'])  / g[i,'COUNTER_11'])
      }

      # Transpose data frame of TCP SGSN
      for(j in 1:15) {
            df$TCP_SGSN_TRANS[1+(j-1)*2,1]   <- df$TCP_SGSN[1,j]
            df$TCP_SGSN_TRANS[2+(j-1)*2,1]   <- df$TCP_SGSN[2,j]
      }

      # TCP GGSN interface
      h <- queryResult$TCP_GGSN

      df$TCP_GGSN <- data.frame(matrix(NA, ncol = 15, nrow = 5))

      df$TCP_GGSN_TRANS<- data.frame(matrix(NA, ncol = 1, nrow = 15*5))
      for(i in 1:5) {
            df$TCP_GGSN[i, 1]     <- as.numeric(      (h[i,'COUNTER_11']   / h[i,'COUNTER_10']*100))
            df$TCP_GGSN[i, 2]     <- as.numeric(      (h[i,'COUNTER_7'] + h[i,'COUNTER_5'])/1024/1024/1024)
            df$TCP_GGSN[i, 3]     <- as.numeric(      (h[i,'COUNTER_19']   / h[i,'COUNTER_18']*100))
            df$TCP_GGSN[i, 4]     <- as.numeric(      (h[i,'COUNTER_14']   / h[i,'COUNTER_13']*100))
            df$TCP_GGSN[i, 5]     <- as.numeric(      (h[i,'COUNTER_17']   / h[i,'COUNTER_6']*100))
            df$TCP_GGSN[i, 6]     <- as.numeric(      (h[i,'COUNTER_12']   / h[i,'COUNTER_4']*100))
            df$TCP_GGSN[i, 7]     <- as.numeric(       h[i,'COUNTER_15']   / h[i,'COUNTER_11'])
            df$TCP_GGSN[i, 8]     <- as.numeric(       h[i,'COUNTER_2']    / h[i,'COUNTER_20'])
            df$TCP_GGSN[i, 9]     <- as.numeric(       h[i,'COUNTER_1']    / h[i,'COUNTER_3'])
            df$TCP_GGSN[i, 10]    <- as.numeric(      (h[i,'COUNTER_9']    / h[i,'COUNTER_18']*100))
            df$TCP_GGSN[i, 11]    <- as.numeric(      (h[i,'COUNTER_8']    / h[i,'COUNTER_13']*100))
            df$TCP_GGSN[i, 12]    <- as.numeric(      (h[i,'COUNTER_22']   / h[i,'COUNTER_18']*100))
            df$TCP_GGSN[i, 13]    <- as.numeric(      (h[i,'COUNTER_21']   / h[i,'COUNTER_13']*100))
            df$TCP_GGSN[i, 14]    <- as.numeric(      (h[i,'COUNTER_16']   / h[i,'COUNTER_11']*100))
            df$TCP_GGSN[i, 15]    <- as.numeric(      (h[i,'COUNTER_15'] - h[i,'COUNTER_16'])  / h[i,'COUNTER_11'])
      }

      # Transpose data frame of TCP GGSN
      for(j in 1:15) {
            df$TCP_GGSN_TRANS[1+(j-1)*5,1]   <- df$TCP_GGSN[1,j]
            df$TCP_GGSN_TRANS[2+(j-1)*5,1]   <- df$TCP_GGSN[2,j]
            df$TCP_GGSN_TRANS[3+(j-1)*5,1]   <- df$TCP_GGSN[3,j]
            df$TCP_GGSN_TRANS[4+(j-1)*5,1]   <- df$TCP_GGSN[4,j]
            df$TCP_GGSN_TRANS[5+(j-1)*5,1]   <- df$TCP_GGSN[5,j]
      }

      # TCP SGW interface
      k <- queryResult$TCP_SGW

      df$TCP_SGW <- data.frame(matrix(NA, ncol = 15, nrow = 5))

      df$TCP_SGW_TRANS<- data.frame(matrix(NA, ncol = 1, nrow = 15*5))
      for(i in 1:5) {
            df$TCP_SGW[i, 1]     <- as.numeric(      (k[i,'COUNTER_11']   / k[i,'COUNTER_10']*100))
            df$TCP_SGW[i, 2]     <- as.numeric(      (k[i,'COUNTER_7'] + k[i,'COUNTER_5'])/1024/1024/1024)
            df$TCP_SGW[i, 3]     <- as.numeric(      (k[i,'COUNTER_19']   / k[i,'COUNTER_18']*100))
            df$TCP_SGW[i, 4]     <- as.numeric(      (k[i,'COUNTER_14']   / k[i,'COUNTER_13']*100))
            df$TCP_SGW[i, 5]     <- as.numeric(      (k[i,'COUNTER_17']   / k[i,'COUNTER_6']*100))
            df$TCP_SGW[i, 6]     <- as.numeric(      (k[i,'COUNTER_12']   / k[i,'COUNTER_4']*100))
            df$TCP_SGW[i, 7]     <- as.numeric(       k[i,'COUNTER_15']   / k[i,'COUNTER_11'])
            df$TCP_SGW[i, 8]     <- as.numeric(       k[i,'COUNTER_2']    / k[i,'COUNTER_20'])
            df$TCP_SGW[i, 9]     <- as.numeric(       k[i,'COUNTER_1']    / k[i,'COUNTER_3'])
            df$TCP_SGW[i, 10]    <- as.numeric(      (k[i,'COUNTER_9']    / k[i,'COUNTER_18']*100))
            df$TCP_SGW[i, 11]    <- as.numeric(      (k[i,'COUNTER_8']    / k[i,'COUNTER_13']*100))
            df$TCP_SGW[i, 12]    <- as.numeric(      (k[i,'COUNTER_22']   / k[i,'COUNTER_18']*100))
            df$TCP_SGW[i, 13]    <- as.numeric(      (k[i,'COUNTER_21']   / k[i,'COUNTER_13']*100))
            df$TCP_SGW[i, 14]    <- as.numeric(      (k[i,'COUNTER_16']   / k[i,'COUNTER_11']*100))
            df$TCP_SGW[i, 15]    <- as.numeric(      (k[i,'COUNTER_15'] - k[i,'COUNTER_16'])  / k[i,'COUNTER_11'])
      }

      # Transpose data frame of TCP SGW
      for(j in 1:15) {
            df$TCP_SGW_TRANS[1+(j-1)*5,1]   <- df$TCP_SGW[1,j]
            df$TCP_SGW_TRANS[2+(j-1)*5,1]   <- df$TCP_SGW[2,j]
            df$TCP_SGW_TRANS[3+(j-1)*5,1]   <- df$TCP_SGW[3,j]
            df$TCP_SGW_TRANS[4+(j-1)*5,1]   <- df$TCP_SGW[4,j]
            df$TCP_SGW_TRANS[5+(j-1)*5,1]   <- df$TCP_SGW[5,j]
      }

      # TCP PGW interface
      l <- queryResult$TCP_PGW

      df$TCP_PGW <- data.frame(matrix(NA, ncol = 15, nrow = 5))

      df$TCP_PGW_TRANS<- data.frame(matrix(NA, ncol = 1, nrow = 15*5))
      for(i in 1:5) {
            df$TCP_PGW[i, 1]     <- as.numeric(      (l[i,'COUNTER_11']   / l[i,'COUNTER_10']*100))
            df$TCP_PGW[i, 2]     <- as.numeric(      (l[i,'COUNTER_7'] + l[i,'COUNTER_5'])/1024/1024/1024)
            df$TCP_PGW[i, 3]     <- as.numeric(      (l[i,'COUNTER_19']   / l[i,'COUNTER_18']*100))
            df$TCP_PGW[i, 4]     <- as.numeric(      (l[i,'COUNTER_14']   / l[i,'COUNTER_13']*100))
            df$TCP_PGW[i, 5]     <- as.numeric(      (l[i,'COUNTER_17']   / l[i,'COUNTER_6']*100))
            df$TCP_PGW[i, 6]     <- as.numeric(      (l[i,'COUNTER_12']   / l[i,'COUNTER_4']*100))
            df$TCP_PGW[i, 7]     <- as.numeric(       l[i,'COUNTER_15']   / l[i,'COUNTER_11'])
            df$TCP_PGW[i, 8]     <- as.numeric(       l[i,'COUNTER_2']    / l[i,'COUNTER_20'])
            df$TCP_PGW[i, 9]     <- as.numeric(       l[i,'COUNTER_1']    / l[i,'COUNTER_3'])
            df$TCP_PGW[i, 10]    <- as.numeric(      (l[i,'COUNTER_9']    / l[i,'COUNTER_18']*100))
            df$TCP_PGW[i, 11]    <- as.numeric(      (l[i,'COUNTER_8']    / l[i,'COUNTER_13']*100))
            df$TCP_PGW[i, 12]    <- as.numeric(      (l[i,'COUNTER_22']   / l[i,'COUNTER_18']*100))
            df$TCP_PGW[i, 13]    <- as.numeric(      (l[i,'COUNTER_21']   / l[i,'COUNTER_13']*100))
            df$TCP_PGW[i, 14]    <- as.numeric(      (l[i,'COUNTER_16']   / l[i,'COUNTER_11']*100))
            df$TCP_PGW[i, 15]    <- as.numeric(      (l[i,'COUNTER_15'] - l[i,'COUNTER_16'])  / l[i,'COUNTER_11'])
      }

      # Transpose data frame of TCP PGW
      for(j in 1:15) {
            df$TCP_PGW_TRANS[1+(j-1)*5,1]   <- df$TCP_PGW[1,j]
            df$TCP_PGW_TRANS[2+(j-1)*5,1]   <- df$TCP_PGW[2,j]
            df$TCP_PGW_TRANS[3+(j-1)*5,1]   <- df$TCP_PGW[3,j]
            df$TCP_PGW_TRANS[4+(j-1)*5,1]   <- df$TCP_PGW[4,j]
            df$TCP_PGW_TRANS[5+(j-1)*5,1]   <- df$TCP_PGW[5,j]
      }

      print(df)
      df
}