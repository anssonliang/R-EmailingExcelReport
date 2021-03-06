# SQL scripts in a list
sql <- list()

sql$WEB <- function() {
      "SELECT
      SUM(ISNULL(PAGE_DELAY_MSEL,0)) PAGE_DELAY_MSEL
      ,SUM(ISNULL(PAGE_SUCCEED_TIMES,0)) PAGE_SUCCEED_TIMES
      ,SUM(ISNULL(FST_PAGE_REQ_NUM,0)) FST_PAGE_REQ_NUM
      ,SUM(ISNULL(PAGE_AVG_SIZE,0)) PAGE_AVG_SIZE
      ,SUM(ISNULL(FST_PAGE_ACK_NUM,0)) FST_PAGE_ACK_NUM
      ,SUM(ISNULL(DATATRANS_DW_DURATION,0)) DATATRANS_DW_DURATION
      ,SUM(ISNULL(PAGE_SR_DELAY_MSEL,0)) PAGE_SR_DELAY_MSEL
      FROM PS.SDR_WEB_BASE_USER_15MIN_%days
      WHERE LOWER(HOST) IN ('www.google.com', 'www.google.dk', 'google.com', 'google.dk', 'www.google.de', 'www.youtube.com', 'youtube.com', 'youtube.dk', 'www.facebook.com',
'www.facebook.dk', 'ekstrabladet.dk', 'login.live.com', 'g.live.com', 'pos.meal4u.dk', 'stokercloud.dk', 'nyheder.tv2.dk','www.pornhub.com', 'www.xnxx.com')
      ;COMMIT;"
}

sql$VOICE <- function(){
      "call cs.sp_hw_Query_Run_Template_KQI_QualityAnalyse(
	101,'-1','%st_date,%end_date;','day',3600,'1',255
      ,'SDR_VOICE_BSC_1DAY_%months'
      ,'SUM(ISNULL(MOALERTCOUNT,0)) MOALERTCOUNT
      ,SUM(ISNULL(MOTCSIRSPDELAYBEYONDCOUNT,0)) MOTCSIRSPDELAYBEYONDCOUNT
      ,SUM(ISNULL(MOCCHIREDCOUNT,0)) MOCCHIREDCOUNT
      ,SUM(ISNULL(CALLPROCEEDCOUNT,0)) CALLPROCEEDCOUNT
      ,SUM(ISNULL(E2EALERTDELAY,0)) E2EALERTDELAY
      ,SUM(ISNULL(CONACKRADIODROPCOUNT,0)) CONACKRADIODROPCOUNT
      ,SUM(ISNULL(SDCCHREQFAILCOUNT,0)) SDCCHREQFAILCOUNT
      ,SUM(ISNULL(CONACKCOUNT,0)) CONACKCOUNT
      ,SUM(ISNULL(MTALERTCOUNT,0)) MTALERTCOUNT
      ,SUM(ISNULL(MTSETUPCOUNT,0)) MTSETUPCOUNT'
      )
       ;COMMIT;"
}

sql$TRAFFIC2G <- function() {
      "SELECT ROUND(SUM(COUNTER_2 + COUNTER_4)/1000/60,2) AS VOICE_TRAFFIC_2G_ERL
       FROM NETHOUSE.SDR_DYN_SDR_VOICE_CALL_DURATION_1DAY_99997 VOICE_SDR
       WHERE IMSI <> ''
       AND ACCESS_TYPE_ID = 0
       AND STARTTIME >= %st_date
       AND STARTTIME < %end_date
       ;COMMIT;"
}

sql$TRAFFIC3G <- function() {
      "SELECT ROUND(SUM(COUNTER_2 + COUNTER_4)/1000/60,2) AS VOICE_TRAFFIC_3G_ERL
      FROM NETHOUSE.SDR_DYN_SDR_VOICE_CALL_DURATION_1DAY_99997 VOICE_SDR
      WHERE IMSI <> ''
      AND ACCESS_TYPE_ID = 1
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$FACEBOOK <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
          ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) like ('%facebook%')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$INSTAGRAM <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
          ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) like ('%instagram%')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$SNAPCHAT <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) LIKE ('%snapchat%')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$YOUTUBE <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) in ('youtube_streaming','youtube_browsing','youtube_fileaccess','youtube_hd_streaming')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$NETFLIX <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) like ('%netflix%')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$YOUSEE <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) LIKE '%yousee%'
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$HBO <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
	AND lower(PROT.PROT_NAME) in ('hbo_streaming','hbo_browsing')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$VIAPLAY <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) like ('%viaplay%')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$TV2 <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) like ('%tv2%')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$APPSTORE_FILEACCESS <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) LIKE '%appstore_fileaccess%'
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$DR <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) like ('%drtv_streaming%')
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$GOOGLEPLAY_FILEACCESS <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) LIKE '%googleplay_fileaccess%'
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$AMAZONS3_FILEACCESS <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) LIKE '%amazons3_fileaccess%'
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$WINDOWS_FILEACCESS <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) LIKE '%windows_fileaccess%'
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$APPLEICLOUD_FILEACCESS <- function() {
      "SELECT ROUND(SUM(USERS.L4_UL_THROUGHPUT + USERS.L4_DW_THROUGHPUT) / (1024*1024*1024),2) AS TRAFFIC_GB
      FROM PS.SDR_FLOW_APP_1DAY_%months USERS
      ,PS.DI_PROT_MAPPING PROT
      WHERE USERS.PROT_CATEGORY= PROT.PROT_CATEGORY
      AND USERS.PROT_TYPE= PROT.PROT
      AND lower(PROT.PROT_NAME) LIKE '%appleicloud_fileaccess%'
      AND STARTTIME >= %st_date
      AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$Retain3G <- function() {
      "SELECT
      ROUND((CASE WHEN (SUM(ISNULL(COUNTER_3,0)))+(SUM(ISNULL(COUNTER_6,0)))+(SUM(ISNULL(COUNTER_4,0)))+(SUM(ISNULL(COUNTER_8,0))) = 0 THEN 0
             ELSE ((SUM(ISNULL(COUNTER_3,0)))+(SUM(ISNULL(COUNTER_6,0)))+(SUM(ISNULL(COUNTER_4,0)))+(SUM(ISNULL(COUNTER_8,0)))-(SUM(ISNULL(COUNTER_1,0))))/((SUM(ISNULL(COUNTER_3,0)))+(SUM(ISNULL(COUNTER_6,0)))+(SUM(ISNULL(COUNTER_4,0)))+(SUM(ISNULL(COUNTER_8,0))))*100 END ),2) AS 'RETAIN_IN_3G_PERCENT_WITHOUT_2G_ORG(%)'
      FROM NETHOUSE.SDR_DYN_CALL_DURATION_REGION_SDR_1DAY_99997
      where STARTTIME >= %st_date
      and   STARTTIME <  %end_date
      ;COMMIT;"
}

sql$DNS <- function() {
      "SELECT
      SUM(ISNULL(MS_DNS_SUCCEED_TIMES,0)) MS_DNS_SUCCEED_TIMES
     ,SUM(ISNULL(MS_DNS_REQ_TIMES,0)) MS_DNS_REQ_TIMES
      FROM PS.SDR_DNS_MS_CISAI_1DAY_%months
      WHERE STARTTIME >= %st_date
        AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$TCP <- function() {
      "SELECT
      SUM(ISNULL(TCPCONNSUCCCOUNT,0)) TCPCONNSUCCCOUNT
     ,SUM(ISNULL(TCPCONNCOUNT,0)) TCPCONNCOUNT
      FROM PS.SDR_TCP_CGISAI_1DAY_%months
      WHERE STARTTIME >= %st_date
        AND STARTTIME < %end_date
      ;COMMIT;"
}

sql$TCP_APP <- function() {
      "SELECT PROT.APP_NAME
      ,SUM(ISNULL(COUNTER_1,0)) COUNTER_1
      ,SUM(ISNULL(COUNTER_2,0)) COUNTER_2
      ,SUM(ISNULL(COUNTER_3,0)) COUNTER_3
      ,SUM(ISNULL(COUNTER_4,0)) COUNTER_4
      ,SUM(ISNULL(COUNTER_5,0)) COUNTER_5
      ,SUM(ISNULL(COUNTER_6,0)) COUNTER_6
      ,SUM(ISNULL(COUNTER_7,0)) COUNTER_7
      ,SUM(ISNULL(COUNTER_8,0)) COUNTER_8
      ,SUM(ISNULL(COUNTER_9,0)) COUNTER_9
      ,SUM(ISNULL(COUNTER_10,0)) COUNTER_10
      ,SUM(ISNULL(COUNTER_11,0)) COUNTER_11
      ,SUM(ISNULL(COUNTER_12,0)) COUNTER_12
      ,SUM(ISNULL(COUNTER_13,0)) COUNTER_13
      ,SUM(ISNULL(COUNTER_14,0)) COUNTER_14
      ,SUM(ISNULL(COUNTER_15,0)) COUNTER_15
      ,SUM(ISNULL(COUNTER_16,0)) COUNTER_16
      ,SUM(ISNULL(COUNTER_17,0)) COUNTER_17
      ,SUM(ISNULL(COUNTER_18,0)) COUNTER_18
      ,SUM(ISNULL(COUNTER_19,0)) COUNTER_19
      ,SUM(ISNULL(COUNTER_20,0)) COUNTER_20
      ,SUM(ISNULL(COUNTER_21,0)) COUNTER_21
      ,SUM(ISNULL(COUNTER_22,0)) COUNTER_22
      from NETHOUSE.SDR_DYN_SEQ_TCP_PERFORMANCE_1DAY_%months TCP,
      (select PROTOCOL_ID, APP_NAME from NETHOUSE.Z_CFG_APP_PROTOCOL
      where APP_NAME in ('Yousee','HBO','Cmore','Netflix','Viaplay','TV2','Youtube','Spotify','Facebook','Instagram','SnapChat','Twitter')
      ) PROT
      WHERE PROT.PROTOCOL_ID = TCP.DIM_6
        AND TCP.STARTTIME >= %st_date
        AND TCP.STARTTIME < %end_date
      GROUP BY PROT.APP_NAME
      ORDER BY PROT.APP_NAME
      ;COMMIT;"
}


sql$TCP_RAT <- function() {
      "SELECT case when DIM_4=2 then '2G' when DIM_4 in (1,5) then '3G' when DIM_4 = 6
              then '4G' else 'Unknown' end as ACCESS_TYPE
      ,SUM(ISNULL(COUNTER_1,0)) COUNTER_1
      ,SUM(ISNULL(COUNTER_2,0)) COUNTER_2
      ,SUM(ISNULL(COUNTER_3,0)) COUNTER_3
      ,SUM(ISNULL(COUNTER_4,0)) COUNTER_4
      ,SUM(ISNULL(COUNTER_5,0)) COUNTER_5
      ,SUM(ISNULL(COUNTER_6,0)) COUNTER_6
      ,SUM(ISNULL(COUNTER_7,0)) COUNTER_7
      ,SUM(ISNULL(COUNTER_8,0)) COUNTER_8
      ,SUM(ISNULL(COUNTER_9,0)) COUNTER_9
      ,SUM(ISNULL(COUNTER_10,0)) COUNTER_10
      ,SUM(ISNULL(COUNTER_11,0)) COUNTER_11
      ,SUM(ISNULL(COUNTER_12,0)) COUNTER_12
      ,SUM(ISNULL(COUNTER_13,0)) COUNTER_13
      ,SUM(ISNULL(COUNTER_14,0)) COUNTER_14
      ,SUM(ISNULL(COUNTER_15,0)) COUNTER_15
      ,SUM(ISNULL(COUNTER_16,0)) COUNTER_16
      ,SUM(ISNULL(COUNTER_17,0)) COUNTER_17
      ,SUM(ISNULL(COUNTER_18,0)) COUNTER_18
      ,SUM(ISNULL(COUNTER_19,0)) COUNTER_19
      ,SUM(ISNULL(COUNTER_20,0)) COUNTER_20
      ,SUM(ISNULL(COUNTER_21,0)) COUNTER_21
      ,SUM(ISNULL(COUNTER_22,0)) COUNTER_22
      from NETHOUSE.SDR_DYN_SEQ_TCP_PERFORMANCE_1DAY_%months TCP,
      WHERE TCP.DIM_4 in (1,2,5,6)
        AND TCP.STARTTIME >= %st_date
        AND TCP.STARTTIME < %end_date
      GROUP BY ACCESS_TYPE
      ORDER BY ACCESS_TYPE
      ;COMMIT;"
}

sql$TCP_BSC <- function() {
      "SELECT DIM.BSCRNC_NAME
      ,SUM(ISNULL(COUNTER_1,0)) COUNTER_1
      ,SUM(ISNULL(COUNTER_2,0)) COUNTER_2
      ,SUM(ISNULL(COUNTER_3,0)) COUNTER_3
      ,SUM(ISNULL(COUNTER_4,0)) COUNTER_4
      ,SUM(ISNULL(COUNTER_5,0)) COUNTER_5
      ,SUM(ISNULL(COUNTER_6,0)) COUNTER_6
      ,SUM(ISNULL(COUNTER_7,0)) COUNTER_7
      ,SUM(ISNULL(COUNTER_8,0)) COUNTER_8
      ,SUM(ISNULL(COUNTER_9,0)) COUNTER_9
      ,SUM(ISNULL(COUNTER_10,0)) COUNTER_10
      ,SUM(ISNULL(COUNTER_11,0)) COUNTER_11
      ,SUM(ISNULL(COUNTER_12,0)) COUNTER_12
      ,SUM(ISNULL(COUNTER_13,0)) COUNTER_13
      ,SUM(ISNULL(COUNTER_14,0)) COUNTER_14
      ,SUM(ISNULL(COUNTER_15,0)) COUNTER_15
      ,SUM(ISNULL(COUNTER_16,0)) COUNTER_16
      ,SUM(ISNULL(COUNTER_17,0)) COUNTER_17
      ,SUM(ISNULL(COUNTER_18,0)) COUNTER_18
      ,SUM(ISNULL(COUNTER_19,0)) COUNTER_19
      ,SUM(ISNULL(COUNTER_20,0)) COUNTER_20
      ,SUM(ISNULL(COUNTER_21,0)) COUNTER_21
      ,SUM(ISNULL(COUNTER_22,0)) COUNTER_22
      from NETHOUSE.SDR_DYN_SEQ_TCP_PERFORMANCE_1DAY_%months TCP,
      DIM_LOC_BSCRNC  DIM
      WHERE TCP.DIM_4 in (2)
      AND TCP.DIM_1 = dim.BSCRNC_ID
      AND DIM.BSCRNC_NAME is not null
      AND TCP.STARTTIME >= %st_date
      AND TCP.STARTTIME < %end_date
      GROUP BY DIM.BSCRNC_NAME
      ORDER BY DIM.BSCRNC_NAME
      ;COMMIT;"
}

sql$TCP_RNC <- function() {
      "SELECT DIM.BSCRNC_NAME
      ,SUM(ISNULL(COUNTER_1,0)) COUNTER_1
      ,SUM(ISNULL(COUNTER_2,0)) COUNTER_2
      ,SUM(ISNULL(COUNTER_3,0)) COUNTER_3
      ,SUM(ISNULL(COUNTER_4,0)) COUNTER_4
      ,SUM(ISNULL(COUNTER_5,0)) COUNTER_5
      ,SUM(ISNULL(COUNTER_6,0)) COUNTER_6
      ,SUM(ISNULL(COUNTER_7,0)) COUNTER_7
      ,SUM(ISNULL(COUNTER_8,0)) COUNTER_8
      ,SUM(ISNULL(COUNTER_9,0)) COUNTER_9
      ,SUM(ISNULL(COUNTER_10,0)) COUNTER_10
      ,SUM(ISNULL(COUNTER_11,0)) COUNTER_11
      ,SUM(ISNULL(COUNTER_12,0)) COUNTER_12
      ,SUM(ISNULL(COUNTER_13,0)) COUNTER_13
      ,SUM(ISNULL(COUNTER_14,0)) COUNTER_14
      ,SUM(ISNULL(COUNTER_15,0)) COUNTER_15
      ,SUM(ISNULL(COUNTER_16,0)) COUNTER_16
      ,SUM(ISNULL(COUNTER_17,0)) COUNTER_17
      ,SUM(ISNULL(COUNTER_18,0)) COUNTER_18
      ,SUM(ISNULL(COUNTER_19,0)) COUNTER_19
      ,SUM(ISNULL(COUNTER_20,0)) COUNTER_20
      ,SUM(ISNULL(COUNTER_21,0)) COUNTER_21
      ,SUM(ISNULL(COUNTER_22,0)) COUNTER_22
      from NETHOUSE.SDR_DYN_SEQ_TCP_PERFORMANCE_1DAY_%months TCP,
      DIM_LOC_BSCRNC  DIM
      WHERE TCP.DIM_4 in (1,5)
      AND TCP.DIM_1 = dim.BSCRNC_ID
      AND DIM.BSCRNC_NAME is not null
      AND TCP.STARTTIME >= %st_date
      AND TCP.STARTTIME < %end_date
      GROUP BY DIM.BSCRNC_NAME
      ORDER BY DIM.BSCRNC_NAME
      ;COMMIT;"
}

sql$TCP_SGSN <- function() {
      "SELECT DIM.SGSN_NAME
      ,SUM(ISNULL(COUNTER_1,0)) COUNTER_1
      ,SUM(ISNULL(COUNTER_2,0)) COUNTER_2
      ,SUM(ISNULL(COUNTER_3,0)) COUNTER_3
      ,SUM(ISNULL(COUNTER_4,0)) COUNTER_4
      ,SUM(ISNULL(COUNTER_5,0)) COUNTER_5
      ,SUM(ISNULL(COUNTER_6,0)) COUNTER_6
      ,SUM(ISNULL(COUNTER_7,0)) COUNTER_7
      ,SUM(ISNULL(COUNTER_8,0)) COUNTER_8
      ,SUM(ISNULL(COUNTER_9,0)) COUNTER_9
      ,SUM(ISNULL(COUNTER_10,0)) COUNTER_10
      ,SUM(ISNULL(COUNTER_11,0)) COUNTER_11
      ,SUM(ISNULL(COUNTER_12,0)) COUNTER_12
      ,SUM(ISNULL(COUNTER_13,0)) COUNTER_13
      ,SUM(ISNULL(COUNTER_14,0)) COUNTER_14
      ,SUM(ISNULL(COUNTER_15,0)) COUNTER_15
      ,SUM(ISNULL(COUNTER_16,0)) COUNTER_16
      ,SUM(ISNULL(COUNTER_17,0)) COUNTER_17
      ,SUM(ISNULL(COUNTER_18,0)) COUNTER_18
      ,SUM(ISNULL(COUNTER_19,0)) COUNTER_19
      ,SUM(ISNULL(COUNTER_20,0)) COUNTER_20
      ,SUM(ISNULL(COUNTER_21,0)) COUNTER_21
      ,SUM(ISNULL(COUNTER_22,0)) COUNTER_22
      from NETHOUSE.SDR_DYN_SEQ_TCP_PERFORMANCE_1DAY_%months TCP,
      DIM_LOC_SGSN DIM
      WHERE TCP.DIM_4 in (1,2,5)
      AND TCP.DIM_2 = DIM.SGSN_ID
      AND DIM.SGSN_NAME is not null
      AND TCP.STARTTIME >= %st_date
      AND TCP.STARTTIME < %end_date
      GROUP BY DIM.SGSN_NAME
      ORDER BY DIM.SGSN_NAME
      ;COMMIT;"
}

sql$TCP_GGSN <- function() {
      "SELECT DIM.GGSN_NAME
      ,SUM(ISNULL(COUNTER_1,0)) COUNTER_1
      ,SUM(ISNULL(COUNTER_2,0)) COUNTER_2
      ,SUM(ISNULL(COUNTER_3,0)) COUNTER_3
      ,SUM(ISNULL(COUNTER_4,0)) COUNTER_4
      ,SUM(ISNULL(COUNTER_5,0)) COUNTER_5
      ,SUM(ISNULL(COUNTER_6,0)) COUNTER_6
      ,SUM(ISNULL(COUNTER_7,0)) COUNTER_7
      ,SUM(ISNULL(COUNTER_8,0)) COUNTER_8
      ,SUM(ISNULL(COUNTER_9,0)) COUNTER_9
      ,SUM(ISNULL(COUNTER_10,0)) COUNTER_10
      ,SUM(ISNULL(COUNTER_11,0)) COUNTER_11
      ,SUM(ISNULL(COUNTER_12,0)) COUNTER_12
      ,SUM(ISNULL(COUNTER_13,0)) COUNTER_13
      ,SUM(ISNULL(COUNTER_14,0)) COUNTER_14
      ,SUM(ISNULL(COUNTER_15,0)) COUNTER_15
      ,SUM(ISNULL(COUNTER_16,0)) COUNTER_16
      ,SUM(ISNULL(COUNTER_17,0)) COUNTER_17
      ,SUM(ISNULL(COUNTER_18,0)) COUNTER_18
      ,SUM(ISNULL(COUNTER_19,0)) COUNTER_19
      ,SUM(ISNULL(COUNTER_20,0)) COUNTER_20
      ,SUM(ISNULL(COUNTER_21,0)) COUNTER_21
      ,SUM(ISNULL(COUNTER_22,0)) COUNTER_22
      from NETHOUSE.SDR_DYN_SEQ_TCP_PERFORMANCE_1DAY_%months TCP,
      DIM_LOC_GGSN DIM
      WHERE TCP.DIM_4 in (1,2,5)
      AND TCP.DIM_3 = DIM.GGSN_ID
      AND DIM.GGSN_NAME is not null
      AND TCP.STARTTIME >= %st_date
      AND TCP.STARTTIME < %end_date
      GROUP BY DIM.GGSN_NAME
      ORDER BY DIM.GGSN_NAME
      ;COMMIT;"
}

sql$TCP_SGW <- function() {
      "SELECT DIM.SGW_NAME
      ,SUM(ISNULL(COUNTER_1,0)) COUNTER_1
      ,SUM(ISNULL(COUNTER_2,0)) COUNTER_2
      ,SUM(ISNULL(COUNTER_3,0)) COUNTER_3
      ,SUM(ISNULL(COUNTER_4,0)) COUNTER_4
      ,SUM(ISNULL(COUNTER_5,0)) COUNTER_5
      ,SUM(ISNULL(COUNTER_6,0)) COUNTER_6
      ,SUM(ISNULL(COUNTER_7,0)) COUNTER_7
      ,SUM(ISNULL(COUNTER_8,0)) COUNTER_8
      ,SUM(ISNULL(COUNTER_9,0)) COUNTER_9
      ,SUM(ISNULL(COUNTER_10,0)) COUNTER_10
      ,SUM(ISNULL(COUNTER_11,0)) COUNTER_11
      ,SUM(ISNULL(COUNTER_12,0)) COUNTER_12
      ,SUM(ISNULL(COUNTER_13,0)) COUNTER_13
      ,SUM(ISNULL(COUNTER_14,0)) COUNTER_14
      ,SUM(ISNULL(COUNTER_15,0)) COUNTER_15
      ,SUM(ISNULL(COUNTER_16,0)) COUNTER_16
      ,SUM(ISNULL(COUNTER_17,0)) COUNTER_17
      ,SUM(ISNULL(COUNTER_18,0)) COUNTER_18
      ,SUM(ISNULL(COUNTER_19,0)) COUNTER_19
      ,SUM(ISNULL(COUNTER_20,0)) COUNTER_20
      ,SUM(ISNULL(COUNTER_21,0)) COUNTER_21
      ,SUM(ISNULL(COUNTER_22,0)) COUNTER_22
      from NETHOUSE.SDR_DYN_SEQ_TCP_PERFORMANCE_1DAY_%months TCP,
      DIM_LOC_SGW DIM
      WHERE TCP.DIM_4 in (6)
      AND TCP.DIM_2 = dim.SGW_ID
      AND DIM.SGW_NAME is not null
      AND TCP.STARTTIME >= %st_date
      AND TCP.STARTTIME < %end_date
      GROUP BY DIM.SGW_NAME
      ORDER BY DIM.SGW_NAME
      ;COMMIT;"
}

sql$TCP_PGW <- function() {
      "SELECT DIM.PGW_NAME
      ,SUM(ISNULL(COUNTER_1,0)) COUNTER_1
      ,SUM(ISNULL(COUNTER_2,0)) COUNTER_2
      ,SUM(ISNULL(COUNTER_3,0)) COUNTER_3
      ,SUM(ISNULL(COUNTER_4,0)) COUNTER_4
      ,SUM(ISNULL(COUNTER_5,0)) COUNTER_5
      ,SUM(ISNULL(COUNTER_6,0)) COUNTER_6
      ,SUM(ISNULL(COUNTER_7,0)) COUNTER_7
      ,SUM(ISNULL(COUNTER_8,0)) COUNTER_8
      ,SUM(ISNULL(COUNTER_9,0)) COUNTER_9
      ,SUM(ISNULL(COUNTER_10,0)) COUNTER_10
      ,SUM(ISNULL(COUNTER_11,0)) COUNTER_11
      ,SUM(ISNULL(COUNTER_12,0)) COUNTER_12
      ,SUM(ISNULL(COUNTER_13,0)) COUNTER_13
      ,SUM(ISNULL(COUNTER_14,0)) COUNTER_14
      ,SUM(ISNULL(COUNTER_15,0)) COUNTER_15
      ,SUM(ISNULL(COUNTER_16,0)) COUNTER_16
      ,SUM(ISNULL(COUNTER_17,0)) COUNTER_17
      ,SUM(ISNULL(COUNTER_18,0)) COUNTER_18
      ,SUM(ISNULL(COUNTER_19,0)) COUNTER_19
      ,SUM(ISNULL(COUNTER_20,0)) COUNTER_20
      ,SUM(ISNULL(COUNTER_21,0)) COUNTER_21
      ,SUM(ISNULL(COUNTER_22,0)) COUNTER_22
      from NETHOUSE.SDR_DYN_SEQ_TCP_PERFORMANCE_1DAY_%months TCP,
      DIM_LOC_PGW DIM
      WHERE TCP.DIM_4 in (6)
      AND TCP.DIM_3 = dim.PGW_ID
      AND DIM.PGW_NAME is not null
      AND TCP.STARTTIME >= %st_date
      AND TCP.STARTTIME < %end_date
      GROUP BY DIM.PGW_NAME
      ORDER BY DIM.PGW_NAME
      ;COMMIT;"
}

sql$FAILURES <- function() {
      "SELECT DATE_TIME,HOST,FAILASCRIPTION_EN AS CAUSE_CATEGORY,FAILSCENE_EN AS SCENARIO,PROTOCOL,FAILCAUSE_EN AS FAILURE_CAUSE,FAILURE_TIMES
      FROM
      (
      SELECT DATEFORMAT(DATEADD(SS,STARTTIME,'19700101 02:00:00'),'YYYY-MM-DD') AS DATE_TIME,
      SITE_NAME AS HOST,499 AS ERROR_CODE,
      SUM(ERROR_CODE_4XX_TIMES) AS FAILURE_TIMES
      FROM PS.SDR_WEB_WEBSITE_PAGE_DSP_FAIL_15MIN_%days
      WHERE UPPER(SITE_NAME) IN ('WWW.GOOGLE.COM','WWW.GOOGLE.DK','GOOGLE.COM','GOOGLE.DK','WWW.GOOGLE.DE','WWW.YOUTUBE.COM','YOUTUBE.COM',
      'YOUTUBE.DK','WWW.FACEBOOK.COM','WWW.FACEBOOK.DK','EKSTRABLADET.DK','LOGIN.LIVE.COM','G.LIVE.COM','POS.MEAL4U.DK','STOKERCLOUD.DK',
      'NYHEDER.TV2.DK','WWW.PORNHUB.COM','WWW.XNXX.COM')
      GROUP BY DATE_TIME,HOST
      UNION ALL
      SELECT DATEFORMAT(DATEADD(SS,STARTTIME,'19700101 02:00:00'),'YYYY-MM-DD') AS DATE_TIME,
      SITE_NAME AS HOST,599 AS ERROR_CODE,
      SUM(ERROR_CODE_5XX_TIMES) AS FAILURE_TIMES
      FROM PS.SDR_WEB_WEBSITE_PAGE_DSP_FAIL_15MIN_%days
      WHERE UPPER(SITE_NAME) IN ('WWW.GOOGLE.COM','WWW.GOOGLE.DK','GOOGLE.COM','GOOGLE.DK','WWW.GOOGLE.DE','WWW.YOUTUBE.COM','YOUTUBE.COM',
      'YOUTUBE.DK','WWW.FACEBOOK.COM','WWW.FACEBOOK.DK','EKSTRABLADET.DK','LOGIN.LIVE.COM','G.LIVE.COM','POS.MEAL4U.DK','STOKERCLOUD.DK',
      'NYHEDER.TV2.DK','WWW.PORNHUB.COM','WWW.XNXX.COM')
      GROUP BY DATE_TIME,HOST
      UNION ALL
      SELECT DATEFORMAT(DATEADD(SS,STARTTIME,'19700101 02:00:00'),'YYYY-MM-DD') AS DATE_TIME,
      SITE_NAME AS HOST,1001 AS ERROR_CODE,
      SUM(ERROR_CODE_1001_TIMES) AS FAILURE_TIMES
      FROM PS.SDR_WEB_WEBSITE_PAGE_DSP_FAIL_15MIN_%days
      WHERE UPPER(SITE_NAME) IN ('WWW.GOOGLE.COM','WWW.GOOGLE.DK','GOOGLE.COM','GOOGLE.DK','WWW.GOOGLE.DE','WWW.YOUTUBE.COM','YOUTUBE.COM',
      'YOUTUBE.DK','WWW.FACEBOOK.COM','WWW.FACEBOOK.DK','EKSTRABLADET.DK','LOGIN.LIVE.COM','G.LIVE.COM','POS.MEAL4U.DK','STOKERCLOUD.DK',
      'NYHEDER.TV2.DK','WWW.PORNHUB.COM','WWW.XNXX.COM')
      GROUP BY DATE_TIME,HOST
      UNION ALL
      SELECT DATEFORMAT(DATEADD(SS,STARTTIME,'19700101 02:00:00'),'YYYY-MM-DD') AS DATE_TIME,
      SITE_NAME AS HOST,1002 AS ERROR_CODE,
      SUM(ERROR_CODE_1002_TIMES) AS FAILURE_TIMES
      FROM PS.SDR_WEB_WEBSITE_PAGE_DSP_FAIL_15MIN_%days
      WHERE UPPER(SITE_NAME) IN ('WWW.GOOGLE.COM','WWW.GOOGLE.DK','GOOGLE.COM','GOOGLE.DK','WWW.GOOGLE.DE','WWW.YOUTUBE.COM','YOUTUBE.COM',
      'YOUTUBE.DK','WWW.FACEBOOK.COM','WWW.FACEBOOK.DK','EKSTRABLADET.DK','LOGIN.LIVE.COM','G.LIVE.COM','POS.MEAL4U.DK','STOKERCLOUD.DK',
      'NYHEDER.TV2.DK','WWW.PORNHUB.COM','WWW.XNXX.COM')
      GROUP BY DATE_TIME,HOST
      UNION ALL
      SELECT DATEFORMAT(DATEADD(SS,STARTTIME,'19700101 02:00:00'),'YYYY-MM-DD') AS DATE_TIME,
      SITE_NAME AS HOST,1003 AS ERROR_CODE,
      SUM(ERROR_CODE_1003_TIMES) AS FAILURE_TIMES
      FROM PS.SDR_WEB_WEBSITE_PAGE_DSP_FAIL_15MIN_%days
      WHERE UPPER(SITE_NAME) IN ('WWW.GOOGLE.COM','WWW.GOOGLE.DK','GOOGLE.COM','GOOGLE.DK','WWW.GOOGLE.DE','WWW.YOUTUBE.COM','YOUTUBE.COM',
      'YOUTUBE.DK','WWW.FACEBOOK.COM','WWW.FACEBOOK.DK','EKSTRABLADET.DK','LOGIN.LIVE.COM','G.LIVE.COM','POS.MEAL4U.DK','STOKERCLOUD.DK',
      'NYHEDER.TV2.DK','WWW.PORNHUB.COM','WWW.XNXX.COM')
      GROUP BY DATE_TIME,HOST
      UNION ALL
      SELECT DATEFORMAT(DATEADD(SS,STARTTIME,'19700101 02:00:00'),'YYYY-MM-DD') AS DATE_TIME,
      SITE_NAME AS HOST,1004 AS ERROR_CODE,
      SUM(ERROR_CODE_1004_TIMES) AS FAILURE_TIMES
      FROM PS.SDR_WEB_WEBSITE_PAGE_DSP_FAIL_15MIN_%days
      WHERE UPPER(SITE_NAME) IN ('WWW.GOOGLE.COM','WWW.GOOGLE.DK','GOOGLE.COM','GOOGLE.DK','WWW.GOOGLE.DE','WWW.YOUTUBE.COM','YOUTUBE.COM',
      'YOUTUBE.DK','WWW.FACEBOOK.COM','WWW.FACEBOOK.DK','EKSTRABLADET.DK','LOGIN.LIVE.COM','G.LIVE.COM','POS.MEAL4U.DK','STOKERCLOUD.DK',
      'NYHEDER.TV2.DK','WWW.PORNHUB.COM','WWW.XNXX.COM')
      GROUP BY DATE_TIME,HOST
      UNION ALL
      SELECT DATEFORMAT(DATEADD(SS,STARTTIME,'19700101 02:00:00'),'YYYY-MM-DD') AS DATE_TIME,
      SITE_NAME AS HOST,1000 AS ERROR_CODE,
      SUM(ERROR_TIME_OUT_TIMES) AS FAILURE_TIMES
      FROM PS.SDR_WEB_WEBSITE_PAGE_DSP_FAIL_15MIN_%days
      WHERE UPPER(SITE_NAME) IN ('WWW.GOOGLE.COM','WWW.GOOGLE.DK','GOOGLE.COM','GOOGLE.DK','WWW.GOOGLE.DE','WWW.YOUTUBE.COM','YOUTUBE.COM',
      'YOUTUBE.DK','WWW.FACEBOOK.COM','WWW.FACEBOOK.DK','EKSTRABLADET.DK','LOGIN.LIVE.COM','G.LIVE.COM','POS.MEAL4U.DK','STOKERCLOUD.DK',
      'NYHEDER.TV2.DK','WWW.PORNHUB.COM','WWW.XNXX.COM')
      GROUP BY DATE_TIME,HOST
      )A
      LEFT JOIN PS.DIM_FAILCAUSE_INFO  DIM
      ON A.ERROR_CODE=DIM.FAILCAUSE_ID AND DIM.SERVICETYPE_ID=2 AND DIM.FAILTYPE_ID=25
      ORDER BY FAILURE_CAUSE,FAILURE_TIMES DESC;
      ;COMMIT;"
}
