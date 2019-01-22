setwd("C:/Users/saurabh/Desktop/Thesis/CODES")
#Initialising Required Libraries
library(fpc)
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(tm)
library(ggpubr)
library(RODBC)
library(VIM)
library(animation)


#Establishing Oracle Connection string
channel <-
  odbcDriverConnect(connection = "Driver={Oracle in OraDb11g_home1};Server=NCI; uid=thesis; pwd=Password;", believeNRows = FALSE)

#dataset loading from csv to R
df_total = data.frame()
datasets = list(
  "Apppackages20162",  "Apppackages20163",
  "Apppackages20164",  "Apppackages20171",
  "Apppackages20172",  "Apppackages20173",
  "Apppackages20174")
for (i in datasets)
{
  print(i)
  dat <-
    read.table(
      file = paste('Datasets/', i, '.tsv', sep = ''),
      quote = "",
      sep = '\t',
      header = FALSE,
      na.strings = 'NULL',
      stringsAsFactors = FALSE
    )
  dat$V8 <- ifelse(is.na(dat$V8), dat$V2, dat$V8)
  dat$V2 <-
    as.POSIXct((dat$V2) / 1000, tz = "UTC", origin = "1970-01-01")
  dat$V8 <-
    as.POSIXct((dat$V8) / 1000, tz = "UTC", origin = "1970-01-01")
  #dat$V4 <- str_trim(gsub("[0-9]", "", str_trim(dat$V4)))
  dat$V6 <- str_trim(gsub("[^a-zA-Z\\.\\-]", "", str_trim(dat$V6)))
  dat$V6 <- ifelse(dat$V6 == "" | is.na(dat$V6), dat$V4, dat$V6)
  dat$V3[is.na(dat$V3)] <- 'Check'
  dat$V5[is.na(dat$V5)] <- 0
  colnames(dat) <-
    c(
      "USERID",      "UUID",
      "ACTION",      "APPPACKAGE",
      "APPUID",      "APP_NAME",
      "INSTALLSRC",  "INSTALLTIME",
      "PACKAGEHASH", "PERMISSIONS",
      "VERSIONNAME", "VERSIONCODE",
      "SHERLOCK_VERSION"
     )
  df_total <- rbind(df_total, dat)
}
df_total <- df_total[-c(7, 9:13)]

summary(df_total)
#loading dataset to oracle sql server
sqlSave(
  channel,
  df_total,
  "APP_USAGE",
  append = TRUE,
  rownames = FALSE,
  varTypes = c(UUID = "TIMESTAMP(6)", APPUID = "NUMBER(38,0)", INSTALLTIME = "TIMESTAMP(6)")
)

#deriving application list from dataset
df_apps = data.frame()
for (i in datasets)
{
  print(i)
  dat <-
    read.table(
      file = paste('Datasets/', i, '.tsv', sep = ''),
      quote = "",
      sep = '\t',
      header = FALSE,
      na.strings = 'NULL',
      stringsAsFactors = FALSE
    )
  dat$V6 <-
    str_trim(gsub("[^a-zA-Z\\.\\-\\+]", "", str_trim(dat$V6)))
  dat <- dat[-c(1:3, 7:13)]
  colnames(dat) <-
    c("APPPACKAGE",
      "APPUID",
      "APP_NAME")
  df_apps <- rbind(df_apps, dat)
}

#loading application list to sql table
sqlSave(channel,
        df_apps,
        "APPS_LIST",
        append = TRUE,
        rownames = FALSE)

#deleting unwanted rows
flg <- sqlQuery(
  channel,
  "DELETE FROM apps_list WHERE ROWID IN ( SELECT row_id
  FROM ( SELECT row_id,rank_n FROM (SELECT RANK() OVER(
  PARTITION BY apppackage
  ORDER BY ROWID
  ) rank_n,ROWID AS row_id
  FROM apps_list
  WHERE apppackage IN ( SELECT DISTINCT apppackage
  FROM apps_list
  GROUP BY apppackage,app_name
  HAVING COUNT(apppackage) >= 1
  )))
  WHERE rank_n > 1)"
  )

#calculating retention period from the dataset
flg <- sqlQuery(
  channel,
  "CREATE TABLE retentions AS
  WITH dt AS (SELECT userid, apppackage,
  installtime, action, RANK() OVER
  (PARTITION BY apppackage,userid ORDER BY uuid DESC) rnk
  FROM app_usage) SELECT ua.userid,
  ua.apppackage, (CASE dt.action WHEN 'Removed' THEN cast('1-JAN-2016' as timestamp)
  ELSE dt.installtime END) AS start_date,
  (CASE dt.action WHEN 'Removed' THEN dt.installtime
  ELSE cast('31-JAN-2018' as timestamp) END) AS end_date,
  (CASE dt.action WHEN 'Removed' THEN dt.installtime
  ELSE cast('31-JAN-2018' as timestamp) END)
  - (CASE dt.action WHEN 'Removed' THEN cast('1-JAN-2016' as timestamp)
  ELSE dt.installtime END) AS periods FROM dt,
  app_usage ua WHERE dt.userid = ua.userid
  AND dt.apppackage = ua.apppackage
  AND dt.rnk = 1 GROUP BY ua.apppackage,
  ua.userid, (CASE dt.action WHEN 'Removed'
  THEN dt.installtime ELSE cast('31-JAN-2018' as timestamp) END),(CASE dt.action WHEN 'Removed' THEN cast('1-JAN-2016' as timestamp)
  ELSE dt.installtime END)",
  errors = TRUE
)


#creating table to store app information
flg <- sqlQuery(
  channel,
  "create table app_stats as
  select apppackage
  , ceil(sum( extract (day from (periods)) * 86400
  + extract (hour from (periods)) *3600
  + extract (minute from (periods))*60
  + extract (second from (periods))
  ) / 86400) as retention_period
  , count(apppackage) as users
  from retentions
  group by apppackage",
  errors = TRUE
)

flg <- sqlQuery(
  channel,
  "declare
  column_exists exception;
  pragma exception_init (column_exists , -01430);
  begin
  execute immediate 'ALTER TABLE APP_STATS ADD CATEGORY VARCHAR2(255)';
  execute immediate 'ALTER TABLE APP_STATS ADD APP_NAME VARCHAR2(255)';
  execute immediate 'ALTER TABLE APP_STATS ADD RATINGS FLOAT(8)';
  exception when column_exists then null;
  end;",
  errors = TRUE
)


#play store dataset
play_store <-
  read.csv(
    file = "Datasets/googleplay.csv",
    header = TRUE,
    sep = ",",
    na.strings = "",
    stringsAsFactors = T
  )

sqlSave(
  channel,
  play_store,
  "PLAY_STORE",
  append = TRUE,
  rownames = FALSE,
  varTypes = c(RATINGS = "FLOAT(8)")
)

flg <- sqlQuery(
  channel,
  "update APP_STATS
  SET (category,app_name,ratings) = (select category, app_name, ratings from play_store where play_store.apppackage = app_stats.apppackage)
  where exists (select category, app_name, ratings from play_store where play_store.apppackage = app_stats.apppackage);",
  errors = TRUE
)

#########################################################
#Modeling Initialisation#
app_stats <- sqlFetch(channel, "APP_STATS")
app_stats <- app_stats[app_stats$CATEGORY != "Operating System", ]
app_stats$NRETENTION <- scale(app_stats$RETENTION_PERIOD)
app_stats$NUSERS <- scale(app_stats$USERS)
aggr(app_stats)
summary(app_stats[, 7:8])

#K cluster determining
wssplots <- function(app_stats, l, nc, seeds) {
  wss <- (nrow(app_stats) - 1) * sum(apply(app_stats, 2, var))
  for (i in 2:nc) {
    set.seed(seeds)
    wss[i] <- sum(kmeans(app_stats[, l], centers = i)$withinss)
  }
  plot(1:nc,
       wss,
       type = "b",
       xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
}
wssplots(app_stats, 2:3, 15, 111)
wssplots(app_stats, 6, 15, 220)

#K-Means Clustering
set.seed(1234)
for (i in 1:100)
{
  clusters <-
    kmeans(
      app_stats[, 7:8],
      centers = 5,
      iter.max = 500,
      nstart = 25
    )
}
app_stats$user_cluster <- as.factor(clusters$cluster)

#cluster analysis
str(clusters)
clusters

#cluster plot
ggplot(
  app_stats,
  aes(
    app_stats$RETENTION_PERIOD,
    app_stats$USERS,
    color = app_stats$user_cluster
  )
) + geom_point()


kmeans.ani(x = app_stats[, 2:3], centers = 5, hints = c("Move centers!", "Find cluster?"), pch = 1:5, col = 1:5)

set.seed(205)
for (i in 1:100)
{
  clusters_app <-
    kmeans(
      app_stats[, 6],
      centers = 5,
      iter.max = 500,
      nstart = 5
    )
}

app_stats$store_cluster <- as.factor(clusters_app$cluster)
str(clusters_app)
clusters_app

ggplot(
  app_stats,
  aes(
    app_stats$RATINGS,
    app_stats$store_cluster,
    color = app_stats$store_cluster
  )
) + geom_point()

app_stats$user_clusters <-
  factor(app_stats$user_cluster, labels = c(2, 5, 3, 1, 4))
app_stats$store_clusters <-
  factor(app_stats$store_cluster, labels = c(3, 4, 2, 5, 1))

###Output File Generation
write.table(
  app_stats,
  file = "Output/app_stats.csv",
  row.names = FALSE,
  na = "",
  col.names = TRUE,
  sep = ","
)

odbcClose(channel)
