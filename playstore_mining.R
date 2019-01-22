library(RCurl)
library(RJSONIO)
library(RODBC)
channel <-
  odbcDriverConnect(connection = "Driver={Oracle in OraDb11g_home1};Server=NCI; uid=thesis; pwd=Password;", believeNRows = FALSE)
apps_list <- sqlFetch(channel, "APPS_LIST", stringsAsFactors = FALSE)


####### Solution 1: Only 300 request can be served per Account;
####### After every 300 request new account has to be created on 42matters.com
df_aps <- data.frame()
API_token='9d7c85f07159cfecbb9ccea5cf164591e832fe97'
for (i in apps_list$APPPACKAGE)
{
  print(i)
  api_urls <-
    paste(
      'https://data.42matters.com/api/v2.0/android/apps/ranks.json?p=',
      i,
      '&access_token',API_token,'=&days=1&country=IL',
      sep = ''
    )
  raw_data <- fromJSON(getURL(api_urls))
  if (length(raw_data) != 0)
  {
    category_name <-
      ifelse(is.null(raw_data$ranks[39][[1]]$category_name),
             '',
             raw_data$ranks[39][[1]]$category_name)
    name <-
      ifelse(is.null(raw_data$title),
             '',
             raw_data$title)
    id <-
      ifelse(is.null(raw_data$package_name),
             '',
             raw_data$package_name)
    rank <-
      ifelse(
        is.null(raw_data$ranks[39][[1]]$positions[[1]]$position),
        0,
        raw_data$ranks[39][[1]]$positions[[1]]$position
      )
    ratings <-
      ifelse(is.null(raw_data$rating),
             0,
             raw_data$rating)
    data_ap <- data.frame(i, category_name, name, id, rank, ratings)
    colnames(data_ap) <-
      c("app", "CATEGORY", "NAME", "ID", "RANK", "RATINGS")
    df_aps <- rbind(df_aps, data_ap)
  }
}
write.table(df_aps, file = "playstore.csv",row.names=FALSE, na="",col.names=FALSE, sep=",",append = TRUE)

##### Solution 2: In this code only 5 request can be served in an hour; least preferred
i <- j <- 0
API_KEY='9GmLbwMLMgxz3BeAo3Two0NzeuRVN8Jin6EPcerWVfesQH4BhLuLeUSlETXfrnMd'
df_app = data.frame()
for (j in 1:ceiling(nrow(apps_list) / 5)) {
  for (i in (5 * (j - 1) + 1):(min(5 * j, nrow(apps_list)))) {
    print(apps_list$APPPACKAGE[i])
    ap <- apps_list$APPPACKAGE[i]
    api_url <-
      paste(
        'https://www.androidrank.org/api/application/',
        ap,
        '?key=',API_KEY,
        sep = ''
      )
    raw_data <- fromJSON(getURL(api_url))
    if (length(raw_data) != 0)
    {
      rnk <-
        ifelse(
          is.null(raw_data$category_rank$current[37][[1]][c('order')]),
          0,
          raw_data$category_rank$current[37][[1]][c('order')]
        )
      dat_app <-
        data.frame(raw_data$category, raw_data$name, raw_data$id, rnk)
      colnames(dat_app) <- c("CATEGORY", "NAME", "ID", "RANK")
      df_app <- rbind(df_app, dat_app)
    }
  }
  Sys.sleep(3600)
}

write.table(df_app, file = "playstore.csv",row.names=FALSE, na="",col.names=FALSE, sep=",", append = TRUE)
