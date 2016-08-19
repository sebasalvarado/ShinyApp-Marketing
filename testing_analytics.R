#Google Analytics Tutorial
require(RGoogleAnalytics)
library(RGA)
library(lubridate)
library(ggplot2)
library(shiny)
library(xts)
#Get API secret and API key 
client_id <- "382868859178-4r1399o1pf149nug2hsa74l1do92qtve.apps.googleusercontent.com"
client_secret <-"oKxAIi6dOmwS8pC4P1rtOyMt"
view_ID <- "ga:107814308"
#Generate the Token
token <- Auth(client_id,client_secret)
#Save the Token in a file
save(token,file="./token_analytics")

DIMENSIONS <- setNames(as.list(c("ga:medium","ga:source","ga:sessions","ga:userType","ga:sessionCount","ga:userGender",
                                   "ga:userAgeBracket","ga:socialNetwork")),c("Medium","Source","Sessions","User Type","Session Count","Gender","Age Range",
                                                                              "Social Network"))
METRICS <- setNames(as.list(c("ga:pageviews","ga:users","ga:newUsers","ga:sessionsPerUser","ga:percentNewSessions",
                              "ga:bounceRate","ga:avgSessionDuration","ga:hits","ga:organicSearches")),
                    c("Page Views","Users","New Users","Sessions Per User","Percent New Sessions",
                      "Bounce Rate","Average Session Duration","Hits","Organic Searches"))
MAX_RESULTS <- 10000

produce_query <- function(input,output,session,start_date,end_date,dimensions,metrics,sort,token){
  init_query <- Init(start.date=start_date, end.date = end_date, dimensions=dimensions,
                     metrics=metrics, max.results=MAX_RESULTS,
                     sort=sort, table.id = view_ID)
  final_query <- QueryBuilder(init_query)
  final_data <- GetReportData(final_query,token,delay=2)
  return(final_data)
}

#'@description Function that takes all the strings that our user selected and forms the
#'string that we can pass to our query producer
#'
#'
generate_metric_string <- function(input,output,session,metrics,dict){
  #We have a list of selected metric
  values <- sapply(metrics, function(next_val){
    if(!is.null(dict[[next_val]])){
      dict[[next_val]]
    }
  })
  values <- values[!sapply(values, is.null)]
  values <- paste(values,collapse = ",")
  return(values)
}

#Future Sessions we have to Validate Token
ValidateToken(token)

#Example
#Metrics:sessions,pageviews,bounces
#Sort: date, browser
#Initial Query
init_query <- Init(start.date = "2015-09-01",end.date = "2016-05-05",dimensions = "ga:date,ga:pagePath,ga:hour,ga:medium",
              metrics = "ga:sessions,ga:pageviews",
              max.results = 5000,
              sort = "-ga:date",
              table.id = view_ID)

#Create the Query Builder
final_query <- QueryBuilder(init_query)
#Extract the data and have it as Data Frame
final_data <- GetReportData(final_query,token,split_daywise = T,delay=5)

INIT_DATE <- "2015-09-01"
FINISH_DATE <- "2016-08-16"

#Type: Users
users_data <- produce_query(INIT_DATE,FINISH_DATE,dimensions="ga:date,ga:userType,ga:sessionCount",
                            metrics="ga:users,ga:newUsers,ga:sessionsPerUser,ga:percentNewSessions",
                            sort="-ga:date",
                            token)

#Type: Sessions
sessions_data <- produce_query(INIT_DATE,FINISH_DATE,
                                     "ga:date,ga:medium,ga:source",
                                     "ga:sessions,ga:bounceRate,ga:avgSessionDuration,ga:hits,ga:users",
                                     "-ga:date",token)

#Type:Traffic Sources
#Potentially take out keyword to hard to analyze now
traffic_source_data <- produce_query(INIT_DATE,FINISH_DATE,
                                           dimensions = "ga:date,ga:source,ga:medium,ga:socialNetwork,ga:sourceMedium",
                                           metrics = "ga:organicSearches,ga:sessions,ga:users",
                                           sort = "-ga:date",
                                           token)
#Create a function that sc
#Type: Audiences
audiences_data <- produce_query(INIT_DATE,FINISH_DATE,
                                dimensions="ga:date,ga:source,ga:userAgeBracket,ga:userGender,ga:medium",
                                metrics = "ga:sessions,ga:users,ga:sessionsPerUser,ga:percentNewSessions",
                                sort="-ga:date",
                                token)


#Plotting our Data:Using ggplot
months<- ymd(users_data$date)
as.Date()
users_data$date <- months
#Create a column with the months
users_data$month <- lapply(months,function(next_month){
  month(next_month)
})
#Name each month
users_data$month <- factor(users_data$month,levels = as.character(1:12),
                           labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"),
                           ordered=TRUE)



##DATA VISUALIZATION EXAMPLES
users_data <- users_data[,c(-2,-6,-7)]
View(users_data)
sapply(users_data,class)
users_data$sessionCount <- as.numeric(users_data$sessionCount)
aggregated <- aggregate(.~date,data=users_data,sum,drop=FALSE)
apply.monthly(users_data,sum)
#AGGREGATING DATA
example <- aggregate(month~.,data=users_data,sum,drop=FALSE)#AGGREGATE TU SUM COLUMNS
aggregated <- aggregate(users~month,data = users_data,sum, drop=FALSE)
agg2 <- aggregate(newUsers~month,data = users_data,sum, drop=FALSE)
total <- merge(aggregated,agg2,by="month")

#USING MELT
melt_agg <- melt(aggregated,id = c("month"))
melt_agg$value <- as.numeric(melt_agg$value)
melted <- melt(id = "month",total)
melted <- melted %>% filter(variable != '')

#PLOTTING THE DATA
#Plot melted data
ggplot(melted,aes(x=month,y=value)) + geom_point(aes(color=variable))
#Plot Sessions vs month:Creating a x axis for one year
ggplot(users_data, aes(x=month,y=newUsers)) + geom_bar(stat="identity")
ggplot(users_data, aes(x=month,y=newUsers)) + geom_line() + geom_point()
ggplot(aggregated,aes(x=Category,y=x)) + geom_point() + geom_line()

#Improve the graph
ggplot(melted,aes(month,value)) + geom_bar(aes(color=variable),stat="identity") + facet_wrap(~variable,ncol=1)
ggplot(melt_agg,aes(month,value)) + geom_bar(aes(color=variable),stat="identity") + facet_wrap(~variable,ncol=1)

ggplot(melt_agg,aes(month,value)) + geom_point(aes(color=variable))
ggplot(users_data, aes(x=month,y=users)) + geom_bar(stat="identity") + facet_wrap(~userType,ncol=1)
ggplot(users_data, aes(x=month,y=users)) + geom_bar(stat="identity") + facet_wrap(~userType,ncol=1)

#Make the graph with colours

#Scatter Plot
theme_new <- theme_bw() + theme(plot.background=element_rect(size=1, color="blue",fill="black"),text=element_text(size=12, family= "Serif",color="ivory"),axis.text.y=element_text(colour = "purple"))
ggplot(melt_agg,aes(x=month,value,group=variable,color=variable)) + geom_point() + geom_line() + facet_wrap(~variable,ncol = 1) + theme_bw()

ggplot(melted, aes(month)) +
  geom_bar() +
  facet_wrap(~variable, scales="free")

