#Google Analytics Tutorial
require(RGoogleAnalytics)
library(RGA)
library(lubridate)
library(ggplot2)
library(shiny)
#Get API secret and API key 
client_id <- "382868859178-4r1399o1pf149nug2hsa74l1do92qtve.apps.googleusercontent.com"
client_secret <-"oKxAIi6dOmwS8pC4P1rtOyMt"
view_ID <- "ga:107814308"
#Generate the Token
token <- Auth(client_id,client_secret)
#Save the Token in a file
save(token,file="./token_analytics")

DIMENSIONS <- setNames(as.list(c("ga:medium","ga:source","ga:userType","ga:sessionCount","ga:userGender",
                                 "ga:userAgeBracket","ga:socialNetwork")),c("Medium","Source","User Type","Session Count","Gender","Age Range",
                                                                            "Social Network"))
METRICS <- setNames(as.list(c("ga:sessions","ga:pageviews","ga:users","ga:newUsers","ga:sessionsPerUser","ga:percentNewSessions",
                              "ga:bounceRate","ga:avgSessionDuration","ga:hits","ga:organicSearches")),
                    c("Sessions","Page Views","Users","New Users","Sessions per User","Percent New Sessions",
                      "Bounce Rate","Average Session Duration","Hits","Organic Searches"))
MAX_RESULTS <- 10000

produce_query <- function(start_date,end_date,dimensions,metrics,sort,token){
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
generate_metric_string <- function(metrics,dict,type="dimensions"){
  #We have a list of selected metric
  values <- sapply(metrics, function(next_val){
    if(!is.null(dict[[next_val]])){
      dict[[next_val]]
    }
  })
  values <- values[!sapply(values, is.null)]
  values <- paste(values,collapse = ",")
  if(type == "dimensions"){
    #Check that dates is not empty
    if(values ==''){
      return("ga:date")
    }
    values <- str_c(values,"ga:date",sep = ",")
    return(values)
  }
  return(values)
}

#'@description function that returns a list of ggplot objects for the User type of Graph
#'
plot_userdata <- function(dataframe,date_range,metrics){
  #Define how to order the x axis based on the date
  difference_days <- (date_range[[2]] - date_range[[1]])[[1]]
  #If its less than 60 days work by weeks
  if(difference_days  < 60){
    
  }
  else{
    months<- lapply(dataframe$date,function(date){
      ymd(date)
    })
    #Add a month column
    dataframe$month <- lapply(months,function(next_month){
      month(next_month)
    })
    #Name it as a factor
    users_data$time <- factor(users_data$month,levels = as.character(1:12),
                               labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"),
                               ordered=TRUE)
  }
  #Find categorical variables that our users want
  if("Age Range" %in% metrics || "User Type" %in% metrics){
    #we will have two plots
    graph1 <- ggplot(dataframe,aes(x=month,y=))
  }
}
#Future Sessions we have to Validate Token
ValidateToken(token)

