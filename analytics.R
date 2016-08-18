#Google Analytics Tutorial
require(RGoogleAnalytics)
library(RGA)
library(lubridate)
library(ggplot2)
library(shiny)
library(reshape2)
#Get API secret and API key 
client_id <- "382868859178-4r1399o1pf149nug2hsa74l1do92qtve.apps.googleusercontent.com"
client_secret <-"oKxAIi6dOmwS8pC4P1rtOyMt"
view_ID <- "ga:107814308"
#Generate the Token
token <- Auth(client_id,client_secret)
#Save the Token in a file
save(token,file="./token_analytics")

CATEGORIZATION <- list("Categorical" = c("Age Range","User Type","Gender","Medium","Source","Social Network","Source Medium"),
                       "Numerical" = c("Users","Sessions per User","New Users","Session Count","Sessions","Hits"),
                       "Percent" = c("Percent New Sessions","Bounce Rate"),
                       "Time" = c("Average Session Duration"))
DIMENSIONS <- setNames(as.list(c("ga:medium","ga:source","ga:userType","ga:sessionCount","ga:userGender",
                                 "ga:userAgeBracket","ga:socialNetwork")),c("Medium","Source","User Type","Session Count","Gender","Age Range",
                                                                            "Social Network"))
METRICS <- setNames(as.list(c("ga:sessions","ga:pageviews","ga:users","ga:newUsers","ga:sessionsPerUser","ga:percentNewSessions",
                              "ga:bounceRate","ga:avgSessionDuration","ga:hits","ga:organicSearches")),
                    c("Sessions","Page Views","Users","New Users","Sessions per User","Percent New Sessions",
                      "Bounce Rate","Average Session Duration","Hits","Organic Searches"))
MAX_RESULTS <- 10000
#Future Sessions we have to Validate Token
ValidateToken(token)
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

#'@description Given a vector/list of Metrics that the user requests divide the metrics
#' into: Categorical, Numerical or Percent to make plotting easier
#'
partition_metrics <- function(metrics){
  final_list <- list()
  categorical <- CATEGORIZATION$Categorical
  #Get the categorical variables we searched for
  cat_metrics <- ifelse(metrics %in% categorical, metrics, NA)
  cat_metrics <- cat_metrics[!is.na(cat_metrics)]
  final_list[["Categorical"]] <- cat_metrics
  #Find the numerical variables in our metrics
  numerical <- CATEGORIZATION$Numerical
  num_metrics <- ifelse(metrics %in% numerical, metrics,NA)
  num_metrics <- num_metrics[!is.na(num_metrics)]
  final_list[["Numerical"]] <- num_metrics
  return(final_list)
}


#'@description function that returns a list of ggplot objects for the User type of Graph
#'@params dataframe, it must be a tidy dataframe because the function makes it long
#'@return List of graph objects to plot
#'
generate_plots<- function(dataframe,list_metrics,date_range){
  #Find the date Range
  difference_days <- (date_range[[2]] - date_range[[1]])[[1]]
  #If its less than 60 days work by weeks
  if(difference_days  < 60){
    #Add a WeekNum to the dataframe
    dataframe$WeekNum <- as.numeric(format(train$date+3,"%U"))
  }
  else{
    #Add a Months Column in my Dataframe
    dataframe$month <- lapply(months,function(next_month){
      month(next_month)
    })
    #Name it as a factor
    dataframe$time <- factor(dataframe$month,levels = as.character(1:12),
                               labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"),
                               ordered=TRUE)
  }
  #Melt the dataframe to make it long
  dataframe <- met(id = 'date',dataframe)
  #Delete the ones where variable is empty
  dataframe <- dataframe %>% filter(variable != '')
  #Know how many categorical variables we have 
}