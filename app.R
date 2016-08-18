library(shiny)
library(shinydashboard)
source("analytics.R")
# Add "messages" tool to dashboard
messageData <- data.frame(from = c("Administrator", "New User", "Support"), 
                          message = c("Sales are steady this month.", 
                                      "How do I register?",
                                      "The new server is ready."),
                          stringsAsFactors = FALSE)


ui <- dashboardPage(
  # change dashboard colour to green
  skin= "green",
  dashboardHeader(title = "Marketing Dashboard",
                  dropdownMenuOutput("messageMenu")),
  dashboardSidebar(
    sidebarMenu(
      tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
      uiOutput("userpanel"),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Facebook Campaigns", tabName = "facebook_campaigns", icon = icon("facebook-square")),
      menuItem("Google Trends", tabName = "google_trends", icon = icon("line-chart")),
      menuItem("Analytics", tabName = "analytics", icon = icon("tasks")),
      menuItem("Market Research", tabName = "market_research", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      # create dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                h1(strong("Dashboard"), align = "center"),
                br(),
                p("some text about what our app does", align = "center"),
                br(),
                box(title = strong("Facebook Campaigns"),
                    status = "info",
                    solidHeader = TRUE,
                    p("some text about what facebook campaigns does")),
                box(title = strong("Google Trends"),
                    status = "info",
                    solidHeader = TRUE,
                    p("some text about what google trends does")),
                box(title = strong("Analytics"),
                    status = "info",
                    solidHeader = TRUE,
                    p("some text about what analytics does")),
                box(title = strong("Market Research"),
                    status = "info",
                    solidHeader = TRUE,
                    p("some text about what market research was"))
              )
      ),
      # create facebook campaigns tab
      tabItem(tabName = "facebook_campaigns",
              fluidRow(
                h1(strong("Facebook Campaigns"), align = "center")
              )
      ),
      tabItem(tabName = "google_trends",
              fluidRow(
                h1(strong("Google Trends"), align  = "center"),
                br()
              )
      ),
      # create analytics tab
      tabItem(tabName = "analytics",
              fluidPage(
                h1(strong("Analytics"), align = "center"),
                fluidRow(
                  column(4, wellPanel(
                    h4(strong("Domain: www.example.com")),
                    # create drop down menu
                    selectInput("input_type", "Type:",
                                c("Users",
                                  "Sessions",
                                  "Traffic Sources")
                    ),
                    submitButton(text = "Refresh"),
                    # create metrics based on type selected
                    uiOutput("ui"),
                    # create date range
                    dateRangeInput("inDateRange", "Date Range Input:"),
                    submitButton("Search")
                  )
                  )
                  ,column(8,tabsetPanel(type = "tabs",
                                        tabPanel("Plot 1",plotOutput("plot1")),
                                        tabPanel("Plot 2", plotOutput("plot2")),
                                        tabPanel("Table", DT::dataTableOutput("table")))
                )
              )
      ),
      # create market research tab
      tabItem(tabName = "market_research",
              fluidRow(
                h1(strong("Market Research"), align = "center")
              )
      )
    )
)
)
)

server <- function(input, output, session) {
  output$value <- renderPrint({ input$input_type})
  output$messageMenu <- renderMenu({
    msgs <- apply(messageData, 1, function(row) {
      messageItem(
        from = row[["from"]],
        message = row[["message"]],
        icon = icon("envelope")
      )
    })
    
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  observe({
    c_label <- input$control_label
    s_options <- list()
    s_options[[paste("Users")]] <- paste0("Users")
    s_options[[paste("Sessions")]] <- paste0("Sessions")
    s_options[[paste("Traffic Sources")]] <- paste0("Traffic Sources")
    updateSelectInput(session, "input_type",
                      choices = s_options,
                      selected = paste0()
    )
  })
  #Function to download the csv file
  output$downloadData <- downloadHandler(
    filename = "dataset.csv",
    content = function(output$table) {
      write.csv(datasetInput(), file)
    }
  )
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    switch(input$input_type,
           "Users" = checkboxGroupInput("checkbox_metrics","Metrics:",
                                        choices = c("Users" = "Users",
                                                    "Age Range" = "Age Range",
                                                    "Gender" = "Gender",
                                                    "% New Sessions" = "% New Sessions",
                                                    "Sessions per User" = "Sessions per User",
                                                    "User Type" = "User Type",
                                                    "New Users" = "New Users",
                                                    "Session Count" = "Session Count"),
                                        selected = "Sessions per User"),
           "Sessions" = checkboxGroupInput("checkbox_metrics", "Metrics:",
                                           choices = c("Sessions" = "Sessions",
                                                       "Medium" = "Medium",
                                                       "Source" = "Source",
                                                       "Hits" = "Hits",
                                                       "Bounce Rate" = "Bounce Rate",
                                                       "Users" = "Users",
                                                       "Average Session Duration" = "Average Session Duration",
                                                       "Age Range" = "Age Range",
                                                       "Gender" = "Gender"),
                                           selected = "Hits"),
           "Traffic Sources" = checkboxGroupInput("checkbox_metrics", "Metrics:",
                                                  choices = c("Source" = "Source",
                                                              "Medium" ="Medium",
                                                              "Social Network" = "Social Network",
                                                              "Source Medium" = "Source Medium",
                                                              "Organic Searches" = "Organic Searches",
                                                              "Sessions" = "Sessions",
                                                              "Users" = "Users"
                                                  ),
                                                  selected = "Medium")
    )
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    dim_str <- generate_metric_string(metrics = (input$checkbox_metrics),dict = DIMENSIONS,type="dimensions")
    metrics_str <- generate_metric_string(metrics = (input$checkbox_metrics),dict = METRICS,type = "metrics")
    table <- produce_query(start_date = "2015-09-01",
                           end_date ="2016-06-06",
                           dimensions = dim_str,
                           metrics = metrics_str,
                           sort = "-ga:date",
                           token = token
    )
    return(table)
    }
  ))
  
  output$plot2 <- renderPlot({
    dim_str <- generate_metric_string(metrics = (input$checkbox_metrics),dict = DIMENSIONS,type="dimensions")
    metrics_str <- generate_metric_string(metrics = (input$checkbox_metrics),dict = METRICS,type = "metrics")
    table <- produce_query(start_date = "2015-09-01",
                          end_date ="2016-06-06",
                          dimensions = dim_str,
                          metrics = metrics_str,
                          sort = "-ga:date",
                          token = token
    )
    months<- lapply(table$date,function(date){
      ymd(date)
    })
    #Create a column with the months
    table$month <- lapply(table$month,function(next_month){
      month(next_month)
    })
    #Name each month
# #     table$month<- factor(table$month,levels = as.character(1:12),
# #                                labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"),
# #                                ordered=TRUE)
#     print(sapply(table,class))
    #aggregated <- aggregate(table$newUsers,by=list(Category=table$month), FUN = sum)
    #graph <- ggplot(aggregated, aes(x=Category,y=x)) + geom_point()
    plot(table$date,table$newUsers)
  })
  
  observe({
    c_num <- input$control_num
    updateDateRangeInput(session, "inDateRange",
                         label = paste("Date Range:"),
                         start = paste("2016-08-", c_num, sep=""),
                         end = paste("2016-12-", c_num, sep=""),
                         min = paste("2015-09-", c_num, sep=""),
                         max = paste("2016-09-", c_num, sep="")
    )})
}

shinyApp(ui, server)