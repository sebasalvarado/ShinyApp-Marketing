library(shiny)
library(stringi)
library(stringr)
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
      # create google trends tab
      tabItem(tabName = "google_trends",
              fluidPage(
                h1(strong("Google Trends"), align  = "center"),
                fluidRow(
                  column(4, wellPanel(
                    helpText("Give one or more terms that you want R to retrieve data from the Google Trends API.
                             Use commas to separate terms."),
                    textInput("terms","Terms:"),
                    
                    selectInput("geography", 
                                label = "Geography:",
                                choices = c("Worldwide","Afghanistan","Albania","Algeria","Angola","Argentina","Armenia","Australia","Austria",  "Azerbaijan","Bahamas","Bahrain","Bangladesh","Belarus","Belgium","Botswana", "Brazil","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Chad","Chile","China","Colombia","Cuba","Cyprus","Czech Republic","Denmark","Djibouti","Ecuador","Egypt","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Hong Kong","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Korea (North)","Korea (South)","Kuwait","Kyrgyzstan","Lebanon","Liberia","Libya","Macedonia","Madagascar","Malawi","Malaysia","Mali","Malta","Mexico","Morocco","Mozambique","Namibia","Nepal","Netherlands","New Zealand","Niger","Nigeria","Norway","Oman","Pakistan","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Romania","Russian Federation","Rwanda","Saudi Arabia","Senegal","Serbia","Sierra Leone","Singapore","Somalia","South Africa","Spain","Sudan","Swaziland","Sweden","Switzerland","Syria","Taiwan","Tajikistan","Tanzania","Thailand","Togo","Tunisia","Turkey","Turkmenistan","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uzbekistan","Venezuela","Viet Nam","Yemen","Zaire","Zambia","Zimbabwe"),
                                selected = "Worldwide"),
                    
                    selectInput("period", 
                                label = "Time Period:",
                                choices = c("2004-present",
                                            "Past30Days",
                                            "Past90Days",
                                            "Past12Months",
                                            "2011",
                                            "2012",
                                            "2013",
                                            "2014",
                                            "2015",
                                            "2016"),
                                selected = "2004-present"),
                    
                    checkboxInput("corr", 
                                  label = "Correlation"),
                    
                    tags$h1(submitButton("Update!")),
                    
                    helpText("To get results, click the 'Update!' button"),
                    
                    br(),
                    br()
                    )),
                  column(8, wellPanel(
                    plotOutput("myplot"),
                    br(),
                    plotOutput("myplot3"),
                    plotOutput("myplot2")
                    
                  ))
                ))),
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
                    submitButton(text = "Search")
                  )
                  )
                  ,column(8,tabsetPanel(type = "tabs",
                                        tabPanel("Plot 1",plotOutput("plot_1")),
                                        tabPanel("Plot 2", plotOutput("plot_2")),
                                        tabPanel("Table", tableOutput("table"))))
                )
              )
      ),
      # create market research tab
      tabItem(tabName = "market_research",
              fluidRow(
                h1(strong("Market Research"), align = "center")
              )
      )
    )))

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
  
  output$table <- renderTable({
    dim_str <- generate_metric_string(metrics = (input$checkbox_metrics),dict = DIMENSIONS,type="dimensions")
    metrics_str <- generate_metric_string(metrics = (input$checkbox_metrics),dict = METRICS,type = "metrics")
    table<- produce_query(start_date = "2015-09-01",
                          end_date ="2016-06-06",
                          dimensions = dim_str,
                          metrics = metrics_str,
                          sort = "-ga:date",
                          token = token
    )
    head(table,20)
  }
  )
  
  observe({
    c_num <- input$control_num
    updateDateRangeInput(session, "inDateRange",
                         label = paste("Date Range:"),
                         start = paste("2016-08-", c_num, sep=""),
                         end = paste("2016-12-", c_num, sep=""),
                         min = paste("2015-09-", c_num, sep=""),
                         max = paste("2016-09-", c_num, sep="")
    )})
  #server code for the google trends page
  gconnect('rlousta13@gmail.com','rafael13')
  
  out <- reactive({
    if(length(input$terms)>0){
      
      unlist(strsplit(input$terms,","))
    }
  })
  
  start_date<-reactive({
    
    if(input$period=="2004-present"){as.Date("2004-01-01")}
    
    else if (input$period=="Past90Days"){as.Date(Sys.time())-90}
    
    else if (input$period=="Past12Months"){
      m=as.POSIXlt(as.Date(Sys.time()))
      m$year=m$year-1
      m}
    
    else if (input$period=="2011"){as.Date("2011-01-01")}
    else if (input$period=="2012"){as.Date("2012-01-01")}
    else if (input$period=="2013"){as.Date("2013-01-01")}
    else if (input$period=="2014"){as.Date("2014-01-01")}
    else if (input$period=="2015"){as.Date("2015-01-01")}
    else if (input$period=="2016"){as.Date("2016-01-01")}
    
  })
  
  end_date<-reactive({
    
    if(input$period %in% c("2004-present",
                           "Past90Days","Past12Months"))
    {
      as.Date(Sys.time())}
    
    else if (input$period=="2011"){as.Date("2011-12-31")}
    else if (input$period=="2012"){as.Date("2012-12-31")}
    else if (input$period=="2013"){as.Date("2013-12-31")}
    else if (input$period=="2014"){as.Date("2014-12-31")}
    else if (input$period=="2015"){as.Date("2015-12-31")}
    else if (input$period=="2016"){as.Date(Sys.time())} 
    
  })
  
  geo<-reactive({
    if(input$geography=="Worldwide"){""}
    
    else{
      
      countries$CODE[countries$COUNTRY==input$geography]
    }
    
  })
  
  data<-reactive({
    if(length(out()>0))
    {
      
      out2<-gtrends(query=out(),start_date=start_date(),end_date=end_date(),geo=geo())
      
    }
    
  })
  
  output$myplot <- renderPlot({
    if(length(out()>0)){
      z=data()
      trend=z$trend
      
      if("end"%in%names(trend)==T)
      {
        trend=select(trend,-end)}
      
      trend <- melt(trend, id='start')
      
      ggplot(trend, aes(start,value, color=variable)) + geom_line()+ggtitle("Interest Over Time")+
        ylab("Relative Trend")+
        theme(plot.title = element_text(size = 18))+
        xlab("Time Period")+
        theme(axis.title.y = element_text(size=16),
              axis.title.x = element_text(size=16),
              axis.text.y = element_text(size=14),
              axis.text.x = element_text(size=14)) +
        theme(legend.title = element_text(colour="black", size=15, 
                                          face="bold"))+
        theme(legend.text = element_text(colour="black", size=14, 
                                         face="bold"))
      
    }
    
  })
  corr<-reactive({
    
    if(input$corr==T & length(out()>1)){
      
      z=data()
      trend=z$trend
      trend=trend[,3:ncol(trend)]
      cor(trend)
      
    }
  }) 
  
  
  output$myplot3 <- renderPlot({
    if(length(corr()>0)){
      data=corr()
      
      qplot(x=Var1, y=Var2, data=melt(cor(data)), fill=value, geom="tile")+
        ggtitle('Correlation Matrix')+theme(axis.title.y =element_blank(),axis.title.x =element_blank(),
                                            axis.text.y = element_text(colour="darkred",size=14,angle=0,hjust=1,vjust=0),
                                            axis.text.x = element_text(colour="darkred",size=14,angle=0,hjust=1,vjust=0))+
        theme(legend.title=element_blank())+
        theme(legend.text = element_text(colour="black", size=14))+scale_fill_gradient2(limits=c(-1, 1),low="skyblue", high="blue")+
        theme(plot.title = element_text(size = 20,colour="black"))
    }
  })
  
  
  output$myplot2 <- renderPlot({
    if(length(out()>0)){
      data=data()
      
      
      z=data$searches
      rr=data$regions
      
      for (i in 1:length(z)){
        n=z[i]
        n=as.data.frame(n)
        names(n)=c("searches","hits")
        n$searches <- factor(n$searches, levels = n$searches[order(n$hits,decreasing =T)])
        
        colors=c("orange","skyblue","#999966")
        
        col=sample(c(1,2,3),1,replace=T)
        
        x11()
        
        print(ggplot(n, aes(searches,hits))+  
                geom_bar(stat='identity',fill=colors[col],color='black')+
                ggtitle(data$headers[2+2*length(z)+i])+ylab('Hits')+
                theme(plot.title = element_text(size = 18,colour="blue"))+
                theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="blue",size=14),axis.text.x = element_text(colour="grey20",size=14,angle=60,hjust=.5,vjust=.5,face="plain"))
              
              
        )
        
        
        if(geo()=='')
        {
          x11()
          
          
          regions = as.data.frame(rr)[c(1,i+1)]
          
          names(regions)=c('region','hits')
          
          regions$region[regions$region=="United States"] = "USA"
          
          world_map = map_data("world")
          
          world_map =merge(world_map, regions, by="region",all.x = TRUE)
          
          world_map = world_map[order(world_map$group, world_map$order),]
          
          g=ggplot(world_map, aes(x=long, y=lat, group=group))+
            geom_polygon(aes(fill=hits), color="gray70") 
          
          print(g+theme(axis.text.y   = element_blank(),
                        axis.text.x   = element_blank(),
                        axis.title.y  = element_blank(),
                        axis.title.x  = element_blank(),
                        panel.background = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank())+
                  scale_fill_gradient(low = "skyblue", high = "blue", guide = "colorbar",na.value="white")+ggtitle(data$headers[2+2*length(z)+i])+ylab('Hits')+
                  theme(legend.key.size = unit(1, "cm"),
                        legend.title = element_text(size = 12, colour = "blue"),
                        legend.title.align=0.3,legend.text = element_text(size = 10))+
                  theme(panel.border = element_rect(colour = "gray70", fill=NA, size=0.5))
          )
        }
      }
      
    }
    
  }) 
  
} 

shinyApp(ui, server)