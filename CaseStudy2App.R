require(RCurl)
require(shiny)
require(shinydashboard)
require(DT)
require(gdata)
require(corrplot)
require(ggplot2)
require(GGally)
require(party)
require(randomForest)
require(reshape)
require(corrgram)
require(pROC)

setwd("~/Desktop/R-Work")
train<-read.xls("CaseStudy2data_2.xlsx", sheet = 1, header=TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Attrition Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName = "dashboard", icon = icon("dashboard")),
      menuItem("EDA",tabname="EDA",icon=icon("th"), startExpanded = TRUE,
               menuSubItem("Plots",tabName = "plots",icon = icon("bar-chart-o")),      
               menuSubItem("Randomforest",tabName = "forest",icon = icon("random")),
               menuSubItem("DecisionTree",tabName = 'tree',icon=icon("tree")),
               menuSubItem("ROC", tabName = "ROC",icon = icon("stats", lib = "glyphicon"))),
      menuItem("Conculison",tabName = 'result',icon = icon("thumbs-up", lib = "glyphicon")),
      menuItem("Data",tabName = 'data',icon=icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard'),
      tabItem(tabName = "eda",
              h2("Exploratory Data Analysis")),
      tabItem(tabName = 'plots',
              h2("Visualization"),
              fluidRow(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("var",
                                label = "Select variable",
                                choices = c(
                                  "Age" = 1,
                                  "Business Travel" = 3,
                                  "Daily Rate" = 4,
                                  "Department" = 5,
                                  "Distance From Home" = 6,
                                  "Education" = 7,
                                  "Education Field" = 8,
                                  "Environment Satisfaction" = 11,
                                  "Gender"= 12,
                                  "Hourly Rate" = 13,
                                  "Job Involvement" = 14,
                                  "Job Level" = 15,
                                  "Job Role" = 16,
                                  "Job Satisfaction" = 17,
                                  "Maritial Status" = 18,
                                  "Monthly Income" = 19,
                                  "Monthly Rate" = 20,
                                  "Number of Companies Worked" = 21,
                                  "Over Time" = 23,
                                  "Percent Salary Hike" = 24,
                                  "Performance Rating" = 25,
                                  "Relationship Satisfaction" = 26,
                                  "Stock Option Level" = 28,
                                  "Total Working Years" = 29,
                                  "Training Times Last year" = 30,
                                  "Work Life Balance" = 31,
                                  "Years at Company" = 32,
                                  "Years in Current Role" = 33,
                                  "Years Since Last Promotion" = 34,
                                  "Years with Current Manager" = 35
                                ),
                                selected = 1),
                    selectInput("facet_row",
                                label = "Facet Row",
                                choices = c(
                                  "Age" = 1,
                                  "Attrition" = 2,
                                  "Business Travel" = 3,
                                  "Daily Rate" = 4,
                                  "Department" = 5,
                                  "Distance From Home" = 6,
                                  "Education" = 7,
                                  "Education Field" = 8,
                                  "Environment Satisfaction" = 11,
                                  "Gender"= 12,
                                  "Hourly Rate" = 13,
                                  "Job Involvement" = 14,
                                  "Job Level" = 15,
                                  "Job Role" = 16,
                                  "Job Satisfaction" = 17,
                                  "Maritial Status" = 18,
                                  "Monthly Income" = 19,
                                  "Monthly Rate" = 20,
                                  "Number of Companies Worked" = 21,
                                  "Over Time" = 23,
                                  "Percent Salary Hike" = 24,
                                  "Performance Rating" = 25,
                                  "Relationship Satisfaction" = 26,
                                  "Stock Option Level" = 28,
                                  "Total Working Years" = 29,
                                  "Training Times Last year" = 30,
                                  "Work Life Balance" = 31,
                                  "Years at Company" = 32,
                                  "Years in Current Role" = 33,
                                  "Years Since Last Promotion" = 34,
                                  "Years with Current Manager" = 35
                                ),
                                selected = 2)
                  ),
                  mainPanel(
                    tabsetPanel(type = "tabs",
                    tabPanel("Basic Plot",plotOutput("AtrStack"), icon= icon("chart-bar",lib="font-awesome")),
                    tabPanel("Histogram",plotOutput("Histogram"), icon = icon("charts", lib = "glyphicon")),
                    tabPanel("Corrgram",plotOutput("Corrgram"), icon = icon("puzzle", lib = "glyphicon"))
                    )
                  )))),
      tabItem(tabName = "forest",
              h2("Randomforest Details"),
              plotOutput("rFo")),
      tabItem(tabName = "ROC",
              h2("Build a ROC Curve"),
              plotOutput("Curve")),
      tabItem(tabName = "tree",
              h2("Decision Tree"),
              plotOutput("Rtree")),
      tabItem(tabName = "result",
              h2("Conclusion"),
              verbatimTextOutput("Decision")
              ),
      tabItem(tabName = "data",
              h2("Employee Data"),
              DT::dataTableOutput("rawtable"),
              downloadButton("downloadData", "Download as CSV"))
    )
  )
)

server <- function(input, output) { 
  output$AtrStack <- renderPlot({
    ec<-as.numeric(input$var)
    fr<-as.numeric(input$facet_row)
    x<-train[, ec]
    f<-names(train[fr])
    if (fr==0){
      f<-names(train[2])
    }
    x_axsis<-names(train[ec])
    ggplot(train,aes(x=x,fill=factor(Attrition)))+
      geom_bar(width=0.5)+
      facet_wrap(as.formula(paste("~",f)))+
      xlab(x_axsis)+
      ylab("Total Count")+
      labs(fill='Attrition')
  }, height=499, width=699)
  output$Histogram <- renderPlot({
    ggplot(data=melt(train), mapping=aes(x=value)) + 
      geom_histogram(bins=40, col= 'Blue',border = 'Red') + 
      facet_wrap (~variable, scales = "free_x")
  },height=499, width=649)
  
  test<-train[, c(2,3,8,14,18,23,28)]
  test$Attrition<-as.integer(train$Attrition)
  test$BusinessTravel<-as.integer(train$BusinessTravel)
  test$EducationField<-as.integer(train$EducationField)
  test$MaritalStatus<-as.integer(train$MaritalStatus)
  test$OverTime<-as.integer(train$OverTime)
  
  output$Corrgram<-renderPlot({
    corrgram(test)
  },height=499, width=649)
  treeoutput<-ctree(Attrition~BusinessTravel+OverTime+StockOptionLevel,data=train)
  output$Rtree<-renderPlot({
    plot(treeoutput, inner_panel=node_inner(treeoutput, pval = TRUE, id = FALSE))
  }, height=599, width=999)
  rFout<-randomForest(train[,-2], as.factor(train[,35]), ntree=500, importance = TRUE)
  output$rFo<-renderPlot({
    varImpPlot(rFout)
    # Need to check the below 
    rFout$importance
  }, height=599, width=999)
  num_exmps = nrow(train)
  L = replace(integer(num_exmps), train[,2]=="Yes", 1)
  M = train[, -2]
  train_idx = sample(c(1:num_exmps), size = num_exmps * 0.7, replace = FALSE)
  
  clf_2<-randomForest(M[train_idx,], as.factor(L[train_idx]), 
                      proximity = TRUE, importance = TRUE)
  pred<-predict(clf_2, M[-train_idx,],type="prob")
  output$Curve<-renderPlot({
    plot(roc(L[-train_idx], as.numeric(pred[,1])), colorize = T)  
  },height=599, width=999)
  output$rawtable = DT::renderDataTable({
    DT::datatable(train, options = list(scrollX = TRUE))
  })
  output$Decision<-renderText(
    "Factors influences attrition: Over Time, Maritial Status, Business Travel and Education Field"
  )
  data<-train
  output$downloadData <-downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(),".csv",sep="")
  },
  content = function(file) {
    write.csv(data,file) 
  })
}

shinyApp(ui, server)