#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(rstudioapi, shiny, shinydashboard, DT, dplyr, highcharter)

# Setwd (1º current wd where is the script, then we move back to the 
# general folder)
current_path = getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("..")
rm(current_path)

# Loading data
data_bydays<-read.csv("./script/house070809day.csv")
data_bydays$Date <-  as.Date(data_bydays$Date)

data_byweeks<-read.csv("./script/house070809week.csv")
data_byweeks$Date <-  as.Date(data_byweeks$Date)

data_bymonths<-read.csv("./script/house070809month.csv")
data_bymonths$Date <-  as.Date(data_bymonths$Date)

prophet_forecast <- read.csv("./script/Forecast.csv")
prophet_forecast$ds <- as.Date(prophet_forecast$ds)
#rename(prophet_forecast, replace = c("ds" = "Date"))


# USER INTERFACE
ui <- dashboardPage(
  dashboardHeader(title = "Energy consumption",
                  titleWidth = 250),
  dashboardSidebar(sidebarMenu(
    menuItem("Datasets", tabName = "Datasets", icon = icon("database")),
    menuItem("Forecast", tabName = "Forecast", icon = icon("tree")),
    menuItem("Graphs", tabName = "Graphs", icon = icon("bar-chart-o")),
    
    
    selectInput(inputId = "Granularity", label = "Select granularity",
                choices = c("Day", "Week", "Month")),
    
    selectInput(inputId = "Variable", label = "Select a variable",
                choices = c("Kitchen", "Laundry", "WH_AC", "GAP")),
    
    dateRangeInput(inputId = "Date", label = "Select date ranges",
                   start = min(data_bydays$Date), end = max(data_bydays$Date),
                   min = min(data_bydays$Date), max = max(data_bydays$Date)))),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Datasets", box(DT::dataTableOutput("Data_by"), width = 12, height="800px")),
      tabItem(tabName = "Forecast", box(plotlyOutput("prophet_forecast"), width = 12,
                                        numericInput(inputId = "datepred",label = "Days to predict",value = 365))),
      tabItem(tabName = "Graphs", box(highchartOutput("plot"), width = 12), box(textOutput("messages")),
      fluidRow(box(infoBoxOutput(width = 6, "box"))))
      )
       
        
      )
      
    
  )


# SERVER
server <- function(input, output) {
  
  # Extract the name of the selected dataset
  get.granularity <- reactive({
    switch(input$Granularity,
           "Day" = data_bydays,
           "Week" = data_byweeks,
           "Month" = data_bymonths)
  })
  
  # Prepare the dataset
  filteredData <- reactive({
    get.granularity() %>% select(Variable = input$Variable,Date) %>% dplyr::filter(Date <= input$Date[2]& Date >= input$Date[1])
  })
  
  output$Date <- renderPrint({
    input$Date })
  
  # Print the table
  output$Data_by <- renderDataTable({
    final_data <- filteredData()
    
  })
  
  #Print the forecast:
  output$prophet_forecast <- renderPlotly ({
    data <- house070809day %>% select(input$Variable,"Date")
    colnames(data) <- c("y","ds")
    prophet_mod <- prophet(data, daily.seasonality = TRUE)
    future <- make_future_dataframe(prophet_mod, periods = input$datepred)
    forecast <- predict(prophet_mod, future)
    plot_ly(x = forecast$ds,y = forecast$yhat,type = "scatter",mode = "line")
  })
  
  #Print the plot
  output$plot <- renderHighchart ({
    data_plot<-filteredData()
    hchart(data_plot, "line",hcaes(x=Date, y=Variable),color = "green")
    
  })
  
  # Print the messages 
  output$messages<- renderText({
    data_text<-filteredData()
    print(paste0("You have consumed a mean of ",
                 round(mean(data_text$Variable)), " watts"))
  })
  
  # info box:
  output$box <- renderInfoBox({
    
    data_text <- get.granularity()
    data_text <-  data_text %>% select(Variable = input$Variable)
    
    
    infoBox(
      subtitle = "Spending", paste0(round(mean(data_text$Variable) * 0.14 , 2), " euros"), icon = icon("list"),
      color = "maroon", fill = TRUE, width = 10)
    
    
  })
}

# RUNNING APP
shinyApp(ui, server)
