
## app.R ##
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(factoextra)
library(NbClust)
library(zipcodeR)
library(leaflet)
library(shinyalert)
library(tools)
packageurl<-"https://cran.r-project.org/src/contrib/Archive/nloptr/nloptr_1.2.1.tar.gz"

install.packages(packageurl, repos=NULL, type="source")
deployApp()
ui <- dashboardPage(title="Dashboard",
                    dashboardHeader(title = span("Dashboard", 
                                                 
                                                 style = "color: black; font-size: 20px;font-weight: bold;font-family:sans serif")),
                    dashboardSidebar(
                      # Remove the sidebar toggle element
                      tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
                      collapsed = T
                      ),
                    dashboardBody(
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      tabsetPanel(
                        id="inTabset",
                        tabPanel("Importing the dataset",
                                 uiOutput("fileUpload"),
                                 DT::dataTableOutput("contents"),
                                 uiOutput("act1")
                                 
                        ),
                        tabPanel("Input data set review",
                                 selectInput("ch","Select Chart",choices = c("Line chart","Bar chart","Map")),
                                 uiOutput("category"),
                                 uiOutput("timetrend"),
                                 tags$hr(),
                                 uiOutput("act2"),
                                 tags$hr(),
                                 uiOutput("dn")),
                        tabPanel("K- means Clustering analysis (2018-2019)",
                                 uiOutput("km"),
                                 fluidRow(
                                   column(4,
                                plotlyOutput("pr")),
                                column(4,plotlyOutput("pr2")),
                                column(4,plotlyOutput("pr3"))))
                      )
                    ) 
)

server <- function(input, output,session) {
  showModal(modalDialog("My startup modal..."))
  
  resetFileUpload <- reactiveVal(FALSE)
  output$fileUpload <- renderUI({
    resetFileUpload() # reactive dependency
    resetFileUpload(FALSE)
    fileInput('file1', 'Choose xls file', accept = c(".xls"))
  })
  
  observeEvent(input$file1, {
    if(file_ext(input$file1$name) != "xls"){
      resetFileUpload(TRUE)
      showModal(modalDialog("That's not a .xls file"))
    }
  })
  observeEvent(input$submit, {
    updateTabsetPanel(session, "inTabset",selected = "Input data set review")
  })
  observeEvent(input$submit2, {
    updateTabsetPanel(session, "inTabset",selected = "K- means Clustering analysis (2018-2019)")
  })
  get_item_list <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL) }
    
    else {
      read_excel(inFile$datapath,col_names = T
                )
    }
  })
  
  output$contents<-renderDataTable({
    get_item_list()[1:100,]
  })
  output$category<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) }
    
    else {
      selectInput("cat","Select Category",choices = unique(get_item_list()$Category))
      
    }
  })
  output$act1<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) }
    
    else {
      actionButton("submit","Continue Analysis")
      
    }
  })
  output$act2<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) }
    
    else {
      actionButton("submit2","Start Analysis")
      
    }
  })
  output$dn<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) }
    
    else {
      downloadButton("dl", "Download")
      
    }
  })
  df<-reactive({
    pd <-subset(get_item_list(), get_item_list()$Category %in% input$cat)
  })
  output$dl <- downloadHandler(
    filename = function() { "ae.xlsx"},
    content = function(file) {write_xlsx(df(), path = file)}
  )
  output$plotly<-renderPlotly({
    sum<-df() %>% 
      group_by(month = lubridate::floor_date(`Order Date`, "month")) %>%
      summarize(Sales = sum(Sales))
    if(input$ch=="Line chart"){
      
      fig <- plot_ly(sum, type = 'scatter', mode = 'lines')%>%
        add_trace(x = ~month , y = ~Sales)%>%
        layout(showlegend = F)
      fig <- fig %>%
        layout(
          width=1800,
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6', width = 900)
      
      
      fig
    }
    else if(input$ch=="Bar chart"){
      fig <- plot_ly(sum, x = ~month, y = ~Sales, type = 'bar',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)',
                                               width = 1.5))
                     )
      fig <- fig %>% layout( width=1800
                            )
      
      fig
    }
      
    })
  output$map<-renderLeaflet({
    df<-data.frame(df())
    colnames(df)[12]<-"Postal"
    
    df %>%
      left_join(.,
                geocode_zip(df$Postal ) %>% mutate(zipcode = as.numeric(zipcode)),
                by = c("Postal" = "zipcode")) %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(
        ~ lng,
        ~ lat,
        popup = ~ as.character(Sales),
        label = ~ as.character(City)
      )
    
  })  
  output$timetrend<-renderUI({
    if(input$ch=="Line chart"){
      plotlyOutput("plotly")
    }
    else if(input$ch=="Bar chart"){
      plotlyOutput("plotly")
    }
    else if(input$ch=="Map"){
      leafletOutput("map")
    }
  })
  output$km<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) }
    
    else {
      selectInput("clu","Cluster by:",c("Customer ID","Customer Name","Order ID"),selected = "Customer ID")
      
    }
  })
kmea<-reactive({
  df<- data.frame(df())
if(input$clu=="Customer ID"){
    su<-df[,c(6,18,21)]
    su2<-su %>% group_by(Customer.ID) %>%
      summarize(Profit = sum(Profit),Sales=sum(Sales))
    
    su2b<-su2 %>% remove_rownames %>% column_to_rownames(var="Customer.ID")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su2b, centers = 4)
    su2b$cluster <- as.character(cls$cluster)
    su2b$client<-as.character(su2$Customer.ID)

    su2b
    
}
  else if(input$clu=="Customer Name"){
    su<-df[,c(7,18,21)]
    
    su3<-su %>% group_by(Customer.Name) %>%
      summarize(Profit = sum(Profit),Sales=sum(Sales))
    
    su3b<-su3 %>% remove_rownames %>% column_to_rownames(var="Customer.Name")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su3b, centers = 4)
    su3b$cluster <- as.character(cls$cluster)
    su3b$client<-as.character(su3$Customer.Name)
    
    su3b
    
  }
  else{
    su<-df[,c(2,18,21)]
    
    su3<-su %>% group_by(Order.ID) %>%
      summarize(Profit = sum(Profit),Sales=sum(Sales))
    
    su3b<-su3 %>% remove_rownames %>% column_to_rownames(var="Order.ID")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su3b, centers = 4)
    su3b$cluster <- as.character(cls$cluster)
    su3b$client<-as.character(su3$Order.ID)
    
    su3b
    
  }
})


kmea2<-reactive({
  df<- data.frame(df())
  if(input$clu=="Customer ID"){
    su<-df[,c(6,19,21)]
    su2<-su %>% group_by(Customer.ID) %>%
      summarize(Profit = sum(Profit),Quantity=sum(Quantity))
    
    su2b<-su2 %>% remove_rownames %>% column_to_rownames(var="Customer.ID")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su2b, centers = 4)
    su2b$cluster <- as.character(cls$cluster)
    su2b$client<-as.character(su2$Customer.ID)
    
    su2b
    
  }
  else if(input$clu=="Customer Name"){
    su<-df[,c(7,19,21)]
    
    su3<-su %>% group_by(Customer.Name) %>%
      summarize(Profit = sum(Profit),Quantity=sum(Quantity))
    
    su3b<-su3 %>% remove_rownames %>% column_to_rownames(var="Customer.Name")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su3b, centers = 4)
    su3b$cluster <- as.character(cls$cluster)
    su3b$client<-as.character(su3$Customer.Name)
    
    su3b
    
  }
  else{
    su<-df[,c(2,19,21)]
    
    su3<-su %>% group_by(Order.ID) %>%
      summarize(Profit = sum(Profit),Quantity=sum(Quantity))
    
    su3b<-su3 %>% remove_rownames %>% column_to_rownames(var="Order.ID")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su3b, centers = 4)
    su3b$cluster <- as.character(cls$cluster)
    su3b$client<-as.character(su3$Order.ID)
    
    su3b
    
  }
})

kmea3<-reactive({
  df<- data.frame(df())
  if(input$clu=="Customer ID"){
    su<-df[,c(6,18,19)]
    su2<-su %>% group_by(Customer.ID) %>%
      summarize(Sales = sum(Sales),Quantity=sum(Quantity))
    
    su2b<-su2 %>% remove_rownames %>% column_to_rownames(var="Customer.ID")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su2b, centers = 4)
    su2b$cluster <- as.character(cls$cluster)
    su2b$client<-as.character(su2$Customer.ID)
    
    su2b
    
  }
  else if(input$clu=="Customer Name"){
    su<-df[,c(7,18,19)]
    
    su3<-su %>% group_by(Customer.Name) %>%
      summarize(Sales = sum(Sales),Quantity=sum(Quantity))
    
    su3b<-su3 %>% remove_rownames %>% column_to_rownames(var="Customer.Name")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su3b, centers = 4)
    su3b$cluster <- as.character(cls$cluster)
    su3b$client<-as.character(su3$Customer.Name)
    
    su3b
    
  }
  else{
    su<-df[,c(2,18,19)]
    
    su3<-su %>% group_by(Order.ID) %>%
      summarize(Sales = sum(Sales),Quantity=sum(Quantity))
    
    su3b<-su3 %>% remove_rownames %>% column_to_rownames(var="Order.ID")
    #km.res <- kmeans(su, 4, nstart = 25)
    cls <- kmeans(x = su3b, centers = 4)
    su3b$cluster <- as.character(cls$cluster)
    su3b$client<-as.character(su3$Order.ID)
    
    su3b
    
  }
})



output$pr<-renderPlotly({
  kmea()
  ggplotly(ggplot() +
             geom_point(data = kmea(), 
                        mapping = aes(x = Profit, 
                                      y = Sales, 
                                      colour = cluster,
                                      text=paste(
                                        "Sales:",Sales,
                                        "<br>Profit :",
                                        Profit,
                                        "<br> Cluster :",
                                        cluster,
                                        "<br> ID :",
                                        client
                                        
                                      ))),tooltip = "text",height = 650)
  
  
  
})
output$pr2<-renderPlotly({
  kmea2()
  ggplotly(ggplot() +
             geom_point(data = kmea2(), 
                        mapping = aes(x = Profit, 
                                      y = Quantity, 
                                      colour = cluster,
                                      text=paste(
                                        "Quantity:",Quantity,
                                        "<br>Profit :",
                                        Profit,
                                        "<br> Cluster :",
                                        cluster,
                                        "<br> ID :",
                                        client
                                        
                                      ))),tooltip = "text",height = 650)
  
  
  
})
output$pr3<-renderPlotly({
  kmea3()
  ggplotly(ggplot() +
             geom_point(data = kmea3(), 
                        mapping = aes(x = Sales, 
                                      y = Quantity, 
                                      colour = cluster,
                                      text=paste(
                                        "Quantity:",Quantity,
                                        "<br>Sales :",
                                        Sales,
                                        "<br> Cluster :",
                                        cluster,
                                        "<br> ID :",
                                        client
                                        
                                      ))),tooltip = "text",height = 650)
  
  
  
})  
}

shinyApp(ui, server)