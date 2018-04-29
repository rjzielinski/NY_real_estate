library(shiny)
library(tidyverse)
library(readr)
library(leaflet)

load("nyhousing_transactions.Rda")

# create codebook
description <- c("names", "County", "Town Name", 
                 "Street Address", "Zip Code", "Sale Price", "Sale Date", 
                 "Name of Seller", "Name of Buyer", "ID", "Full Address",
                 "Full Address", "Longitude", "Latitude", "Month", "Day",
                 "Year", "Longitude", "Latitude")
codebook <- data.frame(name=names(transactions), description)
names(codebook) <- c("Variable", "Variable Description")

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = log(transactions$SalePrice))





# UI
ui <- fluidPage(
  titlePanel("Map of Transactions"),
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      selectInput("transactionyear", "Year of Transaction", choices = 1993:2017, selected = 2017),
      checkboxGroupInput("transactioncounties", "Counties:", choices = c("All", unique(sort(as.character(transactions$County)))),
                         selected = "All")
      #checkboxInput("crosslistedenrollment", "Include Cross-Listed Enrollments", TRUE)
      #checkboxGroupInput("subject", "Departments",
      #                   choices = unique(sort(as.character(fakeenrolls$subject))),
      #                   selected = unique(sort(as.character(fakeenrolls$subject)))[1]
      #),
      #checkboxGroupInput("type", "Type of course",
      #                   choices = unique(sort(as.character(fakeenrolls$type))),
      #                   selected = unique(sort(as.character(fakeenrolls$type)))
      #)
    ),
    # Output(s)
    mainPanel(
      
      tabsetPanel(id = "tabspanel", type = "tabs",
                  tabPanel(title = "Data Table", 
                           br(),
                           DT::dataTableOutput(outputId = "transactionstable")),
                  tabPanel(title = "Map of Real Estate Transactions", 
                           leafletOutput(outputId = "map"),
                           br(),
                           h4(uiOutput(outputId = "n1"))),
                  #tabPanel(title = "Course Enrollments (by dept and level)", 
                  #         plotOutput(outputId = "boxplot2"),
                  #         br(),
                  #         h4(uiOutput(outputId = "n2"))),
                  # New tab panel for Codebook
                  tabPanel("Codebook", 
                           br(),
                           DT::dataTableOutput(outputId = "codebook"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create reactive data frame
  sales <- reactive({
    newval <- transactions %>%
      filter(year == input$transactionyear)
    
    if("All" %in% input$transactioncounties){
      
    } else{
      newval <- newval %>%
        filter(County %in% input$transactioncounties)
    }
    #if (input$crosslistedenrollment) {
    #  newval <- newval %>%
    #    mutate(displayenroll = totalenroll)
    #} else {
    #  newval <- newval %>%
    #    mutate(displayenroll = enroll)
    #}
    #if (! input$specialtopicshonors) {
    #  newval <- newval %>%
    #    filter(! number %in% c(290, 390, 490))
    #}
    return(newval)
  })
  
  output$map <- renderLeaflet({
    m <- sales() %>%
      select(Long, Lat, SalePrice) %>%
      leaflet() %>%
      addTiles() %>%  
      addCircles(lng = ~Long, lat = ~Lat, radius = 10,
                 color = ~pal(log(SalePrice)), opacity = 5)
    
    return(m)
  })
  
  #Create data table
  output$transactionstable <- DT::renderDataTable({
    DT::datatable(data = sales(), 
                  options = list(pageLength = 20), 
                  rownames = FALSE)
  })
  
  # Create data table
  output$codebook <- DT::renderDataTable({
    DT::datatable(data = codebook, 
                  options = list(pageLength = 8), 
                  rownames = FALSE)
  })
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)
