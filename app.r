#Require raw export from Kibana PAYMENTS query that includes 'Payment Date,' 'Cleared Date,' 'Provider ID,'
#'Provider Name,' 'City,' 'State,' 'Zip,' and 'Count' fields.

library(shiny)
library(dplyr)
library(anytime)
library(plotly)

pdata <- data.frame(read.csv("EpochDate.csv", header = TRUE))

  pdata <- dplyr::select(pdata, Payment_Date_Raw = Top.5000000.payment_date,
         Cleared_Date_Raw = Top.500000000.cleared_at,
         Provider_Id = provider_id,
         Provider_Name = Top.500000.provider.name,
         City = Top.500000.geo.city,
         State = Top.500000.geo.state,
         Zip = Top.500000.geo.zipcode,
         Count = Count)
  
paydate_numeric <- as.numeric(pdata$Payment_Date_Raw)
paydate_ms <- paydate_numeric/1000
pdata$Payment_Date <- anydate(paydate_ms)

cleardate_numeric <- as.numeric(pdata$Cleared_Date_Raw)
cleardate_ms <- cleardate_numeric/1000
pdata$Cleared_Date <- anydate(cleardate_ms)

pdata$Days_Late <- difftime(pdata$Cleared_Date, pdata$Payment_Date, units = "days")

pdata_clean <- subset(pdata, select = c(4:11))

str(pdata_clean)

ui <- fluidPage(
  titlePanel("Late Payments"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("shinyDateInput", "Date Range", start = "2015-05-01"),
      sliderInput("daysLateInput", "Days Late", -10, 100, c(-10, 100), pre = ""),
       checkboxInput("filterProvider", "Filter by Provider", FALSE),
       conditionalPanel(
         condition = "input.filterProvider",
         uiOutput("providerOutput")),
      textOutput("summaryText"),
      br(),
      textOutput("avgDaysLate"),
      br(),
      textOutput("medianDaysLate"),
      br(),
      textOutput("maxDaysLate"),
      br(),
      textOutput("totalPayments"),
      width = 3
  ),
  mainPanel(
    #dygraphOutput("daPlot", width = "85%"),
    plotlyOutput("daPlot"),
    br(), br(),
    #tableOutput("results")
    DT::dataTableOutput("results")
    )
  )
)
server <- function(input, output) {
  output$providerOutput <- renderUI({
    selectInput("providerInput", "Provider_Name",
                sort(unique(pdata_clean$Provider_Name)),
                multiple = FALSE,
                selected = "NTTA")
  })
  
  clean <- reactive({
    clean <- data.frame(pdata_clean)
    
    if (is.null(input$providerInput)) {
      return(NULL)
    }
    
    if (input$filterProvider){
      clean <- dplyr::filter(clean, Provider_Name == input$providerInput)
    }
    clean <- dplyr::filter(clean, Days_Late >= input$daysLateInput[1],
                                  Days_Late <= input$daysLateInput[2])
    
    if(nrow(clean) == 0) {
      return(NULL)
    }
    
    clean <- dplyr::filter(clean, Payment_Date >= input$shinyDateInput[1],
                                  Payment_Date <= input$shinyDateInput[2])
    
    output$summaryText <- renderText({
      numOptions <- nrow(clean())
      if (is.null(numOptions)) {
        numOptions <- 0
      }
      
      paste0(numOptions, " records found")
    })
    
    output$totalPayments <- renderText({
      sumOptions <- sum(clean()$Count)
      if (is.null(sumOptions)) {
        sumOptions <- 0
      }
      
      paste0("Total Payments: ", sumOptions)
    })
    
    output$avgDaysLate <- renderText({
      avgOptions <- mean(clean()$Days_Late)
      if (is.null(avgOptions)) {
        avgOptions <- 0
      }
      
      paste0("Average Days Late: ", avgOptions)
    })
    
    output$medianDaysLate <- renderText({
      medOptions <- median(clean()$Days_Late)
      if (is.null(medOptions)) {
        medOptions <- 0
      }
      
      paste0("Median of Days Late: ", medOptions)
    })
    
    output$maxDaysLate <- renderText({
      maxOptions <- max(clean()$Days_Late)
      if (is.null(maxOptions)) {
        maxOptions <- 0
      }
      
      paste0("Max of Days Late: ", maxOptions)
    })
    
    clean
  })
  
  output$daPlot <- renderPlotly({
    if (is.null(clean())) {
      return()
    }
   #as.Date(pdata_clean$Payment_Date, format = "%Y-%m-%d") %>% 
     #dygraph()
    #--------------------
    #ggplot(clean(), aes(Payment_Date,Days_Late)) +
             #geom_line() +
              #scale_x_date(date_breaks = "1 month", date_labels = "%B") + xlab("Date") +
               #ylab("Days Late")
    p <- plot_ly(clean(), x = ~Payment_Date, y = ~Days_Late, text = ~Provider_Name) %>% 
      add_lines(color = ~Days_Late, colors = "black")
    layout(p, xaxis = list(title = "Pay Date",
                           rangeslider = list (type = "date")))

      })
    output$results <- DT::renderDataTable({
      clean()
    })
}

shinyApp(ui = ui, server = server)
