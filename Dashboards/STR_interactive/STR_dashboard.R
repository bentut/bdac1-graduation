library(dplyr)
library(shiny)
library(plotly)
library(RColorBrewer)
Hotel_Occ <- read.csv(file = "HotelOccupancy.csv", header = TRUE)
Hotel_ADR <- read.csv(file = "HotelADR.csv", header = TRUE)
Hotel_Occ$Date <- as.Date(Hotel_Occ$Date)
Hotel_ADR$Date <- as.Date(Hotel_ADR$Date)
colorpal <- colorpal <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B","#FFFFBF","#ABDDA4","#66C2A5","#3288BD","#5E4FA2")
ui <- fluidPage(
  dateRangeInput(inputId = "DateRange", 
                 label = "Input Date Range",
                 start = min(Hotel_Occ$Date),
                 end = max(Hotel_Occ$Date),
                 min = min(Hotel_Occ$Date),
                 max = max(Hotel_Occ$Date),
                 format = "yyyy-mm-dd",
                 startview = "month",
                 separator = "to"),
  selectInput(inputId = "states",
              label = "Select a Location",
              choices = c("Anaheim.SantaAna_CA","Atlanta_GA","Boston_MA","Chicago_IL","Dallas_TX","Denver_CO","Detroit_MI","Houston_TX",
                          "LosAngeles_LongBeach_CA","Miami_FL","Minneapolis_StPaul_MN.WI","Nashville_TN","NewOrleans_LA",
                          "New.York_NY","Norfolk_VirginiaBeach_VA","Oahu_HI","Orlando_FL","Philadelphia_PA","Phoenix_AZ","SanDiego_CA",
                          "SanFrancisco_SanMateo_CA","Seattle_WA","StLouis_MO.IL","Tampa_StPetersburg_FL","Washington_DC.MD.VA"),
              selected = "Atlanta_GA",
              multiple = TRUE,
              selectize = TRUE),
  plotlyOutput("hist"),
  plotlyOutput("hist1")
)

server <- function(input, output) {
  output$hist <- renderPlotly({
    print(class(input$DateRange[1]))
    print(input$DateRange[2])
    print(input$states[1])
    filterdatetest1 <- filter(Hotel_Occ,  between(Date, input$DateRange[1], input$DateRange[2]))
     t <-  plot_ly(filterdatetest1, x = ~Date, y = ~NA, name = 'Atlanta,GA', type = 'scatter', mode = 'lines',
            line = list(color = 'rgb(255, 0, 0)', width = 4)) #%>%
      t <- t %>% layout(title = "Hotel Occupancy by Location/Month",
             xaxis = list(title = "Month"),
             yaxis = list (title = "% Occupied"))
      for (i in 1:length(input$states)){
        t <- t %>% add_trace(y = as.formula(paste("~",input$states[i], sep = "")), name = input$states[i], 
                             line = list(color = for (i in colorpal){
                               print(paste("the color is", i))
                             }, 
                                         width = 4))
      print(as.formula("~input$states[i]"))
      print(paste("~",input$states[i], sep = ""))
      }
      t
  })
  output$hist1 <- renderPlotly({
    print(class(input$DateRange[1]))
    print(input$DateRange[2])
    print(input$states[1])
    filterdatetest2 <- filter(Hotel_ADR,  between(Date, input$DateRange[1], input$DateRange[2]))
    l <-  plot_ly(filterdatetest2, x = ~Date, y = ~NA, name = 'Atlanta,GA', type = 'scatter', mode = 'lines',
                  line = list(color = 'rgb(255, 0, 0)', width = 4)) #%>%
    l <- l %>% layout(title = "Hotel ADR by Location/Month",
                      xaxis = list(title = "Month"),
                      yaxis = list (title = "Average Daily Rate"))
    for (i in 1:length(input$states)){
      l <- l %>% add_trace(y = as.formula(paste("~",input$states[i], sep = "")), name = input$states[i], 
                           line = list(color = for (i in colorpal){
                             print(paste("the color is", i))
                           }, 
                           width = 4))
      print(as.formula("~input$states[i]"))
      print(paste("~",input$states[i], sep = ""))
    }
    l
  })
}

shinyApp(ui = ui, server = server)