# Fitness Data Project - Data Manipulation

library(readr)
library(magrittr)

otf <- read_csv("orangetheory-data.csv")

library(parsedate)
library(lubridate)


# The split stack shape package is similar to a "text-to-columns" feature in Excel.
library(splitstackshape)

otf <- cSplit(otf, 'Date', sep=" at ", type.convert=FALSE)
otf$date <- parse_date(otf$Date_1) #parsedate package
otf$newdate <- as.Date(otf$date)
otf$dayofweek <- format(otf$newdate, "%A")
otf$month <- format(otf$newdate, "%B")
otf$day <- format(otf$newdate, "%d")
otf$year <- format(otf$newdate, "%Y")
#otf$time <- strptime(otf$Date_3, format="%I:%M %p") #This doesn't seem to be working; trying to add a column of times, but it seems to be adding the whole data frame to each line?
otf$time <- as.POSIXct(strptime(otf$Date_3, format="%I:%M %p")) #Need to convert time to POSIXct instead of POSIXlt for the purposes of adding to a data frame.
otf <- cSplit(otf, 'time', sep=" ", type.convert=FALSE) #Split the new timestamp column.

# Clean up columns
otf$time_1 <- NULL
otf$Date_1 <- NULL
otf$Date_2 <- NULL
otf$date <- NULL
otf$`Workout ID` <- NULL
otf$Username <- NULL
otf$time <- otf$time_2
otf$time_2 <- NULL #only remove after copying and renaming column
otf$splats <- otf$`OT Points`
otf$calories <- otf$`Total Calories (kCal)`
otf$`OT Points` <- NULL #only remove after copying and renaming column
otf$`Total Calories (kCal)` <- NULL #only remove after copying and renaming column
otf$averagehr <- otf$`Avg HR`
otf$`Avg HR` <- NULL

#One hot encoding for splat goals -- create a column to denote which workouts where I hit 12 or more splat points, i.e. the goal for OTF.
otf$splat_goal_yes <- ifelse(otf$splats >=12, 1, 0)


otf_instr <- subset(otf, Instructor %in% c("Blasi", "Zach", "GC", "Ericka", "Ryan", "Lora"))


otf$instr_adj <- ifelse(otf$Instructor == "Blasi", "Blasi",
                        ifelse(otf$Instructor == "Zach", "Zach",
                               ifelse(otf$Instructor == "GC", "GC", 
                                      ifelse(otf$Instructor == "Ericka", "Ericka", 
                                             ifelse(otf$Instructor == "Ryan", "Ryan", 
                                                    ifelse(otf$Instructor == "Lora", "Lora", "Other"))))))

otf$time_adj <- ifelse(otf$Date_3 == "5:30PM", "5:30PM",
                       ifelse(otf$Date_3 == "8:15AM", "8:15AM",
                              ifelse(otf$Date_3 == "9:30AM", "9:30AM", 
                                     ifelse(otf$Date_3 == "10:45AM", "10:45AM", "Other"))))


#Shiny App Code Begins

# DASHBOARD PACKAGES

install.packages("shinydashboard")
install.packages("scales")
#install.packages("shinyHeatmaply")
install.packages("ggplot2")
install.packages("shinythemes")
install.packages("plotly")
#install.packages("heatmaply")
#install.packages("d3heatmap")
install.packages("dplyr")

## app.R ##
library(shinydashboard)
#library(DT)
library(scales)
#library(shinyHeatmaply)
library(shiny)
#library(d3heatmap)
library(RColorBrewer)
library(shinythemes)
library(ggplot2)
#library(heatmaply)
library(dplyr)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Fitness Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quick Stats", tabName = "Summary", icon = icon("watch_fitness")),
      menuItem("Performance Over Time", tabName = "Graphs", icon = icon("mobile_alt")),
      menuItem("Frequency", tabName = "Histogram", icon = icon("watch_fitness"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Summary",
        h2("Quick Stats"),
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(
            title = "Select Instructor",
            background = "orange",
            selectInput(inputId = "instructor", 
                        label = "Instructor Name", 
                        choices = c("Blasi", "Ericka", "GC", "Ryan", "Zach"),
                        selected = c("Blasi", "Ericka", "GC", "Ryan", "Zach"),
                        multiple = TRUE,
                        selectize = TRUE, 
                        width = NULL,
                        size = NULL),
            checkboxGroupInput(inputId = "classtime", 
                               label = "Class Time",
                               choices = c("8:15AM", "9:30AM", "10:45AM", "5:30PM"), 
                               selected = c("8:15AM", "9:30AM", "10:45AM", "5:30PM"), 
                               inline = TRUE)
          )
        ),
        fluidRow(

          valueBoxOutput('avgsplatBox'),
          
          valueBoxOutput('avgcalorieBox'),
          
          valueBoxOutput('avghrBox')
        ),

        fluidRow(
          column(width = 11,
                 box(tableOutput('table'), 
                     title = "Selected Workout Results",
                     width="100%"))
        )
      
      ), #tabItem
      
      tabItem(
        tabName = "Graphs",
        h2("Progress Over Time"),
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(
            title = "Select Date Range",
            background = "light-blue",
            dateRangeInput("daterange", 
                           "Choose start and end dates.", 
                           start = "2018-07-15", 
                           end = "2018-09-15", 
                           min = NULL,
                           max = NULL, 
                           format = "yyyy-mm-dd", 
                           startview = "month", weekstart = 0,
                           language = "en", 
                           separator = " to "),
            checkboxGroupInput(inputId = "metric", 
                               label = "Select Metric",
                               choices = c("calories", "splats", "averagehr"), 
                               selected = c("splats"), 
                               inline = TRUE)
          )
        ),
        fluidRow(
          valueBoxOutput('splat_timeBox'),
          
          valueBoxOutput('calorie_TimeBox'),
          
          valueBoxOutput('avg_hrBox')
          
        ),
        fluidRow(
          column(width = 11,
                 box(plotlyOutput('otf_plot'), 
                     width="100%"))
          )
        ), #tabItem
      
      tabItem(
        tabName = "Histogram",
        h2("Frequency"),
        fluidRow(
          box(
            title = "Select Instructor",
            background = "orange",
            selectInput(inputId = "instr_histo", 
                        label = "Instructor Name", 
                        choices = c("Blasi", "Ericka", "GC", "Ryan", "Zach"),
                        selected = c("Blasi", "Ericka", "GC", "Ryan", "Zach"),
                        multiple = TRUE,
                        selectize = TRUE, 
                        width = NULL,
                        size = NULL)
        )
        ),
        fluidRow(
          column(width = 11,
                 box(
                   plotOutput(outputId = "otf_histo"),
                   width = "100%")
          )
        )
      )
      
    
    )
    #tabitems
  ) #dashboardbody
) #dashboardpage

server <- function(input, output) {
 
  output$table <- renderTable({
    filter_otf <-
      otf %>%
      select(Instructor, month, day, year, time_adj, splats, calories, averagehr) %>% 
      filter(Instructor %in% input$instructor) %>% filter(time_adj %in% input$classtime) 
    
    filter_otf},
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    colnames = TRUE)
  
  output$avgsplatBox <- renderValueBox({
    
    filter_otf <-
      otf %>%
      select(Instructor, time_adj, splats, calories, averagehr, splat_goal_yes) %>% 
      filter(Instructor == input$instructor) %>% filter(time_adj %in% input$classtime) 
    
    valueBox(
      filter_otf %>%
        summarise(round(mean(splats, na.rm=TRUE))),
      "Average Splat Points",
      color = "orange"
    )
  })
  
  
  output$avgcalorieBox <- renderValueBox({
    
    filter_otf <-
      otf %>%
      select(Instructor, time_adj, splats, calories, averagehr, splat_goal_yes) %>% 
      filter(Instructor == input$instructor) %>% filter(time_adj %in% input$classtime) 
    
    valueBox(
      filter_otf %>%
        select(Instructor, time_adj, splats, calories, averagehr, splat_goal_yes) %>% 
        summarise(round(mean(calories, na.rm=TRUE))),
      "Average Calories Burned",
      color = "navy"
    )
  })
  
  output$avghrBox <- renderValueBox({
    
    filter_otf <-
      otf %>%
      select(Instructor, time_adj, splats, calories, averagehr, splat_goal_yes) %>% 
      filter(Instructor == input$instructor) %>% filter(time_adj %in% input$classtime) 
    
    valueBox(
      filter_otf %>%
        #select(Instructor, time_adj, splats, calories, averagehr, splat_goal_yes) %>% 
        summarise(round(mean(averagehr, na.rm=TRUE))),
      "Average Heart Rate",
      color = "olive"
    )
  })
  

  

  
  output$otf_plot <- renderPlotly({
    
    colorpal <- colorpal <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B","#FFFFBF","#ABDDA4","#66C2A5","#3288BD","#5E4FA2")

    filterdatetest1 <- filter(otf,  between(newdate, input$daterange[1], input$daterange[2]))
    
    t <-  plot_ly(filterdatetest1, x = ~newdate, y = ~NA, name = 'Metric Measures', type = 'bar', mode = 'markers',
                  line = list(color = 'rgb(255, 0, 0)', width = 2)) 
    t <- t %>% layout(title = "Performance by Selected Date Range",
                      xaxis = list(title = "Month"),
                      yaxis = list (title = "Metrics"))
    for (i in 1:length(input$metric)){
      t <- t %>% add_trace(y = as.formula(paste("~",input$metric[i], sep = "")), name = input$metric[i], 
                           line = list(color = for (i in colorpal){
                             print(paste("the color is", i))
                           }, 
                           width = 2))
    }
    t
  })

  
    output$otf_histo = renderPlot({
    
    filter_otf <-
      otf %>%
      select(Instructor, month, day, year, time_adj, splats, calories, averagehr, splat_goal_yes) %>% 
      filter(Instructor %in% input$instr_histo)
    
    ggplot(filter_otf, aes(splats)) + geom_histogram(binwidth = 2, col = "blue", fill = "orange") + ggtitle("Frequency of Splats Based on Instructor") + xlab("Splats") + ylab("Frequency")
    
  })
  
  
}

shinyApp(ui, server)
