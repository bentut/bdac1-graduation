library(shiny)
library(leaflet)
library(RColorBrewer)
library(rsconnect)

evapponly <- read.csv("EVApp_CP.csv")
chargepointonly <- read.csv("chargepoint_only.csv")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                checkboxGroupInput(inputId = "display", 
                                   label = "Compare Chargepoint Stations in the EVApp Data vs. the new data from Chargepoint",
                                   choices = c("EVApp", "Chargepoint"), 
                                   selected = c("EVApp", "Chargepoint"),
                                   inline = TRUE
                )
  )
)

server <- function(input, output, session) {
  
  
  output$map <- renderLeaflet({
    leaflet(evapponly) %>% addProviderTiles(providers$Stamen.TonerLite) %>%
      fitBounds(~min(EV_NewLong), ~min(EV_NewLat), ~max(EV_NewLong), ~max(EV_NewLat))})
 observe({ 
    proxy <- leafletProxy("map")
    #fires if no checkboxes are selected will print "Null"
    if (length(input$display) == 0) {
      print(input$display[0])
      makemarkers <- proxy %>% clearMarkers()
      # fires if only one checkbox is selected - could be EVAPP or chargepoint
    } else if (length(input$display) == 1) {
      print(input$display[1])
      if (input$display[1] == "EVApp"){
        makemarkers <- proxy %>% clearMarkers() %>%
          addCircleMarkers(data = evapponly, ~EV_NewLong, ~EV_NewLat, radius = 10, color = "	#32CD32", fillColor = "	#32CD32", stroke = FALSE, fillOpacity = .6, group = "Red", label = ~as.character(EV_Address), popup = ~as.character(EV_Address))
      } else {
        makemarkers <- proxy %>% clearMarkers() %>%
          addCircleMarkers(data = chargepointonly, ~CP_lon, ~CP_lat, radius = 6, color = "#ff69b4", fillColor = "#ff69b4", stroke = FALSE, fillOpacity = .6, label = ~as.character(CP_addresses), popup = ~as.character(CP_addresses))
      }
      #fires if both check boxes are selected. 
    } else if (length(input$display) == 2) {
      print(input$display[2]) 
      makemarkers <- proxy %>% clearMarkers() %>%
        addCircleMarkers(data = evapponly, ~EV_NewLong, ~EV_NewLat, radius = 10, color = "	#32CD32", fillColor = "	#32CD32", stroke = FALSE, fillOpacity = .6, group = "Red", label = ~as.character(EV_Address), popup = ~as.character(EV_Address)) %>%
        addCircleMarkers(data = chargepointonly, ~CP_lon, ~CP_lat, radius = 6, color = "#ff69b4", fillColor = "#ff69b4", stroke = FALSE, fillOpacity = .6, label = ~as.character(CP_addresses), popup = ~as.character(CP_addresses))
      #fires if all goes wrong. 
    } else {
      print("TORIIIIIIII")
    }
    
    return(makemarkers)
  })
}

shinyApp(ui, server)
