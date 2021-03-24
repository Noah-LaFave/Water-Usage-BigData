library(carData)
library(ggplot2)
library(leaflet)
library(readxl)
library(raster)
library(RColorBrewer)
corona <- read_xlsx("C:/Users/Peter/Desktop/proje/coronaC.xlsx") #cases per 100k about 3 weeks ago, not total cases per 100k 
total <- read_xlsx("C:/Users/Peter/Desktop/proje/totalCases.xlsx") #more US data

water <- read_xlsx("C:/Users/Peter/Downloads/US-TotW-ByState.xlsx")
water

usa <- getData("GADM", country="USA", level=1) 
usa$Data <- water$State

pal <- colorQuantile("Reds", NULL, n = 5)
pallette <- colorQuantile("Reds", NULL, n = 5)


#create a pop up (onClick)
polygon_popup <- paste0("<strong>State: </strong>", usa$NAME_1, "<br>",
                        "<strong>Millions of Gallons Per Day: </strong>", usa$Data)


ui = fillPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("USA", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 20, 
      plotOutput(outputId = "drought", width = "150%", height = "500px"),
      selectInput("state", "Learn about a specific state:",
                  c("California" = "cal", "Oregon" = "or", "Hawaii"=
                      "ha"), width ="50%"),
      sliderInput("wrp", "Some water stuff:",
                  min = 1, max = 100, value = 40, width = "170%"),
      checkboxInput("legend", "Show legend", TRUE)
    )
)
server <- function(input, output, session) {
  output$drought<-renderPlot({
    wow <- ggplot(data = cars, aes(x = speed, dist))+
      geom_line(color = "#69b3a2", alpha = .9)
    wow
    
  })
  output$USA <-renderLeaflet({
    USA = leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% #the actual map of the world
      setView(-88.2, 39.7,
              zoom = 5) %>% 
      addPolygons(data = usa, 
                  fillColor= ~pal(Data), #the larger the number, more red it is 
                  fillOpacity = 0.7, 
                  weight = 2, #of black edges
                  color = "black",
                  popup = polygon_popup)  #from llibrary 
  })
  observe({
    proxy <- leafletProxy("USA", data = water)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorQuantile("Reds", NULL, n = 5)
      proxy %>% addLegend(position = "bottomleft",
                          pal = pal, values = ~usa$Data,
                          title = "Water Usage",
      )
    }
  })
}
shinyApp(ui, server)



