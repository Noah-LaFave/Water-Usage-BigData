library(carData)
library(ggplot2)
library(leaflet)
library(readxl)
library(raster)
library(RColorBrewer)
library(readxl)
library(shiny)

#Read in the data from USGS
usgs1990_data <- read_excel('data/us90co.xls')
usgs1995_data <- read_excel('data/usco1995.xlsx')
usgs2000_data <- read_excel('data/usco2000.xls')
usgs2005_data <- read_excel('data/usco2005.xls')
usgs2010_data <- read_excel('data/usco2010.xlsx')
usgs2015_data <- read_excel('data/usco2015v2.0.xlsx')

usgs2000_data[usgs2000_data=="" | is.na(usgs2000_data) | usgs2000_data=="--"] <- 0
usgs2005_data[usgs2005_data=="" | is.na(usgs2005_data) | usgs2005_data=="--"] <- 0
usgs1995_data[usgs1995_data=="" | is.na(usgs1995_data) | usgs1995_data=="--"] <- 0
usgs2015_data[usgs2015_data=="" | is.na(usgs2015_data) | usgs2015_data=="--"] <- 0
usgs2010_data[usgs2010_data=="" | is.na(usgs2010_data) | usgs2010_data=="--"] <- 0
usgs1990_data[usgs1990_data=="" | is.na(usgs1990_data) | usgs1990_data=="--"] <- 0


split1990 <- split(usgs1990_data, usgs1990_data$state)
split1995 <- split(usgs1995_data, usgs1995_data$State)
split2000 <- split(usgs2000_data, usgs2000_data$STATE)
split2005 <- split(usgs2005_data, usgs2005_data$STATE)
split2010 <- split(usgs2010_data, usgs2010_data$STATE)
split2015 <- split(usgs2015_data, usgs2015_data$STATE)

water <- read_xlsx("US-TotW-ByState.xlsx")
water$roundNum <- round(water$State,2)

usa <- getData("GADM", country="USA", level=1) 
usa$Data <- water$roundNum

pal <- colorQuantile("Reds", NULL, n = 9)
pallette <- colorQuantile("Reds", NULL, n = 9)


totalUsageH <- data.frame(
  usage = c(1189.38,1012.43,446.65,252.31, 258.07),
  year = c("1990","1995", "2005", "2010", "2015")
)



total <- function(data, state, col){
  new <- data[[state]]
  return(sum(new[[col]]))
}


cal1990<-total(split1990, "CA", "to-frtot")

cal1995 <- total(split1995, "CA", "TO-WFrTo")

cal2000<-total(split2000, "CA", "TO-WFrTo")

cal2005<-sum(split2005$CA$`TO-WFrTo`)

cal2010<-total(split2010, "CA", "TO-WFrTo")

cal2015<-total(split2015, "CA", "TO-WFrTo")


totalUsageCAL <- data.frame(
  usage = c(cal1990,cal1995,cal2000,cal2005,cal2010, cal2015),
  year = c("1990","1995", "2000","2005", "2010", "2015")
)



OR1990<-total(split1990, "OR", "to-frtot")

OR1995 <- total(split1995, "OR", "TO-WFrTo")

#OR2000<-total(split2000, "OR", "TO-WFrTo")

OR2005 <- total(split2005, "OR", "TO-WFrTo")

OR2010<-total(split2010, "OR", "TO-WFrTo")

OR2015<-total(split2015, "OR", "TO-WFrTo")

totalUsageOR <- data.frame(
  usage = c(OR1990,OR1995,OR2005,OR2010, OR2015),
  year = c("1990","1995", "2005", "2010", "2015")
)

tot90 <-sum(usgs1990_data$`to-frtot`)
tot95 <- sum(usgs1995_data$`TO-WFrTo`)#temp_tot00 <- sum(usgs2000_data$`TO-WFrTo`)
tot05 <- sum(usgs2005_data$`TO-WFrTo`)
tot00 = (tot95 + tot05) / 2
tot10 <- sum(usgs2010_data$`TO-WFrTo`)
total15 <- sum(usgs2015_data$`TO-WFrTo`)

all <- data.frame(
  usage = c(tot90, tot95, tot00, tot05, tot10,total15),
  year = c("1990", "1995", "2000", "2005", "2010", "2015")
)


ggplot(totalUsageH, aes(x = as.numeric(year), y = usage))+
  geom_line(size = 2) +
  geom_smooth(method='lm', formula= y~x)+
  theme_bw()

#create a pop up (onClick)
polygon_popup <- paste0("<strong>State: </strong>", usa$NAME_1, "<br>",
                        "<strong>Millions of Gallons Per Day: </strong>", usa$Data)


ui = fillPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("USA", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 20, 
      plotOutput(outputId = "drought", width = "150%", height = "500px"),
      selectInput("state", "Learn about a specific state:",
                  c("Total Usage" = "tot", "California" = "cal", "Oregon" = "or", "Hawaii"=
                      "ha"), width ="50%"),
      checkboxInput("legend", "Show legend", TRUE)
    )
)
data = totalUsageCAL
server <- function(input, output, session) {
  output$drought<-renderPlot({
    if(input$state == "tot"){
      data = all
    }
    if(input$state == "ha"){
      data = totalUsageH
    }
    if(input$state == "or"){
      data = totalUsageOR
    }
    if(input$state == "cal"){
      data = totalUsageCAL
    }
    else{}
    ggplot(data, aes(x = as.numeric(year), y = usage))+
      geom_line(size = 2) +
      geom_smooth(method='lm', formula= y~x)+
      theme_bw()
    
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
      pal <- colorQuantile("Reds", NULL, n = 9)
      proxy %>% addLegend(position = "bottomleft",
                          pal = pal, values = ~usa$Data,
                          title = "Water Usage",
      )
    }
  })
}
shinyApp(ui, server)











