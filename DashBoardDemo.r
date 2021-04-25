library(carData)
library(ggplot2)
library(leaflet)
library(readxl)
library(raster)
library(RColorBrewer)
library(readxl)
library(shinydashboard)

#Read in the data from USGS
usgs1990_data <- read_excel('data/us90co.xls')
usgs1995_data <- read_excel('data/usco1995.xlsx')
usgs2000_data <- read_excel('data/usco2000.xls')
usgs2005_data <- read_excel('data/usco2005.xls')
usgs2010_data <- read_excel('data/usco2010.xlsx')
usgs2015_data <- read_excel('data/usco2015v2.0.xlsx')

#Fix data, as of now turn NA, --, null to 0
usgs1990_data[usgs1990_data=="" | usgs1990_data=="--"] <- 0
usgs1995_data[usgs1995_data=="" | is.na(usgs1995_data) | usgs1995_data=="--"] <- 0
usgs2000_data[usgs2000_data=="" | is.na(usgs2000_data) | usgs2000_data=="--"] <- 0
usgs2005_data[usgs2005_data=="" | is.na(usgs2005_data) | usgs2005_data=="--"] <- 0
usgs2010_data[usgs2010_data=="" | is.na(usgs2010_data) | usgs2010_data=="--"] <- 0
usgs2015_data[usgs2015_data=="" | is.na(usgs2015_data) ] <- 0 #error

split1990 <- split(usgs1990_data, usgs1990_data$state)
split1995 <- split(usgs1995_data, usgs1995_data$State)
split2000 <- split(usgs2000_data, usgs2000_data$STATE)
split2005 <- split(usgs2005_data, usgs2005_data$STATE)
split2010 <- split(usgs2010_data, usgs2010_data$STATE)
split2015 <- split(usgs2015_data, usgs2015_data$STATE)


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


returndF <- function(state){
  var<-data.frame(
    usage = c(total(split1990, state, "to-frtot"),
              total(split1995, state, "TO-WFrTo"),
              total(split2000, state, "TO-WFrTo"), 
              total(split2005, state, "TO-WFrTo"),
              total(split2010, state, "TO-WFrTo"),
              total(split2015, state, "TO-WFrTo")),
    year = c("1990","1995","2000", "2005", "2010", "2015")
  )
  return(var)
}
cal <- returndF("CA")

USAData <- data.frame(matrix(ncol=7, nrow=0))
x <- c("state", "1990", "1995", "2000", "2005", "2010", "2015")
colnames(USAData) <- x

stateAbbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
               "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
               "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
               "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

for (state in stateAbbr) {
  var <- returndF(state)
  USAData[nrow(USAData)+1, ] <- c(state, var$usage)
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


#create a pop up (onClick)
polygon_popup <- paste0("<strong>State: </strong>", usa$NAME_1, "<br>",
                        "<strong>Millions of Gallons Per Day: </strong>", usa$Data)


ui = dashboardPage(
  dashboardHeader(title = "Drought In the US"),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabname = "over", icon=icon('map')))),
  dashboardBody(
    tabItem(
      tabName = "over",
      fluidPage(
        tags$style(HTML("


      .box.box-solid.box-primary>.box-header {
        color:#060606;
        background:#006994 
                          }
      
      .box.box-solid.box-primary{
      border-bottom-color:#006994 ;
      border-left-color:#006994 ;
      border-right-color:#006994 ;
      border-top-color:#006994 ;
      }
      .box.box-solid.box-success>.box-header {
        color:#fff;
        background:#C70039
                          }
      
      .box.box-solid.box-success{
      border-bottom-color:#C70039;
      border-left-color:#C70039;
      border-right-color:#C70039;
      border-top-color:#C70039;
      }

                                    ")),
        box(title = strong("United States Map"), status = "success", solidHeader = TRUE,
            leafletOutput("USA"),
            p(),
            checkboxInput("legend", "Show legend", FALSE)),
        box(title = strong("Drought Levels"), status = 'primary', solidHeader = TRUE,
            plotOutput(outputId = "drought"),
            p(),
            selectInput("state", "Learn about a specific state:",
                        c("Total Usage" = "tot", "California" = "CA", "Oregon" = "OR", "Hawaii"=
                            "HI", "Texas = "), width ="50%"),
            sliderInput("year", "Heatmap Year", min = 1990, step = 5, max = 2015, value = 2010, format="####"))
      )
    )
  )
)
data = totalUsageCAL
server <- function(input, output, session) {
  output$drought<-renderPlot({
    
    
    data <- returndF(input$state)
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















