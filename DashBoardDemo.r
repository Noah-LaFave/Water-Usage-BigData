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


sum(split2015$CA$`TP-TotPop`) #per 1000

usa <- getData("GADM", country="USA", level=1) 

#Data frame that will hold the total water usage for a state and the given year
USAData <- data.frame(matrix(ncol=7, nrow=0))
x <- c("state", "1990", "1995", "2000", "2005", "2010", "2015") #headers
colnames(USAData) <- x #add the headers to the data frame USAData

#State abbreviations including DC
stateAbbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
               "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
               "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
               "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
               "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

#function that adds up the entire column given a data set and state
total <- function(data, state, col){
  new <- data[[state]]
  return(sum(new[[col]]))
}

#data frame we want to return that has usage and corresponding year (per 1000 ppl)
returndF <- function(state){
  #Dividing the total water usage by the total population to make it more meaningful.
  var<-data.frame(
    usage = c(total(split1990, state, "to-frtot") / total(split1990, state, "po-total"),
              total(split1995, state, "TO-WFrTo") / total(split1995, state, "TotalPop"),
              total(split2000, state, "TO-WFrTo") / total(split2000, state, "TP-TotPop"), 
              total(split2005, state, "TO-WFrTo") / total(split2005, state, "TP-TotPop"),
              total(split2010, state, "TO-WFrTo") / total(split2010, state, "TP-TotPop"),
              total(split2015, state, "TO-WFrTo") / total(split2015, state, "TP-TotPop")),
    year = c("1990","1995","2000", "2005", "2010", "2015")
  )
  var$usage <- round(var$usage, 3)
  return(var)
}


# test function of the one above
returndD <- function(state){
  
  var<-data.frame(
    usage = c(total(split1990, state, "to-frtot") / total(split1990, state, "po-total"),
              total(split1995, state, "TO-WFrTo") / total(split1995, state, "TotalPop"),
              #total(split2000, state, "TO-WFrTo") / total(split2000, state, "TP-TotPop"), 
              total(split2005, state, "TO-WFrTo") / total(split2005, state, "TP-TotPop"),
              total(split2010, state, "TO-WFrTo") / total(split2010, state, "TP-TotPop"),
              total(split2015, state, "TO-WFrTo") / total(split2015, state, "TP-TotPop")),
    year = c("1990","1995", "2005", "2010", "2015")
  )
  var$usage <- round(var$usage, 3)
  return(var)
}

#loops through each state, get's the amount of water usage for a given year,
# and adds it to another data frame
for (state in stateAbbr) {
  var <- returndF(state)
  USAData[nrow(USAData)+1, ] <- c(state, var$usage)
}

water <- 0
usa$Data <- 0

pal <- colorQuantile("Blues", NULL, n = 9)
pallette <- colorQuantile("Blues", NULL, n = 9)
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
            selectInput("year", "Pick a Year: ", c("1990", "1995", "2005",
                                                   "2010", "2015"))),
        box(title = strong("Drought Levels"), status = 'primary', solidHeader = TRUE,
            plotOutput(outputId = "drought"),
            p(),
            selectInput("state", "Learn about a specific state:",
                        c("Total Usage" = "tot","ALABAMA" = "AL", "ALASKA"= "AK", "ARIZONA" = "AZ",
                          "ARKANSAS" = "AR", "CALIFORNIA"  = "CA", "COLORADO"= "CO",
                          "CONNECTICUT" = "CT", "DELAWARE" = "DE", "DISTRICT OF COLUMBIA"= 
                            "DC", "FLORIDA" = "FL", "GEORGIA" = "GA", "HAWAII" = "HI",
                          "IDAHO" = "ID", "ILLINOIS" = "IL", "INDIANA" = "IN", "IOWA" = 
                            "IA", "KANSAS" = "KS", "KENTUCKY" = "KY", "LOUISIANA" = 
                            "LA", "MAINE" = "ME", "MARYLAND" = "MD", "MASSACHUSETTS" = 
                            "MA", "MICHIGAN" = "MI", "MINNESOTA" = "MN", "MISSISSIPPI" = 
                            "MS", "MISSOURI" = "MO", "MONTANA" = "MT","NEBRASKA"= 
                            "NE","NEVADA" = "NV", "NEW HAMPSHIRE" = "NH", "NEW JERSEY"= 
                            "NJ", "NEW MEXICO" = "NM", "NEW YORK" = "NY", "NORTH CAROLINA" = 
                            "NC", "NORTH DAKOTA" = "ND", "OHIO" = "OH","OKLAHOMA"="OK",
                          "OREGON" = "OR", "PENNSYLVANIA"= "PA", "PUERTO RICO"= "PR",
                          "RHODE ISLAND" = "RI", "SOUTH CAROLINA" = "SC", "SOUTH DAKOTA" =
                            "SD", "TENNESSEE" = "TN", "TEXAS" = "TX", "UTAH" = "UT",
                          "VERMONT" = "VT", "VIRGINIA" = "VA", "WASHINGTON" = "WA",
                          "WEST VIRGINIA" = "WV","WISCONSIN" = "WI","WYOMING"= "WY"), width ="50%"))
      )
    )
  )
)
#data = totalUsageCAL
server <- function(input, output, session) {
  output$drought<-renderPlot({
    title <- paste("Total Usage Per 1000 People in ", input$state, sep="")
    data <- returndD(input$state)
    if(input$state == "tot"){
      data <- all
      title <- "Total Water Usage in the US"
    }
    
    print(data)
    ggplot(data, aes(x = as.numeric(year), y = usage))+
      geom_point(size = 4, color = "black")+ 
      geom_smooth(method='lm', formula= y~x)+
      theme_bw()+
      theme(axis.text = element_text(size = 12),
            axis.title=element_text(size=14,face="bold"),
            legend.text = element_text(size = 15))+
      xlab("Years")+ ylab("Millions Of Gallons") + labs(title = title)
    
  })
  output$USA <-renderLeaflet({
    
    water <- USAData[[input$year]]
    usa$Data <- water
    
    usa$Data <- as.double(usa$Data)
    polygon_popup <- paste0("<strong>State: </strong>", usa$NAME_1, "<br>",
                            "<strong>Millions of Gallons Per 1000 People: </strong>", usa$Data)
    
    USA = leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% #the actual map of the world
      setView(-98.5795, 39.8283,
              zoom = 4) %>% 
      addPolygons(data = usa, 
                  fillColor= ~pal(Data), #the larger the number, more red it is 
                  fillOpacity = 0.7, 
                  weight = 2, #of black edges
                  color = "black",
                  popup = polygon_popup)  #from library 
    #addLegend(USA, "bottomleft", pal = pallette, values = 0:20)
  })
}
shinyApp(ui, server)
