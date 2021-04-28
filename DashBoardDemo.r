library(ggplot2)
library(leaflet)
library(readxl)
library(raster)
library(RColorBrewer)
library(readxl)
library(shinydashboard, warn.conflicts = FALSE)

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
usgs2015_data[usgs2015_data=="" | is.na(usgs2015_data) ] <- 0 #some files produce errors, removing some checks

split1990 <- split(usgs1990_data, usgs1990_data$state) #splitting up the data for ease of function calling 
split1995 <- split(usgs1995_data, usgs1995_data$State) #doing it per year 
split2000 <- split(usgs2000_data, usgs2000_data$STATE)
split2005 <- split(usgs2005_data, usgs2005_data$STATE)
split2010 <- split(usgs2010_data, usgs2010_data$STATE)
split2015 <- split(usgs2015_data, usgs2015_data$STATE)



usa <- getData("GADM", country="USA", level=1) #data for polygon locations based off external lib 

USAData <- data.frame(matrix(ncol=7, nrow=0)) #creating dataframe for each year

x <- c("state", "1990", "1995", "2000", "2005", "2010", "2015")

colnames(USAData) <- x

stateAbbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
               "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
               "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
               "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
               "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY") #df for each year and state for heatmap 


total <- function(data, state, col){ #function to return total water usage per 1000 people
  new <- data[[state]]
  return(sum(new[[col]]))
}


returndF <- function(state){ #main function to create a df filled with sum of a state for each year 
  
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
returndD <- function(state){ #same idea but cutting out the 2000 year b/c that data is not complete enough
  
  var<-data.frame(
    usage = c(total(split1990, state, "to-frtot") / total(split1990, state, "po-total"),
              total(split1995, state, "TO-WFrTo") / total(split1995, state, "TotalPop"),
              #total(split2000, state, "TO-WFrTo") / total(split2000, state, "TP-TotPop"), 
              total(split2005, state, "TO-WFrTo") / total(split2005, state, "TP-TotPop"),
              total(split2010, state, "TO-WFrTo") / total(split2010, state, "TP-TotPop"),
              total(split2015, state, "TO-WFrTo") / total(split2015, state, "TP-TotPop")),
    year = c(1990,1995, 2005, 2010, 2015)
  )
  var$usage <- round(var$usage, 3)
  return(var)
}

for (state in stateAbbr) { #creating the final df for the heatmap, putting it all together 
  var <- returndF(state)
  USAData[nrow(USAData)+1, ] <- c(state, var$usage)
  
}

water <- 0 #assigning some var for future use 
usa$Data <- 0

pal <- colorQuantile("Blues", NULL, n = 8) #pallete for heatmap 


tot90 <-sum(usgs1990_data$`to-frtot`)
tot95 <- sum(usgs1995_data$`TO-WFrTo`)#temp_tot00 <- sum(usgs2000_data$`TO-WFrTo`)
tot05 <- sum(usgs2005_data$`TO-WFrTo`)
tot00 = (tot95 + tot05) / 2 #filler value because 2000 is not complete enough for use 
tot10 <- sum(usgs2010_data$`TO-WFrTo`) #totals water usage per each year 
total15 <- sum(usgs2015_data$`TO-WFrTo`)


all <- data.frame( #making it a dataframe 
  usage = c(tot90, tot95, tot00, tot05, tot10,total15),
  year = c(1990, 1995, 2000, 2005, 2010, 2015)
)


#create a pop up (onClick)
polygon_popup <- paste0("<strong>State: </strong>", usa$NAME_1, "<br>",
                        "<strong>Millions of Gallons Per Day: </strong>", usa$Data)


ui = dashboardPage(
  dashboardHeader(title = "Water Usage"), #some r shiny dashboard, creating sidebar and stuff 
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabname = "over", icon=icon('map')))),
  dashboardBody(
    tabItem(
      tabName = "over",
      fluidPage( #some css code for making the boxes look nicer, nothing crazy 
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
        #box for US heatmap 
        box(title = strong("United States Map"), status = "success", solidHeader = TRUE,
            leafletOutput("USA"),
            selectInput("year", "Pick a Year: ", c("1990", "1995", "2005",
                                                   "2010", "2015"))),
        #box for water usage 
        box(title = strong("Daily Water Usage"), status = 'primary', solidHeader = TRUE,
            plotOutput(outputId = "drought"),
            p(),
            #select input for specific state 
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
                          "WEST VIRGINIA" = "WV","WISCONSIN" = "WI","WYOMING"= "WY"), width ="50%")),
        
        #final box for prediction 
        box(title = strong("Prediction"),
            textOutput("pred"),
            tags$head(tags$style("#pred{color: red;
                                 font-size: 20px;
                                 }"
            )))
      )
    )
  )
)
data = 3 #defining it for later 
server <- function(input, output, session) {
  #output for drought plot 
  output$drought<-renderPlot({
    title <- paste("Total Usage Per 1000 People in ", input$state, sep="") #dynamically changing title 
    data <- returndD(input$state) #getting data a
    if(input$state == "tot"){ #special case for total usage 
      data <- all
      title <- "Total Water Usage in the US"
    }
    x <- data$usage
    y <- data$year
    
    w <- lm(x ~ y)
    
    p <- predict.glm(w, data.frame(y=2020), #prediction model based off the year 
                 interval = "prediction")
    data <- rbind(data, c(p, 2020))
    
    
    data$color[data$year>=2020]="red" #changing the color 
    data$color[data$year<=2015]="black"
    
    ggplot(data, aes(x = as.numeric(year), y = usage))+ #plotting it with ggplot 
      geom_point(size = 4, color = data$color)+ 
      geom_smooth(method='lm', formula= y~x)+ #lm function 
      theme_bw()+
      theme(axis.text = element_text(size = 12),
            axis.title=element_text(size=14,face="bold"),
            legend.text = element_text(size = 15))+
      xlab("Years")+ ylab("Millions Of Gallons/d") + labs(title = title)+
      scale_color_manual("Legend", values = c("black", "red")) 
    
  })
  
  output$pred <- renderText({ #creating text output for pred 
    data <- returndD(input$state)
    if(input$state == "tot"){
      data <- all
      title <- "Total Water Usage in the US"
    }
    x <- data$usage
    y <- data$year
    w <- lm(x ~ y)
    p <- predict.glm(w, data.frame(y=2020), #same idea for the previous plot 
                     interval = "confidence")
    
    p <- round(p,3)
    o <- paste("Predicted Value for 2020 (Mgal Per Day): ", p) #outputing the prediction 
    
  })
  
  output$USA <-renderLeaflet({
    
    water <- USAData[[input$year]] #getting the year from the user 
    usa$Data <- water #adding that to the usa df 

    usa$Data <- as.double(usa$Data)
    
    #when a polygon is clicked, this will show up 
    
    polygon_popup <- paste0("<strong>State: </strong>", usa$NAME_1, "<br>",
                            "<strong>Millions of Gallons Per 1000 People: </strong>", usa$Data)
    #leaflet map 
    
    USA = leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% #the actual map of the world 
      setView(-98.5795, 39.8283,
              zoom = 4) %>% 
      addPolygons(data = usa, 
                  fillColor= ~pal(Data), #the larger the number, more blue it is 
                  fillOpacity = 0.7, 
                  weight = 2, #of black edges
                  color = "black",
                  popup = polygon_popup)  #from library 
  })
}
shinyApp(ui, server) #run the server























