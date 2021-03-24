library(shinydashboard)
library(pheatmap)
library(ggplot2)
library(ECharts2Shiny)
library(leaflet)

WRP <- read.csv("data/iterations_labeled_seasonal.csv") #slider data

max(WRP$Med_Q_cfs)

FMM <- read.csv("data/FFM_percentiles_wetdrybaseflowmag_02172021_baselineWRP.csv")

FlowRanges <- read.csv("data/FlowRanges_Species_RecUses_Allnodes_03142021.csv")

example<-read.csv("data/example.csv")

ui <- dashboardPage(
  dashboardHeader(title = "WRP Discharge Heatmap"),
  dashboardSidebar(
    sidebarMenu(selectInput("season", "Season:",
                            c("Wet" = "wet", "Dry" = "dry")),
                sliderInput("wrp", "WRP Discharge:",
                            min = 1, max = 100, value = 1),
                strong("Corrisponding Scenerio:"),
                span(textOutput("scen"), style = "color: green; font-size: large;
                      text-align: center;
                      font-weight: bold;"))
  ),
  dashboardBody(
    fluidRow(
      box(title = "heatmap",
          plotOutput(outputId = "heat", height = "70vh")),
      box(leafletOutput("LosAngeles", width = "100%", height = 550))
    )
  )
)



popCon <- function(name){
  link <- paste("?url=inTabpanel_", name, sep = "")
  name <- paste(sep = "<br/>",
                actionLink(link, name),
                "This one would also update to another tab")
}

sepSpec <- function(name){
  hey <- paste("species", name, sep="")
  specific <- FlowRanges[grepl(name, FlowRanges$Node),]
  
}


nodes <- data.frame("name" = c("LA11", "LA20_2","LA20", "F34D","11101250", "F57C","F37B_High",
                               "GLEN","LA3","LA8","LA13","LA14","F319","F300","F45B","F37B_Low"))
species <- data.frame("species" = c("Willow Growth", 
                                    "Willow Adult",
                                    "Steelhead Migration (Prolonged)",
                                    "Typha Growth",
                                    "Typha Adult",
                                    "Cladophora Adult",
                                    "SAS Growth",
                                    "SAS Adult",
                                    "Steelhead Migration (Burst)",
                                    "Steelhead Migration (Smolts)"))                    


#Did they fall in the range? 
fmDry <- FMM[!grepl("Wet", FMM$Season),] #dry only
fmWet <- FMM[!grepl("Dry", FMM$Season),] #wet only


flowRDry0 <- FlowRanges[!grepl("Wet_BFL_Mag_50", FlowRanges$metric),] #dry 
flowRDry1 <- flowRDry0[!grepl("Wet_BFL_Mag_10", flowRDry0$metric),] #dry 

flowRWet0 <- FlowRanges[!grepl("DS_Mag_50", FlowRanges$metric),] #wet 
flowRWet1 <- flowRWet0[!grepl("DS_Mag_10", flowRWet0$metric),] #wet




mydf <- data.frame(Observation = LETTERS[1:20],
                   InitialLat = c(34.184708,34.173175, 34.1411, 34.1411,34.1411,
                                  34.1411,34.1411, 34.1411,34.1411, 34.1411,
                                  34.1411, 34.1411,34.1411, 34.1411,34.1411,
                                  34.1411,34.1411,34.1411,34.1411,34.1411),
                   InitialLong = c(-118.514642,-118.484394, -118.378453, -118.378453,
                                   -118.378453, -118.378453, -118.378453, -118.378453,
                                   -118.378453, -118.378453, -118.378453, -118.378453,
                                   -118.378453, -118.378453,-118.378453,-118.378453,
                                   -118.378453,-118.378453,-118.378453,-118.378453),
                   NewLat = c(34.173175, 34.1411, 34.156839, 34.150964,
                              34.136858, 34.108175, 34.085803, 34.040528,
                              33.948297, 33.931297, 33.944528, 34.049108,
                              33.931297, 33.8639,33.842,33.851269,33.878144,
                              33.851269,33.842,33.817733),
                   NewLong = c(-118.484394,-118.378453, -118.299758, -118.27884,
                               -118.275339, -118.247706, -118.228233, -118.228603,
                               -118.173592, -118.175692, -118.164831, -118.072069,
                               -118.175692, -118.196333,-118.205233,
                               -118.211672,-118.222186,-118.211672,-118.205233,
                               -118.205728),
                   stringsAsFactors = FALSE)
mydf2 <- data.frame(group = LETTERS[1:20],
                    lat = c(mydf$InitialLat, mydf$NewLat),
                    long = c(mydf$InitialLong, mydf$NewLong))

color = 'blue'


server <- function(input, output, session) {
  
  observe(
    output$scen<-{(
      renderText(which.min(abs(WRP$Med_Q_cfs - input$wrp)))
    )}
  )
  
  output$heat<-renderPlot({
    scen <- which.min(abs(WRP$Med_Q_cfs - input$wrp))
    print(scen)
    
    if(input$season == "wet"){
      fm <- fmWet
      flow <- flowRWet1
    }
    else{
      fm <- fmDry
      flow <-flowRDry1
    }
    
    row1 <- fm[which(fm$ReportingNode == "F300" & fm$Scenario == scen),]
    F300p <- row1$p50
    
    print(F300p)
    
    row2 <- fm[which(fm$ReportingNode == "F319" & fm$Scenario == scen),]
    F319p <- row2$p50
    
    
    row3 <- fm[which(fm$ReportingNode == "F34D" & fm$Scenario == scen),]
    F34Dp <- row3$p50
    
    
    row4 <- fm[which(fm$ReportingNode == "F37B" & fm$Scenario == scen),]
    F37Bp <- row4$p50
    
    
    row5 <- fm[which(fm$ReportingNode == "F45B" & fm$Scenario == scen),]
    F45Bp <- row5$p50
    
    
    row6 <- fm[which(fm$ReportingNode == "F57C" & fm$Scenario == scen),]
    F57Cp <- row6$p50
    
    
    row7 <- fm[which(fm$ReportingNode == "GLEN" & fm$Scenario == scen),]
    GLENp <- row7$p50
    
    
    row8 <- fm[which(fm$ReportingNode == "LA1" & fm$Scenario == scen),]
    LA1p <- row8$p50
    
    
    row9 <- fm[which(fm$ReportingNode == "LA11" & fm$Scenario == scen),]
    LA11p <- row9$p50
    
    
    row10 <- fm[which(fm$ReportingNode == "LA13" & fm$Scenario == scen),]
    LA13p <- row10$p50
    
    
    row11 <- fm[which(fm$ReportingNode == "LA14" & fm$Scenario == scen),]
    LA14p <- row11$p50
    
    
    row12 <- fm[which(fm$ReportingNode == "LA2" & fm$Scenario == scen),]
    LA2p <- row12$p50
    
    
    row13 <- fm[which(fm$ReportingNode == "LA20_2" & fm$Scenario == scen),]
    LA20_2p<- row13$p50
    
    
    row14 <- fm[which(fm$ReportingNode == "LA20" & fm$Scenario == scen),]
    LA20p <- row14$p50
    
    
    row15 <- fm[which(fm$ReportingNode == "LA3" & fm$Scenario == scen),]
    LA3p <- row15$p50
    
    
    row16 <- fm[which(fm$ReportingNode == "LA8" & fm$Scenario == scen),]
    LA8p <- row16$p50
    
    first_c <- c("F300","F319","F34D", "F37B", "F45B", "F57C","GLEN", "LA1",
                 "LA11", "LA13","LA14", "LA2", "LA20_2", "LA20", "LA3", "LA8")
    
    
    
    
    range <- flowRWet1[which(flowRWet1$Species_Label == "Willow Adult" & flowRWet1$Node == "F57C"),]
    as.integer(range$Lower_Limit[1])
    
    range <- flowRWet1[which(flowRWet1$Species_Label == "Steelhead Migration (Prolonged)" & flowRWet1$Node == "LA1"),]
    range
    x <- 54
    if(x > range$Lower_Limit[1]){
      print("hi")
    }
    range
    new = 0 
    fowGet <- function(name, species, num, var){
      range <- na.omit(flow[which(flow$Species_Label == species & flow$Node == name),])
      
      print(var)
      
      if(nrow(range) == 0){
        print("Caught row")
        print(num)
        new <- 0
        return(new)
      }
      if(toString(range$Lower_Limit) == "TBD" | toString(range$Upper_Limit) == "TBD"){
        print("caught tbd")
        new = 0 
        return(new)
      }
      if(var > as.integer(range$Lower_Limit[1]) && var < as.integer(range$Upper_Limit[1])){
          print("Valid")
          print(num)
          new <- 1
          return(new)
      }
      else{
        print("else caught")
        print(num)
        new <- 0
        return(new)
      }
    }
    
    
    F300WA <- fowGet("F300", "Willow Adult", 1, F300p)   
    F319WA <-fowGet("F319", "Willow Adult", 2, F319p)
    F34DWA <-fowGet("F34D", "Willow Adult", 3, F34Dp)
    F37BWA <- fowGet("F37B", "Willow Adult", 4, F37Bp)
    F45BWA <- fowGet("F45B", "Willow Adult", 5, F45Bp)
    F57CWA <- fowGet("F57C", "Willow Adult", 6, F57Cp)
    GLENWA<-fowGet("GLEN", "Willow Adult", 7, GLENp)
    LA1WA <-fowGet("LA1", "Willow Adult", 8, LA1p)
    LA11WA <-fowGet("LA11", "Willow Adult", 9, LA11p)
    LA13WA <-fowGet("LA13", "Willow Adult", 10, LA13p)
    LA14WA<-fowGet("LA14", "Willow Adult", 11, LA14p)
    LA2WA<-fowGet("LA2", "Willow Adult", 12, LA2p)
    LA20_2WA<-fowGet("LA20_2", "Willow Adult", 13, LA20_2p)
    LA20WA<-fowGet("LA20", "Willow Adult", 14, LA20p)
    LA3WA<-fowGet("LA3", "Willow Adult", 15, LA3p)
    LA8WA<-fowGet("LA8", "Willow Adult", 16, LA8p)
    second_c <- c(F300WA,F319WA,F34DWA, F37BWA, F45BWA, F57CWA,GLENWA, LA1WA,
                  LA11WA, LA13WA,LA14WA, LA2WA, LA20_2WA, LA20WA, LA3WA, LA8WA)
    
    
    F300TA <- fowGet("F300", "Typha Adult", 1, F300p)   
    F319TA <-fowGet("F319", "Typha Adult", 2, F319p)
    F34DTA <-fowGet("F34D", "Typha Adult", 3, F34Dp)
    F347BTA <- fowGet("F37B", "Typha Adult", 4, F37Bp)
    F45BTA <- fowGet("F45B", "Typha Adult", 5, F45Bp)
    F57CTA <- fowGet("F57C", "Typha Adult", 6, F57Cp)
    GLENTA<-fowGet("GLEN", "Typha Adult", 7, GLENp)
    LA1TA <-fowGet("LA1", "Typha Adult", 8, LA1p)
    LA11TA <-fowGet("LA11", "Typha Adult", 9,LA11p)
    LA13TA <-fowGet("LA13", "Typha Adult", 10,LA13p)
    LA14TA<-fowGet("LA14", "Typha Adult", 11,LA14p)
    LA2TA<-fowGet("LA2", "Typha Adult", 12,LA2p)
    LA20_2TA<-fowGet("LA20_2", "Typha Adult", 13,LA20_2p)
    LA20TA<-fowGet("LA20", "Typha Adult", 14,LA20p)
    LA3TA<-fowGet("LA3", "Typha Adult", 15,LA3p)
    LA8TA<-fowGet("LA8", "Typha Adult", 16,LA8p)
    
    
    third_c <- c(F300TA,F319TA,F34DTA, F347BTA, F45BTA, F57CTA,GLENTA, LA1TA,
                 LA11TA, LA13TA,LA14TA, LA2TA, LA20_2TA, LA20TA, LA3TA, LA8TA)
    
    F300MP <- fowGet("F300", "Steelhead Migration (Prolonged)", 1,F300p)   
    F319MP <-fowGet("F319", "Steelhead Migration (Prolonged)", 2,F319p)
    F34DMP <-fowGet("F34D", "Steelhead Migration (Prolonged)", 3,F34Dp)
    F347BMP <- fowGet("F37B", "Steelhead Migration (Prolonged)", 4,F37Bp)
    F45BMP <- fowGet("F45B", "Steelhead Migration (Prolonged)", 5,F45Bp)
    F57CMP <- fowGet("F57C", "Steelhead Migration (Prolonged)", 6,F57Cp)
    GLENMP<-fowGet("GLEN", "Steelhead Migration (Prolonged)", 7,GLENp)
    LA1MP <-fowGet("LA1", "Steelhead Migration (Prolonged)", 8,LA1p)
    LA11MP <-fowGet("LA11", "Steelhead Migration (Prolonged)", 9,LA11p)
    LA13MP <-fowGet("LA13", "Steelhead Migration (Prolonged)", 10,LA13p)
    LA14MP<-fowGet("LA14", "Steelhead Migration (Prolonged)", 11,LA14p)
    LA2MP<-fowGet("LA2", "Steelhead Migration (Prolonged)", 12,LA2p)
    LA20_2MP<-fowGet("LA20_2", "Steelhead Migration (Prolonged)", 13,LA20_2p)
    LA20MP<-fowGet("LA20", "Steelhead Migration (Prolonged)", 14,LA20p)
    LA3MP<-fowGet("LA3", "Steelhead Migration (Prolonged)", 15,LA3p)
    LA8MP<-fowGet("LA8", "Steelhead Migration (Prolonged)", 16,LA8p)
    
    
    fourth_c <- c(F300MP,F319MP,F34DMP, F347BMP, F45BMP, F57CMP,GLENMP, LA1MP,
                  LA11MP, LA13MP,LA14MP, LA2MP, LA20_2MP, LA20MP, LA3MP, LA8MP)
    
    F300SS <- fowGet("F300", "Steelhead Migration (Smolts)", 1,F300p)   
    F319SS <-fowGet("F319", "Steelhead Migration (Smolts)", 2,F319p)
    F34DSS <-fowGet("F34D", "Steelhead Migration (Smolts)", 3,F34Dp)
    F347BSS <- fowGet("F37B", "Steelhead Migration (Smolts)", 4,F37Bp)
    F45BSS <- fowGet("F45B", "Steelhead Migration (Smolts)", 5,F45Bp)
    F57CSS <- fowGet("F57C", "Steelhead Migration (Smolts)", 6,F57Cp)
    GLENSS<-fowGet("GLEN", "Steelhead Migration (Smolts)", 7,GLENp)
    LA1SS <-fowGet("LA1", "Steelhead Migration (Smolts)", 8,LA1p)
    LA11SS <-fowGet("LA11", "Steelhead Migration (Smolts)", 9,LA11p)
    LA13SS <-fowGet("LA13", "Steelhead Migration (Smolts)", 10,LA13p)
    LA14SS<-fowGet("LA14", "Steelhead Migration (Smolts)", 11,LA14p)
    LA2SS<-fowGet("LA2", "Steelhead Migration (Smolts)", 12,LA2p)
    LA20_2SS<-fowGet("LA20_2", "Steelhead Migration (Smolts)", 13,LA20_2p)
    LA20SS<-fowGet("LA20", "Steelhead Migration (Smolts)", 14,LA20p)
    LA3SS<-fowGet("LA3", "Steelhead Migration (Smolts)", 15,LA3p)
    LA8SS<-fowGet("LA8", "Steelhead Migration (Smolts)", 16,LA8p)
    
    fifth_c <- c(F300SS,F319SS,F34DSS, F347BSS, F45BSS, F57CSS,GLENSS, LA1SS,
                 LA11SS, LA13SS,LA14SS, LA2SS, LA20_2SS, LA20SS, LA3SS, LA8SS)
    
    F300SB <- fowGet("F300", "Steelhead Migration (Burst)", 1,F300p)   
    F319SB <-fowGet("F319", "Steelhead Migration (Burst)", 2,F319p)
    F34DSB <-fowGet("F34D", "Steelhead Migration (Burst)", 3,F34Dp)
    F347BSB <- fowGet("F37B", "Steelhead Migration (Burst)", 4,F37Bp)
    F45BSB <- fowGet("F45B", "Steelhead Migration (Burst)", 5,F45Bp)
    F57CSB <- fowGet("F57C", "Steelhead Migration (Burst)", 6,F57Cp)
    GLENSB<-fowGet("GLEN", "Steelhead Migration (Burst)", 7,GLENp)
    LA1SB <-fowGet("LA1", "Steelhead Migration (Burst)", 8,LA1p)
    LA11SB <-fowGet("LA11", "Steelhead Migration (Burst)", 9,LA11p)
    LA13SB <-fowGet("LA13", "Steelhead Migration (Burst)", 10,LA13p)
    LA14SB<-fowGet("LA14", "Steelhead Migration (Burst)", 11,LA14p)
    LA2SB<-fowGet("LA2", "Steelhead Migration (Burst)", 12,LA2p)
    LA20_2SB<-fowGet("LA20_2", "Steelhead Migration (Burst)", 13,LA20_2p)
    LA20SB<-fowGet("LA20", "Steelhead Migration (Burst)", 14,LA20p)
    LA3SB<-fowGet("LA3", "Steelhead Migration (Burst)", 15,LA3p)
    LA8SB<-fowGet("LA8", "Steelhead Migration (Burst)", 16,LA8p)
    
    sixth_c <- c(F300SB,F319SB,F34DSB,F347BSB, F45BSB, F57CSB,GLENSB, LA1SB,
                 LA11SB, LA13SB,LA14SB, LA2SB, LA20_2SB, LA20SB, LA3SB, LA8SB)
    
    F300AD <- fowGet("F300", "SAS Adult", 1,F300p)   
    F319AD <-fowGet("F319", "SAS Adult", 2,F319p)
    F34DAD <-fowGet("F34D", "SAS Adult", 3,F34Dp)
    F347BAD <- fowGet("F37B", "SAS Adult", 4,F37Bp)
    F45BAD <- fowGet("F45B", "SAS Adult", 5,F45Bp)
    F57CAD <- fowGet("F57C", "SAS Adult", 6,F57Cp)
    GLENAD<-fowGet("GLEN", "SAS Adult", 7,GLENp)
    LA1AD <-fowGet("LA1", "SAS Adult", 8,LA1p)
    LA11AD <-fowGet("LA11", "SAS Adult", 9,LA11p)
    LA13AD <-fowGet("LA13", "SAS Adult", 10,LA13p)
    LA14AD<-fowGet("LA14", "SAS Adult", 11,LA14p)
    LA2AD<-fowGet("LA2", "SAS Adult", 12,LA2p)
    LA20_2AD<-fowGet("LA20_2", "SAS Adult", 13,LA20_2p)
    LA20AD<-fowGet("LA20", "SAS Adult", 14,LA20p)
    LA3AD<-fowGet("LA3", "SAS Adult", 15,LA3p)
    LA8AD<-fowGet("LA8", "SAS Adult", 16,LA8p)
    
    seventh_c <- c(F300AD,F319AD,F34DAD, F347BAD, F45BAD, F57CAD,GLENAD, LA1AD,
                   LA11AD, LA13AD,LA14AD, LA2AD, LA20_2AD, LA20AD, LA3AD, LA8AD)
    
    F300KA <- fowGet("F300", "Rec. Use Kayak", 1,F300p)   
    F319KA <-fowGet("F319", "Rec. Use Kayak", 2,F319p)
    F34DKA <-fowGet("F34D", "Rec. Use Kayak", 3,F34Dp)
    F347BKA <- fowGet("F37B", "Rec. Use Kayak", 4,F37Bp)
    F45BKA <- fowGet("F45B", "Rec. Use Kayak", 5,F45Bp)
    F57CKA <- fowGet("F57C", "Rec. Use Kayak", 6,F57Cp)
    GLENKA<-fowGet("GLEN", "Rec. Use Kayak", 7,GLENp)
    LA1KA <-fowGet("LA1", "Rec. Use Kayak", 8,LA1p)
    LA11KA <-fowGet("LA11", "Rec. Use Kayak", 9,LA11p)
    LA13KA <-fowGet("LA13", "Rec. Use Kayak", 10,LA13p)
    LA14KA<-fowGet("LA14", "Rec. Use Kayak", 11,LA14p)
    LA2KA<-fowGet("LA2", "Rec. Use Kayak", 12,LA2p)
    LA20_2KA<-fowGet("LA20_2", "Rec. Use Kayak", 13,LA20_2p)
    LA20KA<-fowGet("LA20", "Rec. Use Kayak", 14,LA20p)
    LA3KA<-fowGet("LA3", "Rec. Use Kayak", 15,LA3p)
    LA8KA<-fowGet("LA8", "Rec. Use Kayak", 16,LA8p)
    
    eigth_c <- c(F300KA,F319KA,F34DKA,F347BKA, F45BKA, F57CKA,GLENKA, LA1KA,
                 LA11KA, LA13KA,LA14KA, LA2KA, LA20_2KA, LA20KA, LA3KA, LA8KA)
    
    F300FI <- fowGet("F300", "Rec. Use Fishing", 1,F300p)   
    F319FI <-fowGet("F319", "Rec. Use Fishing", 2,F319p)
    F34DFI <-fowGet("F34D", "Rec. Use Fishing", 3,F34Dp)
    F347BFI <- fowGet("F37B", "Rec. Use Fishing", 4,F37Bp)
    F45BFI <- fowGet("F45B", "Rec. Use Fishing", 5,F45Bp)
    F57CFI <- fowGet("F57C", "Rec. Use Fishing", 6,F57Cp)
    GLENFI<-fowGet("GLEN", "Rec. Use Fishing", 7,GLENp)
    LA1FI <-fowGet("LA1", "Rec. Use Fishing", 8,LA1p)
    LA11FI <-fowGet("LA11", "Rec. Use Fishing", 9,LA11p)
    LA13FI <-fowGet("LA13", "Rec. Use Fishing", 10,LA13p)
    LA14FI<-fowGet("LA14", "Rec. Use Fishing", 11,LA14p)
    LA2FI<-fowGet("LA2", "Rec. Use Fishing", 12,LA2p)
    LA20_2FI<-fowGet("LA20_2", "Rec. Use Fishing", 13,LA20_2p)
    LA20FI<-fowGet("LA20", "Rec. Use Fishing", 14,LA20p)
    LA3FI<-fowGet("LA3", "Rec. Use Fishing", 15,LA3p)
    LA8FI<-fowGet("LA8", "Rec. Use Kayak", 16,LA8p)
    
    
    ninth_c <- c(F300FI,F319FI,F34DFI,F347BFI, F45BFI, F57CFI,GLENFI, LA1FI,
                 LA11FI, LA13FI,LA14FI, LA2FI, LA20_2FI, LA20FI, LA3FI, LA8FI)
    
    F300CA <- fowGet("F300", "Cladophora Adult", 1,F300p)   
    F319CA <-fowGet("F319", "Cladophora Adult", 2,F319p)
    F34DCA <-fowGet("F34D", "Cladophora Adult", 3,F34Dp)
    F347BCA <- fowGet("F37B", "Cladophora Adult", 4,F37Bp)
    F45BCA <- fowGet("F45B", "Cladophora Adult", 5,F45Bp)
    F57CCA <- fowGet("F57C", "Cladophora Adult", 6,F57Cp)
    GLENCA<-fowGet("GLEN", "Cladophora Adult", 7,GLENp)
    LA1CA <-fowGet("LA1", "Cladophora Adult", 8,LA1p)
    LA11CA <-fowGet("LA11", "Cladophora Adult", 9,LA11p)
    LA13CA <-fowGet("LA13", "Cladophora Adult", 10,LA13p)
    LA14CA<-fowGet("LA14", "Cladophora Adult", 11,LA14p)
    LA2CA<-fowGet("LA2", "Cladophora Adult", 12,LA2p)
    LA20_2CA<-fowGet("LA20_2", "Cladophora Adult", 13,LA20_2p)
    LA20CA<-fowGet("LA20", "Cladophora Adult", 14,LA20p)
    LA3CA<-fowGet("LA3", "Cladophora Adult", 15,LA3p)
    LA8CA<-fowGet("LA8", "Cladophora Adult", 16,LA8p)
    LA8CA<-fowGet("LA8", "Cladophora Adult", 16)
    
    tenth_c <- c(F300CA,F319CA,F34DCA,F347BCA, F45BCA, F57CCA,GLENCA, LA1CA,
                 LA11CA, LA13CA,LA14CA, LA2CA, LA20_2CA, LA20CA, LA3CA, LA8CA)
    
    
    
    
    
    finally <- data.frame(first_c, second_c, third_c, fourth_c, fifth_c, sixth_c,seventh_c, eigth_c,ninth_c, tenth_c)
    
    
    
    row.names(finally) <- finally$first_c
    finally <- finally[,2:10]
    
    print(finally)
    
    ex_matrix <- data.matrix(finally)
    
    ex_heatmap <- heatmap(ex_matrix, Rowv=NA, Colv=NA, 
                          col = cm.colors(252), scale="column",
                          margins=c(2,10))    
    ex_heatmap
    
  })
  
  PopcontentLA20 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about LA20", onclick = 'Shiny.onInputChange(\"link_clickLA20\",  Math.random())'),
                          "Los Angeles River within Sepulveda Basin")
  PopcontentLA11 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about LA11", onclick = 'Shiny.onInputChange(\"link_clickLA11\",  Math.random())'),
                          "Los Angeles River at Glendale Narrows")
  PopcontentLA20_2 <- paste(sep = "<br/>",
                            ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                            actionLink("?url=inTabset/Home", "Learn about LA20_2", onclick = 'Shiny.onInputChange(\"link_clickLA20_2\",  Math.random())'),
                            "Los Angeles River above Sepulveda Basin")
  Popcontent11101250 <- paste(sep = "<br/>",
                              ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                              actionLink("?url=inTabset/Home", "Learn about 11101250", onclick = 'Shiny.onInputChange(\"link_click11101250\",  Math.random())'),
                              "Rio Hondo above Whittier Narrows Dam")
  PopcontentF34D <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about F34D", onclick = 'Shiny.onInputChange(\"link_clickF34D\",  Math.random())'),
                          "Los Angeles River above confluence with Rio Hondo")
  PopcontentF37B_High<- paste(sep = "<br/>",
                              ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                              actionLink("?url=inTabset/Home", "Learn about F37B_High", onclick = 'Shiny.onInputChange(\"link_clickF37B_High\",  Math.random())'),
                              "Upper Compton Creek")
  PopcontentF37B_Low <- paste(sep = "<br/>",
                              ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                              actionLink("?url=inTabset/Home", "Learn about F37B_Low", onclick = 'Shiny.onInputChange(\"link_clickF37B_Low\",  Math.random())'),
                              "Lower Compton Creek")
  PopcontentF45B <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about F45B", onclick = 'Shiny.onInputChange(\"link_clickF45B\",  Math.random())'),
                          "Rio Hondo below spreading grounds")
  PopcontentF57C <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about F57C", onclick = 'Shiny.onInputChange(\"link_clickF57C\",  Math.random())'),
                          "Los Angeles River above confluence with Arroyo Seco")
  PopcontentF300 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about F300", onclick = 'Shiny.onInputChange(\"link_clickF300\",  Math.random())'),
                          "Los Angeles River above confluence with Burbank Channel")
  PopcontentF319 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about F319", onclick = 'Shiny.onInputChange(\"link_clickF319\",  Math.random())'),
                          "Los Angeles River at Wardlow Gage")
  PopcontentGLEN <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about GLEN", onclick = 'Shiny.onInputChange(\"link_clickGLEN\",  Math.random())'),
                          "Los Angeles River below Glendale WRP")
  PopcontentLA3 <- paste(sep = "<br/>",
                         ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                         actionLink("?url=inTabset/Home", "Learn about LA3", onclick = 'Shiny.onInputChange(\"link_clickLA3\",  Math.random())'),
                         "Los Angeles River below confluence with Rio Hondo")
  PopcontentLA8 <- paste(sep = "<br/>",
                         ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                         actionLink("?url=inTabset/Home", "Learn about LA8", onclick = 'Shiny.onInputChange(\"link_clickLA8\",  Math.random())'),
                         "Los Angeles River above confluence with Rio Hondo")
  PopcontentLA13 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about LA13", onclick = 'Shiny.onInputChange(\"link_clickLA13\",  Math.random())'),
                          "Los Angeles River above Glendale WRP")
  PopcontentLA14 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about LA14", onclick = 'Shiny.onInputChange(\"link_clickLA14\",  Math.random())'),
                          "Los Angeles River below confluence with Burbank Channel")
  
  output$LosAngeles <-  renderLeaflet({
    example <- example[,2:8]
    i = 1 
    disCharge <- WRP[input$wrp,7]
    for (r in 1:nrow(example))   
      for (c in 1:ncol(example))
        if(round(disCharge /10) == 4)
          color = 'red'
    if(round(disCharge /10) == 3)
      color = 'orange'
    
    LosAngeles <- leaflet(options = leafletOptions( 
      minZoom = 8,     maxZoom = 16)) %>%
      setView(lng = -118.2437, lat =34.0522, zoom = 10)%>%
      addTiles(options = providerTileOptions(opacity = .9)) %>%
      addCircleMarkers(lng=-118.247706, lat=34.108175, radius = 11,
                       opacity = 0.8, color = "#006400", 
                       popup=PopcontentLA11)%>% #MarkerHome
      addCircleMarkers(lng=-118.514642, lat=34.184708, radius = 11, 
                       color =  "#006400",popup=PopcontentLA20) %>%
      addCircleMarkers(lng=-118.484394, lat=34.173175, radius = 11, 
                       color =  "#006400",popup=PopcontentLA20_2)%>%
      addCircleMarkers(lng=-118.173592, lat=33.948297, radius = 11, 
                       color =  "#006400",popup=PopcontentF34D) %>%
      addCircleMarkers(lng=-118.228233, lat=34.085803, radius = 11, 
                       color =  "#006400",popup=PopcontentF57C) %>%
      addCircleMarkers(lng=-118.222186, lat=33.878144, radius = 11, 
                       color =  "#006400",popup=PopcontentF37B_High) %>%
      addCircleMarkers(lng=-118.275339, lat=34.136858, radius = 11, 
                       color =  "#006400",popup=PopcontentGLEN) %>%
      addCircleMarkers(lng=-118.196333, lat=33.8639, radius = 11, 
                       color =  "#006400",popup=PopcontentLA3) %>%
      addCircleMarkers(lng=-118.228603, lat=34.040528, radius = 11, 
                       color =  "#006400",popup=PopcontentLA8) %>%
      addCircleMarkers(lng=-118.299758, lat=34.156839, radius = 11, 
                       color =  "#006400",popup=PopcontentLA14) %>%
      addCircleMarkers(lng=-118.205728, lat=33.817733, radius = 11, 
                       color =  "#006400",popup=PopcontentF319) %>%
      addCircleMarkers(lng=-118.378453, lat=34.1411, radius = 11, 
                       color =  "#006400",popup=PopcontentF300) %>%
      addCircleMarkers(lng=-118.164831, lat=33.944528, radius = 11, 
                       color =  "#006400",popup=PopcontentF45B) %>%
      addCircleMarkers(lng=-118.211672, lat=33.851269, radius = 11, 
                       color =  "#006400",popup=PopcontentF37B_Low) %>%
      addCircleMarkers(lng=-118.072069, lat=34.049108, radius = 11, 
                       color =  "#006400",popup=Popcontent11101250) %>%
      addCircleMarkers(lng=-118.278844, lat=34.150964, radius = 11, 
                       color =  "#006400",popup=PopcontentLA13) %>%
      addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group, color = color)
    
    
    
  })  
}




shinyApp(ui, server)

