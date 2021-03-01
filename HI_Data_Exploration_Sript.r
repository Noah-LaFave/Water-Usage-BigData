library(readxl)

#Read in the data from USGS
usgs1990_data <- read_excel('./Data goes here/us90co.xls')
usgs1995_data <- read_excel('./Data goes here/usco1995.xlsx')
usgs2000_data <- read_excel('./Data goes here/usco2000.xls')
usgs2005_data <- read_excel('./Data goes here/usco2005.xls')
usgs2010_data <- read_excel('./Data goes here/usco2010.xlsx')
usgs2015_data <- read.csv('./Data goes here/usco2015v2.0.csv')

#Water Usage Data in Hawaii (b/c it only has 5 counties; small data)
HI_usgs1990_data <- usgs1990_data[c(545:549),] #1990
HI_usgs1995_data <- usgs1995_data[c(544:548),] #1995
HI_usgs2000_data <- usgs2000_data[c(544:548),] #2000
HI_usgs2005_data <- usgs2005_data[c(545:549),] #2005
HI_usgs2010_data <- usgs2010_data[c(547:551),] #2010
HI_usgs2015_data <- usgs2015_data[c(548:552),] #2015

#Population vs Total Water Use (in each county)
par(mfrow=c(3,2))
#1990 
HI_lm_1990 <- lm(HI_usgs1990_data$`to-total` ~ HI_usgs1990_data$`po-total`)
plot(HI_usgs1990_data$`po-total`,HI_usgs1990_data$`to-cuse`, main="1990", xlab = 
       "population", ylab = "water usage total")

#1995
HI_lm_1995 <- lm(HI_usgs1995_data$`TO-WTotl` ~ HI_usgs1995_data$TotalPop)
plot(HI_usgs1995_data$TotalPop,HI_usgs1995_data$`TO-CUTot`, main="1995", xlab = 
       "population", ylab = "water withdrawal total")

#2000
HI_lm_2000 <- lm(HI_usgs2000_data$`TO-WTotl` ~ HI_usgs2000_data$`TP-TotPop`)
plot(HI_usgs2000_data$`TP-TotPop`,HI_usgs2000_data$`TO-WTotl`,main="2000", xlab = 
       "population", ylab = "water withdrawal usage total")

#2005
HI_lm_2005 <- lm(HI_usgs2005_data$`TO-WTotl` ~ HI_usgs2005_data$`TP-TotPop`)
plot(HI_usgs2005_data$`TP-TotPop`,HI_usgs2005_data$`TO-WTotl`,main="2005", xlab = 
       "population", ylab = "water withdrawal usage total")

#2010
HI_lm_2010 <- lm(HI_usgs2010_data$`TO-WTotl` ~ HI_usgs2010_data$`TP-TotPop`)
plot(HI_usgs2010_data$`TP-TotPop`,HI_usgs2010_data$`TO-WTotl`, main="2010",xlab = 
       "population", ylab = "water usage total")

#2015
HI_lm_2015 <- lm(HI_usgs2015_data$X.136 ~ HI_usgs2015_data$X.5)
plot(HI_usgs2015_data$X.5,HI_usgs2015_data$X.136, main="2015",xlab = 
       "population", ylab = "water usage total")


