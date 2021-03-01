library(readxl)

#Read in the data from USGS
usgs1990_data <- read_excel('./Data goes here/us90co.xls')
usgs1995_data <- read_excel('./Data goes here/usco1995.xlsx')
usgs2000_data <- read_excel('./Data goes here/usco2000.xls')
usgs2005_data <- read_excel('./Data goes here/usco2005.xls')
usgs2010_data <- read_excel('./Data goes here/usco2010.xlsx')
usgs2015_data <- read.csv('./Data goes here/usco2015v2.0.csv')

#Summary of Water Usage in Hawaii (b/c it only has 5 counties; small data)
HI_usgs1990_data <- usgs1990_data[c(545:549),] #1990
HI_usgs1995_data <- usgs1995_data[c(544:548),] #1995
HI_usgs2000_data <- usgs2000_data[c(544:548),] #2000
HI_usgs2005_data <- usgs2005_data[c(545:549),] #2005
HI_usgs2010_data <- usgs2010_data[c(547:551),] #2010
HI_usgs2015_data <- usgs2015_data[c(548:552),] #2015
