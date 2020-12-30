#load libraries

library(dplyr)
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(downloader)
library(tidyverse)
library(spdep)
library(car)
library(fs)
library(janitor)
library(rgdal)
library(broom)
library(stringr)
library(mapview)
library(crosstalk)
library(sjmisc)

## Pulling In Data

#Read in MSOA shapefile - add instructions on Rmarkdown version in GitHub 

MSOABoundaries <- st_read(here::here("CASA","Term 1","GIS & Science","GeoData","MSOA_2011_London_gen_MHW.shp"))
st_crs(MSOABoundaries)  
#st_transform(., 27700)
  
typeof(MSOABoundaries$MSOA11CD)
head(MSOABoundaries,5)

#Pull in OD Data 2011 Census

ODData <- read_csv("https://raw.githubusercontent.com/YHuj20/GIS/main/ODData.csv",na = c("", "NA", "n/a"), 
                   locale = locale(encoding = 'Latin1'), 
                   col_names = TRUE)

ODWorkdayData <- read_csv("https://raw.githubusercontent.com/YHuj20/GIS/main/ODDataWorkday.csv",na = c("", "NA", "n/a"), 
                   locale = locale(encoding = 'Latin1'), 
                   col_names = TRUE)

#Pull in Population Numbers by Occupation Type 2011 Census

Occupations <- read_csv("https://raw.githubusercontent.com/YHuj20/GIS/main/Occupation-by-MSOA.csv",na = c("", "NA", "n/a"), 
                        locale = locale(encoding = 'Latin1'), 
                        col_names = TRUE)

WorkdayPopOccupations <- read_csv("https://raw.githubusercontent.com/YHuj20/GIS/main/WD606EW-Occupations-WorkdayPopulation.csv",na = c("", "NA", "n/a"), 
                                  locale = locale(encoding = 'Latin1'), 
                                  col_names = TRUE)

#Pull in ONS data on working from home

HomeWorkingProportions <- read_csv("https://raw.githubusercontent.com/YHuj20/GIS/main/ONS-Ability-to-work-from-home.csv",na = c("", "NA", "n/a"), 
                                   locale = locale(encoding = 'Latin1'), 
                                   col_names = TRUE)

#Pull in ONS data on furloughed staff

Furloughed <- read_csv("https://raw.githubusercontent.com/YHuj20/GIS/main/Furloughed-Staff-by-LA.csv",na = c("", "NA", "n/a"), 
                                   locale = locale(encoding = 'Latin1'), 
                                   col_names = TRUE)


## Data Wrangling 

#Change type to doubles and drop first two non numeric rows

ODData <- ODData %>% 
  mutate_at(vars(`place of work`,`place of work_1`,`place of work_2`,`place of work_3`,`place of work_4`,`place of work_5`,`place of work_6`,`place of work_7`,`place of work_8`,`place of work_9`,`place of work_10`,`place of work_11`), as.numeric)
ODData <- ODData[-c(1,2),]
head(ODData,5)
typeof(ODData$`place of work_2`)

ODWorkdayData <- ODWorkdayData[-c(984:989),]
ODData <- ODData[-c(984:989),]
WorkdayPopOccupations <- WorkdayPopOccupations[-c(984:989),]

# Create new column summing all commuters per MSOA

ODData$ALL <- ODData$`place of work`+ODData$`place of work_1`+ODData$`place of work_2`+ODData$`place of work_3`+ODData$`place of work_4`+ ODData$`place of work_5`+ODData$`place of work_6`+ ODData$`place of work_7`+ODData$`place of work_8`+ODData$`place of work_9`+ODData$`place of work_10`+ODData$`place of work_11`

# Create new column normalising the sum of all commuters per MSOA



## 2011 Baseline Mapping

#Join ODData, ODWorkdayData and MSOA Shapefile

ODData <- ODData %>% 
  rename(
    MSOA11CD = X2,
  )

ODWorkdayData <- ODWorkdayData %>% 
  rename(
    MSOA11CD = `Place of Work: MSOA11CD`,
  )

ODWorkdayData <- ODWorkdayData %>% 
  rename(
    'Workday Population' = `Usual Residence: England and Wales`,
  )

head(ODData,5)
ODData <- left_join(ODWorkdayData, ODData, by = c('MSOA11CD'))

CommuteMap <- left_join(MSOABoundaries, ODData, by = c('MSOA11CD'))


# Mapping 2011 Commuters


qtm(MSOABoundaries)

plot(CommuteMap)

plot(CommuteMap["ALL"])

plot(CommuteMap["ALL"],
     main = "Total number of people commuting for work (2011)",
     breaks = "quantile")

library(RColorBrewer)
pal <- brewer.pal(9, "YlGnBu") 
class(pal)

plot(CommuteMap["ALL"],
     main = "Total number of people commuting for work (2011)",
     breaks = "quantile", nbreaks = 9,
     pal = pal)

#Map workday population 2011

head(CommuteMap,5)

plot(CommuteMap["Workday Population"],
     main = "Workday Population 2011",
     breaks = "quantile", nbreaks = 9,
     pal = pal)


#Delete end NA rows

HomeWorkingProportions <- HomeWorkingProportions[-(366:379),]

#Create 2020 workday population by occupation using % WFH from HomeWorkingProportions

#Workday population calculations

#Add new column with 1- % WFH

HomeWorkingProportions$`% Working from home during Lockdown` <- HomeWorkingProportions$`% Working from home during Lockdown`/100
HomeWorkingProportions$Remaining <- 1 - HomeWorkingProportions$`% Working from home during Lockdown`
names(WorkdayPopOccupations) <- substring(names(WorkdayPopOccupations),1,3)
WorkdayPopOccupations <- WorkdayPopOccupations %>% 
  rename(
    'MSOAName' = '201',
    'MSOACode' = 'mne'
  )


HomeWorkingProportions <- rotate_df(HomeWorkingProportions)
#Create matrix first
WorkdayPopOccupationsMat <- as.matrix(WorkdayPopOccupations)
WorkdayPopOccupationsMat <- WorkdayPopOccupationsMat[,-c(1:3)]
typeof(WorkdayPopOccupationsMat)
class(WorkdayPopOccupationsMat) <- "numeric"
#Convert row to vector
WFHProp <- unlist(HomeWorkingProportions[15,])
class(WFHProp) <- "numeric"
print(WFHProp)
#Multiply vector by matrix
LockdownWFH2020 <- sweep(WorkdayPopOccupationsMat,2,WFHProp ,"*")
#Convert back to tibble
LockdownWFH2020 <- as_tibble(LockdownWFH2020)
#create new column summing people working from home
LockdownWFH2020$ALLWFH <- rowSums(LockdownWFH2020)
LockdownWFH2020$MSOACode <- WorkdayPopOccupations$MSOACode
LockdownWFH2020$MSOAName <- WorkdayPopOccupations$MSOAName
  
#Residential population calculations (adding people working from home)

names(Occupations) <- substring(names(Occupations),1,3)
Occupations <- Occupations %>% 
  rename(
    'MSOAName' = '201',
    'MSOACode' = 'mne'
  )


#Create matrix first
OccupationsMat <- as.matrix(Occupations)
OccupationsMat <- OccupationsMat[,-c(1:3)]
typeof(OccupationsMat)
class(OccupationsMat) <- "numeric"
#Multiply vector by matrix
LockdownResiPop2020 <- sweep(OccupationsMat,2,WFHProp ,"*")
#Convert back to tibble
LockdownResiPop2020 <- as_tibble(LockdownResiPop2020)
#create new column summing people working from home
LockdownResiPop2020$ALLWFH <- rowSums(LockdownResiPop2020)
LockdownResiPop2020$MSOACode <- Occupations$MSOACode
LockdownResiPop2020$MSOAName <- Occupations$MSOAName


#Join based on MSOA code
WfhEstimates <-   tibble(LockdownWFH2020$ALLWFH, LockdownWFH2020$MSOACode)

WfhEstimates <- WfhEstimates %>% 
  rename(
    WorkdayPopWFH = `LockdownWFH2020$ALLWFH`,
  )
WfhEstimates <- WfhEstimates %>% 
  rename(
    MSOACode = `LockdownWFH2020$MSOACode`,
  )

WfhEstimates <- left_join(LockdownResiPop2020, WfhEstimates, by = c('MSOACode'))

WfhEstimates <- WfhEstimates[,-c(1:90)]

WfhEstimates <- WfhEstimates %>% 
  rename(
    ResiWFH = `ALLWFH`,
  )

WfhEstimates$AllResiPop <- Occupations$All
WfhEstimates$AllWorkdayPop <- WorkdayPopOccupations$All
WfhEstimates$RemainingWorkday <- WfhEstimates$AllWorkdayPop - WfhEstimates$WorkdayPopWFH
WfhEstimates$RemainingResi <- WfhEstimates$AllResiPop - WfhEstimates$ResiWFH
WfhEstimates$RedistributedPopulation <- WfhEstimates$RemainingWorkday + WfhEstimates$ResiWFH

#Map the redistributed population

WfhEstimates <- WfhEstimates %>% 
  rename(
    MSOA11CD = MSOACode,
  )
Commute2023 <- left_join(MSOABoundaries, WfhEstimates, by = c('MSOA11CD'))

qtm(MSOABoundaries)

plot(Commute2023)

plot(Commute2023["RedistributedPopulation"])

plot(Commute2023["RedistributedPopulation"],
     main = "Total number of people commuting for work (2011)",
     breaks = "quantile")

library(RColorBrewer)
pal <- brewer.pal(9, "YlGnBu") 
class(pal)

plot(Commute2023["RedistributedPopulation"],
     main = "Total number of people commuting for work (2011)",
     breaks = "quantile", nbreaks = 9,
     pal = pal)


 

#Analysis of reduction of commuters

#Mapping commuters in 2020


## Analysis of potential working from home in 2023

#Analysis of reduction of commuters

#Mapping potential commuters in 2023
