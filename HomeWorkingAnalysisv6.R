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
library(ggmap)
library(leafpop)
library(leaflet)
library(hrbrthemes)
library(ggplot2)
library(viridis)
library(grid)
library(hrbrthemes)
library(scales)
library(reshape2)
library(gapminder)
library(shiny)


#----------------------------------------------------------
## Pulling In Data

#Download MSOA shapefile from: https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip
#Read in MSOA Shapefile - please replace filepath with local filepath


MSOABoundaries <- st_read(here::here("MSOA_2011_London_gen_MHW.shp"))
st_crs(MSOABoundaries)   

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





#---------------------------------

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






#-------------------------------

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


# Mapping 2011 Commuters (Visualising Data)


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

plot(CommuteMap["Workday Population"],
     main = "Workday Population 2011",
     breaks = "quantile", nbreaks = 9,
     pal = pal)


#Delete end NA rows

HomeWorkingProportions <- HomeWorkingProportions[-(366:379),]



#-----------------------

##Calculate Scenario 1 Commuters 

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

#rotate df to use row as vector
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

#Drop occupations columns
WfhEstimates <- WfhEstimates[,-c(1:90)]

WfhEstimates <- WfhEstimates %>% 
  rename(
    ResiWFH = `ALLWFH`,
  )

WfhEstimates$AllResiPop <- Occupations$All
WfhEstimates$AllWorkdayPop <- WorkdayPopOccupations$All
WfhEstimates$RemainingWorkday <- WfhEstimates$AllWorkdayPop - WfhEstimates$WorkdayPopWFH
WfhEstimates$RedistributedPopulation <- WfhEstimates$RemainingWorkday + WfhEstimates$ResiWFH

#Map the redistributed population - Scenario 1

WfhEstimates <- WfhEstimates %>% 
  rename(
    MSOA11CD = MSOACode,
  )
Commute2023 <- left_join(MSOABoundaries, WfhEstimates, by = c('MSOA11CD'))

qtm(MSOABoundaries)

plot(Commute2023)

plot(Commute2023["RedistributedPopulation"])

plot(Commute2023["RedistributedPopulation"],
     main = "Projected Working Day Population in 2023 (S1)",
     breaks = "quantile")

plot(Commute2023["RedistributedPopulation"],
     main = "Projected Working Day Population in 2023 (S1)",
     breaks = "quantile", nbreaks = 9,
     pal = pal)



#--------------------------------------

#Calculate Scenario 2 2023 Commuters

#Normalise ability to work from home score (row)
#rotate tibble to calculate new column
HomeWorkingProportions <- rotate_df(HomeWorkingProportions)
MinAbilityScore <- min(HomeWorkingProportions$`Ability to homework score`, na.rm = TRUE)
MaxAbilityScore <- max(HomeWorkingProportions$`Ability to homework score`, na.rm = TRUE)
class(HomeWorkingProportions$`Ability to homework score`) <- "numeric"
class(MaxAbilityScore) <- "numeric"
class(MinAbilityScore) <- "numeric"
class(HomeWorkingProportions$`% Working from home during Lockdown`) <- 'numeric'
ScalingScore <- MaxAbilityScore-MinAbilityScore
HomeWorkingProportions$NormalisedAbilityScore <-(HomeWorkingProportions$`Ability to homework score`-MinAbilityScore)/ScalingScore
HomeWorkingProportions$Scenario2WFH <- (1-HomeWorkingProportions$NormalisedAbilityScore)*HomeWorkingProportions$`% Working from home during Lockdown`
HomeWorkingProportions <- rotate_df(HomeWorkingProportions)

#calculate working from home for scenario 2

#Residential WFH 2023 S2 calculations
#Convert row (WFH Proportions) to vector
WFHProp2023 <- unlist(HomeWorkingProportions[18,])
class(WFHProp2023) <- "numeric"
print(WFHProp2023)
#Multiply vector by matrix
WFHResiPop2023S2 <- sweep(OccupationsMat,2,WFHProp2023,"*")
#Convert back to tibble
WFHResiPop2023S2 <- as_tibble(WFHResiPop2023S2)
#create new column summing people working from home
WFHResiPop2023S2$ResiAllWFH <- rowSums(WFHResiPop2023S2)
WFHResiPop2023S2$MSOACode <- Occupations$MSOACode
WFHResiPop2023S2$MSOAName <- Occupations$MSOAName

#Workday WFH 2023 S2 calculations
#Multiply vector by matrix
WFHWorkdayPop2023S2 <- sweep(WorkdayPopOccupationsMat,2,WFHProp2023,"*")
#Convert back to tibble
WFHWorkdayPop2023S2 <- as_tibble(WFHWorkdayPop2023S2)
#create new column summing people working from home
WFHWorkdayPop2023S2$WorkdayAllWFH <- rowSums(WFHWorkdayPop2023S2)
WFHWorkdayPop2023S2$MSOACode <- WorkdayPopOccupations$MSOACode
WFHWorkdayPop2023S2$MSOAName <- WorkdayPopOccupations$MSOAName

#Create new tibble of above
WfhEstimates2023 <- tibble(WFHWorkdayPop2023S2$MSOACode, WFHWorkdayPop2023S2$MSOAName, WFHWorkdayPop2023S2$WorkdayAllWFH, WFHResiPop2023S2$ResiAllWFH, WFHResiPop2023S2$MSOACode, WFHResiPop2023S2$MSOAName)

WfhEstimates2023 <- WfhEstimates2023 %>%
  rename(
    MSOA11CD = `WFHWorkdayPop2023S2$MSOACode`,
    MSOA11NM = `WFHWorkdayPop2023S2$MSOAName`,
    WorkdayAllWFH = `WFHWorkdayPop2023S2$WorkdayAllWFH`,
    ResiAllWFH = `WFHResiPop2023S2$ResiAllWFH`
  )

WfhEstimates2023 <- WfhEstimates2023[,-c(5,6)]

#Calculate redistributed population
WfhEstimates2023$AllResiPop <- Occupations$All
WfhEstimates2023$AllWorkdayPop <- WorkdayPopOccupations$All
WfhEstimates2023$RemainingWorkday <- WfhEstimates2023$AllWorkdayPop - WfhEstimates2023$WorkdayAllWFH
WfhEstimates2023$RedistributedPopulation <- WfhEstimates2023$RemainingWorkday + WfhEstimates2023$ResiAllWFH


S2CommuteMap2023 <- left_join(MSOABoundaries, WfhEstimates2023, by = c('MSOA11CD'))










#--------------------------------------

#Plotting graphs of data 


#Drop place of work columns
ODData <- ODData[,-c(5:16)]
ODData <- ODData %>% 
  rename(
    'Resi Population' = ALL,
  )
summary(Commute2023)
summary(ODData)

#Plot graph of workday population 2011

SortedODData <- ODData[order(-ODData$`Workday Population`),]

SortedODData <- SortedODData %>% 
  rename(
    WDPop2011 = 'Workday Population'
  )

SortedODData$idu <- as.numeric(row.names(SortedODData))
Sorted2023S1Data <- WfhEstimates[order(-WfhEstimates$RedistributedPopulation),]
Sorted2023S1Data$idu <- as.numeric(row.names(Sorted2023S1Data))
Sorted2023S2Data <- WfhEstimates2023[order(-WfhEstimates2023$RedistributedPopulation),]
Sorted2023S2Data$idu <- as.numeric(row.names(Sorted2023S2Data))

cuts <- c(0,402.946510979881,453.250875065876,517.907002480845,604.078555737188,724.64837398374,905.351367200113,1206.11630554283,1806.12560118956,3594.08026262626,121238.3868, 238882.6934, 356527)



#Scatter of all three workday populations

#create dataframe of all three sorted populations (for each scenario)
Scatterplot <- tibble(Sorted2023S2Data$RedistributedPopulation, Sorted2023S1Data$RedistributedPopulation, SortedODData$WDPop2011)
Scatterplot$Index <- seq.int(nrow(Scatterplot))
#typeof(Scatterplot$`Sorted2023S2Data$RedistributedPopulation`)
#typeof(Scatterplot$`Sorted2023S1Data$RedistributedPopulation`)
#typeof(Scatterplot$`SortedODData$WDPop2011`)

Scatterplot <- Scatterplot %>% 
  rename(
    '2011 Baseline' = 'SortedODData$WDPop2011',
    'S1' = 'Sorted2023S1Data$RedistributedPopulation',
    'S2' = 'Sorted2023S2Data$RedistributedPopulation'
    )

ScatterplotTall <-reshape2::melt(Scatterplot, id.vars = "Index" )

pal <- wes_palette("Royal1" , 3, type="discrete")

ggplot(ScatterplotTall, aes(Index, value, col=variable)) +
  geom_point(alpha=0.7 ,size=1.2) +
  geom_rug(col="steelblue",alpha=0.1, size=2, sides = "r") +
  theme_classic() +
  theme(legend.position = "bottom") +
  ylab("Workday Population") +
  xlab("MSOA")
  
#-------------------to be published!

ggplot(ScatterplotTall, aes(Index, value, col=variable)) +
  geom_point(alpha=0.7 ,size=3) +
  scale_color_brewer("Scenario", palette = "Dark2") +
  geom_rug(col="black",alpha=0.1, size=3, sides = "r") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=12)) +
  scale_y_continuous(labels = comma) +
  ylab("Workday Population") +
  xlab("MSOA")

#-------------------

#Box Plots

p <- ggplot(ScatterplotTall, aes(Index, value, col=variable))

p + geom_boxplot()+
  scale_color_brewer("Scenario", palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = comma) +
  ylab("Workday Population") +
  xlab("")
  


#---------------------------------------



library(hexbin)
x <- ScatterplotTall$Index
y <- ScatterplotTall$value
bin <-hexbin(x, y, xbins = 40)
my_colors=colorRampPalette(rev(brewer.pal(11,'Spectral')))
plot(bin, main="" , colramp=my_colors , legend=F ) 






#--------------------------------------------------------------------

#Mapping data

#cuts <- c(0,402.946510979881,453.250875065876,517.907002480845,604.078555737188,724.64837398374,905.351367200113,1206.11630554283,1806.12560118956,3594.08026262626,121238.3868, 238882.6934, 356527)
cuts <- c(0,388.571283961705,418.426215309311,453.250875065876,494.398279295582,543.762863836654,604.078555737188,679.444645098752,776.296988412112,905.351367200113,1085.87127165345,1356.3058504823,1806.12560118956,2702.33665664276,5364.29889944219,356527)
#cuts <- c(0,381.761469988134,402.946510979881,426.620925278629,453.250875065876,483.426662879247,517.907002480845,557.683690091941,604.078555737188,658.893197986508,724.64837398374,804.983436156195,905.351367200113,1034.31282524435,1206.11630554283,1446.36294888253,1806.12560118956,2404.11594866679,3594.08026262627,7116.55854901965,356527)

#Remapping

CommuteMap <- CommuteMap[,-c(15:27)]

#2011WorkdayPopulation
tmap_mode("view")

Workdaypop2011 <- tm_shape(CommuteMap) +
  tm_fill(col = "Workday Population", breaks = cuts, palette = "BuPu", border.alpha = 0.5, title="Workday Population") +
  tm_legend(legend.position = c("left","bottom"))+
  tm_layout(title = "Baseline Workday Population Distribution (2011)", title.size = 1.1, title.position = c("centre", "top"))
Workdaypop2011

#2023WorkdayPopulationS1
Workdaypop2023S1 <- tm_shape(Commute2023) +
  tm_fill(col = "RedistributedPopulation", breaks = cuts, palette = "BuPu", border.alpha = 0.5, title="Workday Population") +
  tm_legend(legend.position = c("left","bottom"))+
  tm_layout(title = "Projected Workday Population Distribution - Scenario 1", title.size = 1.1, title.position = c("centre", "top")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("right", "bottom"))
Workdaypop2023S1
  

#2023WorkdayPopulationS2
Workdaypop2023S2 <- tm_shape(S2CommuteMap2023) +
  tm_fill(col = "RedistributedPopulation", breaks = cuts, palette = "BuPu", border.alpha = 0.5, title="Workday Population") +
  tm_legend(legend.position = c("left","bottom"))+
  tm_layout(title = "Projected Workday Population Distribution - Scenario 2", title.size = 1.1, title.position = c("centre", "top"))
Workdaypop2023S2

#TMAP Pretty cuts ------------------------

Workdaypop2011 <- tm_shape(CommuteMap) +
  tm_fill(col = "Workday Population", style = "log10_pretty", palette = "BuPu", border.alpha = 0.5, title="Workday Population") +
  tm_legend(legend.position = c("left","bottom"))+
  tm_layout(title = "Baseline Workday Population Distribution (2011)", title.size = 1.1, title.position = c("centre", "top"))
Workdaypop2011

prettycuts <- c(0,100,316,1000,3162,10000,31623,100000,316228,1000000)

Workdaypop2023S1 <- tm_shape(Commute2023) +
  tm_fill(col = "RedistributedPopulation", breaks = prettycuts, palette = "BuPu", border.alpha = 0.5, title="Workday Population") +
  tm_layout(title = "Projected Workday Population Distribution - Scenario 2", title.size = 1.1, title.position = c("centre", "top"))
Workdaypop2023S1

Workdaypop2023S2 <- tm_shape(S2CommuteMap2023) +
  tm_fill(col = "RedistributedPopulation", breaks = prettycuts, palette = "BuPu", border.alpha = 0.5, title="Workday Population") +
  tm_layout(title = "Projected Workday Population Distribution - Scenario 2", title.size = 1.1, title.position = c("centre", "top"))
Workdaypop2023S2

# -----------------------------------------

#Summary Statistics

print(max(Scatterplot$`2011 Baseline`, na.rm = TRUE))
print(min(Scatterplot$`2011 Baseline`, na.rm = TRUE))
print(max(Scatterplot$S1, na.rm = TRUE))
print(min(Scatterplot$S1, na.rm = TRUE))
print(max(Scatterplot$S2, na.rm = TRUE))
print(min(Scatterplot$S2, na.rm = TRUE))

print(max(Scatterplot$`2011 Baseline`, na.rm = TRUE) - max(Scatterplot$S1, na.rm = TRUE))
print(max(Scatterplot$`2011 Baseline`, na.rm = TRUE) - max(Scatterplot$S2, na.rm = TRUE))
print(max(Scatterplot$S2, na.rm = TRUE) - max(Scatterplot$`2011 Baseline`, na.rm = TRUE))

#Change in population maps?

#Working off of Scenario 2 

PopulationChange <- ODData[,-c(4:5)]
PopulationChange <- left_join(PopulationChange, WfhEstimates2023, by = c('MSOA11CD'))
PopulationChange <- PopulationChange[,-c(4:9)]
PopulationChange <- PopulationChange %>% 
  rename(
    'Scenario 2' = 'RedistributedPopulation',
    '2011 Baseline' = 'Workday Population'
  )

PopulationChange$Difference <- PopulationChange$`Scenario 2`- PopulationChange$`2011 Baseline`
#Add geometry
PopulationChangeMap <- left_join(MSOABoundaries, PopulationChange, by = c('MSOA11CD'))

#Map the difference
prettybins = c(0, 3, 10, 32, 316, 100, 316, 1000, 3162, 10000)

PCM <- tm_shape(PopulationChangeMap) +
  tm_fill(col = "Difference", border.alpha = 0.5, palette = "Spectral", title="Change in Workday Population") +
  tm_legend(legend.position = c("left","bottom"))+
  tm_layout(title = "Projected Workday Population Distribution - Scenario 1", title.size = 1.1, title.position = c("centre", "top")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("right", "bottom"))
PCM

