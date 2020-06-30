## Heat map for LPP work
## Using code from this website as starting point: http://datatricks.co.uk/creating-maps-in-r

## This code can be used to plot any data at a STP level
## Anna Rowan
## Last updated 15/06/2020
rm(list=ls())
## Setting working directory
setwd("//ims.gov.uk/data/NHS_ENGLAND/Secure/SpecCom/Medicines & Diagnostics Policy Unit/Analytical work/Medicines Strategy")

## Useful packages
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(dplyr)

## Load the shapefile - this is the file which allows us to draw the map of STP outlines
shapefile <- readOGR(dsn = "O:/Strategy & Policy/Medicines Analysis/Templates and Other Resources/Resources and References/Geography_RefFiles/STP_Map",
                     layer = "Sustainability_and_Transformation_Partnerships_April_2019_EN_BFC")
## Reshape for ggplot 2 using the Broom package
STP_Map_Data <- tidy(shapefile, region = "stp19cd")
## Now have the data which will plot out STP boundaries on a chart



## Then bring in LPP data (or any data at STP level you want to map)
## Change this bit of code to bring in any data you want to map out at CCG level
LPP <- readRDS(file = "./Low Priority Prescribing/LVM Wave 1/Data/Data for reporting - Post March 2019/Master_Data_CCG_DataToMar2020_Update.rds") %>%
  filter(PERIOD == 202003 & WAVE == "W1") %>%
  group_by(STP_CODE, STP_NAME, DRUG_GROUP) %>%
  summarise(MONTHLY_TOTAL_ACTUAL_COST = sum(MONTHLY_TOTAL_ACTUAL_COST),
            MONTHLY_EST_ACTUALCOST_AT_GOAL = sum(MONTHLY_EST_ACTUALCOST_AT_GOAL),
            PATIENT_COUNT = sum(PATIENT_COUNT)) %>%
  group_by(STP_CODE, STP_NAME) %>%
  summarise(MONTHLY_TOTAL_ACTUAL_COST = sum(MONTHLY_TOTAL_ACTUAL_COST),
            MONTHLY_EST_ACTUALCOST_AT_GOAL = sum(MONTHLY_EST_ACTUALCOST_AT_GOAL),
            PATIENT_COUNT = first(PATIENT_COUNT)) %>%
  mutate(Net_Actual_Spend_Per1000 = ((MONTHLY_TOTAL_ACTUAL_COST) / PATIENT_COUNT) * 1000,
         Spend_Goal_Per1000 = (MONTHLY_EST_ACTUALCOST_AT_GOAL/ PATIENT_COUNT) * 1000,
         Diff_Pound_Per1000 = max(0,Net_Actual_Spend_Per1000 - Spend_Goal_Per1000),
         Diff_Pct = (Net_Actual_Spend_Per1000 / Spend_Goal_Per1000) - 1) %>%
  select(STP_CODE, STP_NAME, Diff_Pound_Per1000) %>%
  dplyr::rename(id = STP_CODE)

## Join this data on to STP mapping data
Heat_Map <- left_join(STP_Map_Data, LPP, by = "id") 

### Now to plot the data

## Create data lables so can highlight the "worst performing" STPs
STP_Labels <- STP_Map_Data %>%
  group_by(id) %>%
  summarise(Max_long = max(long),
            Min_long = min(long),
            Max_lat = max(lat),
            Min_lat = min(lat)) %>%
  mutate(Centre_long = Min_long + ((Max_long - Min_long)/2),
         Centre_lat = Min_lat + ((Max_lat - Min_lat)/2)) %>%
  left_join(., LPP, by = "id") %>%
  mutate(Rank = rank(desc(Diff_Pound_Per1000))) %>%
  filter(Rank <= 5)

## Checks to make sure data mapped across correctly for all STPs
# Check <- Heat_Map %>%
#  filter(is.na(Items_Per_1000))
# unique(Check$id)

## Now create plot but use data you want to show as the fill variable
Map_WithData <- ggplot() +
  geom_polygon(data = Heat_Map, aes(x = long, y = lat, group = group, fill = Diff_Pound_Per1000), color = "#FFFFFF", size = 0.25) +
  ggrepel::geom_label_repel(data = STP_Labels, 
                           aes(x = Centre_long, y = Centre_lat, label = STP_NAME), 
                           size = 4,
                           min.segment.length = unit(0, "lines"),
                           box.padding = unit(1.0, "lines"),
                           point.padding = unit(0, "lines")) +
  coord_fixed(1) +
  scale_fill_gradient(low = "orange", high = "blue", na.value = "grey") + # change colours here
  theme_minimal() +
  labs(fill = "£ distance from goal (per 1,000 patients)") + # legend title
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + # removes grid lines
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + # removes x axis
  theme(axis.title.y = element_blank(),
       axis.text.y = element_blank(),
       axis.ticks.y = element_blank()) # removes y axis

print(Map_WithData)



