######
## INTRODUCTION
## Forecasting the medicines demand due to Covid-19 patients in hospitals in England
## This module uses bed projections and medicines usage assumptions to forecast the demand of medicines, 
## both as individual moieties/chemical substances and as treatment groups

######
## PREPARING THE WORKSPACE
## Clearing workspace
rm(list = ls())

## Setting the work directory
setwd("O:/Strategy & Policy/Medicines Analysis/Covid-19/SecondaryCare_Supportive_Meds")

## Installing and loading packages
## Only need to run this code if packages not already installed
# install.packages("dplyr") #useful for manipulating data, makes code easier to write
# install.packages("reshape")
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("splitstackshape")
## When loading packages the name is not in quotation marks
library(dplyr)
library(reshape)
library(tidyverse)
library(readxl)
library(splitstackshape)

#######
## READING IN INPUT DATA

## Occupied bed forecasts

## R = 1.7 in June, Second spike in July
## Need to read in data twice because headers are across multiple rows
## Set column names first
Second_Spike_July_ColNames <- read_excel("./Input_Data/Patient_Forecast&Actuals/Hospitalisations and beds required R1 18May and RWC 1Jun R1.7 v1.2.xlsx", 
                                        col_names = T, skip=3, n_max = 1) %>%
  names()
## Then read in rest of the data - dropping a couple of columns which do not have col names in input dataset
Second_Spike_July <- read_excel("./Input_Data/Patient_Forecast&Actuals/Hospitalisations and beds required R1 18May and RWC 1Jun R1.7 v1.2.xlsx", 
                               col_names = F, skip=4) %>%
  select(-c("...27", "...28")) 
## Apply the column names to the data set
colnames(Second_Spike_July) <- Second_Spike_July_ColNames
## Slim down the input data so only using the O, O+ and V bed forecasts for second spike scenario
## And rename variables
Second_Spike_July <- Second_Spike_July %>%
  select(Date, `O occupied...17`,`O+ occupied...18`, `V occupied...19`) %>% # select variables of interest 
  dplyr::rename(O_Occ_Beds = `O occupied...17`,
                `O+_Occ_Beds` = `O+ occupied...18`,
                V_Occ_Beds = `V occupied...19`) %>% # rename variables so consistent naming conventions used across all bed projection data
  mutate(Date = as.Date(Date)) # make sure date information is in a date format

## Latest planning scenario - RWC
Planning_Scenario <- read_excel("./Input_Data/Patient_Forecast&Actuals/STP Modelling Tool - England v1.13.xlsx", 
                             sheet = "Data Summary - Occupied", col_names = T, skip=4) %>% # read in data
  select(Date, `Total - V Beds Occupied`, `Total - O+ Beds Occupied`, `Total - O (Other) Beds Occupied`) %>% # select variables of interest
  dplyr::rename(O_Occ_Beds = `Total - O (Other) Beds Occupied`,
                `O+_Occ_Beds` = `Total - O+ Beds Occupied`,
                V_Occ_Beds = `Total - V Beds Occupied`) %>% # rename variables so consistent naming conventions used across all bed projection data
  mutate(Date = as.Date(Date)) # make sure date information is in a date format

## Sit Rep patient numbers
SitRep <- read_excel("./Input_Data/Patient_Forecast&Actuals/COVID19 time series 2020-06-15 v6.8.xlsx", 
                     sheet = "Data - adjusted", col_names = T) %>% # read in data
  filter(Region_ID == "ENG") %>% # only keep the set of information at a national level 
  select(ReportingPeriod, Patients_COVID_V, Patients_COVID_O, Patients_COVID_OPlus) %>% # select variables of interest
  dplyr::rename(O_Occ_Beds = `Patients_COVID_O`,
                `O+_Occ_Beds` = `Patients_COVID_OPlus`,
                V_Occ_Beds = `Patients_COVID_V`) %>% # rename variables so consistent naming conventions used across all beds data
  mutate(Date = as.Date(ReportingPeriod)) # make sure date information is in a date format

## Deaths in hospitals
Deaths <- read_excel("./Input_Data/Patient_Forecast&Actuals/COVID-19-total-announced-deaths-15-June-2020.xlsx",
                     sheet = "Tab1 Deaths by region", col_names = T, skip = 15, n_max = 1) %>% # read in data
  select(-c("...2", "Up to 01-Mar-20")) %>% # select variables of interest
  as.data.frame(.) %>% # fix to ensure next line runs
  melt(., id = c("NHS England Region")) %>% # change data set layout from "wide" to "long" (fewer columns, more rows)
  dplyr::rename(Date = variable,
                Deaths_In_Hospitals = value) %>% # rename variables
  mutate(Date = as.character(Date),
         Date = as.numeric(Date),
         Date = as.Date(Date, origin = "1899-12-30")) %>% # make sure date information is in a date format
  filter(!is.na(Date)) %>% # get rid of missing data
  select(Date, Deaths_In_Hospitals) # select variables of interest

## Medicine assumptions

## Ref table to convert data in VMP quantites to singles/eaches
Ref_VMPQuant_Conversion <- read_excel("./Input_Data/Medicine_InputLists/Ref_VMPQuantConversion_20200616.xlsx") # read in data

## Actual usage in hospitals in May 2020 of Covid supportive medicines
May2020_Usage_Tag <- read.csv("./Input_Data/Medicine_InputLists/Define_Usage_May2020.csv", skip = 1) %>% # read in data
  dplyr::rename(`VMP Name` = 1,
                VMP_Quantity_May2020 = Total) # rename variables so using consistent names across datasets
May2020_Usage_NotInTag <- read.csv("./Input_Data/Medicine_InputLists/Define_Usage_May2020_NotInTag.csv", skip = 1) %>% # read in data
  dplyr::rename(`VMP Name` = 1,
                VMP_Quantity_May2020 = Total) # rename variables so using consistent names across datasets

May2020_Usage <- union(May2020_Usage_Tag, May2020_Usage_NotInTag) # bring the two cuts of May usage data together in to one table

## Ref table with information on Covid daily doses for each medicine in scope
Ref_Covid_DailyDoses <- read_excel("./Input_Data/Medicine_InputLists/Ref - Covid Daily Doses.xlsx") # read in data

## Ref table with information at a VMP level on strength of each product and other variables
Ref_MedsInfo_VMPLevel <- read_excel("./Input_Data/Medicine_InputLists/Ref_MedsInfo_VMPLevel.xlsx") %>% # read in data
  dplyr::rename(`VMP Name` = `VMPP level medicine`) # rename variables so using consistent names across datasets


#######
## FORECASTING MEDICINES DEMAND FOR COVID PATIENTS

## Step 1: Pick beds scenario to use in forecast and summarise latest SitRep data available
Forecast_Scenario <- Planning_Scenario # Change this to the beds forecast used in scenario

## Step 2: Translate actual usage data at VMP level in VMP quantity to usage in terms of strength, 
## group by medicine and then translate to usage in daily doses
## This information is used to estimate the split of Covid demand in a group between different medicines
## i.e. Continuous Opioid demand between Alfentanil, Fentanyl and Morphine

## First checking to make sure that there is usage data for all VMPs included in Covid demand forecast
Check_UsageData_ForAllVMPInRefList <- Ref_MedsInfo_VMPLevel %>% # start with list of VMPs used in Covid demand forecast
  left_join(., May2020_Usage, by = c("VMP Name")) %>% # joining on actual usage information to VMP level medicines list
  filter(is.na(VMP_Quantity_May2020)) # only keeping records with no usage data 
  # at last check there was one VMP with no usage data, 2g/100ml Propofol. This is expected as this product is a special.

## Once happy with the check can run the calculation
Actual_Usage_In_DD_MedsLevel <- Ref_MedsInfo_VMPLevel %>% # start with list of VMPs used in Covid demand forecas
  left_join(., May2020_Usage, by = c("VMP Name")) %>% # joining on actual usage information to VMP level medicines list
  left_join(., Ref_VMPQuant_Conversion, by = c("VMP Name")) %>% # joining VMP conversion factors to VMP level medicines list
  mutate(VMP_Quantity_May2020 = ifelse(is.na(VMP_Quantity_May2020), 0, VMP_Quantity_May2020),
         Usage_In_Singles = VMP_Quantity_May2020 / `VMP Quantity Unit factor`,
         Usage_In_Strength = Usage_In_Singles * `Strength of VMP - Volume`) %>% # create two new variables for usage in singles and in strength
  group_by(Category, Group, `Sub Group`, Medicine) %>% # grouping data at medicine level
  summarise(Usage_In_Strength = sum(Usage_In_Strength),
            `Strength of VMP - Units` = first(`Strength of VMP - Units`)) %>% # summarising usage in strength at medicines level
  left_join(., Ref_Covid_DailyDoses, by = c("Category", "Group", "Sub Group", "Medicine")) %>% # Join on daily doses information from reference table
  select(1:8) %>% # short cut for selecting the first 8 columns, rather than having to type out each column name
  mutate(Usage_In_DD = Usage_In_Strength / `Daily dose - Volume`) %>% # create new variable - usage in terms of number of daily doses
  group_by(Category, Group, `Sub Group`) %>% # group medicines in to treatment groups
  mutate(`Prop Of Usage Through Medicine` = Usage_In_DD / sum(Usage_In_DD)) %>% # calcualte the % of total treatment group usage through each medicine
  ungroup() # ungroup

## Step 3 - Create new data tables for Covid demand forecasts
## One at medicine level, forecasting Covid demand in strength
## One at treatment group level, forecasting Covid demand in daily doses

## First set some variables and values for use in forecast tables
Min_Date <- min(Forecast_Scenario$Date) # set the minimum date covered in forecast table
Max_Date <- max(Forecast_Scenario$Date) # set the maximum data covered in forecast table
Count_Dates <- as.numeric(Max_Date) - (as.numeric(Min_Date) - 1) # Count number of dates in forecast table
## Calculate the average deaths since over last seven days to use to project demand for palliative medicines due to Covid in future
Average_Deaths_Last_7_Days <- Deaths %>% # read in data
  filter(Date >= max(Date) - 6) %>% # keep the latest seven days
  group_by() %>%
  summarise(Avg_Deaths = mean(Deaths_In_Hospitals)) %>% # calculate the mean deaths in hospitals over last seven days
  .$Avg_Deaths # save as a value for reference in future calculations
## Create a blank data table that just has a row for each medicine (with category and group information) 
### and a value for the number of rows we want to have for each medicine (based on dates covered by beds forecast scenario)
Meds_Groupings <- Ref_Covid_DailyDoses %>% # read in data
  select(Category, Group, `Sub Group`, Medicine) %>% # select medicines information
  mutate(Count = Count_Dates) # add in information on number of dates included in forecast scenario

## Then build covid forecast data tables
## one at a medicine level in strength and one at treatment group level in daily doses
## Medicines Level
Covid_Forecast_Table_Medicines <- expandRows(Meds_Groupings, "Count", drop = FALSE) %>% # this bit of code duplicates each row in meds grouping the number of times specified in the "Count" varibale
  group_by(Category, Group, `Sub Group`, Medicine) %>% # group by medicines information
  mutate(Date = seq(Min_Date, Max_Date, by = 1)) %>% # create a new variable which is a sequence of dates based on forecast scenario - this is repeated for each medicine
  ungroup() %>%
  left_join(., Forecast_Scenario, by = c("Date")) %>% # join on the beds forecasts
  left_join(., Deaths, by = c("Date")) %>% # join on the deaths data
  left_join(., Actual_Usage_In_DD_MedsLevel, by = c("Category", "Group", "Sub Group", "Medicine")) %>% # join on acutual usage information
  select(-c("Usage_In_Strength", "Strength of VMP - Units", "Daily dose - Volume", "Daily dose - Units", "Usage_In_DD")) %>% # get rid of some of the variables we don't need
  left_join(., Ref_Covid_DailyDoses, by = c("Category", "Group", "Sub Group", "Medicine")) %>% # join on daily doses information for V, O+, O and EoL patients
  select(-c("Daily dose - Units", "Prop of patients - V", "Prop of patients - O+", "Prop of patients - O",
            "Prop of patients - EoL", "Prop time in hospital - V", "Prop time in hospital - O+", "Prop time in hospital - O",
            "Prop time in hospital - EoL")) %>% # get rid of some information we don't need
  group_by(Date) %>% # group by date - this part of the code is used to fill in the Deaths information
  mutate(Deaths_In_Hospitals = ifelse(is.na(Deaths_In_Hospitals), Average_Deaths_Last_7_Days, Deaths_In_Hospitals)) %>% # where deaths in hospital data is missing, use the average deaths from last seven days
  ungroup() %>%
  filter(!is.na(`Prop Of Usage Through Medicine`)) %>% # drop the medicines where we don't have complete usage assumptions
  mutate(Estimated_Covid_Demands_Strength = 
           (V_Occ_Beds * `Prop Of Usage Through Medicine` * `Avg dose per treatment day - V`) +
           (`O+_Occ_Beds` * `Prop Of Usage Through Medicine` * `Avg dose per treatment day - O+`) +
           (`O_Occ_Beds` * `Prop Of Usage Through Medicine` * `Avg dose per treatment day - O`) +
           (`Deaths_In_Hospitals` * `Prop Of Usage Through Medicine` * `Avg dose per treatment day - EoL`),
         Estimated_Covid_Demand_DailyDose = Estimated_Covid_Demands_Strength / `Daily dose - Volume`) # Calculate new variables for Covid demand
## Treatment Group Level
Covid_Forecast_Table_Group <- Covid_Forecast_Table_Medicines %>% # Take the medicines level table
  group_by(Category, Group, `Sub Group`, Date) %>% # group by treatment group
  summarise(Estimated_Covid_Demand_DailyDose = sum(Estimated_Covid_Demand_DailyDose)) # summarise the estimated demand in daily doses at a treatment group level

## Step 4 - Write outputs to csv files
## Although these tables aren't the final ouput of the model
## it might be useful to look at the demand forecasts for Covid in Excel to do any quick checks or comparisons

write.csv(Covid_Forecast_Table_Medicines, "./Outputs/R_Forecast_Model/Covid_Forecast_Table_Medicines_PlanningScenario_20200618.csv")

write.csv(Covid_Forecast_Table_Group, "./Outputs/R_Forecast_Model/Covid_Forecast_Table_Group_PlanningScenario_20200618.csv")

######
## CHECKS 
## Some output checks to make sure estimated demand is as expected
## Table below selects a subset of forecast - chosen Atipyretics and NMBAs as a test
Check <- Covid_Forecast_Table_Group %>% # read in final output for treatment group Covid demand forecast
  filter(Category == "ITU", Group %in% c("Antipyretics", "NMBAs")) %>% # select subset of treatment groups
  mutate(Ref = paste(Group, `Sub Group`, sep = " ")) # create a unique reference, so NMBA continuos and induction groups are shown seperately
## Plot values as line chart to check that demand is as expected
# Antipyretics taken by 100% of patients on 100% of time
# NMBA continuous taken by 100% of patients 50% of time
# NMBA induction taken by 100% of patients 10% of time
## Do lines show this relationship in demand?
ggplot(data = Check, aes(x = Date, y = Estimated_Covid_Demand_DailyDose, group = Ref)) +
  geom_line(aes(color = Ref)) # code to create line chart
## Yes looks like it

## Then this table has just been formatted so easy to compare by eye - turned from long to wide format
## So can check actual figures
Check_Date <- Check %>% # read in long format
  select(Ref, Date, Estimated_Covid_Demand_DailyDose) %>% # select variables of interst
  cast(., Ref ~ Date, first) # change data table from long to wide format (3 rows and many columns)





