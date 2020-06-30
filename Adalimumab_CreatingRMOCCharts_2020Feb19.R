### R CODE - Creating charts for region and trust level for RMOCs
## Last updated: 19/02/2020
## Anna Rowan

# This just clears the global environment
rm(list = ls())
#----------------------------------------------
# Set up, installing packages and setting working directory

#Only need to run this code if packages not already installed
# install.packages("dplyr") #useful for manipulating data, makes code easier to write
# install.packages("reshape")
# install.packages("varhandle") # converting factors to numeric variables?
# install.packages("tidyverse")
# install.packages("readxl")

#Need to run this each time to bring in R packages
library(dplyr)
library(reshape)
library(varhandle)
library(tidyverse)
library(readxl)

# This sets the working directory, so R knows where to look for data files
setwd("//ims.gov.uk/data/NHS_ENGLAND/Secure/SpecCom/Strategy & Policy/Medicines Analysis/Adalimumab")
# If you want to use a different folder then just change the path in brackets

#----------------------------------
# Read in source data - update to latest version of master dataset
Adalimumab_Master_Dataset <- read.csv(file = "./3. Evidence and data/Adalimumab_Monthly_Data_Nov18_Jan20_With_Tender_Info.csv") %>%
  mutate(RMOC_GROUP = ifelse(Region %in% c("East Midlands", "East of England", "West Midlands"), "Midlands & East",
                              ifelse(Region %in% c("North East", "North West", "Yorkshire and Humber"), "North",
                                     ifelse(Region %in% c("South East", "South West", "Thames Valley and Wessex"), "South", "London"))))

Regions <- unique(Adalimumab_Master_Dataset$Region)
print(Regions)

UnknownRegion <- Adalimumab_Master_Dataset %>%
  filter(Region == "Unknown")

#------------------------------
#----------------------------------
## Regional charts

# Regional summary of dataset
Region <- Adalimumab_Master_Dataset %>%
  group_by(RMOC_GROUP, Region, Month, Type) %>%
  summarise(DDD_NoNegatives = sum(DDD_NoNegatives),
            Cost_NoNegatives = sum(Cost_NoNegatives)) %>%
  group_by(RMOC_GROUP, Region, Month) %>%
  mutate(Total_DDD_NoNegatives = sum(DDD_NoNegatives),
            Total_Cost_NoNegatives = sum(Cost_NoNegatives)) %>% 
  ungroup() %>%
  mutate(Prop_of_Total_DDD = DDD_NoNegatives / Total_DDD_NoNegatives,
         Prop_of_Total_Cost = Cost_NoNegatives / Total_Cost_NoNegatives)

# write.csv(Region, file = "./3. Evidence and data/Output - National Summary/Adalimumab_Regional_Jan2020Extract.csv")

## ------------All regions
Chart_All_Regions_Biosimilar <- Region %>%
  filter(Type != "Originator") %>%
  mutate(Month = as.factor(Month)) %>%
  group_by(Region, Month) %>%
  summarise(Prop_of_Total_DDD = sum(Prop_of_Total_DDD))

ggplot(Chart_All_Regions_Biosimilar, aes(x = Month, y = Prop_of_Total_DDD, group = Region)) +
  geom_line(aes(colour = Region), size = 2) +
  theme_bw() +
  labs(title = "Proportion of Adalimumab prescribing through BVB (DDD)",
       x = "Month",
       y = "Proportion of total DDD through BVB") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) + 
  scale_colour_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent)

# -------- Subset of region
Chart_Region_Biosimilar <- Region %>%
 filter(Type != "Originator" & RMOC_GROUP == "South") %>%
 # filter(Type == "Originator") %>%
  mutate(Month = as.factor(Month)) %>%
  group_by(Region, Month) %>%
  summarise(Prop_of_Total_DDD = sum(Prop_of_Total_DDD))

ggplot(Chart_Region_Biosimilar, aes(x = Month, y = Prop_of_Total_DDD, group = Region)) +
  geom_line(aes(colour = Region), size = 2) +
  theme_bw() +
  labs(title = "Proportion of Adalimumab prescribing through BVB (DDD)",
       x = "Month",
       y = "Proportion of total DDD through BVB") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) + 
  scale_colour_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent)
  
#----------------------------------
## Trust level charts

# ------------------ Chart showing latest three months data only, split by first and second line
# ------------------ With trust names

Trust_LatestMonths <- Adalimumab_Master_Dataset %>%
  filter(Month %in% c(201911, 201912, 202001)) %>%
  group_by(Region, Trust, Trust_Name, Month, Type) %>%
  summarise(DDD_NoNegatives = sum(DDD_NoNegatives),
            Cost_NoNegatives = sum(Cost_NoNegatives)) %>%
  group_by(Region, Trust, Trust_Name, Month) %>%
  mutate(Total_DDD_NoNegatives = sum(DDD_NoNegatives),
         Total_Cost_NoNegatives = sum(Cost_NoNegatives)) %>% 
  ungroup() %>%
  mutate(Prop_of_Total_DDD = DDD_NoNegatives / Total_DDD_NoNegatives,
         Prop_of_Total_Cost = Cost_NoNegatives / Total_Cost_NoNegatives)

print(Regions)

Chart_LatestMonths <- Trust_LatestMonths %>%
  filter(Region == "Thames Valley and Wessex" & Type %in% c("First line", "Second line")) %>%
  mutate(Month = as.character(Month))

ggplot(data = Chart_LatestMonths, aes(x = Month, y = Prop_of_Total_DDD, fill = Type, label = Prop_of_Total_DDD)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(round(Prop_of_Total_DDD,2), accuracy = 1L)), position = position_stack(vjust = 0.5)) +
  theme_bw() +
  facet_wrap(~ Trust_Name, labeller = label_wrap_gen(width=40)) +
  labs(title = "Latest three months: BVB prescribing",
       x = "Month",
       y = "Proportion of BVB prescribing through product") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.x = element_text(size = 8)) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent)


#- Total volume prescribed by trust in each region
Volume_LatestMonths <- Trust_LatestMonths %>%
  group_by(Region, Trust, Trust_Name) %>%
  summarise(Total_Volume = sum(DDD_NoNegatives)) %>%
  mutate(Total_Volume_rounded = round(Total_Volume, -2)) %>%
  arrange(Region, Total_Volume)

write.csv(Volume_LatestMonths, file = "./3. Evidence and data/Outputs for Regions/Volume_Latest_3_Months.csv")


#------------------
# Trusts included in Adalimumab data but with no prescribing data in latest month
Latest_Month <- max(Adalimumab_Master_Dataset$Month)

Trusts_Check <- Adalimumab_Master_Dataset %>%
  group_by(RMOC_GROUP, Region, Trust_Name, Trust) %>%
  summarise(Count = n(),
            In_Latest_Month = sum(ifelse(Month == Latest_Month, 1, 0)))

Not_In_Latest_Month <- Trusts_Check %>%
  filter(In_Latest_Month == 0)

write.csv(Not_In_Latest_Month, file = "./3. Evidence and data/Outputs for Regions/Trust not in latest month.csv")
