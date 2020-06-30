#---------------------------------------------------------
### LPP W1 & LPP W3 Setting national goals for 40 age/sex bands - working with data split by age and sex 
#---------------------------------------------------------
#R script - reading in NHS BSA data split by age/sex of patients at CCG level and adding patient numbers
# Then using this dataset to calculate national goals for LPP W1 & LPP W3
# Anna Rowan
# Last updated: 19/08/2019
# This just clears the global environment
rm(list = ls())
#----------------------------------------------
# Set up, installing packages and setting working directory

#Only need to run this code if packages not already installed
# install.packages("dplyr") #useful for manipulating data, makes code easier to write
# install.packages("reshape")
# install.packages("varhandle") # converting factors to numeric variables?
# install.packages("tidyverse")

#Need to run this each time to bring in R packages
library(dplyr)
library(reshape)
library(varhandle)
library(tidyverse)

# This sets the working directory, so R knows where to look for data files
setwd("//ims.gov.uk/data/NHS_ENGLAND/Secure/SpecCom/Medicines & Diagnostics Policy Unit/Analytical work/Medicines Strategy")
# If you want to use a different folder then just change the path in brackets

#--------------------------------------------
# Bringing in CCG mapping and patient number datasets

# Reading in csv file from working directory
# CCG changes mapped from 2013 to 2019 (so refer to all data using CCG as in 2019)
CCG_Mapping_2013_To_2019 <- read.csv(file="./Useful Resources/Geography_RefFiles/CCG_Changes_Over_Time_2013To2019.csv")

# Set base year, year on which to base the national gaols
# These variables are used to choose correct patient nums for base years
BY_W1 <- "2017/2018"
BY_W3 <- "2018/2019"
BaseYear <- c(BY_W1, BY_W3)
# Reading in R data file from working directory - patient numbers in FY 2017/18
# Based on code saved here (\SpecCom\Medicines & Diagnostics Policy Unit\Analytical work\Medicines Strategy\Useful Resources\RegisteredPatients\PatientNums_CombinedDatset.R)
PatientNums <- readRDS(file = "./Useful Resources/RegisteredPatients/PatientNums_FY14ToFY19.rds") %>%
  filter(F_YEAR %in% BaseYear) # This filter only keeps data for base years for W1 and W3

#-------------------------------------------
# Cleaning and arranging prescribing data - at age/sex level for national goals

# Bringing in base year prescribing data for waves one and three
W1_Base <- readRDS(file = "./Low Priority Prescribing/LVM Wave 1/Data/Data from BSA/May2019/DR0979_wave1_demog_2017_18.Rds") # 532,244 obs. of 15 variables
W3_Base <- readRDS(file = "./Low Priority Prescribing/LVM Wave 1/Data/Data from BSA/May2019/DR0979_wave3_demog_2018_19.Rds") # 383,355 obs. of 15 variables

# Check the list of variables in the dataset
names(W3_Base)

# Check the unique values for DRUG_GROUP and AGE_SEX_GROUP
unique(W3_Base$DRUG_GROUP)
unique(W3_Base$AGE_SEX_GROUP)

# Bring data together
PrescribingData_V1 <- union(W1_Base, W3_Base) %>% 
  select(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_SEX_GROUP, TOTAL_ITEMS) %>%
  mutate(AGE_SEX_GROUP = as.character(AGE_SEX_GROUP),
         CCG_CODE = substr(CCG_CODE,1,3),
         TOTAL_ITEMS = ifelse(is.na(TOTAL_ITEMS), 0, TOTAL_ITEMS))

#-------------
# Assigning unknowns to age/sex bands
# We do not know the patient age/sex for all items prescribed, so first task is to assign unknowns to complete age/sex groups
 
# First want to check how many items can't be assigned to complete age/sex groups
# Create summary dataset which groups items by how much information we know about the patient
Check_Groups_BaseYear <- PrescribingData_V1 %>%
  mutate(Group = ifelse(substr(AGE_SEX_GROUP,1,1) == "U" & substr(AGE_SEX_GROUP,3,3) == "U", "Unknown - all",
                        ifelse(substr(AGE_SEX_GROUP,1,1) == "U" & substr(AGE_SEX_GROUP,3,3) != "U", "Unknown - sex",
                               ifelse(substr(AGE_SEX_GROUP,1,1) != "U" & substr(AGE_SEX_GROUP,3,3) == "U", "Unknown - age", "Known - all"))),
         TOTAL_ITEMS = ifelse(is.na(TOTAL_ITEMS), 0 , TOTAL_ITEMS)) %>%
  group_by(DRUG_GROUP, Group) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  group_by(DRUG_GROUP) %>%
  mutate(Overall_Total = sum(TOTAL_ITEMS)) %>%
  ungroup() %>%
  mutate(`% of total` = TOTAL_ITEMS / Overall_Total)
# Chart to show the % of items where we know patient info and size of unknown group
ggplot() + 
  geom_bar(aes(x = DRUG_GROUP, y = `% of total`, fill = Group), data = Check_Groups_BaseYear, stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Product", y = " % of base year prescriptions in each group") + 
  coord_flip()

# There are three groups of unknowns
# 1 - items where we know patients age but not sex
# 2 - items where we know patients sex but not age
# 3 - items where we have no patient information
# Different approaches for estimating patient information for each group

# Group 1 - Items where we know patients age but not sex
# Here we assume that for each product, patients with unknown sex follow the same F/M split as other patients in same age group
# For "known" group - we need to calculate % M and F for each product and age group
Known_F_M <- PrescribingData_V1 %>%
  filter(substr(AGE_SEX_GROUP,1,1) != "U" & substr(AGE_SEX_GROUP,3,3) != "U") %>%
  mutate(SEX_GROUP= ifelse(substr(AGE_SEX_GROUP,1,1) == "F", "F", "M"),
         AGE_GROUP = substr(AGE_SEX_GROUP,3,length(AGE_SEX_GROUP))) %>%
  group_by(DRUG_GROUP, AGE_GROUP, SEX_GROUP) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  group_by(DRUG_GROUP, AGE_GROUP) %>%
  mutate(OVERALL_ITEMS = sum(TOTAL_ITEMS),
         `%_ITEMS` = TOTAL_ITEMS/ OVERALL_ITEMS) %>%
  ungroup() %>%
  arrange(DRUG_GROUP, SEX_GROUP, AGE_GROUP) %>%
  select(DRUG_GROUP, SEX_GROUP, AGE_GROUP, `%_ITEMS`)

SEX <- c("F", "M")
F_YEAR <- unique(PrescribingData_V1$F_YEAR)
FIX = expand.grid(F_YEAR, SEX)%>%
  dplyr::rename(F_YEAR = Var1,
                SEX = Var2) %>%
  mutate(SEX = as.character(SEX))
  
Unknown_SEX_one <- PrescribingData_V1 %>%
  filter(substr(AGE_SEX_GROUP,1,1) == "U" & substr(AGE_SEX_GROUP,3,3) != "U") %>%
  mutate(AGE_GROUP = substr(AGE_SEX_GROUP,3,length(AGE_SEX_GROUP))) %>%
  group_by(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_GROUP) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  left_join(., Known_F_M, by = c("DRUG_GROUP","AGE_GROUP")) %>%
  filter(is.na(`%_ITEMS`)) %>%
  left_join(., FIX, by = "F_YEAR") %>%
  mutate(`%_ITEMS` = ifelse(is.na(`%_ITEMS`), 0.5 , `%_ITEMS`),
         SEX_GROUP = ifelse(is.na(SEX_GROUP), SEX, SEX_GROUP),
         AGE_SEX_GROUP = paste(SEX_GROUP, AGE_GROUP, sep = " "),
         ESTIMATE_TOTAL_ITEMS = `%_ITEMS` * TOTAL_ITEMS) %>%
  select(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_SEX_GROUP, ESTIMATE_TOTAL_ITEMS)

Unknown_SEX_two <- PrescribingData_V1 %>%
  filter(substr(AGE_SEX_GROUP,1,1) == "U" & substr(AGE_SEX_GROUP,3,3) != "U") %>%
  mutate(AGE_GROUP = substr(AGE_SEX_GROUP,3,length(AGE_SEX_GROUP))) %>%
  group_by(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_GROUP) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  left_join(., Known_F_M, by = c("DRUG_GROUP","AGE_GROUP")) %>%
  filter(!is.na(`%_ITEMS`)) %>%
  mutate(`%_ITEMS` = ifelse(is.na(`%_ITEMS`), 0.5 , `%_ITEMS`),
         AGE_SEX_GROUP = paste(SEX_GROUP, AGE_GROUP, sep = " "),
         ESTIMATE_TOTAL_ITEMS = `%_ITEMS` * TOTAL_ITEMS) %>%
  select(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_SEX_GROUP, ESTIMATE_TOTAL_ITEMS)

Unknown_SEX = union(Unknown_SEX_one, Unknown_SEX_two)

# Group 2 - Items where we know patients sex but not age
# Here we assume that for each product, patients with unknown age follow the same age distribution as other patients in same sex group
# For "known" group - we need to calculate % in each age group for each product and sex group

Known_AGE <- PrescribingData_V1 %>%
  filter(substr(AGE_SEX_GROUP,1,1) != "U" & substr(AGE_SEX_GROUP,3,3) != "U") %>%
  mutate(SEX_GROUP = ifelse(substr(AGE_SEX_GROUP,1,1) == "F", "F", "M"),
         AGE_GROUP = substr(AGE_SEX_GROUP,3,length(AGE_SEX_GROUP))) %>%
  group_by(DRUG_GROUP, AGE_GROUP, SEX_GROUP) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  group_by(DRUG_GROUP, SEX_GROUP) %>%
  mutate(OVERALL_ITEMS = sum(TOTAL_ITEMS),
         `%_ITEMS` = TOTAL_ITEMS/ OVERALL_ITEMS) %>%
  ungroup() %>%
  arrange(DRUG_GROUP, SEX_GROUP, AGE_GROUP) %>%
  select(DRUG_GROUP, SEX_GROUP, AGE_GROUP, `%_ITEMS`)

Unknown_AGE <- PrescribingData_V1 %>%
  filter(substr(AGE_SEX_GROUP,1,1) != "U" & substr(AGE_SEX_GROUP,3,3) == "U") %>%
  mutate(SEX_GROUP = ifelse(substr(AGE_SEX_GROUP,1,1) == "F", "F", "M")) %>%
  group_by(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, SEX_GROUP) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  left_join(., Known_AGE, by = c("DRUG_GROUP","SEX_GROUP")) %>%
  mutate(AGE_SEX_GROUP = paste(SEX_GROUP, AGE_GROUP, sep = " "),
         ESTIMATE_TOTAL_ITEMS = `%_ITEMS` * TOTAL_ITEMS) %>%
  select(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_SEX_GROUP, ESTIMATE_TOTAL_ITEMS)

# Group 3 - Items where we don't know anything about patient
# Here we just assume that for each product, patients are assigned to age/sex groups in same distribution as patients where we know all information
# Assigning prescriptions where age/sex is unknown to age/sex bands based on national distribution for each drug group
Known_ALL <- PrescribingData_V1 %>%
  filter(substr(AGE_SEX_GROUP,1,1) != "U" & substr(AGE_SEX_GROUP,3,3) != "U") %>%
  group_by(DRUG_GROUP, AGE_SEX_GROUP) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  group_by(DRUG_GROUP) %>%
  mutate(OVERALL_ITEMS = sum(TOTAL_ITEMS),
         `%_ITEMS` = TOTAL_ITEMS/ OVERALL_ITEMS) %>%
  ungroup() %>%
  arrange(DRUG_GROUP, AGE_SEX_GROUP) %>%
  select(DRUG_GROUP, AGE_SEX_GROUP, `%_ITEMS`)

Unknown_ALL <- PrescribingData_V1 %>%
  filter(substr(AGE_SEX_GROUP,1,1) == "U" & substr(AGE_SEX_GROUP,3,3) == "U") %>%
  group_by(F_YEAR,CCG_CODE, DRUG_GROUP, WAVE) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  left_join(., Known_ALL, by = "DRUG_GROUP") %>%
  mutate(ESTIMATE_TOTAL_ITEMS = `%_ITEMS` * TOTAL_ITEMS) %>%
  select(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_SEX_GROUP, ESTIMATE_TOTAL_ITEMS)

# Now need to bring all of this together and carry out checks of items at a national and CCG level
# Union all datasets and group by CCG, Drug Group and Age Sex group
Union_Unknowns <- union(Unknown_ALL, Unknown_AGE) %>%
  union(., Unknown_SEX) %>%
  mutate(ESTIMATE_TOTAL_ITEMS = ifelse(is.na(ESTIMATE_TOTAL_ITEMS), 0 , ESTIMATE_TOTAL_ITEMS)) %>%
  group_by(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_SEX_GROUP) %>%
  summarise(ESTIMATE_TOTAL_ITEMS = sum(ESTIMATE_TOTAL_ITEMS))
  
BaseYear_No_Unknowns <- PrescribingData_V1 %>%
  filter(substr(AGE_SEX_GROUP,1,1) != "U" & substr(AGE_SEX_GROUP,3,3) != "U") %>%
  group_by(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_SEX_GROUP) %>%
  summarise(ESTIMATE_TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  union(., Union_Unknowns) %>%
  group_by(F_YEAR, CCG_CODE, DRUG_GROUP, WAVE, AGE_SEX_GROUP) %>%
  summarise(ESTIMATE_TOTAL_ITEMS = sum(ESTIMATE_TOTAL_ITEMS))

######################################################
# CHECKS - NATIONAL TOTALS
# Total items - raw data equals adjusted data
Raw_National_Items <- sum(PrescribingData_V1$TOTAL_ITEMS)

Estimate_National_Items <- sum(BaseYear_No_Unknowns$ESTIMATE_TOTAL_ITEMS)

# CHECKS - CCG TOTALS
# Total Items
CCG_Check_Raw <- PrescribingData_V1 %>%
  group_by(CCG_CODE) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS))

CCG_Check_Adjusted <- BaseYear_No_Unknowns %>%
  group_by(CCG_CODE) %>%
  summarise(ESTIMATE_TOTAL_ITEMS = sum(ESTIMATE_TOTAL_ITEMS))

CCG_Check_Total <- left_join(CCG_Check_Raw, CCG_Check_Adjusted, by = "CCG_CODE") %>%
  mutate(Diff = TOTAL_ITEMS - ESTIMATE_TOTAL_ITEMS) %>%
  filter(Diff != 0)

# All match so now can use this dataset to look at the distribution of prescribing in base years and set national prescribinging goals
# Also going to remove some datasets to clear up the global environment
rm(CCG_Check_Adjusted)
rm(CCG_Check_Raw)
rm(CCG_Check_Total)
rm(FIX)
rm(Known_AGE)
rm(Known_ALL)
rm(Known_F_M)
rm(W1_Base)
rm(W3_Base)
rm(Union_Unknowns)
rm(Unknown_AGE)
rm(Unknown_ALL)
rm(Unknown_SEX)
rm(Unknown_SEX_one)
rm(Unknown_SEX_two)
rm(F_YEAR)

######################################################################
# Now have a prescribing dataset for base years with no unknowns, all prescriptions have been assigned to a patient age/sex group 
# Next step is to join on registered patient numbers at CCG level

# First need to make sure CCG refs are consistent between prescribing data and patient nums
# count unique CCG CODES - if 191 then up to date for 2019
Unique_CCG = unique(BaseYear_No_Unknowns$CCG_CODE)
# 191 CCG CODES

# We can match on the prescribing data to the registered patient counts for FY base year
# Use a skelton dataset to make sure we have a row for each ccg, age sex group and drug group

CCG_2019 <- CCG_Mapping_2013_To_2019 %>%
  group_by(CCG.19) %>%
  summarise(count = n()) %>%
  mutate(CCG_char = as.character(CCG.19)) %>%
  .$CCG_char
DRUG_GROUP <- BaseYear_No_Unknowns %>%
  group_by(DRUG_GROUP) %>%
  summarise(count = n()) %>%
  .$DRUG_GROUP
AGE_SEX_GROUP <- BaseYear_No_Unknowns %>%
  group_by(AGE_SEX_GROUP) %>%
  summarise(count = n()) %>%
  .$AGE_SEX_GROUP

Skeleton_Data_Set <- expand.grid(CCG_2019 ,DRUG_GROUP, AGE_SEX_GROUP) %>%
  dplyr::rename(CCG_CODE = Var1,
                DRUG_GROUP = Var2,
                AGE_SEX_GROUP = Var3) %>% # 191,000 observations
  mutate(WAVE = ifelse(substr(DRUG_GROUP, (str_length(DRUG_GROUP) - 9), str_length(DRUG_GROUP)) == "[NEW 2019]", "W3", 
                       ifelse(substr(DRUG_GROUP, 1, 7) == "Needles", "W3","W1")),
         F_YEAR = ifelse(WAVE == "W1", BY_W1, BY_W3))

BaseYear_Master <- PatientNums %>%
  select(F_YEAR, CCG_CODE, AGE_SEX_GROUP, PATIENT_COUNT) %>%
  left_join(Skeleton_Data_Set, ., by = c("F_YEAR","CCG_CODE", "AGE_SEX_GROUP")) %>%
  left_join(., BaseYear_No_Unknowns, by = c("F_YEAR", "CCG_CODE", "AGE_SEX_GROUP", "DRUG_GROUP", "WAVE")) %>%
  mutate(ESTIMATE_TOTAL_ITEMS = ifelse(is.na(ESTIMATE_TOTAL_ITEMS), 0 , ESTIMATE_TOTAL_ITEMS)) %>%
  arrange(CCG_CODE, DRUG_GROUP, AGE_SEX_GROUP)

# Check total items again
Check_Again_Items <- sum(BaseYear_Master$ESTIMATE_TOTAL_ITEMS)
# No lost items - so can go ahead

#--------------
# Set national prescribing goals for each age/sex band
# These are based on policy targets for the three product groups
# First, we are going to look at the distribution of "overall prescribing rates" and use this information to choose a goal for each product
# So we look at CCG overall prescribing rate at a point in the distribution and the associated reduction in number of items prescribed

# Need to calculate prescribing rates at CCG level
OverallRates_CCG_BaseYear <- BaseYear_Master %>%
  group_by(DRUG_GROUP, WAVE, CCG_CODE) %>%
  summarise(ESTIMATE_TOTAL_ITEMS = sum(ESTIMATE_TOTAL_ITEMS),
            PATIENT_COUNT = sum(PATIENT_COUNT)) %>%
  mutate(CCG_ACTUAL_ITEMS_PER_1000 = (ESTIMATE_TOTAL_ITEMS / PATIENT_COUNT) * 1000) %>%
  group_by(DRUG_GROUP) %>%
  mutate(NAT_RATE_PER_1000 = (sum(ESTIMATE_TOTAL_ITEMS) / sum(PATIENT_COUNT)) * 1000) %>%
  ungroup()

# And then distribution
Distribution_CCG_BaseYear <- OverallRates_CCG_BaseYear %>%
  group_by(DRUG_GROUP) %>%
  mutate(DIST = ntile(CCG_ACTUAL_ITEMS_PER_1000, 100)) %>%
  ungroup() %>%
  group_by(DRUG_GROUP, WAVE, DIST) %>%
  summarise(CCG_ACTUAL_ITEMS_PER_1000 = max(CCG_ACTUAL_ITEMS_PER_1000)) %>%
  filter(DIST %in% c(5, 10, 15, 20, 25, 30, 40, 50)) %>%
  cast(., DRUG_GROUP ~ DIST, value = "CCG_ACTUAL_ITEMS_PER_1000")

NAT_SUMMARY <- BaseYear_Master %>%
  group_by(DRUG_GROUP) %>%
  summarise(ACT_TOTAL_ITEMS = sum(ESTIMATE_TOTAL_ITEMS),
            NAT_PATIENT_COUNT = sum(PATIENT_COUNT))

Explore_Dist_BaseYear <- left_join(Distribution_CCG_BaseYear, NAT_SUMMARY, by = "DRUG_GROUP") %>%
  mutate(RED_ITEMS_AT_5TH = (((`5` * NAT_PATIENT_COUNT) / 1000) - ACT_TOTAL_ITEMS) / ACT_TOTAL_ITEMS,
         RED_ITEMS_AT_10TH = (((`10` * NAT_PATIENT_COUNT) / 1000) - ACT_TOTAL_ITEMS) / ACT_TOTAL_ITEMS,
         RED_ITEMS_AT_15TH = (((`15` * NAT_PATIENT_COUNT) / 1000) - ACT_TOTAL_ITEMS) / ACT_TOTAL_ITEMS,
         RED_ITEMS_AT_20TH = (((`20` * NAT_PATIENT_COUNT) / 1000) - ACT_TOTAL_ITEMS) / ACT_TOTAL_ITEMS,
         RED_ITEMS_AT_25TH = (((`25` * NAT_PATIENT_COUNT) / 1000) - ACT_TOTAL_ITEMS) / ACT_TOTAL_ITEMS,
         RED_ITEMS_AT_30TH = (((`30` * NAT_PATIENT_COUNT) / 1000) - ACT_TOTAL_ITEMS) / ACT_TOTAL_ITEMS,
         RED_ITEMS_AT_40TH = (((`40` * NAT_PATIENT_COUNT) / 1000) - ACT_TOTAL_ITEMS) / ACT_TOTAL_ITEMS,
         RED_ITEMS_AT_50TH = (((`50` * NAT_PATIENT_COUNT) / 1000) - ACT_TOTAL_ITEMS) / ACT_TOTAL_ITEMS,
         TARGET_TO_CALCULATE_NAT_GOAL = ifelse(DRUG_GROUP %in% c("Immediate Release Fentanyl", "Lidocaine Plasters", "Liothyronine", 'Travel Vaccines', "Amiodarone [NEW 2019]", "Dronedarone [NEW 2019]"), 
                                               RED_ITEMS_AT_25TH, RED_ITEMS_AT_15TH))

write.csv(Explore_Dist_BaseYear, file = "./Low Priority Prescribing/LVM Wave 1/Data/Data for reporting - Post March 2019/LPP_W1_&_W3_Scenarios_For_Goals_BaseYear.csv")

# Use the target reduction (% reduction in prescribing) for each drug group to caclulate a national target for each age/sex band
National_Goals_AgeSex <- Explore_Dist_BaseYear %>%
  select(DRUG_GROUP, TARGET_TO_CALCULATE_NAT_GOAL) %>%
  left_join(BaseYear_Master, . , by = "DRUG_GROUP") %>%
  mutate(TARGET_GROUP = ifelse(DRUG_GROUP %in% c('Co-Proxamol','Glucosamine and Chondroitin','Herbal Medicines',
                                                 'Homeopathy', "Omega-3 Fatty Acid Compounds", "Minocycline for acne [NEW 2019]",
                                                 "Silk garments [NEW 2019]"),'No prescribing',
                               ifelse(DRUG_GROUP %in% c('Immediate Release Fentanyl','Lidocaine Plasters','Liothyronine',
                                                        'Travel Vaccines', "Amiodarone [NEW 2019]" , "Dronedarone [NEW 2019]"),'Defined indications only',
                                      'Non-routine only'))) %>%
  group_by(F_YEAR, DRUG_GROUP, WAVE, AGE_SEX_GROUP, TARGET_GROUP) %>% # No longer at CCG level
  summarise(ANNUAL_NAT_ITEMS = sum(ESTIMATE_TOTAL_ITEMS),
            NAT_PATIENT_COUNT = sum(PATIENT_COUNT),
            TARGET_TO_CALCULATE_NAT_GOAL = first(TARGET_TO_CALCULATE_NAT_GOAL)) %>%
  mutate(ANNUAL_NAT_ITEMS_PER_1000 = (ANNUAL_NAT_ITEMS)/ (NAT_PATIENT_COUNT) * 1000,
         REDUCTION_GOAL = ifelse(TARGET_GROUP == "No prescribing", -1, TARGET_TO_CALCULATE_NAT_GOAL),
         ANNUAL_ITEMS_PER_1000_GOAL = ANNUAL_NAT_ITEMS_PER_1000  + (ANNUAL_NAT_ITEMS_PER_1000 * REDUCTION_GOAL)) %>%
  select(F_YEAR, DRUG_GROUP, WAVE, AGE_SEX_GROUP, TARGET_GROUP, REDUCTION_GOAL, ANNUAL_NAT_ITEMS_PER_1000, ANNUAL_ITEMS_PER_1000_GOAL) %>%
  mutate(GOAL_PER_1000_Y1 = ANNUAL_ITEMS_PER_1000_GOAL + (0.9 * (ANNUAL_NAT_ITEMS_PER_1000 - ANNUAL_ITEMS_PER_1000_GOAL)),
         GOAL_PER_1000_Y2 = ANNUAL_ITEMS_PER_1000_GOAL + ((2/3) * (ANNUAL_NAT_ITEMS_PER_1000 - ANNUAL_ITEMS_PER_1000_GOAL)),
         GOAL_PER_1000_Y3 = ANNUAL_ITEMS_PER_1000_GOAL + ((1/3) * (ANNUAL_NAT_ITEMS_PER_1000 - ANNUAL_ITEMS_PER_1000_GOAL)),
         GOAL_PER_1000_SS = ANNUAL_ITEMS_PER_1000_GOAL) %>%
  select(F_YEAR, DRUG_GROUP, WAVE, AGE_SEX_GROUP, GOAL_PER_1000_Y1, GOAL_PER_1000_Y2, GOAL_PER_1000_Y3, GOAL_PER_1000_SS)


# Save national goals for each product, split by age and sex band
write.csv(National_Goals_AgeSex, file = "./Low Priority Prescribing/LVM Wave 1/Data/Data for reporting - Post March 2019/LPP_W1_&_W3_NationalGoals_UpdateJan2020.csv")
saveRDS(National_Goals_AgeSex, file = "./Low Priority Prescribing/LVM Wave 1/Data/Data for reporting - Post March 2019/LPP_W1_&_W3_NationalGoals_UpdateJan2020.rds")

