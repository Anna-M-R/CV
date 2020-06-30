### R CODE - Comparing primary and secondary care data
## Last updated: 09/08/2019
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
setwd("//ims.gov.uk/data/NHS_ENGLAND/Secure/SpecCom/Medicines & Diagnostics Policy Unit/Analytical work/Medicines Strategy")
# If you want to use a different folder then just change the path in brackets

#----------------------------------
# Read in source data
SC_Master <- read.csv(file = "./Low Priority Prescribing/LVM Wave 1/Data/SecondaryCare_Data/MasterData_AprToJun2019_V3.csv") %>%
  filter(Month != 201906)

# Compare to primary care prescribing
PC_Master = read_rds("./Low Priority Prescribing/LVM Wave 1/Data/Data for reporting - Post March 2019/Master_Data_August2019_CCG.rds") %>%
  filter(PERIOD >= 201904 & DRUG_GROUP %in% c("Once Daily Tadalafil", "Liothyronine", "Immediate Release Fentanyl", "Lidocaine Plasters"))

# Check months covered in pri and sec data
unique(SC_Master$Month)
unique(PC_Master$PERIOD)
# Both cover April and May 2019

# -----------------------------
# Match together primary and secondary care data
SC_STP_Summary <- SC_Master %>%
  group_by(STP_CODE,DRUG_GROUP) %>%
  summarise(PATIENT_COUNT = first(PATIENT_COUNT),
            SC_Cost = sum(Cost)) %>%
  mutate(DRUG_GROUP_Label = as.character(DRUG_GROUP)) %>%
  select(STP_CODE, DRUG_GROUP_Label, PATIENT_COUNT, SC_Cost) %>%
  dplyr::rename(DRUG_GROUP = DRUG_GROUP_Label)

PC_STP_Summary <- PC_Master %>%
  group_by(COMM_REGION_NAME, STP_CODE, STP_NAME, DRUG_GROUP) %>%
  summarise(PC_Cost = sum(MONTHLY_TOTAL_ACTUAL_COST))

PC_SC_STP <- left_join(PC_STP_Summary , SC_STP_Summary, by = c("STP_CODE", "DRUG_GROUP")) %>%
  mutate(DRUG_GROUP_Label = gsub(" ", "\n", DRUG_GROUP),
         COMM_REGION_NAME_Label = substr(COMM_REGION_NAME,1,str_length(COMM_REGION_NAME)-21))

#----------------------------------------------
# National level charts
Nat_PC_SC <- PC_SC_STP %>%
  group_by(DRUG_GROUP, DRUG_GROUP_Label) %>%
  summarise(PATIENT_COUNT = sum(PATIENT_COUNT),
            `Primary care - Total spend` = sum(PC_Cost),
            `Secondary care - Total spend` = sum(SC_Cost)) %>%
  as.data.frame(.) %>%
  melt(., id = c("DRUG_GROUP", "DRUG_GROUP_Label")) %>%
  filter(variable != "PATIENT_COUNT")

# Chart showing total spend
ggplot(Nat_PC_SC, aes(x = DRUG_GROUP_Label, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "LPP W1 prescribing",
       subtitle = "National spend: April to May 2019") +
  xlab("LPP W1 drug groups") +
  ylab("£ spend") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma)
# Chart showing proportion of spend through primary and secondary care
ggplot(Nat_PC_SC, aes(x = DRUG_GROUP_Label, y = value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "LPP W1 prescribing",
       subtitle = "National spend: April to May 2019") +
  xlab("LPP W1 drug groups") +
  ylab("% of spend") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent)

#-------------------------------------------
# Spend by Region
Region <- PC_SC_STP %>%
  group_by(COMM_REGION_NAME_Label, DRUG_GROUP) %>%
  summarise(`Primary Care - Total spend` = sum(PC_Cost),
            `Secondary Care - Total spend` = sum(SC_Cost)) %>%
  as.data.frame(.) %>%
  melt(., id= c("COMM_REGION_NAME_Label", "DRUG_GROUP"))

Chart_Region <- Region %>%
  filter(DRUG_GROUP == "Lidocaine Plasters")

# Total spend
ggplot(Chart_Region, aes(x = COMM_REGION_NAME_Label, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "LPP W1 prescribing: Lidocaine Plasters",
       subtitle = "Regional spend: April to May 2019") +
  xlab("Region") +
  ylab("£ spend") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()
# Proportion of spend through pri and sec care
ggplot(Chart_Region, aes(x = COMM_REGION_NAME_Label, y = value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "LPP W1 prescribing: Lidocaine Plasters",
       subtitle = "Regional spend: April to May 2019") +
  xlab("Region") +
  ylab("Propotion of spend") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()


#-------------------------------------------
# Spend by STP
STP <- PC_SC_STP %>%
  dplyr::rename(`Primary Care` = PC_Cost,
            `Secondary Care` = SC_Cost) %>%
  as.data.frame(.) %>%
  melt(., id= c("COMM_REGION_NAME", "COMM_REGION_NAME_Label","STP_NAME", "STP_CODE", "DRUG_GROUP", "DRUG_GROUP_Label")) %>%
  filter(variable != "PATIENT_COUNT")

Chart_STP <- STP %>%
  filter(COMM_REGION_NAME == "Midlands Commissioning Region")

ggplot(Chart_STP, aes(x = STP_NAME, y = value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "LPP W1 prescribing",
       subtitle = "STP spend: April to May 2019") +
  xlab("STP") +
  ylab("Total spend split between primary and secondary care") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  facet_wrap(~DRUG_GROUP)

#-------------------------------------
# At STP level want to compare total spend per 1,000 patients and % of total spend through secondary care
STP_Per1000_PropSC <- PC_SC_STP %>%
  mutate(Total_Spend_Per_1000 = ((PC_Cost + SC_Cost) / PATIENT_COUNT) * 1000,
         Prop_SC = SC_Cost / (PC_Cost + SC_Cost))

Chart_CostVsPropSC <- STP_Per1000_PropSC %>%
  filter(DRUG_GROUP == "Once Daily Tadalafil")

ggplot(data = Chart_CostVsPropSC, aes(x = Total_Spend_Per_1000, y = Prop_SC)) +
  geom_point() +
  labs(title = "Once Daily Tadalafil: Spend through Primary and Secondary Care",
       subtitle = "STP spend: April to May 2019") +
  xlab("Total spend per 1,000 patients") +
  ylab("Proportion of total spend through secondary care") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) + 
  ggrepel::geom_text_repel(data = subset(Chart_CostVsPropSC, Prop_SC > .25 | Total_Spend_Per_1000 > 100), 
            aes(x = Total_Spend_Per_1000, y = Prop_SC, label = STP_NAME),
            size = 2)






