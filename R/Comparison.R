library(gtsummary)
library(gt)
library(here)
library(tidyverse)
library(readxl)
library(arules)
library(DT)
library(rvest)


#Load Shelby Data
load(here("Data/shelbyMortality.RData"))

#Create Shelby total population dataset 
Year <- c("2015", "2016", "2017", "2018", "2019", "2015-2019")
total_pop <- c("938072", "934612", "936954", "935767", "937166", "4682571")
realShelby_pop <- data.frame(Year = Year, total_pop = total_pop)

#Limit Data Set
dt <- shelby_data |> select(c(death_yr, DDODYr, ageCat, RankableUnderlyingDeathCause))

#Make Death Cause Factor
dt$RankableUnderlyingDeathCause <- as.factor(factor(dt$RankableUnderlyingDeathCause))

tbl_summary(
  data=dt,
  by =ageCat,
  include = RankableUnderlyingDeathCause,
  label = list(
    RankableUnderlyingDeathCause ~ "Cause of Death"
  ),
  sort = all_categorical(FALSE) ~ "frequency", #Sort by Highest to lowest
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  missing_text = "Missing") |> 
  add_overall(col_label = "**Total**  \nN = {style_number(N)}") |> 
  bold_labels() |> 
  modify_header(label = "**Variable**") |> 
  add_stat_label() |> 
  as_gt() |> 
  gt::tab_header(title ="Table 3. Causes of Death Between 2015-2019 in Shelby County By Age")

#2015 Table
dt_15 <-dt |> 
  filter(death_yr == 2015)
  


  yr_2015 <- tbl_summary(
    data=dt_15,
    by =ageCat,
    include = RankableUnderlyingDeathCause,
    label = list(
      RankableUnderlyingDeathCause ~ "Cause of Death"
    ),
    sort = all_categorical(FALSE) ~ "frequency", #Sort by Highest to lowest
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}"
                     ),
    missing_text = "Missing") |> 
    add_overall(col_label = "**Total**  \nN = {style_number(N)}") |> 
    bold_labels() |> 
    modify_header(label = "**Variable**") |> 
    add_stat_label() |>
    as_tibble()
  # as_gt() |> 
  # gt::tab_header(title ="Table 4a. Causes of Death 2015 in Shelby County By Age")

#Create Merge For Total Population Numbers and Causes
  # cause_2015 <- c("Diseases of heart (I00-I09,I11,I13,I20-I51)",
  #                 "Malignant neoplasms (C00-C97)",
  #                 "Other",
  #                 "Accidents (unintentional injuries) (V01-X59,Y85-Y86)",
  #                 "Cerebrovascular diseases (I60-I69)",
  #                 "Alzheimer's disease (G30)",
  #                 "Chronic lower respiratory diseases (J40-J47)",
  #                 "Diabetes mellitus (E10-E14)",
  #                 "Assault (homicide) (*U01–*U02,X85–Y09,Y87.1)",
  #                 "Influenza and pneumonia (J09-J18)")
 
  
# Pull from realShelby_pop number in year 2015
pTotal_2015 <- as.data.frame(938072) |> 
  rename_at('938072', ~'2015_total')
  
  
#Format new dataframe
yr_2015 <- na.omit(yr_2015)

#Merge 2015 Data with Total Living Population Data
yr_2015 <- merge(yr_2015, pTotal_2015)

#Fix Column Names
yr_2015 <- yr_2015 |> rename_at(2, ~'Total') |> 
                      rename_at(3, ~'Age 1-14') |> 
                      rename_at(4, ~'Age 15-24') |> 
                      rename_at(5, ~'Age 25-34') |> 
                      rename_at(6, ~'Age 35-44') |> 
                      rename_at(7, ~'Age 45-54') |> 
                      rename_at(8, ~'Age 55-64') |> 
                      rename_at(9, ~'Age 65+')

#Remove Commas and Calculate Crude Mortality Rate
yr_2015 <- yr_2015 |>
  mutate(Total = as.numeric(gsub(",", "",Total))) |> 
  mutate(`Age 65` = as.numeric(gsub(",", "", `Age 65+`)))
yr_2015 <- yr_2015 |> 
  mutate(dCrude_2015 = (as.numeric(yr_2015$Total) / yr_2015$`2015_total`)* 100000)

#Filter to Top 10 
yr_2015_t10 <- yr_2015[c(1:10),]


#Create Shelby County Age-Per-Group 2015
# Age_Groups <-   c("Age 1-14",
#                  "Age 15-24",
#                  "Age 25-34",
#                  "Age 35-44",
#                  "Age 45-54",
#                  "Age 55-64",
#                  "Age 65+")
# total_pop_2015 <- c(184685,
#                     133255,
#                     134434,
#                     119453,
#                     122842,
#                     116748,
#                     113176)
#                     
#                      
#                      
#                      
#                      
# 
# 
# ageGroupedShelby_2015 <- data.frame(Ages = Age_Groups,
#                                     Total_pop = total_pop_2015,
#                                     ageWeighted = weight_AgeGroup)


#Add Age-adjusted
yr_2015_t10$adjRate_2015 = c("205.0200325",
                             "176.9700214",
                             "117.6488909",
                              "49.6255873",
                             "46.23374696",
                             "46.97822513",
                             "36.33324178",
                             "25.31371242",
                             "20.04100648",
                             "17.11498658")
  

save(yr_2015_t10, file = here("Data/yr_2015_t10.RData"))



####################################################


# 2016 Table 
##############
  dt_16 <- dt |> 
    filter(death_yr == 2016) 

  yr_2016 <- tbl_summary(
    data=dt_16,
    by =ageCat,
    include = RankableUnderlyingDeathCause,
    label = list(
      RankableUnderlyingDeathCause ~ "Cause of Death"
    ),
    sort = all_categorical(FALSE) ~ "frequency", #Sort by Highest to lowest
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}"),
    missing_text = "Missing") |> 
    add_overall(col_label = "**Total**  \nN = {style_number(N)}") |> 
    bold_labels() |> 
    modify_header(label = "**Variable**") |> 
    add_stat_label() |> 
    as_tibble()
  
# Pull from realShelby_pop number in year 2016
  pTotal_2016 <- as.data.frame(934612) |> 
    rename_at('934612', ~'2016_total')
  
#Format new dataframe
  yr_2016 <- na.omit(yr_2016)
  
#Merge 2016 Data with Total Living Population Data
  yr_2016 <- merge(yr_2016, pTotal_2016)
  
#Fix Column Names
  yr_2016 <- yr_2016 |> 
    rename_at(2, ~'Total') |> 
    rename_at(3, ~'Age 1-14') |> 
    rename_at(4, ~'Age 15-24') |> 
    rename_at(5, ~'Age 25-34') |> 
    rename_at(6, ~'Age 35-44') |> 
    rename_at(7, ~'Age 45-54') |> 
    rename_at(8, ~'Age 55-64') |> 
    rename_at(9, ~'Age 65+')
  
#Remove Commas and Calculate Crude Mortality Rate
  yr_2016 <- yr_2016 |>
    mutate(Total = as.numeric(gsub(",", "",Total))) |> 
    mutate(`Age 65` = as.numeric(gsub(",", "", `Age 65+`)))
  
  yr_2016 <- yr_2016 |> 
    mutate(dCrude_2016 = (as.numeric(yr_2016$Total) / yr_2016$`2016_total`)* 100000)
  
#Filter to Top 10 
  yr_2016_t10 <- yr_2016[c(1:10),]
  
#Add Age Adjusted
  yr_2016_t10$adjRate_2016 = c("187.1281394",
                               "181.8366172",
                               "118.9794054",
                               "54.360909",
                               "47.72291479",
                               "37.70751404",
                               "35.41316352",
                               "26.61193117",
                               "24.04217562",
                               "17.54223713")
  #Export dataframe
  save(yr_2016_t10, file = here("Data/yr_2016_t10.RData"))    
  
#############  
# 2017 Table  
  dt_17 <- dt |> 
    filter(death_yr == 2017)
  
  yr_2017 <- tbl_summary(
    data=dt_17,
    by =ageCat,
    include = RankableUnderlyingDeathCause,
    label = list(
      RankableUnderlyingDeathCause ~ "Cause of Death"
    ),
    sort = all_categorical(FALSE) ~ "frequency", #Sort by Highest to lowest
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}"),
    missing_text = "Missing") |> 
    add_overall(col_label = "**Total**  \nN = {style_number(N)}") |> 
    bold_labels() |> 
    modify_header(label = "**Variable**") |> 
    add_stat_label() |>
    as.tibble()
  
  
# Pull from realShelby_pop number in year 2017
  pTotal_2017 <- as.data.frame(936954) |> 
    rename_at('936954', ~'2017_total')
  
#Format new dataframe
  yr_2017 <- na.omit(yr_2017)
  
#Merge 2017 Data with Total Living Population Data
  yr_2017 <- merge(yr_2017, pTotal_2017)
  
#Fix Column Names
  yr_2017 <- yr_2017 |> 
    rename_at(2, ~'Total') |> 
    rename_at(3, ~'Age 1-14') |> 
    rename_at(4, ~'Age 15-24') |> 
    rename_at(5, ~'Age 25-34') |> 
    rename_at(6, ~'Age 35-44') |> 
    rename_at(7, ~'Age 45-54') |> 
    rename_at(8, ~'Age 55-64') |> 
    rename_at(9, ~'Age 65+')
  
  #Remove Commas and Calculate Crude Mortality Rate
  yr_2017 <- yr_2017 |>
    mutate(Total = as.numeric(gsub(",", "",Total))) |> 
    mutate(`Age 65` = as.numeric(gsub(",", "", `Age 65+`)))
  
  yr_2017 <- yr_2017 |> 
    mutate(dCrude_2017 = (as.numeric(yr_2017$Total) / yr_2017$`2017_total`)* 100000)
  
  #Filter to Top 10 
  yr_2017_t10 <- yr_2017[c(1:10),]
  
  #Add Age Adjusted
  yr_2017_t10$adjRate_2017 = c("175.4689304",
                               "155.30255",
                               "117.4301176",
                               "49.14276625",
                               "47.5573192",
                               "43.07143885",
                               "32.99056169",
                               "26.86334376",
                               "20.34598101",
                               "17.04187726")
  #Export as dataframe
  save(yr_2017_t10, file = here("Data/yr_2017_t10.RData"))
  
######
  # 2018 Table  
  dt_18 <- dt |> 
    filter(death_yr == 2018)
  
  yr_2018 <- tbl_summary(
    data=dt_18,
    by =ageCat,
    include = RankableUnderlyingDeathCause,
    label = list(
      RankableUnderlyingDeathCause ~ "Cause of Death"
    ),
    sort = all_categorical(FALSE) ~ "frequency", #Sort by Highest to lowest
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}"),
    missing_text = "Missing") |> 
    add_overall(col_label = "**Total**  \nN = {style_number(N)}") |> 
    bold_labels() |> 
    modify_header(label = "**Variable**") |> 
    add_stat_label() |> 
    as.tibble() 
  
# Pull from realShelby_pop number in year 2018
  pTotal_2018 <- as.data.frame(935767) |> 
    rename_at('935767', ~'2018_total')
  
#Format new dataframe
  yr_2018 <- na.omit(yr_2018)
  
#Merge 2018 Data with Total Living Population Data
  yr_2018 <- merge(yr_2018, pTotal_2018)
  
#Fix Column Names
  yr_2018 <- yr_2018 |> 
    rename_at(2, ~'Total') |> 
    rename_at(3, ~'Age 1-14') |> 
    rename_at(4, ~'Age 15-24') |> 
    rename_at(5, ~'Age 25-34') |> 
    rename_at(6, ~'Age 35-44') |> 
    rename_at(7, ~'Age 45-54') |> 
    rename_at(8, ~'Age 55-64') |> 
    rename_at(9, ~'Age 65+')
  
#Remove Commas and Calculate Crude Mortality Rate
  yr_2018 <- yr_2018 |>
    mutate(Total = as.numeric(gsub(",", "",Total))) |> 
    mutate(`Age 65` = as.numeric(gsub(",", "", `Age 65+`)))
  
  yr_2018 <- yr_2018 |> 
    mutate(dCrude_2018 = (as.numeric(yr_2018$Total) / yr_2018$`2018_total`)* 100000)
  
#Filter to Top 10 
  yr_2018_t10 <- yr_2018[c(1:10),]
  
#Add Age Adjusted
  yr_2018_t10$adjRate_2018 = c("176.7632776",
                               "152.477776",
                               "136.1033483",
                               "41.47183846",
                               "44.3042135",
                               "35.64602312",
                               "32.46773706",
                               "24.83463503",
                               "24.03129993",
                               "17.81589918")
#Export as dataframe
  save(yr_2018_t10, file = here("Data/yr_2018_t10.RData"))  
######  
  
# 2019 Table  
  dt_19 <- dt |> 
    filter(death_yr == 2019)
  
  yr_2019 <- tbl_summary(
    data=dt_19,
    by =ageCat,
    include = RankableUnderlyingDeathCause,
    label = list(
      RankableUnderlyingDeathCause ~ "Cause of Death"
    ),
    sort = all_categorical(FALSE) ~ "frequency", #Sort by Highest to lowest
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}"),
    missing_text = "Missing") |> 
    add_overall(col_label = "**Total**  \nN = {style_number(N)}") |> 
    bold_labels() |> 
    modify_header(label = "**Variable**") |> 
    add_stat_label() |> 
    as.tibble()

  # Pull from realShelby_pop number in year 2019
  pTotal_2019 <- as.data.frame(937166) |> 
    rename_at('937166', ~'2019_total')
  
  #Format new dataframe
  yr_2019 <- na.omit(yr_2019)
  
  #Merge 2019 Data with Total Living Population Data
  yr_2019 <- merge(yr_2019, pTotal_2019)
  
  #Fix Column Names
  yr_2019 <- yr_2019 |> 
    rename_at(2, ~'Total') |> 
    rename_at(3, ~'Age 1-14') |> 
    rename_at(4, ~'Age 15-24') |> 
    rename_at(5, ~'Age 25-34') |> 
    rename_at(6, ~'Age 35-44') |> 
    rename_at(7, ~'Age 45-54') |> 
    rename_at(8, ~'Age 55-64') |> 
    rename_at(9, ~'Age 65+')
  
  #Remove Commas and Calculate Crude Mortality Rate
  yr_2019 <- yr_2019 |>
    mutate(Total = as.numeric(gsub(",", "",Total))) |> 
    mutate(`Age 65` = as.numeric(gsub(",", "", `Age 65+`)))
  
  yr_2019 <- yr_2019 |> 
    mutate(dCrude_2019 = (as.numeric(yr_2019$Total) / yr_2019$`2019_total`)* 100000)
  
  #Filter to Top 10 
  yr_2019_t10 <- yr_2019[c(1:10),]
  
  #Add Age Adjusted
  yr_2019_t10$adjRate_2019 = c("197.0913564",
                               "159.5395148",
                               "140.7622793",
                               "59.79802488",
                               "47.13848307",
                               "30.07978146",
                               "28.1572498",
                               "22.69449253",
                               "24.20413639",
                               "17.63447893")
  
  #Export as Dataframe
  save(yr_2019_t10, file = here("Data/yr_2019_t10.RData"))

######