library(gtsummary)
library(here)
library(tidyverse)
library(readxl)
library(arules)

#Load Data
data <- read_excel(here("Raw Data/Mortality dataset 2015-2019.xlsx"))

#Create Vector Variables

data$death_yr <- factor(data$DDODYr,
                        levels = c("2015", "2016", "2017", "2018", "2019"),
                        labels = c("2015", "2016", "2017", "2018", "2019"))

data$sex <- factor(data$DSex,
       levels = c("M", "F", "U"),
       labels = c("Male", "Female", "Unknown"))

data$race_ethnicity <- factor(data$DRace_Ethnicity,
                         levels = c("Non-Hispanic White", "Non-Hispanic Black/African American",
                                    "Other Race Non-Hispanic", "Hispanic", "Unknown"),
                         labels = c("Non-Hispanic White", "Non-Hispanic Black/African American",
                                   "Other Race Non-Hispanic", "Hispanic", "Unknown"))

data$education <- factor(data$DEducationCode,
                    levels = c(1:9),
                    labels = c("8th Grade or Less", "9th - 12th No Diploma", "HS Graduate or GED",
                               "Some College", "Associate's Degree", "Bachelor's Degree",
                               "Master's Degree", "Doctorate or Professional Degree", "Unknown"),
                    ordered = is.ordered(1:9))


data$ageCat <- discretize(data$DAgeYrs,
                          method = "fixed",
                          breaks = c(1, 15, 25, 35, 45, 55, 65, 141),
                          right = FALSE,
                          labels = c("1-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                          )
#Fix incorrect age values
data <- data |> 
  mutate(DAgeYrs = ifelse(!is.na(DAgeYrs) & DAgeYrs > 140, NA, DAgeYrs))

#Rename Completed Dataset and export
shelby_data <- data
save(shelby_data, file = here("Data/shelbyMortality.RData"))

#Create Descriptive Table by Sex
tbl_summary(data = shelby_data,
            by = sex,
            include = c(sex, DDODYr, race_ethnicity, DAgeYrs, ageCat,
                        education,RankableUnderlyingDeathCause),
            label = list(
              DDODYr ~ "Year of Death",
              race_ethnicity ~ "Race and Ethnicity",
              DAgeYrs ~ "Age (Yrs)",
              ageCat ~ "Age (Grouped)",
              education ~ "Education",
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
                      gt::tab_header(title ="Table 1. Descriptive Statistics 2015-2019 in Shelby County By Sex")

#Create Descriptive Table by Death Year
tbl_summary(data = shelby_data,
            by = death_yr,
            include = c(death_yr, sex, race_ethnicity, DAgeYrs, ageCat,
                        education,RankableUnderlyingDeathCause),
            label = list(
              sex ~ "Sex",
              race_ethnicity ~ "Race and Ethnicity",
              DAgeYrs ~ "Age (Yrs)",
              ageCat ~ "Age (Grouped)",
              education ~ "Education",
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
            gt::tab_header(title ="Table 1. Causes of Death Between 2015-2019 in Shelby County By Year")

#Create Descriptive Table by Age Cat
tbl_summary(data = shelby_data,
            by = ageCat,
            include = c(death_yr, sex, race_ethnicity, DAgeYrs,
                        education,RankableUnderlyingDeathCause),
            label = list(
              death_yr ~ "Year of Death",
              sex ~ "Sex",
              race_ethnicity ~ "Race and Ethnicity",
              DAgeYrs ~ "Age (Yrs)",
              education ~ "Education",
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
  gt::tab_header(title ="Table 1. Causes of Death Between 2015-2019 in Shelby County By Age")


  


###################### THE EVERYTHING TABLE #############
create_stratified_table <- function(data) {
  data %>%
    tbl_summary(
      by = ageCat,
      include = c(sex, race_ethnicity, DAgeYrs, education, RankableUnderlyingDeathCause),
      label = list(
        sex ~ "Sex",
        race_ethnicity ~ "Race and Ethnicity",
        DAgeYrs ~ "Age (Yrs)",
        education ~ "Education",
        RankableUnderlyingDeathCause ~ "Cause of Death"
      ),
      sort = all_categorical() ~ "frequency",
      statistic = list(all_continuous() ~ "{mean} ({sd})"),
      missing_text = "Missing"
    ) %>%
    add_overall() %>%
    bold_labels() %>%
    modify_header(label = "**Variable**") %>%
    add_stat_label()
}

# Create and display the stratified table
stratified_table <- tbl_strata(
  data = shelby_data,
  strata = death_yr,
  .tbl_fun = create_stratified_table,
  .header = "**{strata}**"
) %>%
  as_gt() %>%
  gt::tab_header(title = "Table 2. Causes of Death Between 2015-2019 in Shelby County By Year and Age Category")

stratified_table






