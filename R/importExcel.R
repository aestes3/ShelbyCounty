library(gtsummary)
library(gt)
library(here)
library(tidyverse)
library(readxl)
library(arules)
library(DT)
library(rvest)

#Load Shelby Top 10
shelby_t10 <- read_xlsx(here("Raw Data/Multiple Cause of Death, 1999-2020.xlsx"),
                        sheet = "Top 10")

save(shelby_t10, file = here("Data/shelby_t10.RData"))

#All Tennessee Data 2015-2019
#####
tenn2015_2019 <- read_xlsx(path = here("Raw Data/Tennesse Leading Cause of Death.xlsx"),
                           sheet = "2015-2019")

tenn2015 <- read_xlsx(path = here("Raw Data/Tennesse Leading Cause of Death.xlsx"),
                      sheet = "2015")

tenn2016 <- read_xlsx(path = here("Raw Data/Tennesse Leading Cause of Death.xlsx"),
                      sheet = "2016")

tenn2017 <- read_xlsx(path = here("Raw Data/Tennesse Leading Cause of Death.xlsx"),
                      sheet = "2017")

tenn2018 <- read_xlsx(path = here("Raw Data/Tennesse Leading Cause of Death.xlsx"),
                      sheet = "2018")

tenn2019 <- read_xlsx(path = here("Raw Data/Tennesse Leading Cause of Death.xlsx"),
                      sheet = "2019")
#Export all
save(tenn2015_2019, file = here("Data/tenn2015-2019.RData"))
save(tenn2015, file = here("Data/tenn2015.RData"))
save(tenn2016, file = here("Data/tenn2016.RData"))
save(tenn2017, file = here("Data/tenn2017.RData"))
save(tenn2018, file = here("Data/tenn2018.RData"))
save(tenn2019, file = here("Data/tenn2019.RData"))
#####

#All US Data 2015-2019
#####
us2015_2019 <- read_xlsx(path = here("Raw Data/US All Cause Mortality 2015-2019.xlsx"),
                           sheet = "2015-2019")

us2015 <- read_xlsx(path = here("Raw Data/US All Cause Mortality 2015-2019.xlsx"),
                      sheet = "2015")

us2016 <- read_xlsx(path = here("Raw Data/US All Cause Mortality 2015-2019.xlsx"),
                      sheet = "2016")

us2017 <- read_xlsx(path = here("Raw Data/US All Cause Mortality 2015-2019.xlsx"),
                      sheet = "2017")

us2018 <- read_xlsx(path = here("Raw Data/US All Cause Mortality 2015-2019.xlsx"),
                      sheet = "2018")

us2019 <- read_xlsx(path = here("Raw Data/US All Cause Mortality 2015-2019.xlsx"),
                      sheet = "2019")

#Export all
save(us2015_2019, file = here("Data/us2015_2019.RData"))
save(us2015, file = here("Data/us2015.RData"))
save(us2016, file = here("Data/us2016.RData"))
save(us2017, file = here("Data/us2017.RData"))
save(us2018, file = here("Data/us2018.RData"))
save(us2019, file = here("Data/us2019.RData"))


#Load All Combined
allCause <- read_xlsx(here("Raw Data/Multiple Cause of Death, 1999-2020.xlsx"),
                      sheet = "All Cause All ")

save(allCause, file = here("Data/allCause.RData"))

allHeart <- read_xlsx(here("Raw Data/Multiple Cause of Death, 1999-2020.xlsx"),
                     sheet = "All Heart")

save(allHeart, file = here("Data/allHeart.RData"))
