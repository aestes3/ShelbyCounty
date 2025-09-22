library(gtsummary)
library(gt)
library(here)
library(tidyverse)
library(readxl)
library(arules)
library(DT)
library(rvest)

#Load Shelby County Data
#####

# 2015
load(here("Data/yr_2015_t10.RData"))
#2016
load(here("Data/yr_2016_t10.RData"))
#2017
load(here("Data/yr_2017_t10.RData"))
#2018
load(here("Data/yr_2018_t10.RData"))
#2019
load(here("Data/yr_2019_t10.RData"))

#Load Top 10 
load(here("Data/shelby_t10.RData"))

#####
#Load Tennessee State Data
#####

#2015-2019
load(here("Data/tenn2015-2019.RData"))
# 2015
load(here("Data/tenn2015.RData"))
#2016
load(here("Data/tenn2016.RData"))
#2017
load(here("Data/tenn2017.RData"))
#2018
load(here("Data/tenn2018.RData"))
#2019
load(here("Data/tenn2019.RData"))

#####
#Load US Data
####

#2015-2019
load(here("Data/us2015_2019.RData"))
# 2015
load(here("Data/us2015.RData"))
#2016
load(here("Data/us2016.RData"))
#2017
load(here("Data/us2017.RData"))
#2018
load(here("Data/us2018.RData"))
#2019
load(here("Data/us2019.RData"))

#####

#Load All Combined
load(here("Data/allCause.RData"))

#Load All Heart
load(here("Data/allHeart.RData"))

##  Start Plot Crude VS Adjusted All Years Overall(Figure 1. Shelby County)
shelby_t10Long <- shelby_t10 |> 
  pivot_longer(
    cols = c(`Top 10 Crude`, `Top 10  Adjusted`),
    names_to = "Rate_Type",
    values_to = "Rate"
  )
 

fig1_shelby <- ggplot(shelby_t10Long, 
                      aes(x = Years, y = Rate, color = Rate_Type, group = Rate_Type)) +
                      geom_line(linewidth = 3) +
                      geom_point() +
                      labs(
                      title = "Top 10 Mortality Rates Shelby County (2015-2019)",
                      tag = "Figure 1.",
                      x = "Year",
                      y = "Rate per 100,000",
                      color = "Rate Type"
                      ) +
                      theme_classic() +
                      ylim(400, 900)
fig1_shelby

#Filter Data to get crude and adjusted
#####
shelby_adjCrude <- yr_2015_t10 |> 
                                select(`**Variable**`, `dCrude_2015`,`adjRate_2015`)

#Filter 2016
yr_2016_t10 <- yr_2016_t10 |> 
                              select(`**Variable**`, `dCrude_2016`,`adjRate_2016`)
# Full_join (Outer) 2016
shelby_adjCrude <- full_join(shelby_adjCrude, yr_2016_t10, by="**Variable**")

#Filter 2017
yr_2017_t10 <- yr_2017_t10 |> 
                            select(`**Variable**`, `dCrude_2017`,`adjRate_2017`)
  
# Full_join (Outer) 2017
shelby_adjCrude <- full_join(shelby_adjCrude, yr_2017_t10, by="**Variable**")

#Filter 2018
yr_2018_t10 <- yr_2018_t10 |> 
                            select(`**Variable**`, `dCrude_2018`,`adjRate_2018`)
  
# Full_join (Outer) 2018
shelby_adjCrude <- full_join(shelby_adjCrude, yr_2018_t10, by="**Variable**")

#Filter 2019
yr_2019_t10 <- yr_2019_t10 |> 
  select(`**Variable**`, `dCrude_2019`,`adjRate_2019`)

# Full_join (Outer) 2019
shelby_adjCrude <- full_join(shelby_adjCrude, yr_2019_t10, by="**Variable**")

#Fix Column types
shelby_adjCrude$adjRate_2015 <- as.numeric(shelby_adjCrude$adjRate_2015)
shelby_adjCrude$adjRate_2016 <- as.numeric(shelby_adjCrude$adjRate_2016)
shelby_adjCrude$adjRate_2017 <- as.numeric(shelby_adjCrude$adjRate_2017)
shelby_adjCrude$adjRate_2018 <- as.numeric(shelby_adjCrude$adjRate_2018)
shelby_adjCrude$adjRate_2019 <- as.numeric(shelby_adjCrude$adjRate_2019)


save(shelby_adjCrude, file = here("Data/shelby_adjCrude.RData"))
writexl::write_xlsx(shelby_adjCrude, path = here("Raw Data/shelby_adjCrude.xlsx"))

#####

#Filter Tennessee to get  crude/adjusted
#####

#Standardize names
tennessee_adjCrude <- tenn2015 |> 
  select(`113 Cause Name`, `dCrude_2015`,`adjRate_2015`)

#Filter 2016
tenn2016 <- tenn2016 |> 
  select(`113 Cause Name`,`dCrude_2016`,`adjRate_2016`)

# Full_join (Outer) 2016
tennessee_adjCrude <- full_join(tennessee_adjCrude, tenn2016, by="113 Cause Name")

#Filter 2017
tenn2017 <- tenn2017 |> 
  select(`113 Cause Name`, `dCrude_2017`,`adjRate_2017`)

# Full_join (Outer) 2017
tennessee_adjCrude <- full_join(tennessee_adjCrude, tenn2017, by="113 Cause Name")

#Filter 2018
tenn2018 <- tenn2018 |> 
  select(`113 Cause Name`, `dCrude_2018`,`adjRate_2018`)

# Full_join (Outer) 2018
tennessee_adjCrude <- full_join(tennessee_adjCrude, tenn2018, by="113 Cause Name")

#Filter 2019
tenn2019 <- tenn2019 |> 
  select(`113 Cause Name`, `dCrude_2019`,`adjRate_2019`)

# Full_join (Outer) 2019
tennessee_adjCrude <- full_join(tennessee_adjCrude, tenn2019, by="113 Cause Name")

save(tennessee_adjCrude, file = here("Data/tennessee_adjCrude.RData"))

writexl::write_xlsx(tennessee_adjCrude, path = here("Raw Data/tennessee_adjCrude.xlsx"))

#####




#Change format to long
shelby_adjLong <- shelby_adjCrude |> 
  pivot_longer(
    cols = -c(1,2),
    names_to = "key",
    values_to = "rate") |> 
  mutate(
    year = as.integer(sub(".*_(\\d+)", "\\1", key)),  # Extract year
    rate_type = ifelse(grepl("^dCrude", key), "Crude", "Adjusted")  # Identify rate type
  ) |> 
  select(`**Variable**`, year, rate_type, rate) |> 
  na.omit()  # Remove NA values

shelby_adjLong <- shelby_adjLong |> 
  mutate(year = as.factor(year))



#Figure 2. Age Adjusted By Disease and Year
fig2_shelby <- ggplot(shelby_adjLong, aes(x = rate, y = fct_reorder(`**Variable**`, rate, .fun = max, .desc = TRUE), 
                      fill = year)) +
  geom_col(position = "dodge") +
  scale_x_continuous(limits = c(0, 220), breaks = seq(0, 220, by = 20)) +
  labs(
    title = "Age-adjusted death rates for leading causes of death: 2015-2019",
    tag = "Figure 2.",
    caption = "Note: Influenza is due to merging",
    x = "Deaths per 100,000 U.S. standard population",
    y = "",
    fill = "year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2")
fig2_shelby

#Plot Individual Diseases (Some Broken)
# fig3_shelby <- ggplot(shelby_adjLong, aes(x = year, y = rate, color = rate_type, group = rate_type)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   facet_wrap(~ `**Variable**`, scales = "free_y") +  # Free y-scale for clarity
#   labs(
#     title = "Crude vs. Adjusted Mortality Rates (2015-2019)",
#     x = "Year",
#     y = "Mortality Rate (per 100,000)",
#     color = "Rate Type"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Compare all top 10 adjusted
fig3_all <- ggplot(allCause, aes(x = Years, y = Rate, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Adds 10% padding at the top only
  labs(
    title = "Top 10 Causes Age Adjusted Mortality Rates among All (2015-2019)",
    tag = "Figure 3.",
    x = "Year",
    y = "Rate per 100,000"
  ) +
  theme_minimal()

fig3_all


#Compare all Heart Age-adjusted
fig4_allHeart <- ggplot(allHeart, aes(x = Years, y = Rate, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Adds 10% padding at the top only
  labs(
    title = "Heart Disease Age Adjusted Mortality Rates among All (2015-2019)",
    tag = "Figure 4.",
    x = "Year",
    y = "Rate per 100,000"
  ) +
  theme_minimal()

fig4_allHeart

