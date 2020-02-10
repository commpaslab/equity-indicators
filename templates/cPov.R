# ....................................................
# Equity Metrics from American Community Survey 
# Hannah Lewis and Michele Claibourn
# Last updated: 6/20/2019
# ....................................................
# File Information ----
# Metric: Child Poverty, black and white 
# Based on: ACS 2013-2017
# Geography: General
# Description: Child Poverty "Equity Scores" per 
#   census tract of charlottesville
# ....................................................
# 1. Load libraries, provide api key (if needed), identify variables
# 2. Define variables, pull tables, derive estimates
# 3. Create equity table, save
# 4. Derive metrics and margin of errors
# 5. Add geography
# 6. Examine and save
# ....................................................


# 1. Load libraries, provide api key (if needed), identify variables ----

# setwd("C:/Users/hnlew/Desktop/Equity Project/Child Poverty")
# or create Rproject in working folder

# Load libraries
library(tidycensus)
library(tidyverse)
library(tigris)


# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variable view helper
acs_var <- load_variables(2017, "acs5", cache = TRUE)

# Variable of interest - Table used:
##  - ALL Child (below 18) Population - B01001 "Sex by Age" 
##  - WHITE child Population - B01001A "Sex by Age (WHITE ALONE)"
##  - BLACK child Population - B01001B "Sex by Age (BLACK or AFRICAN AMERICAN ALONE)"
##  - ALL Children (below 18) in poverty - B17001 "Poverty Status in the Past 12 Months by Sex by Age"
##  - WHITE Children in poverty - B17001A "Poverty Status in the Past 12 Months by Sex by Age (WHITE ALONE)"
##  - BLACK Children in poverty - B17001B "Poverty Status in the Past 12 Months by Sex by Age (BLACK or AFRICAN AMERICAN ALONE)"

# ...................................................
# 2. Define state, Census tract, ACS 5-year survey you wish to pull from
state <- "VA"
county <- "079"
year <- 2017

# ....................................................
# 3. Define variables, pull tables ----

## Pulling from table B01001 "Sex by Age" 
## to find the total number of children
variables <- c(maleto5 = 'B01001_003',
               male5to9 = 'B01001_004',
               male10to14 = 'B01001_005',
               male15to17 = 'B01001_006',
               
               femaleto5 = 'B01001_027',
               female5to9 = 'B01001_028',
               female10to14 = 'B01001_029',
               female15to17 = 'B01001_030'
)

pop_tract <- get_acs(geography = "tract", variables = variables, year = year,
                     state = state, county = county, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(pop = sum(estimate),
              pop_moe = moe_sum(moe, estimate, na.rm = TRUE))

pop_county <- get_acs(geography = "county", variables = variables, year = year,
                      state = state, county = county, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(pop = sum(estimate),
              pop_moe = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001H "Sex by Age (WHITE ALONE, NOT HISPANIC)" 
## to Find Total Number of White Children
variablesA <- c(maleto5 = 'B01001H_003',
                male5to9 = 'B01001H_004',
                male10to14 = 'B01001H_005',
                male15to17 = 'B01001H_006',
                
                femaleto5 = 'B01001H_018',
                female5to9 = 'B01001H_019',
                female10to14 = 'B01001H_020',
                female15to17 = 'B01001H_021'
)

pop_tract_A <- get_acs(geography = "tract", variables = variablesA, year = year,
                       state = state, county = county, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(pop_A = sum(estimate),
              pop_moe_A = moe_sum(moe, estimate, na.rm = TRUE))

pop_county_A <- get_acs(geography = "county", variables = variablesA, year = year,
                        state = state, county = county, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(pop_A = sum(estimate),
              pop_moe_A = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001B "Sex by Age (BLACK or AFRICAN AMERICAN ALONE)" 
## to Find Total Number of Black or African American Children
variablesB <- c(maleto5 = 'B01001B_003',
                male5to9 = 'B01001B_004',
                male10to14 = 'B01001B_005',
                male15to17 = 'B01001B_006',
                
                femaleto5 = 'B01001B_018',
                female5to9 = 'B01001B_019',
                female10to14 = 'B01001B_020',
                female15to17 = 'B01001B_021'
)

pop_tract_B <- get_acs(geography = "tract", variables = variablesB, year = year,
                       state = state, county = county, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(pop_B = sum(estimate),
              pop_moe_B = moe_sum(moe, estimate, na.rm = TRUE))

pop_county_B <- get_acs(geography = "county", variables = variablesB, year = year,
                        state = state, county = county, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(pop_B = sum(estimate),
              pop_moe_B = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001I "Sex by Age (HISPANIC or LATINO)" 
## to Find Total Number of Hispanic or Latino Children
variablesC <- c(maleto5 = 'B01001I_003',
                male5to9 = 'B01001I_004',
                male10to14 = 'B01001I_005',
                male15to17 = 'B01001I_006',
                
                femaleto5 = 'B01001I_018',
                female5to9 = 'B01001I_019',
                female10to14 = 'B01001I_020',
                female15to17 = 'B01001I_021'
)

pop_tract_C <- get_acs(geography = "tract", variables = variablesC, year = year,
                       state = state, county = county, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(pop_C = sum(estimate),
              pop_moe_C = moe_sum(moe, estimate, na.rm = TRUE))

pop_county_C <- get_acs(geography = "county", variables = variablesC, year = year,
                        state = state, county = county, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(pop_C = sum(estimate),
              pop_moe_C = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B17001 "Poverty Status in the Past 12 Months by Sex by Age" 
## to find the total number of children in poverty
variables <- c(maleto5 = 'B17001_004',
               male5 = 'B17001_005',
               male6to11 = 'B17001_006',
               male12to14 = 'B17001_007',
               male15 = 'B17001_008',
               male16to17 = 'B17001_009',
               
               femaleto5 = 'B17001_018',
               female5 = 'B17001_019',
               female6to11 = 'B17001_020',
               female12to14 = 'B17001_021',
               female15 = 'B17001_022',
               female16to17 = 'B17001_023'
)

cPov_tract <- get_acs(geography = "tract", variables = variables, year = year, 
                      state = state, county = county, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(cPov = sum(estimate),
              cPov_moe = moe_sum(moe, estimate, na.rm = TRUE))

cPov_county <- get_acs(geography = "county", variables = variables, year = year,
                       state = state, county = county, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(cPov = sum(estimate),
              cPov_moe = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B17001H "Poverty Status in the Past 12 Months by Sex by Age (WHITE ALONE, NOT HISPANIC)" 
## to find the total number of white children in poverty
variables_A <- c(maleto5 = 'B17001H_004',
                 male5 = 'B17001H_005',
                 male6to11 = 'B17001H_006',
                 male12to14 = 'B17001H_007',
                 male15 = 'B17001H_008',
                 male16to17 = 'B17001H_009',
                 
                 femaleto5 = 'B17001H_018',
                 female5 = 'B17001H_019',
                 female6to11 = 'B17001H_020',
                 female12to14 = 'B17001H_021',
                 female15 = 'B17001H_022',
                 female16to17 = 'B17001H_023'
)

cPov_tract_A <- get_acs(geography = "tract", variables = variables_A, year = year, 
                        state = state, county = county, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(cPov_A = sum(estimate),
              cPov_moe_A = moe_sum(moe, estimate, na.rm = TRUE))

cPov_county_A <- get_acs(geography = "county", variables = variables_A, year = year,
                         state = state, county = county, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(cPov_A = sum(estimate),
              cPov_moe_A = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B17001B "Poverty Status in the Past 12 Months by Sex by Age (BLACK or AFRICAN AMERICAN ALONE)" 
## to find the total number of black or african american children in poverty
variables_B <- c(maleto5 = 'B17001B_004',
                 male5 = 'B17001B_005',
                 male6to11 = 'B17001B_006',
                 male12to14 = 'B17001B_007',
                 male15 = 'B17001B_008',
                 male16to17 = 'B17001B_009',
                 
                 femaleto5 = 'B17001B_018',
                 female5 = 'B17001B_019',
                 female6to11 = 'B17001B_020',
                 female12to14 = 'B17001B_021',
                 female15 = 'B17001B_022',
                 female16to17 = 'B17001B_023'
)

cPov_tract_B <- get_acs(geography = "tract", variables = variables_B, year = year, 
                        state = state, county = county, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(cPov_B = sum(estimate),
              cPov_moe_B = moe_sum(moe, estimate, na.rm = TRUE))

cPov_county_B <- get_acs(geography = "county", variables = variables_B, year = year,
                         state = state, county = county, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(cPov_B = sum(estimate),
              cPov_moe_B = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B17001I "Poverty Status in the Past 12 Months by Sex by Age (HISPANIC or LATINO)" 
## to find the total number of Hispanic or Latino children in poverty
variables_C <- c(maleto5 = 'B17001I_004',
                 male5 = 'B17001I_005',
                 male6to11 = 'B17001I_006',
                 male12to14 = 'B17001I_007',
                 male15 = 'B17001I_008',
                 male16to17 = 'B17001I_009',
                 
                 femaleto5 = 'B17001I_018',
                 female5 = 'B17001I_019',
                 female6to11 = 'B17001I_020',
                 female12to14 = 'B17001I_021',
                 female15 = 'B17001I_022',
                 female16to17 = 'B17001I_023'
)

cPov_tract_C <- get_acs(geography = "tract", variables = variables_C, year = year, 
                        state = state, county = county, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(cPov_C = sum(estimate),
              cPov_moe_C = moe_sum(moe, estimate, na.rm = TRUE))

cPov_county_C <- get_acs(geography = "county", variables = variables_C, year = year,
                         state = state, county = county, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(cPov_C = sum(estimate),
              cPov_moe_C = moe_sum(moe, estimate, na.rm = TRUE))


# ....................................................
# 4. Create equity table ----
# join columns
equityTable_cPov_tract <- left_join(pop_tract, pop_tract_A) %>%
    left_join(pop_tract_B) %>%
    left_join(pop_tract_C) %>% 
    left_join(cPov_tract) %>%
    left_join(cPov_tract_A) %>%
    left_join(cPov_tract_B) %>% 
    left_join(cPov_tract_C)

equityTable_cPov_county <- left_join(pop_county, pop_county_A) %>%
    left_join(pop_county_B) %>%
    left_join(pop_county_C) %>% 
    left_join(cPov_county) %>%
    left_join(cPov_county_A) %>%
    left_join(cPov_county_B) %>% 
    left_join(cPov_county_C)

# bind total row to table
equityTable_cPov <- bind_rows(equityTable_cPov_tract, equityTable_cPov_county)


# ....................................................
# 5. Derive metrics and margin of errors ----
# Percent of children in poverty
equityTable_cPov <- equityTable_cPov %>% 
    mutate(Per = (cPov/pop)*100,
           Per_moe = (moe_prop(cPov, pop, cPov_moe, pop_moe))*100)

# Percent of children in poverty - white
equityTable_cPov <- equityTable_cPov %>% 
    mutate(Per_A = (cPov_A/pop_A)*100,
           Per_moe_A = (moe_prop(cPov_A, pop_A, cPov_moe_A, pop_moe_A))*100)

# Percent of children in poverty - black
equityTable_cPov <- equityTable_cPov %>% 
    mutate(Per_B = (cPov_B/pop_B)*100,
           Per_moe_B = (moe_prop(cPov_B, pop_B, cPov_moe_B, pop_moe_B))*100)

# Percent of children in poverty - Hispanic
equityTable_cPov <- equityTable_cPov %>% 
    mutate(Per_C = (cPov_C/pop_C)*100,
           Per_moe_C = (moe_prop(cPov_C, pop_C, cPov_moe_C, pop_moe_C))*100)

# Ratio of black children in poverty to white children in poverty
equityTable_cPov <- equityTable_cPov %>% 
    mutate(Ratio_B = Per_B/Per_A,
           Ratio_B_moe = moe_ratio(Per_B, Per_A, Per_moe_B, Per_moe_A))

# Ratio of Hispanic children in poverty to white children in poverty
equityTable_cPov <- equityTable_cPov %>% 
    mutate(Ratio_C = Per_C/Per_A,
           Ratio_C_moe = moe_ratio(Per_C, Per_A, Per_moe_C, Per_moe_A))

# Difference in percent of children in poverty - black-white
equityTable_cPov <- equityTable_cPov %>% 
    mutate(Diff_B = Per_B - Per_A,
           Diff_B_moe = sqrt(Per_moe_B^2 + Per_moe_A^2))

# Difference in percent of children in poverty - Hispanic-white
equityTable_cPov <- equityTable_cPov %>% 
    mutate(Diff_C = Per_C - Per_A,
           Diff_C_moe = sqrt(Per_moe_C^2 + Per_moe_A^2))

# Final formatting
var <- c("pop", "pop_A", "pop_B", "pop_C", "cPov", "cPov_A", "cPov_B", "cPov_C")
equityTable_cPov <- equityTable_cPov %>% 
    mutate_at(var, as.integer) %>% 
    ungroup()


# ....................................................
# 6. Add geography ----
# get current cville tracts
geo <- tracts(state = 'VA', county = county)

# join data to tracts
equityTable_cPov_geo <- geo_join(geo, equityTable_cPov, by = "GEOID")


# ....................................................
# 7. Save ----

# create file name
# make descriptive filename: [indicator abbreviation]_[geography code]_[geography level].RDS

filename <- paste("cPov_",county,"_tract_", year,".RDS", sep = "")
filename_geo <- paste("cPov_",county,"_tract_", year,"_geo",".RDS", sep = "")

# save equityTable and equityTable_geo
saveRDS(equityTable_cPov, file = filename) 
saveRDS(equityTable_cPov_geo, file = filename_geo) 

