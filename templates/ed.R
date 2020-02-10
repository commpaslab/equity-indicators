# ....................................................
# Equity Metrics from American Community Survey 
# Hannah Lewis and Michele Claibourn
# Last updated: 6/26/2019
# ....................................................
# File Information ----
# Metric: Educational Attainment, white alone, black or african american, hispanic or latino
# Based on: ACS 2013-2017
# Description: Educational Attainment "Equity Scores" per 
#   census tract
# ....................................................
# 1. Load libraries, provide api key (if needed), identify variables
# 2. Define variables, pull tables, derive estimates
# 3. Create equity table, save
# 4. Derive metrics and margin of errors
# 5. Add geography
# 6. Examine and save
# ....................................................

# 1. Load libraries, provide api key (if needed), identify variables ----

# setwd("C:/Users/hnlew/Desktop/Equity Project")
# or create Rproject in working folder

# Load libraries
library(tidycensus)
library(tidyverse)
library(tigris)

library(conflicted)
library(dplyr)
select <- dplyr::select

#import function to find variables specific to education level
source("find.variables.r") 

# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variable view helper
acs_var <- load_variables(2017, "acs5", cache = TRUE)

# Census tables of interest
#  - ALL adult (above 25) educational attainment - B15002
#  - WHITE adult educational attainment - C15002H
#  - BLACK adult educational attainment - C15002B
#  - HISPANIC adult educational attainment - C15002I

# ...................................................
# 2. Define state, Census tract, ACS 5-year survey you wish to pull from, and the degree level you are interested in
# where level = 
#       HS: High School degree (or equivalent) or higher
#       AS: Associates degree or higher
#       BA: Bachelors Degree or higher

state <- "VA"
county <- "003"
year <- 2017
level <- 'AS' 

# ....................................................
# 3. Define variables, pull tables ----

## Pulling Population -------------------------------

## Pulling from the table B15002 "Sex by Educational Attainment for the Population 25 years and over"
## The table includes a "total estimate" of the population
pop_tract <- get_acs(geography= "tract", variables = "B15002_001",
                     state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable)
names(pop_tract) <- c("GEOID", "NAME", "pop", "pop_moe")

pop_county <- get_acs(geography= "county", variables = "B15002_001",
                      state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable)
names(pop_county) <- c("GEOID", "NAME", "pop", "pop_moe")

## Pulling from the table C15002H "Sex by Educational Attainment for the population 25 years and older (WHITE ALONE, NOT HISPANIC OR LATINO)" 
## The table includes a "total estimate" of the population
pop_tract_H <- get_acs(geography= "tract", variables = "C15002H_001",
                     state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable)
names(pop_tract_H) <- c("GEOID", "NAME", "pop_H", "pop_moe_H")

pop_county_H <- get_acs(geography= "county", variables = "C15002H_001",
                      state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable)
names(pop_county_H) <- c("GEOID", "NAME", "pop_H", "pop_moe_H")

## Pulling from the table C15002B "Sex by Educational Attainment for the population 25 years and older (BLACK OR AFRICAN AMERICAN ALONE)" 
## The table includes a "total estimate" of the population
pop_tract_B <- get_acs(geography= "tract", variables = "C15002B_001",
                       state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable)
names(pop_tract_B) <- c("GEOID", "NAME", "pop_B", "pop_moe_B")

pop_county_B <- get_acs(geography= "county", variables = "C15002B_001",
                        state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable)
names(pop_county_B) <- c("GEOID", "NAME", "pop_B", "pop_moe_B")

## Pulling from the table C15002I "Sex by Educational Attainment for the population 25 years and older (HISPANIC OR LATINO)" 
## The table includes a "total estimate" of the population
pop_tract_I <- get_acs(geography= "tract", variables = "C15002I_001",
                       state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable)
names(pop_tract_I) <- c("GEOID", "NAME", "pop_I", "pop_moe_I")

pop_county_I <- get_acs(geography= "county", variables = "C15002I_001",
                        state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable)
names(pop_county_I) <- c("GEOID", "NAME", "pop_I", "pop_moe_I")

#--------------------------------------------------------------
## Use the function find.variables to find the total number of adults (25 and older) with chosen 
## level of educational attainment ('HS', 'AS', 'BA') or higher

## Total
ed_tract <- get_acs(geography = "tract", variables = find.variables(level)$T, 
                     state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(ed = sum(estimate),
              ed_moe = moe_sum(moe, estimate))

ed_county <- get_acs(geography = "county", find.variables(level)$T,
                      state = state, county = county, year = year, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(ed = sum(estimate),
              ed_moe = moe_sum(moe, estimate))

## WHITE ALONE, NOT HISPANIC OR LATINO
ed_tract_H <- get_acs(geography = "tract", variables = find.variables(level)$H, 
                      state = state, county = county, year = year, survey = "acs5") %>%
    select(-variable) %>%
    group_by(GEOID, NAME) %>%
    summarize(ed_H = sum(estimate),
              ed_moe_H = moe_sum(moe, estimate))

ed_county_H <- get_acs(geography = "county", variables = find.variables(level)$H,
                       state = state, county = county, year = year, survey = "acs5") %>% 
    select(-variable) %>% 
    group_by(GEOID, NAME) %>%
    summarize(ed_H = sum(estimate),
              ed_moe_H = moe_sum(moe, estimate))


## BLACK OR AFRICAN AMERICAN ALONE
ed_tract_B <- get_acs(geography = "tract", variables = find.variables(level)$B, 
                                                    state = state, county = county, year = year, survey = "acs5") %>%
           select(-variable) %>%
           group_by(GEOID, NAME) %>%
           summarize(ed_B = sum(estimate),
                     ed_moe_B = moe_sum(moe, estimate))

ed_county_B <- get_acs(geography = "county", variables = find.variables(level)$B,
                                                   state = state, county = county, year = year, survey = "acs5") %>% 
           select(-variable) %>% 
           group_by(GEOID, NAME) %>%
           summarize(ed_B = sum(estimate),
                     ed_moe_B = moe_sum(moe, estimate))

## HISPANIC OR LATINO
ed_tract_I <- get_acs(geography = "tract", variables = find.variables(level)$I, 
                                                    state = state, county = county, year = year, survey = "acs5") %>%
           select(-variable) %>%
           group_by(GEOID, NAME) %>%
           summarize(ed_I = sum(estimate),
                     ed_moe_I = moe_sum(moe, estimate))

ed_county_I <- get_acs(geography = "county", variables = find.variables(level)$I,
                                                     state = state, county = county, year = year, survey = "acs5") %>% 
           select(-variable) %>% 
           group_by(GEOID, NAME) %>%
           summarize(ed_I = sum(estimate),
                     ed_moe_I = moe_sum(moe, estimate))


# ....................................................
# 3. Create equity table ----
# joing columns
equityTable_tract <- left_join(pop_tract, pop_tract_H) %>%
    left_join(pop_tract_B) %>%
    left_join(pop_tract_I) %>%
    left_join(ed_tract) %>%
    left_join(ed_tract_H) %>%
    left_join(ed_tract_B) %>%
    left_join(ed_tract_I)

equityTable_county <- left_join(pop_county, pop_county_H) %>%
    left_join(pop_county_B)%>%
    left_join(pop_county_I) %>%
    left_join(ed_county) %>%
    left_join(ed_county_H) %>%
    left_join(ed_county_B) %>%
    left_join(ed_county_I)

# bind total row to table
equityTable_ed <- bind_rows(equityTable_tract, equityTable_county)


# ....................................................
# 4. Derive metrics and margin of errors ----
# Percent of adults (25+) with chosen education level or higher
equityTable_ed <- equityTable_ed %>% 
    mutate(Per = (ed/pop)*100,
           Per_moe = (moe_prop(ed, pop, ed_moe, pop_moe))*100)

# Percent of adults (25+) with chosen education level or higher - white
equityTable_ed <- equityTable_ed %>%  
    mutate(Per_H = (ed_H/pop_H)*100,
           Per_moe_H = (moe_prop(ed_H, pop_H, ed_moe_H, pop_moe_H))*100)

# Percent of adults (25+) with chosen education level or higher - black
equityTable_ed <- equityTable_ed %>%  
    mutate(Per_B = (ed_B/pop_B)*100,
           Per_moe_B = (moe_prop(ed_B, pop_B, ed_moe_B, pop_moe_B))*100)

# Percent of adults (25+) with chosen education level or higher - hispanic
equityTable_ed <- equityTable_ed %>%  
    mutate(Per_I = (ed_I/pop_I)*100,
           Per_moe_I = (moe_prop(ed_I, pop_I, ed_moe_I, pop_moe_I))*100)

# Ratio of black adults (25+) with chosen education level or higher to white children with chosen education level or higher
equityTable_ed <- equityTable_ed %>% 
    mutate(Ratio_B = Per_B/Per_H,
           Ratio_B_moe = moe_ratio(Per_B, Per_H, Per_moe_B, Per_moe_H))

# Ratio of Hispanic adults (25+) with chosen education level or higher to white children with chosen education level or higher
equityTable_ed <- equityTable_ed %>% 
    mutate(Ratio_I = Per_I/Per_H,
           Ratio_I_moe = moe_ratio(Per_I, Per_H, Per_moe_I, Per_moe_H))

# Difference in percent of adults who have achived chosen education level or higher - black-white
equityTable_ed <- equityTable_ed %>%  
    mutate(Diff_B = Per_B - Per_H,
           Diff_B_moe = sqrt(Per_moe_B^2 + Per_moe_H^2))

# Difference in percent of adults who have achived chosen education level or higher - hispanic-white
equityTable_ed <- equityTable_ed %>%  
    mutate(Diff_I = Per_I - Per_H,
           Diff_I_moe = sqrt(Per_moe_I^2 + Per_moe_H^2))

# Final formatting
var <- c("pop", "pop_H", "pop_B","pop_I", "ed", "ed_H", "ed_B", "ed_I")
equityTable_ed <- equityTable_ed %>% 
    mutate_at(var, as.integer) %>% 
    ungroup()

# ....................................................
# 5. Add geography ----
# get current cville tracts
geo <- tracts(state = 'VA', county = county)

# join data to tracts
equityTable_ed_geo <- geo_join(geo, equityTable_ed, by = "GEOID")

# ....................................................
# 6. Save ----

# create file name
# make descriptive filename: [education level]_[indicator abbreviation]_[geography code]_[geography level].RDS

filename <- paste(level, "ed_",county,"_tract_", year,".RDS", sep = "")
filename_geo <- paste(level,"ed_",county,"_tract_", year,"_geo",".RDS", sep = "")

# save equityTable and equityTable_geo
saveRDS(equityTable_ed, file = filename) 
saveRDS(equityTable_ed_geo, file = filename_geo) 
