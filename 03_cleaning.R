
##############################################################################@

# Author: Carolin Kroeger
# Github: ckroeger95

# Date: March 2023

# 03_cleaning: Cleans the data sets and combine them into a final data set

##############################################################################@
# Set up  --------------------------------------------------------------------
##############################################################################@

#### Goal Section 1 Set up 
# This section prepares the data for the main analysis. It loads libraries and
# data sets, cleans and mutates relevant variables, and adds lagged effects. 

##############################################################################@
 #### 1 | Libraries & data ---------------------------------------------------
##############################################################################@

# Load libraries

library(sf)
library(countrycode)
library(tidyverse)
library(stringdist)
library(stringi)
library(broom.mixed)
library(raster)

# Load FAO & joint weather data https://climate.copernicus.eu/ESOTC/2019/about-data-and-analysis#UTCI 
gallup <- 
  read_csv('data/gallup/gallup.csv', show_col_types = FALSE) %>% 
  mutate(ISO_3 = countrycode(NAME_0, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(NAME_0=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(NAME_0=='Northern Cyprus', 'CYP2', ISO_3))

# Moderator WDI 
wdi.data <- 
  read_csv('data/wdi/wdi_data_final.csv', show_col_types= FALSE) %>% 
  mutate(ISO_3 = countrycode(country_name, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(country_name=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(country_name=='Northern Cyprus', 'CYP2', ISO_3)) %>% 
  dplyr::select(-country_name, -country_code)

# Moderator GNI 
gni.data <- 
  read_csv('data/wdi/gni.csv', show_col_types = FALSE) %>%  
  rename(NAME_0 = `Country Name`, 
         series_name = `Series Name`, 
         `2014` = `2014 [YR2014]`, 
         `2015` = `2015 [YR2015]`,
         `2016` = `2016 [YR2016]`,
         `2017` = `2017 [YR2017]`) %>% 
  dplyr::select(NAME_0, series_name, `2014`, `2015`, `2016`, `2017`) %>% 
  pivot_longer(cols=c(`2014`, `2015`, `2016`, `2017`)) %>% 
  mutate(ISO_3 = countrycode(NAME_0, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(NAME_0=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(NAME_0=='Northern Cyprus', 'CYP2', ISO_3), 
         value = ifelse(value == '..', NA, value), 
         series_name = ifelse(series_name == 'GNI per capita, Atlas method (current US$)', 'gni', NA)) %>% 
  rename(gni = value, 
         year = name) %>% 
  mutate(gni = as.numeric(gni)) %>% 
  dplyr::select(ISO_3, year, gni) %>% 
  group_by(ISO_3) %>% 
  mutate(gni = ifelse(is.na(gni), mean(gni, na.rm=T), gni))

# Load map at country level (layer 0)
countries <- 
  read_sf(dsn='data/map', layer='gadm36_0') %>% 
  mutate(ISO_3 = countrycode(NAME_0, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(NAME_0=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(NAME_0=='Northern Cyprus', 'CYP2', ISO_3)) %>% 
  filter(NAME_0 != 'Antarctica')

# Merge all data sets together
ses_data <-  merge(gallup, wdi.data, by=c('year', 'ISO_3'), all.x=T)
ses_data <-  merge(ses_data, gni.data, by=c('year', 'ISO_3'), all.x=T)

##############################################################################@
#### 2 | Cleaning -------------------------------------------------------------
##############################################################################@

# Rename variables  
ses_data <-
  ses_data %>% 
  rename(
    
    # Moderators 
    gdp = `GDP per capita, PPP (constant 2017 international $)`, 
    gni = gni, 
    ag.employ = `Employment in agriculture (% of total employment) (modeled ILO estimate)`, 
    formality = `Firms formally registered when operations started (% of firms)`, 
    vulnerable.employment =  `Vulnerable employment, total (% of total employment) (modeled ILO estimate)`, 
    wage.labour = `Wage and salaried workers, total (% of total employment) (modeled ILO estimate)`, 
    urbanicity = `Urban population (% of total population)`, 
    
    # Individual variables 
    children = WP1230Recoded, # 1 = children, 0 = None
    age = WP1220, 
    gender = WP1219,     # Gender / WP1219: 0 = Female, 1 = Male
    partner = WP1223,     # Urban / Rural: WP7572: 0=Rural, 1= Urban
    money_for_food = WP40,             # WP40: not having enough money for food 
    money_for_shelter = WP43,          # WP 43: not having enough money for shelter
    feeling_HH_income = WP2319,        # WP2319: Feelings about HH income
    local_job_market = WP89,           # WP89: Local job market: 1 = Bad time, 0 = good time
    employment_7days = EMP_2010,       # EMP_2010: Employment: Do heat days predict whether people had employment in last 7 days? (1=wanted more employment)
    health_problems = WP23,            # WP23: Health Problems
    health_near_perfect = WP14449,     # WP14449: physical health near perfect
    felt_active_productive = WP14448,  # WP14448: felt active and productive (1= yes, they felt active and productive)
    pain_yesterday = WP68              # WP68: Physical pain yesterday
    )


# Mutate new variables 

ses_data <-  
  ses_data %>% 
  mutate(
    
    # Food insecurity measures
    severe_fi = ifelse(fi_index >= 7, 1, 0), 
    mild_fi = ifelse(fi_index >= 1, 1, 0), 
    
    # area 
    WP14 = stri_trans_general(WP14, 'Latin-ASCII'),
    area = stri_trans_general(WP14, 'Latin-ASCII'),
    area = replace(area, area == 'A rural area or on a farm', 'Rural'), 
    area = replace(area, area == 'A small town or village', 'Rural'),
    area = replace(area, area == 'A large city', 'Urban'),
    area = replace(area, area == 'A suburb of a large city', 'Urban'),
    area = as.factor(area), 
    
    # Encoding & Numeric 
    gni = as.numeric(gni), 
    gender = as.factor(gender), 
    partner = as.factor(partner), 
    children = as.factor(children), 
    year = as.factor(year),
    
    # Logarithms 
    log_gni = log(gni),
    log_gdp = log(gdp), 
    log_income_2 = log(income_2), 
    
    # Continent
    continent = countrycode(country.mix, origin='country.name', destination='continent'), 
    continent = ifelse(country.mix=='Kosovo', 'Europe', continent) )

# Mutate grouped variables

ses_data <-  
  ses_data %>% 
  group_by(country.mix, year) %>% 
  mutate(
    income_decile = ntile(income_2, 10), 
    income_quartile = ntile(income_2, 4) ) 

# Share of poor interviews per day
ses_data <-  
  ses_data %>% 
  group_by(FAO.address, WP4, year) %>% 
  mutate(li.interviews = sum(income_quartile==1), 
         total.interviews = n(),
         share.li.interviews = li.interviews/total.interviews)

ses_data <- do.call(data.frame,                     
                lapply(ses_data,
                       function(x) replace(x, is.infinite(x), NA)))

##############################################################################@
#### 3 | Weather data --------------------------------------------
##############################################################################@

utci <- read_csv('data/utci/utci.csv', show_col_types = F)

# Add the FAO.state, NAME_1, and area_sqkm to the utci data frame 
utci <- merge(utci, 
              ses_data %>% dplyr::select(FAO.address, FAO.state, NAME_0, NAME_1, area_sqkm) %>% unique(), 
              by='FAO.address', all.x=T)

df_lag <- 
  
  utci %>% 
  
  group_by(FAO.address) %>% 
  complete(WP4 = seq.Date(min(WP4), max(WP4), by='day')) %>% #
  arrange(WP4) %>% 
      mutate(
             mean_utci_mix = ifelse(area_sqkm > summary(area_sqkm)[3] & stringdist(NAME_1, FAO.state)<2, 
                                        mean_utci_cross, mean_utci_exact), 
             year = format(WP4, '%Y'), 
             month = format(WP4, '%m'), 
             utci.2bin = cut(mean_utci_mix, breaks=seq(from=-28, to=42, by=2)),
             utci.4bin = cut(mean_utci_mix, breaks=seq(from=-26, to=42, by=4)) ) %>% 
  ungroup() %>% 
  
  group_by(FAO.address, year) %>% 
      mutate(
            percentile = ntile(mean_utci_mix, 100), 
            threshold_10P = mean( mean_utci_mix[percentile==90], na.rm=T), 
            threshold_05P = mean( mean_utci_mix[percentile==95], na.rm=T),
            utci.avg.state = mean(mean_utci_mix, na.rm=T), 
            relative.season = ntile(utci.avg.state, 4), 
            sd_utci_year = sd(mean_utci_mix) ) %>% 
  ungroup() %>% 
    
  group_by(FAO.address) %>% 
      mutate(
            day_1_utci = lag(mean_utci_mix, n=1, default=NA), 
            day_2_utci = lag(mean_utci_mix, n=2, default=NA), 
            day_3_utci = lag(mean_utci_mix, n=3, default=NA), 
            day_4_utci = lag(mean_utci_mix, n=4, default=NA), 
            day_5_utci = lag(mean_utci_mix, n=5, default=NA), 
            day_6_utci = lag(mean_utci_mix, n=6, default=NA), 
            day_7_utci = lag(mean_utci_mix, n=7, default=NA), 
            
            day_1_percentile = lag(percentile, n=1, default=NA), 
            day_2_percentile = lag(percentile, n=2, default=NA), 
            day_3_percentile = lag(percentile, n=3, default=NA), 
            day_4_percentile = lag(percentile, n=4, default=NA), 
            day_5_percentile = lag(percentile, n=5, default=NA), 
            day_6_percentile = lag(percentile, n=6, default=NA), 
            day_7_percentile = lag(percentile, n=7, default=NA), 
            
            hot_day_10P = ifelse(percentile >= 90, 1, 0), 
            day_1_hot = lag(hot_day_10P, n=1, default=NA), 
            day_2_hot = lag(hot_day_10P, n=2, default=NA), 
            day_3_hot = lag(hot_day_10P, n=3, default=NA), 
            day_4_hot = lag(hot_day_10P, n=4, default=NA), 
            day_5_hot = lag(hot_day_10P, n=5, default=NA), 
            day_6_hot = lag(hot_day_10P, n=6, default=NA), 
            day_7_hot = lag(hot_day_10P, n=7, default=NA),
            
            hot_day_05P = ifelse(percentile >= 95, 1, 0), 
            hot_day_15P = ifelse(percentile >= 85, 1, 0), 
            
            # includes the day itself
            hotdays_year = zoo::rollsumr(hot_day_10P, 365, fill=NA), 
            hotdays_month = zoo::rollsumr(hot_day_10P, 30, fill=NA),
            hotdays_2week = zoo::rollsumr(hot_day_10P, 14, fill=NA), 
            
            hotdays_week_05P = zoo::rollsumr(hot_day_05P, 7, fill=NA), 
            hotdays_week_10P = zoo::rollsumr(hot_day_10P, 7, fill=NA), 
            hotdays_week_15P = zoo::rollsumr(hot_day_15P, 7, fill=NA), 
            
            hot_day_26 = ifelse(mean_utci_mix >= 26, 1, 0), 
            hot_day_28 = ifelse(mean_utci_mix >= 28, 1, 0), 
            hot_day_30 = ifelse(mean_utci_mix >= 30, 1, 0), 
            
            hotdays_week_26 = zoo::rollsumr(hot_day_26, 7, fill=NA), 
            hotdays_week_28 = zoo::rollsumr(hot_day_28, 7, fill=NA), 
            hotdays_week_30 = zoo::rollsumr(hot_day_30, 7, fill=NA),
            
            hot_week_26 = ifelse(hotdays_week_26 > 2, 1, 0), 
            hot_week_28 = ifelse(hotdays_week_28 > 2, 1, 0), 
            hot_week_30 = ifelse(hotdays_week_30 > 2, 1, 0), 
            
            hot_week_05P = ifelse(hotdays_week_05P > 2, 1, 0), 
            hot_week_10P = ifelse(hotdays_week_10P > 2, 1, 0), 
            hot_week_15P = ifelse(hotdays_week_15P > 2, 1, 0), 
            hotdays_2week = hotdays_2week - hotdays_week_10P) %>% 
  
  ungroup() %>% 
      
  group_by(month, FAO.address, year) %>% 
      mutate(utci.avg.month.state = mean(mean_utci_mix, na.rm=T) ) %>%
  ungroup() %>% 
  
  group_by(NAME_0, year) %>%
  mutate(utci.avg.country= mean(utci.avg.state, na.rm=T) ) %>% 

  dplyr::select(
    -lon, -lat, -FAO.state, -NAME_1, -area_sqkm,
    -mean_utci_exact, -min_utci_exact, -max_utci_exact )

# Relevel
df_lag$utci.2bin = relevel(df_lag$utci.2bin, '(20,22]')
df_lag$utci.4bin = relevel(as.factor(df_lag$utci.4bin), '(18,22]')

data <-  merge(ses_data,
               df_lag, 
               by=c('FAO.address', 'WP4', 'year'), 
               all.x=T)

##############################################################################@
#### 4 | Precipitation --------------------------------------------
##############################################################################@

precip <- read_csv('data/precipitation/precip.csv', show_col_types=F)

precip_lag <- 
  precip %>% 
  
  group_by(GID_1) %>% 
  
  complete(WP4 = seq.Date(min(WP4), max(WP4), by='day')) %>% #
  arrange(WP4) %>% 
  
  group_by(GID_1) %>% 
  
  reframe(
    WP4 = WP4, 
    precip = precip, 
    precip_month = zoo::rollmeanr(precip, 30, fill=NA),
    precip_week = zoo::rollmeanr(precip, 7, fill=NA)  )


data <- merge(data, 
              precip_lag, 
              by=c('WP4', 'GID_1'), 
              all.x=T)

data <- 
  data %>% 
  mutate(log_precip_week = log(precip_week),
         log_precip_month = log(precip_month))

data <- do.call(data.frame,                     
                    lapply(data,
                           function(x) replace(x, is.infinite(x), NA)) )

write_csv(data, 'time-savers/data_final.csv')

