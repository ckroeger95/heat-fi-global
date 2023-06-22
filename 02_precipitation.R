
##############################################################################@

# Author: Carolin Kroeger
# Github: ckroeger95

# Date: March 2023

# 02_precipitation: Extracts precipitation from the NOAA's PSL .nc files

# Precipitation data sources
# https://psl.noaa.gov/data/gridded/data.cpc.globalprecip.html 
# units: daily total of precipitation in mm
# https://psl.noaa.gov/thredds/catalog/Datasets/cpc_global_precip/catalog.html

##############################################################################@
# 1 Set up --------------------------------------------------
##############################################################################@

library(sf)
library(raster)
library(tidyverse)
library(countrycode)
library(zoo)

# state shapefile
states <- 
  read_sf(dsn='data/map', layer='gadm36_1') %>% 
  mutate(ISO_3 = countrycode(NAME_0, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(NAME_0=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(NAME_0=='Northern Cyprus', 'CYP2', ISO_3)) %>% 
  filter(NAME_0 != 'Antarctica')

# key that connects shapefile names to the FAO names 
key.precip <- 
  read_csv('data/utci/key.utci.csv', show_col_types = F)

##############################################################################@
# 2 Create key for lon + lat -------------------------------------------
##############################################################################@

# combination of latlon points
lonlat <- 
  brick('data/precipitation/precip.2017.nc') %>% 
  rotate() %>% 
  as.data.frame(xy=TRUE) %>% 
  rename(lon = x, lat = y) %>% 
  dplyr::select(lon, lat) %>% 
  st_as_sf(dim='XY', coords=c('lon', 'lat'), crs='+proj=longlat +datum=WGS84 +no_defs')

# match latlon into the state polygons
lonlat.match = st_join(lonlat, states, left=F)

# Turn sf object into data frame with lat lon
df_lonlat.match = 
  lonlat.match %>% 
  mutate(lon = unlist(map(lonlat.match$geometry,1)),
         lat = unlist(map(lonlat.match$geometry,2))) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) 

# Save key for future use
write_csv(df_lonlat.match, file='data/precipitation/lonlat.match.csv')


##############################################################################@
# 3 Turn daily .nc into yearly df -------------------------------------------
##############################################################################@

###### 2013 -------------------------------------------
##############################################################################@

# Read raster and find out how many bands it has
precip.brick <-  brick('data/precipitation/precip.2013.nc')
bands = seq(from=1, to=length(precip.brick@z[["Date/time"]]))

precip.all <- data.frame(
  GID_1 = character(), 
  WP4 = character(), 
  precip = double(), 
  stringsAsFactors = FALSE)

for (i in bands) {
  
  precip.raster = raster('data/precipitation/precip.2013.nc', band=i) # band = i / day of the year
  
  # Rotate from 0-360 to -180 to 180
  precip.raster = rotate(precip.raster)
  
  # turn into data frame, and omit NAs
  precip.df = 
    precip.raster %>% 
    as.data.frame(xy=T) %>% 
    na.omit() %>% 
    rename(lon = x,  lat = y, precip = Daily.total.of.precipitation) %>% 
    mutate(WP4 = precip.raster@z[[1]])
  
  precip.df2 <- merge(precip.df, 
                      df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                      by=c('lon', 'lat'))
  
  precip.df2 <- 
    precip.df2 %>% 
    group_by(GID_1, WP4) %>% 
    reframe(precip = mean(precip, na.rm=T))
  
  precip.all = rbind(precip.all, precip.df2)
  
  print(i)
  
}

write_csv(precip.all, 'data/precipitation/precip.2013.csv')


###### 2014 -------------------------------------------
##############################################################################@

# Read raster and find out how many bands it has
precip.brick <-  brick('data/precipitation/precip.2014.nc')
bands = seq(from=1, to=length(precip.brick@z[["Date/time"]]))

precip.all <- data.frame(
  GID_1 = character(), 
  WP4 = character(), 
  precip = double(), 
  stringsAsFactors = FALSE)

for (i in bands) {
  
  precip.raster = raster('data/precipitation/precip.2014.nc', band=i) # band = i / day of the year
  
  # Rotate from 0-360 to -180 to 180
  precip.raster = rotate(precip.raster)
  
  # turn into data frame, and omit NAs
  precip.df = 
    precip.raster %>% 
    as.data.frame(xy=T) %>% 
    na.omit() %>% 
    rename(lon = x,  lat = y, precip = Daily.total.of.precipitation) %>% 
    mutate(WP4 = precip.raster@z[[1]])
  
  precip.df2 <- merge(precip.df, 
                      df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                      by=c('lon', 'lat'))
  
  precip.df2 <- 
    precip.df2 %>% 
    group_by(GID_1, WP4) %>% 
    reframe(precip = mean(precip, na.rm=T))
  
  precip.all = rbind(precip.all, precip.df2)
  
  print(i)
  
}

write_csv(precip.all, 'data/precipitation/precip.2014.csv')


###### 2015 -------------------------------------------
##############################################################################@

# Read raster and find out how many bands it has
precip.brick <-  brick('data/precipitation/precip.2015.nc')
bands = seq(from=1, to=length(precip.brick@z[["Date/time"]]))

precip.all <- data.frame(
  GID_1 = character(), 
  WP4 = character(), 
  precip = double(), 
  stringsAsFactors = FALSE)

for (i in bands) {
  
  precip.raster = raster('data/precipitation/precip.2015.nc', band=i) # band = i / day of the year
  
  # Rotate from 0-360 to -180 to 180
  precip.raster = rotate(precip.raster)
  
  # turn into data frame, and omit NAs
  precip.df = 
    precip.raster %>% 
    as.data.frame(xy=T) %>% 
    na.omit() %>% 
    rename(lon = x,  lat = y, precip = Daily.total.of.precipitation) %>% 
    mutate(WP4 = precip.raster@z[[1]])
  
  precip.df2 <- merge(precip.df, 
                      df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                      by=c('lon', 'lat'))
  
  precip.df2 <- 
    precip.df2 %>% 
    group_by(GID_1, WP4) %>% 
    reframe(precip = mean(precip, na.rm=T))
  
  precip.all = rbind(precip.all, precip.df2)
  
  print(i)
  
}

write_csv(precip.all, 'data/precipitation/precip.2015.csv')


###### 2016 -------------------------------------------
##############################################################################@

# Read raster and find out how many bands it has
precip.brick <-  brick('data/precipitation/precip.2016.nc')
bands <-  seq(from=1, to=length(precip.brick@z[["Date/time"]]))

precip.all <- data.frame(
  GID_1 = character(), 
  WP4 = character(), 
  precip = double(), 
  stringsAsFactors = FALSE)

for (i in bands) {
  
  precip.raster = raster('data/precipitation/precip.2016.nc', band=i) # band = i / day of the year
  
  # Rotate from 0-360 to -180 to 180
  precip.raster = rotate(precip.raster)
  
  # turn into data frame, and omit NAs
  precip.df = 
    precip.raster %>% 
    as.data.frame(xy=T) %>% 
    na.omit() %>% 
    rename(lon = x,  lat = y, precip = Daily.total.of.precipitation) %>% 
    mutate(WP4 = precip.raster@z[[1]])
  
  precip.df2 <- merge(precip.df, 
                      df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                      by=c('lon', 'lat'))
  
  precip.df2 <- 
    precip.df2 %>% 
    group_by(GID_1, WP4) %>% 
    reframe(precip = mean(precip, na.rm=T))
  
  precip.all = rbind(precip.all, precip.df2)
  
  print(i)
  
}

write_csv(precip.all, 'data/precipitation/precip.2016.csv')


######### 2017 -------------------------------------------
##############################################################################@

# Read raster and find out how many bands it has
precip.brick <-  brick('data/precipitation/precip.2017.nc')
bands = seq(from=1, to=length(precip.brick@z[["Date/time"]]))

precip.all <- data.frame(
  GID_1 = character(), 
  WP4 = character(), 
  precip = double(), 
  stringsAsFactors = FALSE)

for (i in bands) {
  
  precip.raster = raster('data/precipitation/precip.2017.nc', band=i) # band = i / day of the year
  
  # Rotate from 0-360 to -180 to 180
  precip.raster = rotate(precip.raster)
  
  # turn into data frame, and omit NAs
  precip.df = 
    precip.raster %>% 
    as.data.frame(xy=T) %>% 
    na.omit() %>% 
    rename(lon = x,  lat = y, precip = Daily.total.of.precipitation) %>% 
    mutate(WP4 = precip.raster@z[[1]])
  
  precip.df2 <- merge(precip.df, 
                      df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                      by=c('lon', 'lat'))
  
  precip.df2 <- 
    precip.df2 %>% 
    group_by(GID_1, WP4) %>% 
    reframe(precip = mean(precip, na.rm=T))
  
  precip.all = rbind(precip.all, precip.df2)
  
  print(i)
  
}

write_csv(precip.all, 'data/precipitation/precip.2017.csv')


##############################################################################@
# 4 Combine into one file -------------------------------------------
##############################################################################@

precip <- 
  rbind(
    read_csv('data/precipitation/precip.2013.csv', show_col_types=F),
    read_csv('data/precipitation/precip.2014.csv', show_col_types=F),
    read_csv('data/precipitation/precip.2015.csv', show_col_types=F),
    read_csv('data/precipitation/precip.2016.csv', show_col_types=F),
    read_csv('data/precipitation/precip.2017.csv', show_col_types=F) )

write_csv(precip, 'data/precipitation/precip.csv')  



