
##############################################################################@

# Author: Carolin Kroeger
# Github: ckroeger95

# Date: March 2023

# 01_utci: Extracts ºC observations from the ERA-5 HEAT data set and prepare it for analysis

# Source link to ERA-5 HEAT 
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/derived-utci-historical?tab=overview


##############################################################################@
# 1 Set up --------------------------------------------------
##############################################################################@

library(sf)
library(raster)
library(tidyverse)
library(countrycode)

# state shapefile
states <- 
  read_sf(dsn='data/map', layer='gadm36_1') %>% 
  mutate(ISO_3 = countrycode(NAME_0, origin='country.name', destination='iso3c'), 
         ISO_3 = ifelse(NAME_0=='Kosovo', 'XKO', ISO_3),
         ISO_3 = ifelse(NAME_0=='Northern Cyprus', 'CYP2', ISO_3)) %>% 
  filter(NAME_0 != 'Antarctica')

# key that connects shapefile names to the Gallup World Poll names 
key.utci <- 
  read_csv('data/utci/key.utci.csv', show_col_types = F)

##############################################################################@
# 2 Create key for lon + lat -------------------------------------------
##############################################################################@

# combination of latlon points
lonlat <- 
  brick('data/utci/UTCI_2013/nc/ECMWF_utci_20130301_v1.1_con.nc') %>% 
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
write_csv(df_lonlat.match, file='data/utci/lonlat.match.csv')

##############################################################################@
# 3 Turn daily .nc into yearly df -------------------------------------------
##############################################################################@

##### 2013 -------------------------------------------
##############################################################################@

# Df to store results into 
utci.all = data.frame(
              WP4=character(), FAO.address=character(), lon=double(), lat=double(), 
              min_utci_cross=double(), max_utci_cross=double(), mean_utci_cross=double(),
              sd_area_mean=double(),
              min_utci_exact=double(), max_utci_exact=double(), mean_utci_exact=double(),
              sd_utci_exact=double(), stringsAsFactors=FALSE)   

# Load all files in the path (currently stored in monthly folders but could be stored yearly to reduce manual labour)
files <- paste0('data/utci/UTCI_2013/nc/', list.files(path='data/utci/UTCI_2013/nc/'))

for (i in files) {
  
  start = Sys.time()
  
  utci <- # One day with hourly obs for each lat + lon
    brick(i) %>% 
    as.data.frame(xy=T)
  
  # min, max, mean, sd for a unique lon + lat combination)
  utci$mean_utci_exact <-  apply(utci[,-c(1,2)],1,mean)
  utci$min_utci_exact  <-  apply(utci[,-c(1,2)],1,min)
  utci$max_utci_exact  <-  apply(utci[,-c(1,2)],1,max)
  utci$sd_utci_exact   <-  apply(utci[,-c(1,2)],1,sd) # sd of utci on that day 
  
  utci <- 
    utci %>% 
    rename(lon = x,  lat = y) %>% 
    mutate(WP4 = substr(colnames(utci)[3], start=2, stop=11), 
           WP4 = as.Date(WP4, format='%Y.%m.%d'), 
           mean_utci_exact = mean_utci_exact - 273.15, 
           min_utci_exact = min_utci_exact - 273.15, 
           max_utci_exact = max_utci_exact - 273.15) %>% 
    dplyr::select(lon, lat, WP4, mean_utci_exact, min_utci_exact, max_utci_exact, sd_utci_exact) 
  
  df_utci <- merge(utci, 
                   df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                   by=c('lon', 'lat'))
  
  df2_utci <- df_utci %>% 
    group_by(GID_1) %>% 
    mutate(mean_utci_cross = mean(mean_utci_exact, na.rm=T), 
           min_utci_cross = mean(min_utci_exact, na.rm=T), 
           max_utci_cross = mean(max_utci_exact, na.rm=T), 
           sd_area_mean = sd(mean_utci_exact, na.rm=T))
  
  df3_utci <- merge(key.utci %>% dplyr::select(lon, lat, FAO.address), 
                    df2_utci, 
                    by=c('lon', 'lat'), all.x=T)
  
  utci.all = rbind(utci.all, df3_utci)
  
  end = Sys.time()
  print(end-start)  
  
}

write_csv(utci.all, file='data/utci/UTCI_2013/utci.2013.csv')

##### 2014 -------------------------------------------
##############################################################################@

# Df to store results into 
utci.all = data.frame(
  WP4=character(), 
  FAO.address=character(), 
  lon=double(), 
  lat=double(), 
  min_utci_cross=double(), 
  max_utci_cross=double(), 
  mean_utci_cross=double(),
  sd_area_mean=double(),
  min_utci_exact=double(), 
  max_utci_exact=double(), 
  mean_utci_exact=double(),
  sd_utci_exact=double(), stringsAsFactors=FALSE)   

# Load all files in the path (currently stored in monthly folders but could be stored yearly to reduce manual labour)
files <- paste0('data/utci/UTCI_2014/nc/', list.files(path='data/utci/UTCI_2014/nc/'))

for (i in files) {
  
  start = Sys.time()
  
  utci <- # One day with hourly obs for each lat + lon
    brick(i) %>% 
    as.data.frame(xy=T)
  
  # min, max, mean, sd for a unique lon + lat combination)
  utci$mean_utci_exact <-  apply(utci[,-c(1,2)],1,mean)
  utci$min_utci_exact  <-  apply(utci[,-c(1,2)],1,min)
  utci$max_utci_exact  <-  apply(utci[,-c(1,2)],1,max)
  utci$sd_utci_exact   <-  apply(utci[,-c(1,2)],1,sd) # sd of utci on that day 
  
  utci <- 
    utci %>% 
    rename(lon = x,  lat = y) %>% 
    mutate(WP4 = substr(colnames(utci)[3], start=2, stop=11), 
           WP4 = as.Date(WP4, format='%Y.%m.%d'), 
           mean_utci_exact = mean_utci_exact - 273.15, 
           min_utci_exact = min_utci_exact - 273.15, 
           max_utci_exact = max_utci_exact - 273.15) %>% 
    dplyr::select(lon, lat, WP4, mean_utci_exact, min_utci_exact, max_utci_exact, sd_utci_exact) 
  
  df_utci <- merge(utci, 
                   df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                   by=c('lon', 'lat'))
  
  df2_utci <- df_utci %>% 
    group_by(GID_1) %>% 
    mutate(mean_utci_cross = mean(mean_utci_exact, na.rm=T), 
           min_utci_cross = mean(min_utci_exact, na.rm=T), 
           max_utci_cross = mean(max_utci_exact, na.rm=T), 
           sd_area_mean = sd(mean_utci_exact, na.rm=T))
  
  df3_utci <- merge(key.utci %>% dplyr::select(lon, lat, FAO.address), 
                    df2_utci, 
                    by=c('lon', 'lat'), all.x=T)
  
  utci.all = rbind(utci.all, df3_utci)
  
  end = Sys.time()
  print(end-start)  
  
}

write_csv(utci.all, file='data/utci/UTCI_2014/utci.2014.csv')


##### 2015 -------------------------------------------
##############################################################################@

# Df to store results into 
utci.all = data.frame(
  WP4=character(), FAO.address=character(), lon=double(), lat=double(), 
  min_utci_cross=double(), max_utci_cross=double(), mean_utci_cross=double(),
  sd_area_mean=double(),
  min_utci_exact=double(), max_utci_exact=double(), mean_utci_exact=double(),
  sd_utci_exact=double(), stringsAsFactors=FALSE)   

# Load all files in the path (currently stored in monthly folders but could be stored yearly to reduce manual labour)
files <- paste0('data/utci/UTCI_2015/nc/', list.files(path='data/utci/UTCI_2015/nc/'))

for (i in files) {
  
  start = Sys.time()
  
  utci <- # One day with hourly obs for each lat + lon
    brick(i) %>% 
    as.data.frame(xy=T)
  
  # min, max, mean, sd for a unique lon + lat combination)
  utci$mean_utci_exact <-  apply(utci[,-c(1,2)],1,mean)
  utci$min_utci_exact  <-  apply(utci[,-c(1,2)],1,min)
  utci$max_utci_exact  <-  apply(utci[,-c(1,2)],1,max)
  utci$sd_utci_exact   <-  apply(utci[,-c(1,2)],1,sd) # sd of utci on that day 
  
  utci <- 
    utci %>% 
    rename(lon = x,  lat = y) %>% 
    mutate(WP4 = substr(colnames(utci)[3], start=2, stop=11), 
           WP4 = as.Date(WP4, format='%Y.%m.%d'), 
           mean_utci_exact = mean_utci_exact - 273.15, 
           min_utci_exact = min_utci_exact - 273.15, 
           max_utci_exact = max_utci_exact - 273.15) %>% 
    dplyr::select(lon, lat, WP4, mean_utci_exact, min_utci_exact, max_utci_exact, sd_utci_exact) 
  
  df_utci <- merge(utci, 
                   df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                   by=c('lon', 'lat'))
  
  df2_utci <- df_utci %>% 
    group_by(GID_1) %>% 
    mutate(mean_utci_cross = mean(mean_utci_exact, na.rm=T), 
           min_utci_cross = mean(min_utci_exact, na.rm=T), 
           max_utci_cross = mean(max_utci_exact, na.rm=T), 
           sd_area_mean = sd(mean_utci_exact, na.rm=T))
  
  df3_utci <- merge(key.utci %>% dplyr::select(lon, lat, FAO.address), 
                    df2_utci, 
                    by=c('lon', 'lat'), all.x=T)
  
  utci.all = rbind(utci.all, df3_utci)
  
  end = Sys.time()
  print(end-start)  
  
}

write_csv(utci.all, file='data/utci/UTCI_2015/utci.2015.csv')


##### 2016 -------------------------------------------
##############################################################################@

# Df to store results into 
utci.all = data.frame(
  WP4=character(), FAO.address=character(), lon=double(), lat=double(), 
  min_utci_cross=double(), max_utci_cross=double(), mean_utci_cross=double(),
  sd_area_mean=double(),
  min_utci_exact=double(), max_utci_exact=double(), mean_utci_exact=double(),
  sd_utci_exact=double(), stringsAsFactors=FALSE)   

# Load all files in the path (currently stored in monthly folders but could be stored yearly to reduce manual labour)
files <- paste0('data/utci/UTCI_2016/nc/', list.files(path='data/utci/UTCI_2016/nc/'))

for (i in files) {
  
  start = Sys.time()
  
  utci <- # One day with hourly obs for each lat + lon
    brick(i) %>% 
    as.data.frame(xy=T)
  
  # min, max, mean, sd for a unique lon + lat combination)
  utci$mean_utci_exact <-  apply(utci[,-c(1,2)],1,mean)
  utci$min_utci_exact  <-  apply(utci[,-c(1,2)],1,min)
  utci$max_utci_exact  <-  apply(utci[,-c(1,2)],1,max)
  utci$sd_utci_exact   <-  apply(utci[,-c(1,2)],1,sd) # sd of utci on that day 
  
  utci <- 
    utci %>% 
    rename(lon = x,  lat = y) %>% 
    mutate(WP4 = substr(colnames(utci)[3], start=2, stop=11), 
           WP4 = as.Date(WP4, format='%Y.%m.%d'), 
           mean_utci_exact = mean_utci_exact - 273.15, 
           min_utci_exact = min_utci_exact - 273.15, 
           max_utci_exact = max_utci_exact - 273.15) %>% 
    dplyr::select(lon, lat, WP4, mean_utci_exact, min_utci_exact, max_utci_exact, sd_utci_exact) 
  
  df_utci <- merge(utci, 
                   df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                   by=c('lon', 'lat'))
  
  df2_utci <- df_utci %>% 
    group_by(GID_1) %>% 
    mutate(mean_utci_cross = mean(mean_utci_exact, na.rm=T), 
           min_utci_cross = mean(min_utci_exact, na.rm=T), 
           max_utci_cross = mean(max_utci_exact, na.rm=T), 
           sd_area_mean = sd(mean_utci_exact, na.rm=T))
  
  df3_utci <- merge(key.utci %>% dplyr::select(lon, lat, FAO.address), 
                    df2_utci, 
                    by=c('lon', 'lat'), all.x=T)
  
  utci.all = rbind(utci.all, df3_utci)
  
  end = Sys.time()
  print(end-start)  
  
}

write_csv(utci.all, file='data/utci/UTCI_2016/utci.2016.csv')



##### 2017 -------------------------------------------
##############################################################################@

# Df to store results into 
utci.all = data.frame(
  WP4=character(), FAO.address=character(), lon=double(), lat=double(), 
  min_utci_cross=double(), max_utci_cross=double(), mean_utci_cross=double(),
  sd_area_mean=double(),
  min_utci_exact=double(), max_utci_exact=double(), mean_utci_exact=double(),
  sd_utci_exact=double(), stringsAsFactors=FALSE)   

# Load all files in the path (currently stored in monthly folders but could be stored yearly to reduce manual labour)
files <- paste0('data/utci/UTCI_2017/nc/', list.files(path='data/utci/UTCI_2017/nc/'))

for (i in files) {
  
  start = Sys.time()
  
  utci <- # One day with hourly obs for each lat + lon
    brick(i) %>% 
    as.data.frame(xy=T)
  
  # min, max, mean, sd for a unique lon + lat combination)
  utci$mean_utci_exact <-  apply(utci[,-c(1,2)],1,mean)
  utci$min_utci_exact  <-  apply(utci[,-c(1,2)],1,min)
  utci$max_utci_exact  <-  apply(utci[,-c(1,2)],1,max)
  utci$sd_utci_exact   <-  apply(utci[,-c(1,2)],1,sd) # sd of utci on that day 
  
  utci <- 
    utci %>% 
    rename(lon = x,  lat = y) %>% 
    mutate(WP4 = substr(colnames(utci)[3], start=2, stop=11), 
           WP4 = as.Date(WP4, format='%Y.%m.%d'), 
           mean_utci_exact = mean_utci_exact - 273.15, 
           min_utci_exact = min_utci_exact - 273.15, 
           max_utci_exact = max_utci_exact - 273.15) %>% 
    dplyr::select(lon, lat, WP4, mean_utci_exact, min_utci_exact, max_utci_exact, sd_utci_exact) 
  
  df_utci <- merge(utci, 
                   df_lonlat.match %>% dplyr::select(lon, lat, GID_1), 
                   by=c('lon', 'lat'))
  
  df2_utci <- df_utci %>% 
    group_by(GID_1) %>% 
    mutate(mean_utci_cross = mean(mean_utci_exact, na.rm=T), 
           min_utci_cross = mean(min_utci_exact, na.rm=T), 
           max_utci_cross = mean(max_utci_exact, na.rm=T), 
           sd_area_mean = sd(mean_utci_exact, na.rm=T))
  
  df3_utci <- merge(key.utci %>% dplyr::select(lon, lat, FAO.address), 
                    df2_utci, 
                    by=c('lon', 'lat'), all.x=T)
  
  utci.all = rbind(utci.all, df3_utci)
  
  end = Sys.time()
  print(end-start)  
  
}

write_csv(utci.all, file='data/utci/UTCI_2017/utci.2017.csv')



# 4 Combine into one utci file --------------------------------------------

utci <- rbind(
  read_csv('data/utci/UTCI_2013/utci.2013.csv', show_col_types = F),
  read_csv('data/utci/UTCI_2014/utci.2014.csv', show_col_types = F), 
  read_csv('data/utci/UTCI_2015/utci.2015.csv', show_col_types = F), 
  read_csv('data/utci/UTCI_2016/utci.2016.csv', show_col_types = F),
  read_csv('data/utci/UTCI_2017/utci.2017.csv', show_col_types = F) )

write_csv(utci, file='data/utci/utci.csv')











