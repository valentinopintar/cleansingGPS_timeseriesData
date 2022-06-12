rm(list=ls())  

library(tidyverse)

setwd(choose.dir())   # choose directory s .csv file with start and end of flyght

data_format <- "%Y-%m-%d %H:%M:%S"
tz = "CET"

# loading csv files and merging them into a dataframe
all_csv_data <- 
  list.files(pattern = "*.csv") %>% 
  purrr::map_df(~read_csv(.))
  
all_csv_data$UTC_datetime <- as.POSIXct(all_csv_data$UTC_datetime, format = data_format, tz = tz)

# filtering by start and end of flight
# table must contain these column names: "device_id", "start", "end"
startend_data <- readxl::read_excel(file.choose())

# convert if dates are not loaded as POSIXct
startend_data$pocetak <- as.POSIXct(startend_data$pocetak, format = data_format, tz = tz)
startend_data$kraj <- as.POSIXct(startend_data$kraj, format = data_format, tz = tz)

csv_data_WGS84 <- all_csv_data %>%
  left_join(startend_data, by = "device_id") %>%
  filter(UTC_datetime > pocetak & UTC_datetime < kraj)

# create and export WGS84 .csv files
for(i in unique(csv_data_WGS84$device_id)) {
  list_data <- split(csv_data_WGS84, csv_data_WGS84$device_id)
  names(list_data) <- paste0(names(list_data), "_2021_WGS84")
  list2env(list_data, .GlobalEnv)
}
for (i in names(list_data)) {
  if (!file.exists("csv_WGS84")) dir.create("csv_WGS84", recursive = TRUE)
  write.csv(list_data[[i]], file.path("csv_WGS84", paste0(i, ".csv")))
}

# create shapefile
data_sp_WGS84 <- csv_data_WGS84 
sp::coordinates(data_sp_WGS84)=~Longitude+Latitude
sp::proj4string(data_sp_WGS84) <- sp::CRS("+init=epsg:4326")
data_sp_HTRS <- sp::spTransform(data_sp_WGS84, sp::CRS("+init=epsg:3765"))

# export htrs .csv
csv_data_HTRS <- as(data_sp_HTRS, "data.frame")
for(i in unique(csv_data_HTRS$device_id)) {
  list_data <- split(csv_data_HTRS, csv_data_HTRS$device_id)
  names(list_data) <- paste0(names(list_data), "_2021_HTRS")
  list2env(list_data, .GlobalEnv)
}

for (i in names(list_data)) {
  if (!file.exists("csv_HTRS")) dir.create("csv_HTRS", recursive = TRUE)
  write.csv(list_data[[i]], file.path("csv_HTRS", paste0(i, ".csv")))
}

# .shp export / split unique by device_id
unique_ <- unique(data_sp_HTRS$device_id)
data_sp_HTRS$UTC_time <- strftime(data_sp_HTRS$UTC_time, format="%H:%M:%S")  #zbog ne mogucnosti snimanja .shp ukoliko postoji hms klasa
for(i in 1:length(unique_)) {
  if (!file.exists("shp_HTRS")) dir.create("shp_HTRS", recursive = TRUE)
  temp <- data_sp_HTRS[data_sp_HTRS$device_id == unique_[i],]
  rgdal::writeOGR(temp, dsn="shp_HTRS", layer = paste0(unique_[i], "_2021_HTRS.shp"), driver="ESRI Shapefile", overwrite_layer = TRUE)
}
