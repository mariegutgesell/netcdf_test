##Code to test netcdf data extraction

library(tidyverse)
library(sf)
library(ncdf4)

# 
##Read in test .nc files (this code works for 1 file)
filenames <- file.path(
   "data", 
  paste0("goa_dischargex_09011980_08311981", ".nc")
)

nc1 <- nc_open("data/goa_dischargex_09011980_08311981.nc", auto_GMT = TRUE)

# # ##Extract variables for latitude, longitude and discharge from the netcdf file
lon_var <- ncvar_get(nc1, varid = "lon")
lat_var <- ncvar_get(nc1, varid = "lat")
q_var <- ncvar_get(nc1, varid = "q")
year_var <- ncvar_get(nc1, varid = "year")
month_var <- ncvar_get(nc1, varid = "month")
day_var <- ncvar_get(nc1, varid = "day")

##create string of dates to add as column head
date_strings <- paste(year_var, month_var, day_var, sep = "-")
##Create matrix with each day as column and matrix of discharge
q_var <- as.matrix(q_var)
colnames(q_var) <- date_strings

##this is the matrix that has daily discharge for each associated lat/long 
discharge_matrix <- cbind(q_var)

##Calculate mean monthly discharge
# Calculate the number of rows (latitudes/longitudes) and columns (dates) in the matrix
num_rows <- nrow(discharge_matrix)
num_cols <- ncol(discharge_matrix)

# Convert column names (dates) to Date objects
dates <- as.Date(colnames(discharge_matrix), format = "%Y-%m-%d")

# Extract year and month from dates
year_month <- format(dates, "%Y-%m")

# Initialize an empty list to store monthly discharge values
monthly_discharge <- vector("list", length = length(unique(year_month)))

# Loop over each unique year-month combination
for (ym in unique(year_month)) {
  # Extract the corresponding indices for each month
   indices <- which(year_month == ym)
 
 # Extract the discharge values for the month
 discharge_month <- discharge_matrix[, indices]
  
 # Calculate the mean discharge for the month
  mean_discharge_month <- rowMeans(discharge_month, na.rm = TRUE)
 
 # Store the mean discharge values in the list
  monthly_discharge[[ym]] <- mean_discharge_month
}

# Convert the list of monthly discharge values into a matrix
monthly_discharge_matrix <- do.call(cbind, monthly_discharge)

 ##Add lat and long to mean monthly discharge matrix
monthly_discharge_matrix <- cbind(lon_var, lat_var, monthly_discharge_matrix)




###testing code to do for all files ---------------
filenames <- list.files("data", pattern = "\\.nc$", full.names = TRUE)

# Initialize an empty list to store the results for each file
result_list <- vector("list", length(filenames))

# Loop over each .nc file
for (i in seq_along(filenames)) {
  nc_file <- filenames[i]
  
  # Open the .nc file
  nc <- nc_open(nc_file, auto_GMT = TRUE)
  
  # Extract variables for latitude, longitude, and discharge from the netCDF file
  lon_var <- ncvar_get(nc, varid = "lon")
  lat_var <- ncvar_get(nc, varid = "lat")
  q_var <- ncvar_get(nc, varid = "q")
  year_var <- ncvar_get(nc, varid = "year")
  month_var <- ncvar_get(nc, varid = "month")
  day_var <- ncvar_get(nc, varid = "day")
  
  # Create string of dates to add as column headers
  date_strings <- paste(year_var, month_var, day_var, sep = "-")
  
  # Create matrix with each day as column and matrix of discharge
  q_var <- as.matrix(q_var)
  colnames(q_var) <- date_strings
  
  # Calculate mean monthly discharge
  # Calculate the number of rows (latitudes/longitudes) and columns (dates) in the matrix
  num_rows <- nrow(q_var)
  num_cols <- ncol(q_var)
  
  # Convert column names (dates) to Date objects
  dates <- as.Date(colnames(q_var), format = "%Y-%m-%d")
  
  # Extract year and month from dates
  year_month <- format(dates, "%Y-%m")
  
  # Initialize an empty list to store monthly discharge values
  monthly_discharge <- vector("list", length = length(unique(year_month)))
  
  # Loop over each unique year-month combination
  for (ym in unique(year_month)) {
    # Extract the corresponding indices for each month
    indices <- which(year_month == ym)
    
    # Extract the discharge values for the month
    discharge_month <- q_var[, indices]
    
    # Calculate the mean discharge for the month
    mean_discharge_month <- rowMeans(discharge_month, na.rm = TRUE)
    
    # Store the mean discharge values in the list
    monthly_discharge[[ym]] <- mean_discharge_month
  }
  
  # Convert the list of monthly discharge values into a matrix
  monthly_discharge_matrix <- do.call(cbind, monthly_discharge)
  
  
  # Store the result in the result list
  result_list[[i]] <- monthly_discharge_matrix
  
  # Close the .nc file
  nc_close(nc)
}

# Combine the results into a single data structure 
combined_result <- do.call(cbind, result_list)
# Add lat and long to mean monthly discharge matrix
combined_result <- cbind(lon_var, lat_var, combined_result)

##Calculate monthly mean across all years --------------
library(data.table)
combined_result <- as.data.frame(combined_result)

annual_mean <- melt(setDT(combined_result), id.vars = c("lon_var", "lat_var"), variable.name = "date") %>%
  separate(date, into = c("year", "month"), sep = "-") %>%
  group_by(lon_var, lat_var, month) %>%
  summarise_at(vars(value), list(mean_discharge = mean))




