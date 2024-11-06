# Geog 418 Final Project Code

This document contains all the code you will need to complete for final project except for the code you have already used in Assignmnets 1-3. The code is divided into sections based on the major tasks you need to complete for the project:

[Go to Section](#Cleaning your climate data)
Cleaning Your Climate Data
- [Introduction](#introduction)
1. [Cleaning your climate data](#cleaning Your Climate Data)
2. Mapping your climate data
3. Spatial interpolation of your climate data
4. Creating a density map of your events data
5. Combining your climate and events data
6. Performing an ordinary least squres regression
7. Perfroming a geographically weighted regression

Note that you are not provide with code to perform descriptive statistics, spatial descriptive statistics, and point pattern analysis. You should perform these analyses on your input data to provide some context to your study. In addition, you are not provided with code for performing a Global Moran's I test, which you should be performing on the residuals from your OLS regression to evaluate if you are negating any important statistical assumptions (i.e. independence of errors).

Before you move on to the code, please read over these tips to make your life easier:
- Make sure you are replacing file names with those that are relevant to your data
- Each chunk of code ends with printing a shapefile and/or csv file, and each subequent chunk begins with reading that specific file. The reason we do this is so that you do not need to re-run all your code to run a specific chunk. However, this requires you to make sure that you are using consistent file names when printing and reading these files (this should make more sense when you look at the code).
- You are responsible for setting values for the resolution of your datasets (e.g. your interpolated surface and density map). You should make sure your selected resolution makes sense given the phenomenon you are investigating and given the methods you are using. Also, you should ensure that you are using a consistent resolution for your different methods.
- Finally, this is a challenging project with no single right outcome. It is your responsibility to get your code to work as best as possible and provide a write-up based on the intstructions in Brightspace that best descirbes the work you accomplished. Be critical of the methods you use and the results you obtain, and clearly articulate your findings and what the reader needs to be aware of when understanding your work.

# Code
## Cleaning Your Climate Data
You are responsible for downloading at least one climate/weather variable from PCIC's Weather Station Data Portal across multiple stations. It is recommended that you select a variable and station sources (e.g. EC = Environment Canada) so that you have a representation of points distributed across the province. You will also need to select an appropriate date range for your study. Do not include stations with no observations. Once you have selected your data, select the Station Data tab and check "Clip time series to filter data range". Download as a CSV. Please note that this will take a long time. Once complete, you should see a folder with multiple CSV files, each one pertaining to a different station. Before processing this data, download the stataion metadata by selecting the Station Metadata tab, select "By Station", and select download. 

You will want to create a shapefile of your weather stations that contains your weather variable. Here is the code to do this:

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}

#You may or may not need all these libraries
library(tmap)
library(spdep)
library(raster)
library(sf)
library(lubridate)
library(dplyr)
library(gstat)
library(ggplot2)
library(maps)

##Create an csv file. You will use this later to save your data
# Create an empty data frame with specified columns
empty_data <- data.frame(Native.ID = character(), TEMP = numeric(), 
                         Longitude = numeric(), Latitude = numeric(), stringsAsFactors = FALSE)
csv_file_name <- "BC_AVG_TEMP.csv"

# Write the empty data frame to a CSV file
write.csv(empty_data, file = csv_file_name, row.names = FALSE)
```

You will have a CSV file for each weather station. You will need to perform some calculation on the data in each CSV. An easy way to do this is to use a for loop to run through every CSV file in a folder and apply the same statistics. The code below runs through all csv files in folder to calculate an aggregate measure of temperature.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#First, list all CSV files in the directory to make sure you are in the right folder
csv_files <- list.files(path = "./Data/BCH", pattern = "\\.csv$", full.names = TRUE)

#Next, loop through each CSV file and perform your calculations.
for (file in csv_files) {

#Read each file
hourly_data <- read.csv(file, skip = 1, header = TRUE)
file_name <- file

#Adjust the date/time column so that it is usable in calculations
hourly_data$time <- lubridate::ymd_hms(hourly_data$time) 

#Convert your variable column (I call it AirTemp here) to numeric and remove NA's
hourly_data$AirTemp <- as.numeric(hourly_data$AirTemp)
hourly_data <- hourly_data %>%
  filter(!is.na(AirTemp))

#Here is an example of how to calculate daily average temperature from all the data.
daily_avg_temp <- hourly_data %>%
  group_by(date = as.Date(time)) %>%
  summarize(daily_avg_temp = mean(AirTemp, na.rm = TRUE))

# Display the daily average temperatures
print(daily_avg_temp)

#Here is an example of hot to calculate monthly average temperature
monthly_avg_temp <- hourly_data %>%
  group_by(year = year(time), month = month(time)) %>%
  summarize(monthly_avg_temp = mean(AirTemp, na.rm = TRUE)) %>%
  ungroup()  # Ungroup for any future modifications

# Display the monthly average temperatures
print(monthly_avg_temp)

#What if we want to calculate an everage over seveal months, such as the length of the fire season?
# Filter for the months from May to October
average_temp_may_october <- hourly_data %>%
  filter(month(time) >= 5 & month(time) <= 10) %>%
  summarize(TEMP = mean(AirTemp, na.rm = TRUE))  # Replace 'temperature' with your column name

# Display the average temperature
print(average_temp_may_october)
```

In your empty CSV, you want to write your caluclation and include the weather station that it came from. You need to do this so that you can later join this CSV file with a shapefile of your weather station locations.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#First, extract the filename as this is the name of your weather station.
file_name <- basename(file_name)

#Remove the file extension
file_name_no_ext <- sub("\\.[^.]*$", "", file_name)

# Display the result
print(file_name_no_ext)

#Read the existing CSV file
file_path <- csv_file_name
data <- read.csv(file_path)

#Round the temperature values to two decimals
Roundedtemp <- round(average_temp_may_october,2)

#Convert the weather station ID column to character
data$Native.ID <- as.character(data$Native.ID)

#Now, add your weather station and temperature values to the file
new_values <- data.frame(Native.ID = file_name_no_ext, 
                         TEMP = Roundedtemp, 
                         stringsAsFactors = FALSE)
data <- bind_rows(data, new_values)

#Check your data to make sure that the row has been added.
print(head(data))

#Save the updated data frame back to a new CSV file
output_file_path <- csv_file_name
write.csv(data, file = output_file_path, row.names = FALSE)
}

#Merge the climate data for each station with the location data found in the metadata file
metadata <- read.csv("./Data/station-metadata-by-history.csv")
climatedata <- read.csv("BC_AVG_TEMP.csv")

merged_data <- merge(metadata, climatedata, by = "Native.ID")

#Remove the last two columns which are duplicate Latitude and Longitude
merged_data <- merged_data[, -((ncol(merged_data)-1):ncol(merged_data))]

#Change column names for Latitude and Longitude to remove the x
colnames(merged_data)[colnames(merged_data) %in% c("Latitude.x", "Longitude.x")] <- c("Longitude", "Latitude")

#Omit NA's
merged_data <- na.omit(merged_data)

#If there are erroneous temperature valuesn filter data to remove these.
merged_data <- merged_data[merged_data$TEMP <= 100, ]

#Write the dataset so that it  is stored
write.csv(merged_data, file = "ClimateData.csv", row.names = FALSE)
```
We finish this last chunk of code by writing our ClimateData.csv file so that we have a copy saved. You can now use this dataset to create a shapefile of weather stations.

## Creating a Shapefile and Mapping Climate Stations
In this step you will open your ClimateData.csv file, create a shapefile, and then create a map of the data so you can see if the data has been cleaned as expected.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Read the CSV file
climate_data <- read.csv("ClimateData.csv")

# Ensure Latitude and Longitude columns are correctly formatted
# Assuming the columns are named "Latitude" and "Longitude"
climate_data <- climate_data %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Create a simple feature object (sf) using Latitude and Longitude
climate_sf <- st_as_sf(climate_data, coords = c("Longitude", "Latitude"), crs = 3005)

# Optionally, you can select columns that you want to keep in the shapefile
# climate_sf <- climate_sf %>% select(Your_Columns_Here)

# Write the shapefile to disk
st_write(climate_sf, "ClimateData.shp")

# Confirmation message
print("Shapefile has been created: ClimateData.shp")

# Load the shapefiles
climate_sf <- st_read("ClimateData.shp")
bc_boundary <- st_read("ABMS_PROV_polygon.shp")

# Create the map
ggplot() +
  geom_sf(data = bc_boundary, fill = "lightgrey", color = "black") +
  # Map the TEMP variable to color
  geom_sf(data = climate_sf, aes(color = TEMP), size = 2) + 
  scale_color_gradient(low = "blue", high = "red") + # Adjust color gradient as needed
  theme_minimal() +
  labs(title = "Map of Climate Data Points in British Columbia",
       subtitle = "Overlayed on BC Boundary",
       x = "Longitude",  # Use Longitude for x-axis
       y = "Latitude",   # Use Latitude for y-axis
       color = "Temperature (°C)") + # Label for color legend
  theme(legend.position = "bottom")
```


## Interpolating Your Weather Station Data
In this section you will create an interpolated surface of your weather data variable. You can use either inverse distance weighting or kriging to accomplish, or use both methods and compare the results between them.
### Inverse Distance Weighting

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#Read the shapefile
climate_data <- st_read("ClimateData.shp")
abms_prov_polygon <- st_read("ABMS_PROV_polygon.shp")  # Ensure the path is correct


#Create a grid for the interpolation. Adjust the extent and resolution of the grid according to your needs
bbox <- st_bbox(abms_prov_polygon)
grid <- st_make_grid(st_as_sfc(bbox), cellsize = c(50000, 50000))  # Adjust the cell size

#Interpolate using IDW
idw_result <- gstat::idw(TEMP ~ 1, 
                         locations = climate_data, 
                         newdata = st_as_sf(grid), 
                         idp = 2)

#Convert idw_result to an sf object
idw_sf <- st_as_sf(idw_result)

#Extract coordinates 
idw_sf <- st_as_sf(idw_result)

#Plot the results using geom_sf() for better handling of sf objects
ggplot(data = idw_sf) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted values
  scale_fill_viridis_c() +
  labs(title = "IDW Interpolation of Temperature", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

#Save the result to a shapefile if needed
st_write(idw_sf, "IDW_Result.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)


#########################################

# Step 1: Load the polygon shapefile for clipping
abms_prov_polygon <- st_read("ABMS_PROV_polygon.shp")  # Ensure the path is correct

# Verify the structure of the polygon shapefile
print(head(abms_prov_polygon))
# Check the CRS of both objects
crs_idw <- st_crs(idw_sf)  # CRS of IDW result
crs_polygon <- st_crs(abms_prov_polygon)  # CRS of the polygon shapefile

print(crs_idw)
print(crs_polygon)

# Step to transform the CRS of either shapefile if they do not match
if (crs_idw != crs_polygon) {
  # Transform the IDW result to match the CRS of the polygon
  idw_sf <- st_transform(idw_sf, crs = crs_polygon)  # Transform IDW result to polygon's CRS
  message("Transformed IDW result CRS to match the polygon.")
} else {
  message("CRS of IDW result and polygon already match.")
}

# Now attempt the intersection again
idw_clipped <- st_intersection(idw_sf, abms_prov_polygon)

# Check the results of clipping
print(st_geometry(idw_clipped))  # Check geometry to ensure it's clipped correctly


#Create the map of the clipped results
ggplot(data = idw_clipped) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted temperature values
  scale_fill_viridis_c(option = "D") +  # Use viridis color scale for better readability
  labs(title = "Clipped IDW Interpolation of Temperature",
       fill = "Temperature (°C)",  # Change label as appropriate
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

#Save the map as an image file (optional)
ggsave("Clipped_IDW_Interpolation_Map.png", width = 10, height = 8, dpi = 300)

```

### Kriging

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}

# Read the shapefile
climate_data <- st_read("ClimateData.shp")

f.0 <- as.formula(TEMP ~ 1) 

# Create variogram. Be sure to test out the three different models.
var.smpl <- variogram(f.0, climate_data, cloud = FALSE) 
dat.fit  <- fit.variogram(var.smpl, fit.ranges = TRUE, fit.sills = TRUE,
                          vgm(model="Sph", nugget = 8, psill = 20, 
                              range = 400000))
plot(var.smpl, dat.fit)

# Define the grid
xmin <- st_bbox(climate_data)$xmin
xmax <- st_bbox(climate_data)$xmax
ymin <- st_bbox(climate_data)$ymin
ymax <- st_bbox(climate_data)$ymax

# Create a regular grid
n <- 50000  # Number of points
grd <- st_as_sf(expand.grid(x = seq(xmin, xmax, length.out = sqrt(n)),
                            y = seq(ymin, ymax, length.out = sqrt(n))),
                coords = c("x", "y"), crs = st_crs(climate_data))

dat.krg <- krige(f.0, climate_data, grd, dat.fit, debug.level=0)

# Convert the kriging output to an sf object
kriging_results_sf <- st_as_sf(dat.krg)

# Create a Raster from Kriging Results
# 1. Convert to a data frame with coordinates for raster creation
coords_df <- as.data.frame(st_coordinates(kriging_results_sf))
coords_df$predicted_temp <- kriging_results_sf$var1.pred  # Replace with your prediction column

# 2. Create the raster from the resulting data frame
predicted_raster <- rasterFromXYZ(coords_df)

# Visualize the raster
tm_shape(predicted_raster) +
  tm_raster(palette = "viridis", title = "Predicted Temperature") +
  tm_layout(title = "Kriging Results for Temperature") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar()
```

## Create Density Dataset of Point Events
In this section you will create a raster dataset of the density of points per unity area across the province. You will convert your point shapefile into a density raster showing the number of events per raster cell. Be mindful that the resolution you select here should match the resolution of your spatial interpolation outputs.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Load your point data (make sure to adjust the path)
C_FIRE_PNT_point <- st_read("C_FIRE_PNT_point.shp")
abms_prov_polygon <- st_read("ABMS_PROV_polygon.shp")  # Ensure the path is correct

# Ensure bbox2 is valid and formatted correctly
bbox2 <- st_bbox(abms_prov_polygon)

raster_res <- 50000  # 50 km in meters
raster_template <- raster(extent(bbox2), res = c(raster_res, raster_res))

# Estimate density using kernel density estimate
density_raster <- raster::rasterize(st_as_sf(C_FIRE_PNT_point), raster_template, fun = "count", field = 1)

# Ensure all NAs are turned to zeros in the raster
density_raster[is.na(density_raster)] <- 0

# Convert the raster to a data frame and replace any potential NAs with zeros
density_df <- as.data.frame(density_raster, xy = TRUE)
density_df[is.na(density_df)] <- 0  # Replace NAs in the data frame with zeros

# Step to rename the 'layer' column to 'fires' if applicable
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Convert to a spatial points data frame using sf (if needed later)
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(abms_prov_polygon))

# Plotting the density map with the polygon boundary
ggplot() +
  geom_raster(data = density_df, aes(x = x, y = y, fill = fires)) +  # Use 'fires' from the data frame
  geom_sf(data = abms_prov_polygon, fill = NA, color = "black") + # Boundary polygon
  scale_fill_viridis_c(option = "plasma") +  # Using a color scale
  theme_minimal() +
  labs(title = "Density Map of Fire Points",
       x = "Longitude",
       y = "Latitude",
       fill = "Density")

# Convert the raster to a data frame
density_df <- as.data.frame(density_raster, xy = TRUE)

# Rename the 'layer' column to 'fires'
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Replace NA values with zeros
density_df[is.na(density_df$fires), "fires"] <- 0

# Convert to a spatial points data frame using sf
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(abms_prov_polygon))

# Write to a shapefile
st_write(density_sf, "density_points.shp", delete_dsn = TRUE)

# Create a simple map
ggplot() +
  geom_sf(data = abms_prov_polygon, fill = NA, color = "black") +  # Plot the boundary polygon
  geom_sf(data = density_sf, aes(color = fires), size = 1) +  # Plot the density points with color mapping
  scale_color_viridis_c(option = "plasma", name = "Density of Fires") +  # Color scale for density values
  theme_minimal() +
  labs(title = "Density of Fires within Boundary",
       x = "Longitude",
       y = "Latitude")
```

## Combining your Climate and Events Data
In this section you will combine the Climate and Events data by adding the density values from the Events dataset to the polygons in the interpolated surface.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Perform the spatial join
joined_data <- st_join(idw_clipped, density_sf, join = st_intersects)

# Select needed columns
final_data <- joined_data[, c("var1.pred", "fires")]

# Rename column
final_data <- final_data %>%
  rename(temperature = var1.pred)

# Replace NA values in the fires column with 0
final_data <- final_data %>%
  mutate(fires = ifelse(is.na(fires), 0, fires))

# Create the map
ggplot(data = final_data) +
  geom_sf(aes(fill = fires)) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Temperature Map",
       fill = "Temperature (°C)") +
  theme(legend.position = "right")

# Save final_data as a shapefile
st_write(final_data, "final_data.shp", delete_dsn = TRUE)

# Convert final_data to a data frame
final_data_df <- st_drop_geometry(final_data)

# Write as CSV
write.csv(final_data_df, "final_data.csv", row.names = FALSE)
```

## Performing Oridnary Least Squares Regression
In this section you will perform an Ordinary Least Squares Regression to assess if your climate variable is able to explain the variability in the density of events at a "global" scale.


```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}

# Step 1: Read the shapefile
final_data_sf <- st_read("final_data.shp")

# Step 2: Fit the OLS regression model on the entire spatial data
# Use "temprtr" instead of "temperature"
ols_model <- lm(fires ~ temprtr, data = final_data_sf)

# Step 3: Add residuals to the original spatial data frame
final_data_sf$residuals <- resid(ols_model)

# Step 4: Inspect the updated spatial object to verify residuals are added
print(head(final_data_sf))

# Step 5: (Optional) Save the updated shapefile with residuals
st_write(final_data_sf, "final_data_with_residuals.shp", delete_dsn = TRUE)

# Step 6: Create a map of residuals from the OLS regression
ggplot(data = final_data_sf) +
  geom_sf(aes(fill = residuals)) + # Map the residuals to fill color
  scale_fill_viridis_c(option = "C", name = "Residuals") + # Use a color scale
  theme_minimal() +
  labs(title = "Map of Residuals from OLS Regression",
       fill = "Residuals") +
  theme(legend.position = "right")

# Optional: Save the plot if desired
ggsave("residuals_map.png", width = 10, height = 8, dpi = 300)
```

## Performing Geographically Weighted Regression
In this section you will perform Geographically Weighted Regression to assess if your climate variable is able to explain the variability in the density of events at local scales. 
```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Read the multipolygon shapefile (with residuals included)
final_data_sf <- st_read("final_data.shp")

# Preview the data to check variable names and content
print(head(final_data_sf))
print(colnames(final_data_sf))

# Convert the sf object to Spatial object
final_data_sp <- as_Spatial(final_data_sf)

# Create neighborhood structure
neighbors <- poly2nb(final_data_sp, queen = TRUE)

# Check neighbors for any issues
print(summary(neighbors))

# Check for any empty neighbors
if (any(sapply(neighbors, length) == 0)) {
  warning("Some polygons have no neighbors. This may cause issues for GWR.")
}

# Prepare the dependent and independent variables
dependent_var <- final_data_sp@data$fires
independent_vars <- final_data_sp@data$temprtr

# Check if both variables are numeric
if (!is.numeric(dependent_var) || !is.numeric(independent_vars)) {
  stop("Dependent and independent variables must be numeric.")
}

# Run GWR with a fixed bandwidth of 200 km
fixed_bandwidth <- 200000  # Bandwidth in meters (200 km)

gwr_model_fixed <- gwr(dependent_var ~ independent_vars, 
                       data = final_data_sp, 
                       bandwidth = fixed_bandwidth, 
                       se.fit = TRUE)

# Validate that the model ran successfully
if (is.null(gwr_model_fixed)) {
  stop("The GWR model did not return any results.")
}

if (is.null(gwr_model_fixed$SDF)) {
  stop("The GWR model SDF is NULL, indicating it might not have calculated properly.")
}

# Print GWR summary
print(summary(gwr_model_fixed))

# Extract coefficients and create a dataframe for visualization
gwr_results_fixed <- as.data.frame(gwr_model_fixed$SDF)

# Step 11: Check the GWR results and confirm the structure
print(head(gwr_results_fixed))  # Display first few rows to inspect
print(colnames(gwr_results_fixed))  # Verify the column names in the GWR output

# Step 12: Extracting coordinates from the original spatial data
coordinates_fixed <- st_coordinates(final_data_sf)  # Get coordinates from the original data

# Check the structure of the coordinates
print(head(coordinates_fixed))  # Show first few coordinates
print(dim(coordinates_fixed))    # Confirm dimensions

# Step 13: Combine the GWR results with the coordinates
# Assuming GWR results correspond directly (else we may need to adjust identifiers),
# Make sure to bind them under the known column names for proper mapping.
gwr_results_fixed <- cbind(gwr_results_fixed, coordinates_fixed)

# Check the structure of combined results
print(colnames(gwr_results_fixed))  # Check if columns are correct
print(head(gwr_results_fixed))       # Check combined result head

# Now convert to an sf object for visualization
# Adjusting the coordinate column names based on what exists in gwr_results_fixed
# Normally, standard output names would have been “coords.X1” and “coords.Y” or similar
gwr_output_sf_fixed <- st_as_sf(gwr_results_fixed, coords = c("X", "Y"), crs = st_crs(final_data_sf))

# Plotting GWR coefficients with the fixed bandwidth
ggplot(data = gwr_output_sf_fixed) +
  geom_sf(aes(fill = Estimate), color = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "GWR Coefficients with Fixed Bandwidth of 200 km",
       fill = "GWR Estimate") +
  theme_minimal()

# Optional: Save the plot
ggsave("gwr_coefficients_fixed_bandwidth.png", width = 10, height = 8, dpi = 300)

```
