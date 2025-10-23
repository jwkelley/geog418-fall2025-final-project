# Geog 418 Final Project Code

This document contains all the code you will need to complete for final project except for the code you have already used in Assignmnets 1-3. The code is divided into sections based on the major tasks you need to complete for the project:

1. [Cleaning your climate data](#cleaning-your-climate-data)
2. [Mapping your climate data](#mapping-your-climate-data)
3. [Spatial interpolation of your climate data](#spatial-interpolation-of-your-climate-data)
4. [Creating a density map of your events data](#creating-a-density-map-of-your-events-data)
5. [Combining your climate and events data](#combining-your-climate-and-events-data)
6. [Performing ordinary least squares regression](#performing-oridnary-least-squares-regression)
7. [Performing geographically weighted regression](#performing-geographically-weighted-regression)

Note that you are not provided with code to perform descriptive statistics, spatial descriptive statistics, and point pattern analysis. You should perform these analyses on your input data to provide some context to your study. In addition, you are not provided with code for performing a Global Moran's I test, which you should be performing on the residuals from your OLS regression to evaluate if you are negating any important statistical assumptions (i.e. independence of errors).

Before you move on to the code, please read over these tips to make your life easier:
- Make sure you are replacing file names with those that are relevant to your data
- Each chunk of code ends with printing a shapefile and/or csv file, and each subequent chunk begins with reading that specific file. The reason we do this is so that you do not need to re-run all your code to run a specific chunk. However, this requires you to make sure that you are using consistent file names when printing and reading these files (this should make more sense when you look at the code).
- You are responsible for setting values for the resolution of your datasets (e.g. your interpolated surface and density map). You should make sure your selected resolution makes sense given the phenomenon you are investigating and given the methods you are using. Also, you should ensure that you are using a consistent resolution for your different methods.
- Finally, this is a challenging project with no single right outcome. It is your responsibility to get your code to work as best as possible and provide a write-up based on the instructions in Brightspace that best descirbes the work you accomplished. Be critical of the methods you use and the results you obtain, and clearly articulate your findings and what the reader needs to be aware of when understanding your work.

# Code
## Cleaning Your Climate Data
You are responsible for downloading at least one climate/weather variable from [PCIC's Weather Station Data Portal](https://services.pacificclimate.org/met-data-portal-pcds/app/) across multiple stations. It is recommended that you select a variable and station sources (e.g. EC = Environment Canada) so that you have a representation of points distributed across the province. You will also need to select an appropriate date range for your study. Do not include stations with no observations. Once you have selected your data, select the Station Data tab and check "Clip time series to filter data range". Download the time series as a CSV. **Please note that this will take a long time**. The example below shows a selection for hourly temperatures from the EC and EC_raw stations for the 2023 fire season.

<img width="2543" height="941" alt="image" src="https://github.com/user-attachments/assets/e89ec952-2119-42a5-8641-d3f40c3aba31" />

Once complete, you should see a folder with multiple CSV files, each one pertaining to a different station. Before processing this data, download the stataion metadata by selecting the Station Metadata tab, select "By History", and select download. 

<img width="708" height="498" alt="image" src="https://github.com/user-attachments/assets/2db7143b-5100-4ff1-ac2c-1e3d288b63aa" />

Now that you have downloaded the climate data, you will want to create a shapefile of your weather stations that contains your weather variable. Here is the code to do this:

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#Here are many of the libraries you will need. 
#You may need to add more if your receive an error 
#message saying that a specific function cannot be found.
library(spdep)
library(sf)
library(dplyr)
library(gstat)
library(ggplot2)
library(spgwr)
library(tmap)
library(stars)
library(spatstat)
library(lubridate)


#Set variable for working directory
dir <- "C:/Users/Jason/OneDrive - University of Victoria/Geog418_202509/FinalProject/"


#Here we will prep a template file to add the climate data summaries for each station

#Create an empty data frame with the needed columns. 
#You will use this to write your weather station data to when you want to 
#Note that AirTemp refers specificlly to my variable of interest
data <- data.frame(Native.ID = character(), AirTemp = numeric(),
                   stringsAsFactors = FALSE)

csv_file_name <- "BC_AVG_TEMP.csv" #We are giving the file this name as an example of calculating average temperatures in BC.

#First, list all CSV files in the directory to make sure you are in the right folder. The folder we are looking at here is called BCH as this is the weather station network from which the data was obtained in PCIC.
csv_files <- list.files(path = paste0(dir,"/Data/EC_raw/"), 
                        pattern = "\\.csv$", 
                        full.names = TRUE)

```

You will have a CSV file for each weather station. You will need to perform some calculation on the data in each CSV. An easy way to do this is to use a for loop to run through every CSV file in a folder and apply the same statistics. The code below is an example of a single loop for you to test.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
########################################################
### This section will help you setup the loop         ##
### Practice with a single file to make sure          ##
### you are getting the results you need then         ##
### copy the final code into the for loop below       ##
### Note the chages between the practice version here ##
### and the final version in the loop below           ##
########################################################
file <- csv_files[1] #grab the first file to test
file_path <- file

#Read file
hourly_data <- read.csv(file, skip = 1, header = TRUE)

#Adjust the date/time column so that it is usable in calculations
hourly_data$time <- lubridate::ymd_hms(hourly_data$time) 
hourly_data$Year <- lubridate::year(hourly_data$time)
hourly_data$Month <- lubridate::month(hourly_data$time)
hourly_data$Day <- lubridate::yday(hourly_data$time)

#Convert your variable column (I call it AirTemp here) to numeric and remove NA's
hourly_data$AirTemp <- as.numeric(hourly_data$air_temperature)
hourly_data <- hourly_data %>%
  filter(!is.na(AirTemp))

#Here is an example of how to calculate daily average temperature from the hourly data.
daily_avg_temp <- hourly_data %>%
  dplyr::group_by(Day) %>%
  dplyr::summarise(daily_avg_temp = mean(AirTemp, na.rm = TRUE))

# We can average this over the season
Season_avg_Daily <- mean(daily_avg_temp$daily_avg_temp)

# # Display the daily average temperatures
print(Season_avg_Daily)

#Here is an example of how to calculate monthly average temperature
monthly_avg_temp <- hourly_data %>%
  dplyr::group_by(Year, Month) %>%
  dplyr::summarise(monthly_avg_temp = mean(AirTemp, na.rm = TRUE)) %>%
  ungroup()  # Ungroup for any future modifications

#averaged over the season
Season_avg_monthy <- mean(monthly_avg_temp$monthly_avg_temp)

# Display the monthly average temperatures
print(Season_avg_monthy)

# #What if we want to calculate an average for a specific subset, 
#such as the middle of fire season?
# Filter for the months from July and August
average_temp_July_August <- hourly_data %>%
  dplyr::filter(Month >= 7 & Month <= 8) %>%
  dplyr::summarise(TEMP = mean(AirTemp, na.rm = TRUE))  # Replace 'TEMP' with your column name

# Display the average temperature
print(average_temp_July_August)

#For the example below we will use season mean temp
season_mean_temp <- hourly_data %>%
  dplyr::summarise(AirTemp = mean(AirTemp, na.rm = TRUE))

#Extract the filename as this is the name of your weather station.
file_name <- basename(file)

#Remove the file extension - This will be how the stations are named in the metadata file
file_name_no_ext <- sub("\\.[^.]*$", "", file_name)

# Display the result
print(file_name_no_ext)

#Round the temperature values to two decimals
Roundedtemp <- round(season_mean_temp,2)

#Now, add your weather station and temperature values to the file
new_values <- data.frame(Native.ID = file_name_no_ext, 
                         AirTemp = Roundedtemp, 
                         stringsAsFactors = FALSE)

data <- bind_rows(data, new_values)

#Check your data to make sure that the row has been added.
print(head(data))

#########################################################
# Go back and repeat for file <- csv_files[2] to see if # 
# everything works as expected. If so you can copy your #
# code into the loop below.                             #
#########################################################
```

The code below is the full loop. Any changes that you made above make sure are reflected in the full loop below.

```{r Data Cleaning 2, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Make sure to reset the 'data' object
# so that your practice is not in the final result
data_final <- data.frame(Native.ID = character(), AirTemp = numeric(),
                         stringsAsFactors = FALSE)


#Next, loop through each CSV file and perform your calculations to calculate something about this variable. Here we calculate daily, monthly, and season average temperatures. 
for (file in csv_files) {
  # file <- csv_files[2] #WE DONT NEED THIS LINE INSIDE THE LOOP
  file_path <- file
  
  #Read file
  hourly_data <- read.csv(file, skip = 1, header = TRUE)
  
  #Adjust the date/time column so that it is usable in calculations
  hourly_data$time <- lubridate::ymd_hms(hourly_data$time) 
  hourly_data$Year <- lubridate::year(hourly_data$time)
  hourly_data$Month <- lubridate::month(hourly_data$time)
  hourly_data$Day <- lubridate::yday(hourly_data$time)
  
  #Convert your variable column (I call it AirTemp here) to numeric and remove NA's
  hourly_data$AirTemp <- as.numeric(hourly_data$air_temperature)
  hourly_data <- hourly_data %>%
    filter(!is.na(AirTemp))
  
  #For the example below we will use season mean temp
  season_mean_temp <- hourly_data %>%
    dplyr::summarise(AirTemp = mean(AirTemp, na.rm = TRUE))
  
  #Extract the filename as this is the name of your weather station.
  file_name <- basename(file)
  
  #Remove the file extension - This will be how the stations are named in the metadata file
  file_name_no_ext <- sub("\\.[^.]*$", "", file_name)
  
  # Display the result
  print(file_name_no_ext)
  
  #Round the temperature values to two decimals
  Roundedtemp <- round(season_mean_temp,2)
  
  #Now, add your weather station and temperature values to the file
  new_values <- data.frame(Native.ID = file_name_no_ext, 
                           AirTemp = Roundedtemp, 
                           stringsAsFactors = FALSE)
  
  data_final <- bind_rows(data_final, new_values) #NOTE WE HAVE CHANGED TO THE data_final OBJECT
}
```

In your empty CSV, you want to write your caluclations and include the weather station that it came from. You need to do this so that you can later join this CSV file with a shapefile of your weather station locations.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#Check your data to make sure that the rows have been added.
print(head(data_final))

#Save the updated data frame back to a new CSV file
output_file_path <- paste0(dir, csv_file_name)
write.csv(data_final, file = output_file_path, row.names = FALSE)

#Read in the output climate data
climatedata <- read.csv(paste0(dir,"BC_AVG_TEMP.csv"))

#There is some duplicates in the metadata so we will filter the data to remove duplicates
metadata <- read.csv(paste0(dir,"Data/station-metadata-by-history.csv"))
metadata$Record.EndDate <- lubridate::ymd(metadata$Record.End)
metadata <- subset(metadata, metadata$Record.EndDate >= "2023-03-30")

#Merge the climate data for each station with the location data found in the metadata file
merged_data <- merge(metadata, climatedata, by = "Native.ID")

#Omit NA's
merged_data <- na.omit(merged_data)

#Look at the range of temperature data
range(merged_data$AirTemp)

#If there are erroneous temperature valuesn filter data to remove these.
merged_data <- merged_data[merged_data$AirTemp <= 100, ]

#Write the dataset so that it  is stored
write.csv(merged_data, file = paste0(dir,"ClimateData.csv"), row.names = FALSE)
```
We finish this last chunk of code by writing our ClimateData.csv file so that we have a copy saved. You can now use this dataset to create a shapefile of weather stations.

## Mapping your Climate Data
In this step you will open your ClimateData.csv file, create a shapefile, and then create a map of the data so you can see if the data has been cleaned as expected.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Read the CSV file
climate_data <- read.csv(paste0(dir,"ClimateData.csv"))

# Ensure Latitude and Longitude columns are correctly formatted
# Assuming the columns are named "Latitude" and "Longitude"
climate_data <- climate_data %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Create a simple feature object (sf) using Latitude and Longitude
climate_sf <- st_as_sf(climate_data, coords = c("Longitude", "Latitude"), crs = 4326)

#Transform to a better CRS
climate_sf <- st_transform(climate_sf, crs = 3005)

# Optionally, you can clean up your data by selecting only the columns 
# that you want to keep in the shapefile
colnames(climate_sf)
head(climate_sf)
climate_sf <- climate_sf %>% select("Native.ID", "Station.ID", "Station.Name",
                                    "Elevation..m.", "AirTemp")

# Write the shapefile to disk
st_write(climate_sf, paste0(dir,"ClimateData.shp"), append = FALSE)

# Confirmation message
print(paste0("Shapefile has been created: ClimateData.shp @ ", paste0(dir,"ClimateData.shp")))

# Load the shapefiles
climate_sf <- st_read(paste0(dir,"ClimateData.shp"))

bc_boundary <- st_read(paste0(dir,"Data/BC.shp"))

bc_boundary <- st_transform(bc_boundary, crs = 3005)

# Create the map
tm_shape(bc_boundary) +
  tm_polygons(
    fill = "lightgrey",
    col = "black"
  ) +
  tm_shape(climate_sf) +
  tm_symbols(
    fill = "AirTemp",
    size = 0.4,
    fill.scale = tm_scale(values = "-brewer.rd_bu"),
    fill.legend = tm_legend(title = "Temperature (°C)")
  ) +
  tm_title("Map of Climate Data Points in British Columbia") +
  tm_layout(
    legend.position = c("right", "center"),
    frame = FALSE
  )

```

## Spatial Interpolation of Your Climate Data
In this section you will create an interpolated surface of your weather data variable. You can use either inverse distance weighting or kriging to accomplish, or use both methods and compare the results between them.

### Inverse Distance Weighting
```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#############################
###         IDW           ###
#############################
#Setup a template to be used for all surfaces
res    <- 10000                     # meters
rasCRS <- st_crs(bc_boundary)      # projected CRS (e.g., EPSG:3005)
bb     <- st_bbox(bc_boundary)

# Regular empty grid over the boundary bbox
tmpl <- st_as_stars(bb, 
                    dx = res, 
                    dy = res, 
                    crs = rasCRS,
                    values = 0)

names(tmpl) <- "blank"

#Save Template
write_stars(tmpl, file.path(dir, "grid_1km_template.tif"))


#Read the shapefile
climate_data <- st_read(paste0(dir,"ClimateData.shp"))


#Use the template grid for the interpolation.
grid <- read_stars(file.path(dir, "grid_1km_template.tif"))

Power <- 2
nb = 10

#Interpolate using IDW
idw_result <- gstat::idw(AirTemp ~ 1, #Formula 
                         locations = climate_data, #Sample points
                         newdata = grid, #Output grid
                         idp = Power, #P level
                         nmax = nb) #Number of neighbours

tm_shape(idw_result) +
  tm_raster(col = "var1.pred", 
            col.scale = tm_scale_continuous(values = "viridis"),
            col.legend = tm_legend(title = "IDW Temp (°C)")) +
tm_shape(bc_boundary) + 
  tm_polygons(col = "red",
              fill_alpha = 0.1)

```

You may want a way to evaluate how the parameters of IDW impact your results. In the code below we perform leave-one-out cross validation to estimate how the surface changes as different points are left out.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
nb <- 10
Power <- 2

cv <- numeric(nrow(climate_data))
  
for(i in 1:nrow(climate_data)){
  train <- climate_data[-i,]
  test <- climate_data[i,]
  
  estimate <- gstat::idw(AirTemp ~ 1, #Formula 
                         locations = train, #Sample points
                         newdata = test, #Output grid
                         idp = Power, #P level
                         nmax = nb,
                         debug.level = 0) #Number of neighbours
  
  cv[i] <- estimate$var1.pred
}
result <- data.frame(Obs = climate_data$AirTemp, Pred = cv)
result$Resid <- result$Obs - result$Pred
RMSE <- sqrt(mean(result$Resid^2))
bias <- mean(result$Resid)

ggplot(result, aes(x = Obs, y = Pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", col = "grey") +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  xlim(5,20) +
  ylim(5,20) +
  xlab("Observed (°C)") +
  ylab("Predicted (°C)") +
  annotate( "text", x = -Inf, y = Inf,
            hjust = -0.1, vjust = 1.1,
            label = paste0("RMSE = ", round(RMSE, 2), "  Bias = ", round(bias, 2))
  ) +
  theme_classic()
```

You can use this to test a range of neighbour values (nb) and Power values. After you have a final IDW surface you might want to clip it to the BC polygon and map it.
```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Check the CRS of both objects
crs_idw <- st_crs(idw_result)  # CRS of IDW result
crs_polygon <- st_crs(bc_boundary)  # CRS of the polygon shapefile

# Transform the CRS of either shapefile if they do not match
if (crs_idw != crs_polygon) {
  # Transform the IDW result to match the CRS of the polygon
  idw_result <- st_transform(idw_result, crs = crs_polygon)  # Transform IDW result to polygon's CRS
  message("Transformed IDW result CRS to match the polygon.")
} else {
  message("CRS of IDW result and polygon already match.")
}

#Clip to the BC boundary
idw_clipped <- idw_result[bc_boundary]

#Create the map of the clipped results
map <- tm_shape(bc_boundary) + 
  tm_polygons(fill = "grey90", 
              col = "grey40") +
  tm_shape(idw_clipped) +
  tm_raster("var1.pred", 
            col.scale = tm_scale_continuous(values = "viridis"),
            col.legend = tm_legend(
              title = "IDW Pred Temperature (°C)",
              position = tm_pos_out("right", "center"))) +
  tm_layout(frame = FALSE)

#Save the map as an image file (optional)
tmap_save(map, filename = file.path(dir, "Clipped_IDW_Interpolation_Map.png"))
write_stars(idw_clipped["var1.pred"], file.path(dir, "idw_airtemp_1Km.tif"))
```

### Kriging
This section outlines the Ordinary Kriging portion of the spatial interpolation.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#####################################
###         Kriging               ###
#####################################
# Read the shapefile
climate_data <- st_read((paste0(dir,"ClimateData.shp")))

#We need to identify and deal with any duplicated points
dup <- duplicated(st_coordinates(climate_data)) #Look for any coordinates that are exact
sum(dup) #If this is greater than 0 then you have some duplicated points

#Add coordinate columns
climate_data$X <- st_coordinates(climate_data)[,1]
climate_data$Y <- st_coordinates(climate_data)[,2]

#calculate the mean variable for any duplicated points
means <- climate_data %>%
  st_drop_geometry() %>%
  group_by(X, Y) %>%
  summarise(AirTemp = mean(AirTemp, na.rm = TRUE), .groups = "drop")

#Keep all other data from the first occurance of the duplicated station
firsts <- climate_data %>%
  group_by(X, Y) %>%
  slice(1) %>%
  ungroup()

climate_data_nodup <- firsts %>%
  select(-AirTemp) %>%
  left_join(means, by = c("X", "Y"))

#Start with setting up an empty raster template
grid <- read_stars(file.path(dir, "grid_1km_template.tif"))

# Create variogram. Be sure to test out the three different models.
var.smpl <- variogram(AirTemp ~ 1, climate_data_nodup, cloud = FALSE,
                      cutoff = 500000, width = 25000) 
plot(var.smpl)


dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(model="Gau", nugget = 0.4, psill = 4.5, 
                              range = 180000))

plot(var.smpl, dat.fit)

dat.krg <- krige(AirTemp ~ 1, climate_data_nodup, grid, dat.fit, debug.level=0)



krgResult <- dat.krg[bc_boundary]


map_krg <- tm_shape(bc_boundary) + 
  tm_polygons(fill = "grey90", 
              col = "grey40") +
  tm_shape(krgResult) +
  tm_raster("var1.pred", 
            col.legend = tm_legend(
              "KRG Pred Temperature (°C)",
              position = tm_pos_out("right", "center")),
            col.scale = tm_scale_continuous(values = "viridis")) +
  tm_layout(frame = FALSE)

tmap_save(map_krg, 
          filename = file.path(dir, "Clipped_OK_Interpolation_Map.png"))

map_krg_var <- tm_shape(bc_boundary) + 
  tm_polygons(fill = "grey90", 
              col = "grey40") +
  tm_shape(krgResult) +
  tm_raster("var1.var", 
            col.legend = tm_legend(
              "KRG Variance (°C)",
              position = tm_pos_out("right", "center")),
            col.scale = tm_scale_continuous(values = "viridis")) +
  tm_layout(frame = FALSE)

tmap_save(map_krg_var, 
          filename = file.path(dir, "Clipped_OK_Interpolation_Var.png"))


write_stars(krgResult["var1.pred"], file.path(dir, "ok_airtemp_1Km.tif"))
write_stars(krgResult["var1.var"], file.path(dir, "ok_variance_1Km.tif"))
```

## Creating a Density Map of Your Events Data
In this section you will create a raster dataset of the density of points per unit area across the province. You will convert your point shapefile into a density raster showing the number of events per raster cell. Be mindful that the resolution you select here should match the resolution of your spatial interpolation outputs.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
########################################################################
### Density Surface                                                  ###
########################################################################
#Load the province polygon
bc <- st_read(file.path(dir,
                        "Data/BC.shp"))

#Load the fire data
firePoints <- st_read(file.path(dir,
                                "Data/H_FIRE_PNT_point.shp"))
#Add date columns to data
firePoints$Date <- as.Date(firePoints$IGN_DATE, format = "%Y%m%d")
firePoints$Year <- year(firePoints$Date)
firePoints$Month <- month(firePoints$Date)
firePoints$Day <- yday(firePoints$Date)

#Subset to the time of your analysis
firePoints <- subset(firePoints, firePoints$Year == 2023)
firePoints <- subset(firePoints, firePoints$Day >= 91 & firePoints$Day <= 304)

#Subset to fires >= 0.25 ha
firePoints <- subset(firePoints, firePoints$SIZE_HA >= 0.25)

firePoints$X <- st_coordinates(firePoints)[,1]
firePoints$Y <- st_coordinates(firePoints)[,2]

# Create the map
tm_shape(bc_boundary) +
  tm_polygons(
    fill = "lightgrey",
    col = "black"
  ) +
  tm_shape(firePoints) +
  tm_symbols(
    fill = "red",
    size = 0.4,
    ) +
  tm_title("Map of Fire Data Points in British Columbia") +
  tm_layout(
    frame = FALSE
  )

#Start with setting up an empty raster template
grid <- read_stars(file.path(dir, "grid_1km_template.tif"))

kma_ext <- as.matrix(st_bbox(grid))

#Now create the observation window
window <- as.owin(list(xrange = c(kma_ext[1], kma_ext[3]),
                       yrange = c(kma_ext[2], kma_ext[4])))

#Finally, create a ppp object for doing the KDE.
kma1.ppp <- ppp(x = firePoints$X, y = firePoints$Y, window = window)

#Sigma estimation
bw.d1 <- bw.diggle(kma1.ppp)

#KDE Surface
kde.bwo1 <- density(kma1.ppp, sigma = bw.d1, 
                    at = "pixels", eps = c(1000, 1000))

#Convert to points per km2 instead of m2
kde_km2 <- kde.bwo1 * 1000000

#Convert to a raster data type
kde_st <- st_as_stars(kde_km2)

#Add the CRS back
st_crs(kde_st) <- st_crs(firePoints)

#Clip to the BC polygon
kde_st <- kde_st[bc]

#Make a map
map_kde <- tm_shape(bc_boundary) + 
  tm_polygons(fill = "grey90", 
              col = "grey40") +
  tm_shape(kde_st) +
  tm_raster("v", 
            col.legend = tm_legend(
              "Fire Density \n(# pnts/Km2)",
              position = tm_pos_out("right", "center")),
            col.scale = tm_scale_continuous(values = "viridis")) +
  tm_layout(frame = FALSE)
```

## Combining Your Climate and Events Data
In this section you will combine the Climate and Events data by adding the density values from the Events dataset to the polygons in the interpolated surface.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#Stack and align the Density layer and the interpolated surface
stack <- c(st_warp(idw_clipped, dest = grid, method = "near"),
           st_warp(kde_st, dest = grid, method = "near"))

final_data_sf <- st_as_sf(stack, as_points = TRUE, merge = FALSE, na.rm = FALSE)
colnames(final_data_sf) <- c("Temperature", "TemperatureVar", "FireDensity", "geometry")
final_data_sf <- final_data_sf %>% filter(!is.na(Temperature) & !is.na(FireDensity))
st_write(final_data_sf, file.path(dir, "final_data.shp"), append = FALSE)
```

## Performing Oridnary Least Squares Regression
In this section you will perform an Ordinary Least Squares Regression to assess if your climate variable is able to explain the variability in the density of events at a "global" scale.


```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Fit the OLS regression model on the entire spatial data
# Use "temprtr" instead of "temperature"
ols_model <- lm(FrDnsty ~ Temprtr, data = final_data_sf)
summary(ols_model)

par(mfrow = c(2,2))
plot(ols_model)
par(mfrow = c(1,1))

final_data_sf$Pred <- ols_model$fitted.values
final_data_sf$Resid <- ols_model$residuals

ggplot(final_data_sf, aes(x = FrDnsty, y = Pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", col = "grey") +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  xlim(0,0.006) +
  ylim(0,0.006) +
  xlab("Observed (°C)") +
  ylab("Predicted (°C)") +
  annotate( "text", x = -Inf, y = Inf,
            hjust = -0.1, vjust = 1.1,
            label = paste0("RMSE = ", round(RMSE, 2), "  Bias = ", round(bias, 2))
  ) +
  theme_classic()


# (Optional) Save the updated shapefile with residuals
st_write(final_data_sf, "final_data_with_residuals.shp", delete_dsn = TRUE)

# Create a map of residuals from the OLS regression
tm_shape(bc) + 
  tm_polygons(fill = "grey90", 
              col = "grey40") +
  tm_shape(final_data_sf) +
  tm_symbols(fill = "Resid",
             col_alpha = 0,
             fill_alpha = 0.6,
             size = 0.4,
             fill.scale = tm_scale(values = "-brewer.rd_bu"),
             fill.legend = tm_legend(title = "Fire Density \n(pnts/Km2)")) +
  tm_layout(frame = FALSE)
```

You will then want to perform an analysis of spatial autocorrelation on the OLS residuals. Here is some code to show how to do a Global Moran's I with point data.

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Create neighborhood structure
knn <- knearneigh(st_coordinates(final_data_sf), k = 8, longlat = FALSE)
nb <- knn2nb(knn, sym = TRUE)
lw_knn <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Check neighbors for any issues
print(summary(nb))

mi <- moran.test(final_data_sf$Resid, lw_knn, zero.policy = TRUE)
```

## Performing Geographically Weighted Regression
In this section you will perform Geographically Weighted Regression to assess if your climate variable is able to explain the variability in the density of events at local scales. 
```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
# Check for any empty neighbors
if (any(sapply(nb, length) == 0)) {
  warning("Some polygons have no neighbors. This may cause issues for GWR.")
}

# Prepare the dependent and independent variables
dependent_var <- final_data_sf$FrDnsty
independent_vars <- final_data_sf$Temprtr

# Check if both variables are numeric
if (!is.numeric(dependent_var) || !is.numeric(independent_vars)) {
  stop("Dependent and independent variables must be numeric.")
}

# Run GWR with a fixed bandwidth of 200 km
fixed_bandwidth <- 200000  # Bandwidth in meters (200 km)

gwr_model_fixed <- gwr(dependent_var ~ independent_vars, 
                       data = final_data_sf,
                       coords = st_coordinates(final_data_sf),
                       bandwidth = fixed_bandwidth, 
                       se.fit = TRUE,
                       hatmatrix = TRUE)

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

final_data_sf <- cbind(final_data_sf, gwr_results_fixed)

tm_shape(bc) + 
  tm_polygons(fill = "grey90", 
              col = "grey40") +
  tm_shape(final_data_sf) +
  tm_symbols(fill = "pred",
             col_alpha = 0,
             fill_alpha = 0.6,
             size = 0.4,
             fill.scale = tm_scale(values = "-brewer.rd_bu"),
             fill.legend = tm_legend(title = "Fire Density \n(pnts/Km2)")) +
  tm_layout(frame = FALSE)

```
