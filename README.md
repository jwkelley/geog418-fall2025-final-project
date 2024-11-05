# geog418-fall2024-final-project
Final Project Instructions and Code



Welcome to your final project for Geog 418. This project will provide you with an opportunity to utilize the skills gained in the first half of this course along with some new ones to provide a spatial analysis of climate-driven events in British Columbia.&nbsp;

## Learning Outcomes
After completing this project, you will be able to:
- Download and clean climate data and create spatially interpolated surfaces
- Perform geographically weighted regression to estimate the influence of climate on natural events
- Utilize all spatial analysis methods used in this course
- Employ ChatGPT or Microsoft Pilot for solving coding problems


## Instructions
### A. Define Your Topic

You must select a topic that allows you to perform an analysis on two variables: (1) point locations of some climate-mediated event (e.g. wildfires, landslides, deaths to heat exposure), and (2) climate data from point locations such as weather stations. For example, you could analyze how the number of days of 25 degrees Celsius influences wildfire occurrence. It is recommended that your project focus on the province of British Columbia.

### B. Collect and Analyze Point Data of Climate-Driven Event

Collect point data for your "events" variable (e.g. wildfires, landslides, etc.). It is recommended that you search the BC Data Catalogue for this dataset unless you know of other credible sources. Apply descriptive statistics, spatial descriptive statistics, and point pattern analysis to describe the nature of these events for a specific timeframe.


### C. Collect and Process Climate Data
Collect point data for your climate variable (e.g. mean temperature, min temperature, daily maximum precipitation, etc.). This data must be point data. It is recommended that you collect this data from the Weather Station Data Portal from the Pacific Climate Impacts Consortium (PCIC).

Clean this data so that you have each weather station represented by a single value for a specific variable. For example, if you are interested in the influence of temperature on wildfires, you will want to download daily or monthly temperatures and clean the data to provide a single value, such as mean temperature during the wildfire season, average minimum daily temperature, etc.

### D. Create Spatially Interpolated Climate Surfaces
Once your data is cleaned, perform both inverse distance weighting and kriging to create spatially interpolated surfaces of your climate data. As part of your analysis you should evaluate the quality of each method based on the objective of your analysis.

### E. Estimate Influence of Climate on Event Occurrence
Perform a regression analysis to determine the degree to which your climate variable is able to explain the variability in your event occurrence. Perform a test of spatial autocorrelation on the residuals from your regression model to determine if your analysis is negating a primary assumption with performing a regression analysis.&nbsp;

You will likely find the you are negating a primary assumption, which will lead you to perform a geographically weighted regression. From this analysis, make conclusions about the influence of climate on your events.

### F. Deliverable
You have two options for creating your deliverable:
- Submit a brief scientific report the follows the general flow of a scientific paper (Introduction, Methods, Results, Conclusion).&nbsp;
- Submit a tutorial that walks a reader through all the steps you completed to perform this analysis.

You have two options for how you submit your deliverable:
- An PDF of a knitted R Markdown file&nbsp;
- A GitHub webpage

Whatever you choose, you must also submit your code and data, and we must be able to run your code with your data on our computer.&nbsp;

### G. Data Processing
You will be provided with R code for the following:
- cleaning and mapping weather station data from PCIC
- performing IDW and kriging
- performing regression and geographically weighted regression

This code will not be perfect, and you will need to fix multiple things to create professional looking outputs and accurate results. You are encouraged to utilize ChatGPT and/or Microsoft Pilot to learn how to best fix these issues. We will discuss how to do this effectively and responsibly in lecture.

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

