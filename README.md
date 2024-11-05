# geog418-fall2024-final-project
Final Project Instructions and Code



Welcome to your final project for Geog 418. This project will provide you with an opportunity to utilize the skills gained in the first half of this course along with some new ones to provide a spatial analysis of climate-driven events in British Columbia.&nbsp;

## Learning Outcomes
After completing this project, you will be able to:
download and clean climate data and create spatially interpolated surfaces
perform geographically weighted regression to estimate the influence of climate on natural events
utilize all spatial analysis methods used in this course
employ ChatGPT or Microsoft Pilot for solving coding problems


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

```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#####
##Read in and clean data
VanCity <- readOGR(".", 'local-area-boundary', verbose = FALSE)

```

## Study Area
As mentioned above, this study focuses on the distribution of bicycle theft and mischief crimes in Vancouver, BC. A map of the study area and crime points are shown below for reference (Figure 1).

```{r Study Area Map, echo=FALSE, eval=TRUE, warning=FALSE, fig.cap="Selected vancouver crimes in 2020."}
map_Crime <- tm_shape(VanCity) + #make the main shape
  tm_polygons(col = "gray50", border.col = "black") +  #fill polygons
  tm_shape((kma_crime2)) +
  tm_symbols(size = 0.05, col = "blue", shape = 20, alpha = 0.25) +
  tm_shape((kma_crime1)) +
  tm_symbols(size = 0.05, col = "pink", shape = 20, alpha = 0.25) +
  tm_layout(title = "Vancouver Crimes 2020", 
            title.position = c("LEFT", "BOTTOM"),
            legend.position = c("LEFT", "TOP")) +
  tm_add_legend(type = "symbol", 
              labels = c("Mischief", "Theft of bicycle"), 
              col = c(adjustcolor( "blue", alpha.f = 0.5), 
                      adjustcolor( "pink", alpha.f = 0.5)), 
              shape = c(20,20))

map_Crime
```

## Data

Data for this study were collected from the Vancouver Police Department’s crime database, retrieved from https://geodash.vpd.ca/opendata/. This dataset has been updated weekly since 2003 and contains a variety of information about the crime such as the crime type, the GPS location at which it occurred, the name of the 100 block that it occurred on, and the time at which it occurred, measured to the minute [6].
