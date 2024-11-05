# geog418-fall2024-final-project
Final Project Instructions and Code

---
title: "Geog 418 Final Project"
author: "Geog 418"
date: "05/11/2024"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

##R Markdown requires that you have a an application installed on you machine that can run Latex. If you do not already have Latex on your computer, you can install it direction from R with the following code:

tinytex::install_tinytex()


## For this lab you will need the following libraries: 
##, raster, spatstat, tmap, knitr


library("raster")
library("spatstat")
library("tmap")
library("knitr")
library("sf")
library("ggplot2")
library("spatstat")
library("sp")
library("st")
library("dplyr")
library("plyr")



dir <- "C:/Users/chrisbone/OneDrive - University of Victoria/Courses/Geog 418 Spatial Analysis/2024/Assignment 2"
setwd(dir)

```

## Introduction

Vancouver, British Columbia is seeing a concerning rise in crime in 2020 so far, with violent crimes up 5.6% and commercial break ins up 47.9%, according to the Vancouver Police Department [1]. This rise in crime comes amidst the COVID-19 pandemic, a global event that has forced the re-organization of the daily routines of the many people worldwide. In Vancouver, this re-organization has largely taken the form of people spending more time at home, and businesses reducing their hours or closing altogether. Such shifts in the routine activities of the general public is thought to have a measurable effect on the spatial and temporal distribution of crime, as described by routine activity theory [2]. According to routine activity theory, a crime requires the convergence of a “likely offender”, a “suitable target” and “the absence of capable guardians” in time and space [2]. Given this theory, the increase in crime rates, and the dramatic re-arrangement of routine activities in Vancouver, it is highly relevant that the spatial dynamics of crime be studied in Vancouver at this time.

While there have been several studies conducted on the spatial distribution of crimes in Vancouver in the past [3,4,5], there is a lack of temporally relevant crime studies in the literature. Aside from scientific literature, the other main resource for information about crime in Vancouver is the VPD, who provide regular updates comparing the number of crime incidents from year to year. While these reports are useful, they do not provide significant detail about the spatial aspect of crime data. In addition to the need for updated scientific analysis of crime in Vancouver, there is also a more tangible, practical need for information about the location of recent crimes so as to ensure the most effective distribution of police resources. Without current information about crime hotspots, police may be focusing their energy in the wrong areas, putting public safety at risk.

The objective of this analysis is to provide insight into the spatial distribution of bicycle thefts and mischief crimes in 2020 through several different point pattern analysis tests. Generally speaking, these statistical tests all aim to answer the the following questions:

1. Are bicycle thefts and mischief crimes in Vancouver in 2020 randomly located?
2. If the crimes being studied are not found to be randomly located, what spatial pattern do they have?
3. If the data are clustered, where do those clusters occur?


```{r Data Cleaning, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE}
#####
##Read in and clean data
VanCity <- readOGR(".", 'local-area-boundary', verbose = FALSE)
VanCrime <- read.csv("./crimedata_csv_all_years.csv")

#clean up the columns
VanCrime_Clean <- VanCrime[which(VanCrime$YEAR == 2014),]
# range(VanCrime_Clean$YEAR)
# range(VanCrime_Clean$X)

#omit values with NA
VanCrime_Clean <- na.omit(VanCrime_Clean)
# range(VanCrime_Clean$X)

VanCrime_Clean <- VanCrime_Clean[which(VanCrime_Clean$X > 0), ]
# range(VanCrime_Clean$X)
# range(VanCrime_Clean$Y)

Coords <- VanCrime_Clean[,c("X", "Y")]
crs <- CRS("+init=epsg:32610")

#create a file type called a SpatialPointsDataFrame
VanCrimePoints <- SpatialPointsDataFrame(coords = Coords, data = VanCrime_Clean, proj4string = crs)

#transform the projection of both datasets to ensure that they are the same
VanCrimePoints <- spTransform(VanCrimePoints, CRS("+init=epsg:3005"))
VanCity <- spTransform(VanCity, CRS("+init=epsg:3005"))

#intersect the two datasets
VanCrimePoints <- raster::intersect(VanCrimePoints, VanCity)

#convert the crimes data type to factor
VanCrimePoints@data$TYPE <- as.factor(VanCrimePoints@data$TYPE)
# levels(VanCrimePoints$TYPE)

kma_crime1 <- VanCrimePoints[which(VanCrimePoints$TYPE == "Mischief"),]
kma_crime1$x <- coordinates(kma_crime1)[,1]
kma_crime1$y <- coordinates(kma_crime1)[,2]

kma_crime2 <- VanCrimePoints[which(VanCrimePoints$TYPE == "Break and Enter Residential/Other"),]
kma_crime2$x <- coordinates(kma_crime2)[,1]
kma_crime2$y <- coordinates(kma_crime2)[,2]

#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates
zd <- zerodist(kma_crime1)
# zd

zd <- zerodist(kma_crime2)
# zd

#if there are duplicates, remove them
kma_crime1 <- remove.duplicates(kma_crime1)
kma_crime2 <- remove.duplicates(kma_crime2)

#create an "extent" object which can be used to create the observation window for spatstat
kma1.ext <- as.matrix(extent(kma_crime1)) 
kma2.ext <- as.matrix(extent(kma_crime2)) 

#observation window
window1 <- as.owin(list(xrange = kma1.ext[1,], yrange = kma1.ext[2,]))
window2 <- as.owin(list(xrange = kma2.ext[1,], yrange = kma2.ext[2,]))

#create ppp oject from spatstat
kma1.ppp <- ppp(x = kma_crime1$x, y = kma_crime1$y, window = window1)
kma2.ppp <- ppp(x = kma_crime2$x, y = kma_crime2$y, window = window2)
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
