# !!! Needs some attention: what is happening with NAs? Seems to be different
# whether weighting is T vs. F



# Load required packages
library(sf)
library(stringr)
library(tidyverse)
library(terradactyl)

# Optionally, read in SiteKey if it has already been generated and saved as a csv file
SiteKey <- read.csv("C:\\Users\\aheller\\Documents\\Analysis\\ESDKeysWorking\\SiteKeyOutput_MLRA035X.csv")

# MLRA of interest
mlra <- "035X"

# Define arguments for function
# How do you want to subset plots?
# To select all plots within an administrative area, have a shapefile ready
# If subsetting plots by polygon...
keypolyset = FALSE # Set to TRUE or FALSE
# Define path to shapefile
shapefile <- "C:\\Users\\aheller\\Documents\\GIS\\RP GIS\\RP_DSM_boundary_dissolved_10kmbuffer_5070.shp"

# If state...
keystateset = TRUE # Set to TRUE or FALSE
keystate <- "NM" # Select state of interest



# Optional: weight variables
# Assign weight scalars to one of three weight classes, from least (1) to greatest (3)
# Potential variables are: average annual precipitation, percent slope, elevation,
# soil depth, volume of soil surface gravels, volume of soil surface large fragments,
# weighted subsurface volume of gravel, weighted subsurface volume of large fragments,
# particle size class (PSC), and soil surface texture
weights = FALSE
one <- c("AvgPrecip", "Elevation")
two <- c("Slope", "PSC", "SoilDepth")
three <- c("SurfaceGravel", "SurfaceLGFrags", "SubsurfGravel", "SubsurfLGFrags", "SurfaceTextures")



# Are you reading plots from a geodatabase or from the Landscape Data Commons?
# Source = "LDC" for Landscape Data Commons
# Source = "GDB" for geodatabase
source = "GDB"
# If source = "GDB", point to geodatabse
source.dsn <-"C:\\Users\\aheller\\Documents\\Raw Data\\AIMTerrestrialEdtBackup9-1-22.gdb"


# Extend timeout
options(timeout = 4000000)


# Run SiteKey function
PlotsKeyed <- KeyPlots(source.dsn, mlra, keypolyset, keystateset, weights)


