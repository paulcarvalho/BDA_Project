# Paul Carvalho
# Big Data Analysis
# Final Project
# Spring 2017

# Data cleaning

# House keeping -----------------------------------------------------------------------------------
rm(list=ls())

# Directories -------------------------------------------------------------------------------------
# setwd("C:/Users/pgcar/Desktop/BDA/Project")
setwd("C:/Users/humphriesLAB/Desktop/Paul Carvalho/Courses/Big Data/BDA_Project")

# Libraries ---------------------------------------------------------------------------------------
library(readxl)
library(stringr)
library(plyr)

# Load data ---------------------------------------------------------------------------------------
lombok.data = system.file("WCS_Lombok_FishCatchData_West Lombok_CPUE_Analysis_TR.xlsx", package = "readxl")
lombok_raw = read_excel("WCS_Lombok_FishCatchData_West Lombok_CPUE_Analysis_TR.xlsx", sheet = "Raw")
str(lombok_raw)


# Remove columns that are not needed --------------------------------------------------------------
# lombok_trunc = lombok_raw[,-c(25:29)] # Only need this code if r includes NA columns
lombok_trunc = lombok_raw # Only use this code is NA columns at end of data frame are omitted
lombok_trunc$`Sale (%)` = NULL # Too many NAs
lombok_trunc$`Trip Hours (Jam)` = NULL # Too many NAs
lombok_trunc$`Operational cost (Rp)` = NULL
lombok_trunc$`Zoning system` = NULL # Too many NAs
lombok_trunc$`Catch per fish (kg)` = as.numeric(lombok_trunc$`Catch per fish (kg)`)
lombok_trunc$`Total Catch (Kg)` = as.numeric(lombok_trunc$`Total Catch (Kg)`)

# Rename variables in data frame ------------------------------------------------------------------
names(lombok_trunc)[3] = 'Trip_ID'
names(lombok_trunc)[7] = 'Fish_Collector'
names(lombok_trunc)[9] = 'Fishing_Gear_Group'
names(lombok_trunc)[10] = 'Fishing_Gear'
names(lombok_trunc)[11] = 'MPA_status'
names(lombok_trunc)[12] = 'Fishing_Ground'
names(lombok_trunc)[15] = 'Common_name'
names(lombok_trunc)[16] = 'Local_name'
names(lombok_trunc)[17] = 'Weight_per_fish_kg'
names(lombok_trunc)[18] = 'Total_catch_kg'
names(lombok_trunc)[19] = 'Price_rp'
names(lombok_trunc)[20] = 'Size_cm'


# Checking for misspelling fisher names -----------------------------------------------------------
unique(sort(lombok_trunc$Fishers)) #Use this code to look over fisher names
lombok_trunc$Fishers = gsub("Darmawam", "Darmawan", as.character(lombok_trunc$Fishers)) 
lombok_trunc$Fishers = gsub("jaila", "Jaila", as.character(lombok_trunc$Fishers))
lombok_trunc$Fishers = gsub("Jamuhur", "Jamhur", as.character(lombok_trunc$Fishers))
lombok_trunc$Fishers = gsub("muniah", "Muniah", as.character(lombok_trunc$Fishers))
lombok_trunc$Fishers = gsub("usup", "Usup", as.character(lombok_trunc$Fishers))


# Check misspelling in fish family ------------------------------------------------------------------
unique(sort(lombok_trunc$Family)) #Use this code to look over family names
lombok_trunc$Family = gsub("serranidae", "Serranidae", as.character(lombok_trunc$Family)) 


# Split genus from species to check misspelling ----------------------------------------------------
gs_lombok = strsplit(lombok_trunc$Species, " ", fixed=TRUE)
gs_lombok = str_split_fixed(lombok_trunc$Species," ", 2)
gs_lombok = unlist(gs_lombok)
lombok_trunc$Genus = gs_lombok[,1]
lombok_trunc$Species = gs_lombok[,2]


# Check misspelling in genus -----------------------------------------------------------------------
unique(sort(lombok_trunc$Genus))
lombok_trunc$Genus = gsub("parupeneus", "Parupeneus", as.character(lombok_trunc$Genus))


# Check misspelling in species -----------------------------------------------------------------------
unique(sort(lombok_trunc$Species))
lombok_trunc$Species = gsub("Guttatus", "guttatus", as.character(lombok_trunc$Species))
lombok_trunc$Species = gsub("Ignobilis", "ignobilis", as.character(lombok_trunc$Species))
lombok_trunc$Species = gsub("Louti", "louti", as.character(lombok_trunc$Species))


# Checking misspelling in fishing ground -----------------------------------------------------------
unique(sort(lombok_trunc$Fishing_Ground))
lombok_trunc$Fishing_Ground = gsub("Batu gendang", "Batu Gendang", as.character(lombok_trunc$Fishing_Ground))


# Check misspelling in common name -----------------------------------------------------------------
unique(sort(lombok_trunc$Common_name))
lombok_trunc$Common_name = gsub("Blackfin barracuda", "Blackfin Barracuda", as.character(lombok_trunc$Common_name))
lombok_trunc$Common_name = gsub("Bluespine unicornfish", "Bluespine Unicornfish", as.character(lombok_trunc$Common_name))
lombok_trunc$Common_name = gsub("Harlequin sweetlips", "Harlequin Sweetlips", as.character(lombok_trunc$Common_name))

# write cleaned data to csv file
write.table(lombok_trunc, file = "Lombok_cleaned.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", 
            row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))
