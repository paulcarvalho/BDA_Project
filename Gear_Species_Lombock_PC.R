# Paul Carvalho
# Big Data Analysis
# Final Project
# Spring 2017

# Data Analysis

# House keeping -------------------------------------------------------------------------------------
rm(list=ls())

# Directories ---------------------------------------------------------------------------------------
# setwd("C:/Users/pgcar/Desktop/BDA/Project")
setwd("C:/Users/humphriesLAB/Desktop/Paul Carvalho/Courses/Big Data/BDA_Project")

# Libraries -----------------------------------------------------------------------------------------
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotrix)


# Functions -----------------------------------------------------------------------------------------
calc_percent_total = function(catch_data, by_weight){
  # Calculate the percent of catch for each row in the data frame.
  # 
  # Input:
  #   catch_data - a dataframe for catch data.
  #   by_weight - TRUE for weight and FALSE for frequency 
  #
  # Output:
  #   Returns a vector with percentages of total weight or frequency (all values sum to 100).
  if(by_weight==TRUE){
    percent_catch = (catch_data$total_catch / (sum(catch_data$total_catch, na.rm=TRUE))) * 100
    return(percent_catch)
  }
  else{
    percent_frequency = (catch_data$total_frequency / (sum(catch_data$total_frequency, na.rm=TRUE))) * 100
    return(percent_frequency)
  }
  
}

sort_catch = function(catch_data, by_weight, sort_decreasing){
  # Sort the levels in factor "Genus_Species" by percent catch or frequency.
  #
  # Input:
  #   catch_data - a dataframe for catch data.
  #   by_weight - TRUE to sort by weight and FALSE to sort by freuency.
  #   sort_decreasing - If TRUE sort percent catch in decreasing order. If FALSE sort in increasing order.
  #
  # Output:
  #   Returns the sorted factor "Genus_Species".
  if(by_weight == TRUE){
    return(factor(catch_data$Genus_Species, 
                  levels = catch_data$Genus_Species[order(catch_data$percent_catch,decreasing=sort_decreasing)]))
  }
  else{
    return(factor(catch_data$Genus_Species, 
                  levels = catch_data$Genus_Species[order(catch_data$total_frequency,decreasing=sort_decreasing)]))
  }
  
}


# Load data -----------------------------------------------------------------------------------------
raw_lombok_data = read.csv("Lombok_cleaned.csv", header = TRUE)

# Create a variable with genus and species name
raw_lombok_data$Genus_Species = paste(raw_lombok_data$Genus, raw_lombok_data$Species, sep = " ")
raw_lombok_data$Frequency = raw_lombok_data$Total_catch_kg/raw_lombok_data$Weight_per_fish_kg

# Total catch by gear type --------------------------------------------------------------------------
data_geartype = ddply(raw_lombok_data, c("Fishing_Gear_Group"), 
                      function(df)c(total_catch=sum(df$Total_catch_kg,na.rm=TRUE),
                                    total_frequency=sum(df$Frequency,na.rm=TRUE)))
# Remove NA
data_geartype = data_geartype[-4,]
# Plot catch for each gear type by weight
ggplot(data_geartype, aes(x=Fishing_Gear_Group, y=total_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Fishing Gear", y="Total fish weight (kg)", title = "Lombock catch by gear (weight)")

# Plot catch for each gear type by frequency
ggplot(data_geartype, aes(x=Fishing_Gear_Group, y=total_frequency)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Fishing Gear", y="Number of fish", title = "Lombock catch by gear (frequency)")


# Catch composition: entire catch -------------------------------------------------------------------
# Sum of total catch for each species and create new dataframe
lombok_data_no_net = subset(raw_lombok_data, Fishing_Gear_Group != "Net") # Remove net data
lombok_data_no_net$Fishing_Gear_Group = factor(lombok_data_no_net$Fishing_Gear_Group) # Remove net from factor levels
total_catch_df = ddply(lombok_data_no_net, c("Genus_Species"), 
                       function(df)c(total_catch=sum(df$Total_catch_kg,na.rm=TRUE),
                                     total_frequency=sum(df$Frequency,na.rm=TRUE)))
# Remove the first row because it does not contain data
total_catch_df = total_catch_df[-1,]

# Check for NAs
num_NA = sum(is.na(total_catch_df))

# Calculate percent of total weight and frequency for each species
total_catch_df$percent_catch = calc_percent_total(total_catch_df, TRUE)
total_catch_df$percent_frequency = calc_percent_total(total_catch_df, FALSE)

# Sort by weight and frequency
sorted_total_weight = total_catch_df[order(total_catch_df$percent_catch,decreasing = TRUE),]
sorted_total_frequency = total_catch_df[order(total_catch_df$total_frequency,decreasing = TRUE),]

# Sort the order of levels in factor genus_species for sorted dataframes
sorted_total_weight$Genus_Species = sort_catch(sorted_total_weight,TRUE,TRUE)
sorted_total_frequency$Genus_Species = sort_catch(sorted_total_frequency,FALSE,TRUE)

# Plot catch composition by weight (top 50 species)
ggplot(sorted_total_weight[1:50,], aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=75))+
  labs(x="Species", y="Percentage Catch", title = "Lombock catch composition by weight")

# Plot catch composition by frequency (top 50 species)
ggplot(sorted_total_frequency[1:50,], aes(x=Genus_Species, y=percent_frequency)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Species", y="Percentage Catch", title = "Lombock catch composition by frequency")


# Subset data by fishing gear -----------------------------------------------------------------------
gear_catch_df = ddply(raw_lombok_data, c("Genus_Species","Fishing_Gear_Group"), 
                       function(df)c(total_catch=sum(df$Total_catch_kg)))
# Spears
spear_df = subset(gear_catch_df, Fishing_Gear_Group == 'Speargun')
# Nets
net_df = subset(gear_catch_df, Fishing_Gear_Group == 'Net')
# Handline
handline_df = subset(gear_catch_df, Fishing_Gear_Group == 'Handline')


# Spear ---------------------------------------------------------------------------------------------
spear_df$percent_catch = calc_percent_total(spear_df)
spear_df$Genus_Species = sort_catch(spear_df, TRUE)

ggplot(spear_df, aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90)) 

# Find top 75 percent of the catch
spear_sorted = spear_df[order(spear_df$percent_catch,decreasing = TRUE),]
sum(spear_sorted$percent_catch[1:20])
spear_sorted_75 = spear_sorted[1:20,]

# Plot top 75% of the catch
ggplot(spear_sorted_75, aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Genus Species", y="Percentage Catch", title = "Lombock Speargun Catch Composition")


# Net --------------------------------------------------------------------------------------------------
net_df$percent_catch = calc_percent_total(net_df)
net_df$Genus_Species = sort_catch(net_df, TRUE)

ggplot(net_df, aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Genus Species", y="Percentage Catch", title = "Lombock Net Catch Composition")


# Handline ------------------------------------------------------------------------------------------------
Species_Handline = subset(test, Fishing_Gear_Group == "Handline")
str(Species_Handline)
line_df = Species_Handline
line_df$percent_catch = ((line_df$total_catch / (sum(line_df$total_catch, na.rm=TRUE))) * 100)
line_df$Genus_Species = factor(line_df$Genus_Species, 
                              levels = line_df$Genus_Species[order(line_df$total_catch,decreasing=TRUE)])

ggplot(line_df, aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90)) 

line_sorted = line_df[order(line_df$percent_catch,decreasing = TRUE),]

sum(line_sorted$percent_catch[1:50])

line_top_90 = line_sorted[1:50,]

ggplot(line_top_90, aes(x=Genus_Species, y= percent_catch)) + 
geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Genus Species", y="Percentage Catch", title = "Lombock Handline Catch Composition")
