# Directories ---------------------------------------------------------------------------------------
setwd("C:/Users/pgcar/Desktop/BDA/Project")


# Libraries -----------------------------------------------------------------------------------------
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotrix)


# Functions -----------------------------------------------------------------------------------------
calc_percent_total = function(catch_data){
  # Calculate the percent of catch for each row in the data frame.
  # 
  # Input:
  #   catch_data - a dataframe for catch data.
  # 
  # Output:
  #   Returns a vector with percentages of total catch (all values sum to 100).
  percent_catch = (catch_data$total_catch / (sum(catch_data$total_catch, na.rm=TRUE))) * 100
  return(percent_catch)
}

sort_catch = function(catch_data, sort_decreasing){
  # Sort the levels in factor "Genus_Species" by percent catch.
  #
  # Input:
  #   catch_data - a dataframe for catch data.
  #   sort_decreasing - If TRUE sort percent catch in decreasing order. If FALSE sort in increasing order.
  #
  # Output:
  #   Returns the sorted factor "Genus_Species".
  return(factor(catch_data$Genus_Species, 
                levels = catch_data$Genus_Species[order(catch_data$percent_catch,decreasing=sort_decreasing)]))
}


# Load data -----------------------------------------------------------------------------------------
raw_lombok_data = read.csv("Lombok_cleaned.csv", header = TRUE)
  # Create a variable with genus and species name
  raw_lombok_data$Genus_Species = paste(raw_lombok_data$Genus, raw_lombok_data$Species, sep = " ")

  
# Catch composition: entire catch -------------------------------------------------------------------
# Sum of total catch for each species and create new dataframe
total_catch_df = ddply(raw_lombok_data, c("Genus_Species"), 
                       function(df)c(total_catch=sum(df$Total_Catch_kg),
                                     std.err=std.error(df$Total_Catch_kg)))
# Calculate percent of total catch for each species
total_catch_df$percent_catch = calc_percent_total(total_catch_df)
# Sort the order of levels in factor genus_species
total_catch_df$Genus_Species = sort_catch(total_catch_df,TRUE)
# Remove rows with NA in percent_catch variable
total_catch_df = total_catch_df[!is.na(total_catch_df$percent_catch),]
# Plot the top 50 species that make up total catch.
sum(total_catch_df$percent_catch[1:50])
top_50species = total_catch_df[1:50,]
ggplot(top_50species, aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Species", y="Percentage Catch", title = "Lombock Catch Composition")


# Subset data by fishing gear -----------------------------------------------------------------------
gear_types = unique(raw_lombok_data$Fishing_Gear_Group)
# Spears
lombok_catch_spear = subset(raw_lombok_data, Fishing_Gear_Group == 'Speargun')
# Nets
lombok_catch_net = subset(raw_lombok_data, Fishing_Gear_Group == 'Net')
# Handline
lombok_catch_handline = subset(raw_lombok_data, Fishing_Gear_Group == 'Handline')


Species_Speargun = subset(Lom, Fishing_Gear_Group == "Speargun")
Species_Speargun

str(Species_Speargun)

test = ddply(Lom, c("Fishing_Gear_Group","Genus_Species"), 
             function(df) c(total_catch=sum(df$Total_Catch_kg),
                          std.err=std.error(df$Total_Catch_kg))) 

test.sort = test[order(test$total_catch), ]###(test, decreasing = TRUE, 'total_catch')


# Spear ---------------------------------------------------------------------------------------------
Species_Speargun = subset(test, Fishing_Gear_Group == "Speargun")

#Species_Speargun_sort = Species_Speargun[order(Species_Speargun$total_catch), ]
#Species_Speargun = Species_Speargun(sort(test$total_catch, decreasing = TRUE))
#ggplot(Species_Speargun_sort, aes(x=Genus_Species, y=total_catch)) + 
#  geom_bar(stat='identity') +
#  theme(axis.text.x = element_text(angle=90)) 

spear_df = Species_Speargun
spear_df$percent_catch = (spear_df$total_catch / (sum(spear_df$total_catch, na.rm=TRUE))) * 100
spear_df$Genus_Species = factor(spear_df$Genus_Species, 
                                levels = spear_df$Genus_Species[order(spear_df$total_catch,decreasing=TRUE)])

ggplot(spear_df, aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90)) 

spear_sorted = spear_df[order(spear_df$percent_catch,decreasing = TRUE),]
sum(spear_sorted$percent_catch[1:50])

spear_top_90 = spear_sorted[1:50,]

ggplot(spear_top_90, aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Genus Species", y="Percentage Catch", title = "Lombock Speargun Catch Composition")




# Net --------------------------------------------------------------------------------------------------
Species_Net = subset(test, Fishing_Gear_Group == "Net")
str(Species_Net)

net_df = Species_Net
net_df$percent_catch = ((net_df$total_catch / (sum(net_df$total_catch, na.rm=TRUE))) * 100)
net_df$Genus_Species = factor(net_df$Genus_Species, 
                                levels = net_df$Genus_Species[order(net_df$total_catch,decreasing=TRUE)])

ggplot(net_df, aes(x=Genus_Species, y=percent_catch)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90))+
  labs(x="Genus Species", y="Percentage Catch", title = "Lombock Net Catch Composition")


#spear_sorted = spear_df[order(spear_df$percent_catch,decreasing = TRUE),]
#sum(spear_sorted$percent_catch[1:50])

#spear_top_90 = spear_sorted[1:50,]

#ggplot(spear_top_90, aes(x=Genus_Species, y=percent_catch)) + 
# geom_bar(stat='identity') +
#  theme(axis.text.x = element_text(angle=90)) 



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
