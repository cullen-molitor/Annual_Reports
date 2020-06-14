#
#
#
#
#
#
#
#     Everything here can be shared among any annual report .Rmd file
#
#
#
#
#
#
#

# Save this file after changing the years 

Year_to_Filter_Data_by <- 2019 # <== CHANGE ME TO FILTER DATA <==

Export_END_Year <- 2020 # <== CHANGE ME TO REFLECT ENDING YEAR OF DATA TEXT FILE <== 

# DO NOT CHANGE the following unless you would like to re-factor the levels or add additional libraries
library(tidyverse)
library(ggpubr)
library(glue)
library(lubridate)
library(rmarkdown)
siteInfo1 <- read_csv("Meta_Data/Site_info.csv")
SpeciesName <- read_csv("Meta_Data/SpeciesComplete.csv") 
SiteColor <- as.character(siteInfo1$Color)
names(SiteColor) <- siteInfo1$SiteName
SiteLine <- siteInfo1$LineType
names(SiteLine) <- siteInfo1$SiteName
IslandLevels <- c("San Miguel Island", "Santa Rosa Island", "Santa Cruz Island", "Anacapa Island",  "Santa Barbara Island")
MPA_Levels <- c("Santa Rosa Island", "Santa Cruz Island", "Anacapa Island",  "Santa Barbara Island")
SiteLevels <- c(
  # San Miguel
  "Wyckoff Ledge", "Miracle Mile", "Hare Rock",
  # Santa Rosa
  "Johnson's Lee North", "Johnson's Lee South", "Rodes Reef", "Cluster Point", "Trancion Canyon", "Chickasaw", "South Point",
  # Santa Cruz
  "Fry's Harbor", "Pelican Bay", "Yellow Banks", "Devil's Peak Member", "Pedro Reef",
  "Gull Island South", "Scorpion Anchorage", "Potato Pasture", "Cavern Point", "Little Scorpion",
  # Anacapa
  "Admiral's Reef", "East Fish Camp", "Lighthouse", "Cathedral Cove" , "Landing Cove", "Black Sea Bass Reef", "Keyhole", 
  # Santa Barbara
  "Arch Point", "Cat Canyon", "Webster's Arch", "SE Sea Lion Rookery", "Graveyard Canyon", "Southeast Reef")
MonthLevels <- c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')




