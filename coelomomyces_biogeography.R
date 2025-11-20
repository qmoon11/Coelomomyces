#This script does the preliminary Coelomomyces biogeographic analysis

#### Load Packages and Occurance data####
#Load packages
library(readr)
library(dplyr)
library(rworldmap)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Read in observation/occurrence data from CSV file
df <- read.csv("Coelomomyces_Observations.csv")

##### Occurrences per country ####

# Count the number of occurrence records for each country
country_counts <- df %>%
  count(Countries, name = "Occurrences")

# Join the occurrences data to world map country polygons, matching by country name
sPDF <- joinCountryData2Map(country_counts,
                            joinCode = "NAME",
                            nameJoinColumn = "Countries",
                            verbose = TRUE)

# Find the maximum number of occurrences for palette creation (not strictly required for fixedWidth, but shown here for reference)
max_value <- max(sPDF@data$Occurrences, na.rm = TRUE)


# Create a yellow to red palette for a smooth, continuous gradient (100 colors recommended for visual clarity)
my_palette <- colorRampPalette(c("lightblue", "blue", "yellow", "orange", "red"))
colors <- my_palette(100)

# Plot a world map colored by number of occurrences per country,
# using fixed-width bins for a continuous gradient legend
mapCountryData(sPDF,
               nameColumnToPlot = "Occurrences",     # Use occurrence counts to color each country
               colourPalette = colors,               # Apply the yellow-to-red color palette
               catMethod = "fixedWidth",             # Use evenly spaced bins for a continuous legend
               numCats = 100,                        # 100 color steps for smooth gradient
               mapTitle = "Coelomomyces Occurrences by Country",
               addLegend = TRUE                      # Show legend on the map
)



##### Richness Map ####

# Calculate the number of unique species (richness) reported in each country
species_per_country <- df %>%
  group_by(Countries) %>%
  summarise(UniqueSpecies = n_distinct(Species))

# Join the species richness data to world map country polygons, matching by country name
sPDF <- joinCountryData2Map(species_per_country,
                            joinCode = "NAME",
                            nameJoinColumn = "Countries",
                            verbose = TRUE)

# Create the same yellow to red palette for mapping species richness
my_palette <- colorRampPalette(c("lightblue", "blue", "yellow", "orange", "red"))
colors <- my_palette(100)

# Plot a world map colored by species richness per country,
# using a continuous gradient and showing legend
mapCountryData(sPDF, 
               nameColumnToPlot = "UniqueSpecies",    # Color by number of unique species
               colourPalette = colors,                # Apply yellow-to-red color gradient
               catMethod = "fixedWidth",              # Continuous legend
               numCats = 100,                         # 100 color bins
               mapTitle = "Coelomomyces Richness by Country",
               addLegend = TRUE
)
