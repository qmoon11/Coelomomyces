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

#Add no in lab observations to empty cells in lab column
df$Laboratory.Experiment[df$Laboratory.Experiment == ""] <- "no"


##### Occurrences per country ####

# Filter the dataframe to only records where lab == "no"
df_no_lab <- df %>% filter(Laboratory.Experiment == "no")

# Count the number of occurrence records for each country (for lab == "no")
country_counts <- df_no_lab %>%
  count(Countries, name = "Occurrences")

# Join the occurrences data to world map country polygons, matching by country name
sPDF <- joinCountryData2Map(country_counts,
                            joinCode = "NAME",
                            nameJoinColumn = "Countries",
                            verbose = TRUE)

# Find the maximum number of occurrences for palette creation
max_value <- max(sPDF@data$Occurrences, na.rm = TRUE)

# Create a blue to red palette for a smooth gradient (100 colors)
my_palette <- colorRampPalette(c("lightblue", "blue", "yellow", "orange", "red"))
colors <- my_palette(100)

# Plot the world map using only occurrence records with lab == "no"
mapCountryData(sPDF,
               nameColumnToPlot = "Occurrences",
               colourPalette = colors,
               catMethod = "fixedWidth",
               numCats = 100,
               mapTitle = "Coelomomyces Field Occurrences by Country",
               addLegend = TRUE
)


#save as png
png("Coelomomyces_Occurrences_by_Country.png")  # Start writing to PDF

mapCountryData(sPDF,
               nameColumnToPlot = "Occurrences",
               colourPalette = colors,
               catMethod = "fixedWidth",
               numCats = 100,
               mapTitle = "Coelomomyces Field Occurrences by Country",
               addLegend = TRUE
)

dev.off()  # Finish writing to PDF



#save as pdf
pdf("Coelomomyces_Occurrences_by_Country.pdf")  # Start writing to PDF

mapCountryData(sPDF,
               nameColumnToPlot = "Occurrences",
               colourPalette = colors,
               catMethod = "fixedWidth",
               numCats = 100,
               mapTitle = "Coelomomyces Field Occurrences by Country",
               addLegend = TRUE
)

dev.off()  # Finish writing to PDF


##### Richness Map ####

# Calculate unique species richness per country, for lab == "no"
species_per_country <- df %>%
  filter(Laboratory.Experiment == "no") %>%
  group_by(Countries) %>%
  summarise(UniqueSpecies = n_distinct(Species))

# Proceed with the same mapping code as before
sPDF <- joinCountryData2Map(species_per_country,
                            joinCode = "NAME",
                            nameJoinColumn = "Countries",
                            verbose = TRUE)

my_palette <- colorRampPalette(c("lightblue", "blue", "yellow", "orange", "red"))
colors <- my_palette(100)

mapCountryData(sPDF, 
               nameColumnToPlot = "UniqueSpecies",
               colourPalette = colors,
               catMethod = "fixedWidth",
               numCats = 100,
               mapTitle = "Coelomomyces Species Richness by Country",
               addLegend = TRUE
)

#save as png
png("Coelomomyces_Richness_by_Country.png", width = 1200, height = 800, res = 150)

mapCountryData(sPDF, 
               nameColumnToPlot = "UniqueSpecies",
               colourPalette = colors,
               catMethod = "fixedWidth",
               numCats = 100,
               mapTitle = "Coelomomyces Species Richness by Country",
               addLegend = TRUE
)

dev.off()


#save as pdf
pdf("Coelomomyces_Richness_by_Country.pdf")  # Start writing to PDF

mapCountryData(sPDF, 
               nameColumnToPlot = "UniqueSpecies",
               colourPalette = colors,
               catMethod = "fixedWidth",
               numCats = 100,
               mapTitle = "Coelomomyces Species Richness by Country",
               addLegend = TRUE
)

dev.off()  # Finish writing to PDF
