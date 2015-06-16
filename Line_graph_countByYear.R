# Author: Dario Rodriguez
# Data started: 11/06/2015
# Date last modified: 12/06/2015
# Scope: Import a dataset from the NBN Gateway, split the data by years and plot a graph with the count of taxa

# Clean up any previous data
rm(list = ls())

# Libraries
library(lubridate)
library(extrafont)
library(ggplot2)
#font_import() To only be done the first time
# fonts() To check what fonts can be used

# Set up the working directory and and import the file with the data
setwd("Y:/Data Services/Dario Rodriguez/Work Space/PROJECTS/R Programming/Repositories/Graphs-Training")
dataImported <- read.csv(file.choose(), sep = '\t', colClasses = "character")
#sapply(dataImported, class) # Check that everything has been imported as class

# Only select the columns needed
dataForGraph <- dataImported[, c(2, 3)]

# Convert to date the column Date
dataForGraph$Date <- as.Date(dataForGraph$Date, "%d/%m/%Y")
dataForGraph$Year <- year(dataForGraph$Date)

# Create a table with the count of species per year
dataCount <- as.data.frame(table(dataForGraph[3]))
colnames(dataCount) <- c("Year", "Count")

# Produce the basic graph with a line and some connecting points
p <- ggplot(dataCount, aes(x = Year, y = Count, group = 1)) + geom_line(colour = "Red", size = 0.8) +
    geom_point(size = 3.5, colour = 'Blue', fill = 'lightblue', shape = 22)

# Add some formatting to the graph
p + expand_limits(y = 0) +
  geom_text(aes(label = Count), size = 5, vjust = -0.9, fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 160, 25)) +
  expand_limits(y = 155) +
  ggtitle("Number of species identified per year for dataset GA001257:\nEunicella verrucosa, Survey 1999-2014 (MBA)\n") + 
  labs(x = "", y = "Count") +
  theme(plot.title = element_text(colour = "#006600", face = "bold", size = 20, family = "Calibri"),
        axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 12),
        panel.background = element_rect(fill = "#FFFFCC"),
        panel.grid.major = element_line(colour = "#999999", size = 0.3),
        panel.grid.minor = element_line(colour = "#CCCCCC", size = 0.1)) +
        annotate("text", x = "2012", y = 137.5, label = "Total observations:\n855", size = 6, family = "Calibri")

# Save the graph. The tittle is cut off when using ggsave
pathToSave <- "C:/Users/Dario Rodriguez/Desktop/CountSpecies GA001257.png"
ggsave(filename = pathToSave, plot = p, width = 30, height = 20, units = "cm", dpi = 300)

# Clear everything
rm(list = ls())