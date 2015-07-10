# Author: Dario Rodriguez
# Date started: 08/06/2015
# Date finalised: 08/06/2015
# Scope: create a graph showing the count of site status (SAC, cSAC and SCI) from
# the SAC live spreadhseet and the SACs_sites tab
# ---------------------------------------------------------------------------------------------

# Clear any previous data
rm(list=ls())

# Load the libraries we need
library(xlsx)
library(ggplot2)

# Import the tables with the data from Excel
path <- "Y:\\Data Services\\Dario Rodriguez\\Work Space\\PROJECTS\\R Programming\\Scripts\\MPA_Live_spreadsheets\\Spreadsheets\\Offshore_SACs_SPAs_Offin_20150518.xlsx"
dataOff <- read.xlsx(path, 1, stringsAsFactors = TRUE)
dataSAC <- read.xlsx(path, 3, stringsAsFactors = TRUE)
dataOffIn <- read.xlsx(path, 8, stringsAsFactors = TRUE)

# Only keep the columns we need for the graph and count the number of site status for each group 
# Offshore
dataOff <- as.data.frame(table(dataOff[3]))
colnames(dataOff) = c("Status", "Count")

# SACs
dataSAC <- as.data.frame(table(dataSAC[3]))
colnames(dataSAC) = c("Status", "Count")

# Offshore and inshore
dataOffIn <- as.data.frame(table(dataOffIn[3]))
colnames(dataOffIn) = c("Status", "Count")


# Make the graphs
# Offshore
p <- ggplot(dataOff, aes(x = Status, y = sort(Count, decreasing = TRUE), fill = Status)) + geom_bar(stat = "identity", colour = "black") +
  guides(fill = FALSE) +
  geom_text(aes(label = sort(Count, decreasing = TRUE)), vjust = -0.2, color = "Blue", size = 3) +
  scale_y_continuous(breaks = seq(0, 80, 5)) +
  theme(axis.text.x = element_text(colour = "darkred", size = 12), axis.text.y = element_text(colour = "darkred", size = 10),
        panel.grid.major.y = element_line(colour = "#660000", size = 0.001), panel.grid.minor.y =  element_blank(), panel.grid.major.x =  element_blank(), panel.background = element_rect(fill = '#CCFFCC', colour = 'black')) +
  ggtitle("Count of offshore status") +
  labs(x = "", y = "Count") +
  scale_fill_manual(values = c("#FFFF33", "#FF33FF", "#009933", "#FF3300"))

# SACs
q <- ggplot(dataSAC, aes(x = Status, y = sort(Count, decreasing = TRUE), fill = Status)) + geom_bar(stat = "identity", colour = "black") +
     guides(fill = FALSE) +
     geom_text(aes(label = sort(Count, decreasing = TRUE)), vjust = -0.2, color = "Blue", size = 3) +
     scale_y_continuous(breaks = seq(0, 80, 5)) +
     theme(axis.text.x = element_text(colour = "darkred", size = 12), axis.text.y = element_text(colour = "darkred", size = 10),
           panel.grid.major.y = element_line(colour = "#660000", size = 0.001), panel.grid.minor.y =  element_blank(), panel.grid.major.x =  element_blank(), panel.background = element_rect(fill = '#CCFFCC', colour = 'black')) +
     ggtitle("Count of SAC status") +
     labs(x = "", y = "Count") +
     scale_fill_manual(values = c("#FFFF33", "#FF33FF", "#009933"))

# Offshore and inshore
r <- ggplot(dataOffIn, aes(x = Status, y = sort(Count, decreasing = TRUE), fill = Status)) + geom_bar(stat = "identity", colour = "black") +
  guides(fill = FALSE) +
  geom_text(aes(label = sort(Count, decreasing = TRUE)), vjust = -0.2, color = "Blue", size = 3) +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  theme(axis.text.x = element_text(colour = "darkred", size = 12), axis.text.y = element_text(colour = "darkred", size = 10),
        panel.grid.major.y = element_line(colour = "#660000", size = 0.001), panel.grid.minor.y =  element_blank(), panel.grid.major.x =  element_blank(), panel.background = element_rect(fill = '#CCFFCC', colour = 'black')) +
  ggtitle("Count of offshore and inshore status") +
  labs(x = "", y = "Count") +
  scale_fill_manual(values = c("#FFFF33", "#FF33FF", "#009933", "#FF3300", "#FFCC00", "#3399FF"))

# Save the graphs in
pathToSave <- "Y:\\Data Services\\Dario Rodriguez\\Work Space\\PROJECTS\\R Programming\\Training\\Cookbook\\Graphs_live_spredsheets\\"
ggsave(filename = paste(pathToSave, "Offshore.png"), plot = p, width = 18, height = 12, units = "cm", dpi = 300)
ggsave(filename = paste(pathToSave, "SACs.png"), plot = q, width = 18, height = 12, units = "cm", dpi = 300)        
ggsave(filename = paste(pathToSave, "OffshoreInshore.png"), plot = r, width = 18, height = 12, units = "cm", dpi = 300)
