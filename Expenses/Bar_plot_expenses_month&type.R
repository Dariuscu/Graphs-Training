# Author: Dario Rodriguez
# Date started: 01/06/2015
# Date finalised: 02/06/2015
# Scope: create a graph showing the percentage of expenses by groups during the last two years.
# ----------------------------------------------------------------------------------------------

# Clear any previous data
rm(list=ls())

# Load the libraries we need
library(xlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(ggplot2)

# Import the table with the data from Excel
path <- "Y:\\Data Services\\Dario Rodriguez\\Work Space\\PROJECTS\\R Programming\\Training\\Cookbook\\Expenses\\Gastos_2015.xlsm"
data <- read.xlsx(path, 5, stringsAsFactors = FALSE)

# Transform the table from wide to long
data2 <-  data[, c(1, 6, 2, 4, 5, 3 )] %>%
          melt(id.vars = "Date", variable.name = "ExpenseType", value.name = "Amount") %>%
          mutate(newDate = as.Date(paste(Date, '01'), '%Y %B %d')) %>%
          arrange(newDate) %>%
          mutate(Year = year(newDate)) %>%
          mutate(Month = month(newDate, label = TRUE, abbr = TRUE))    
data2$Year <- as.factor(data2$Year)

# To create the field newData in another way            
#data3 <- separate(data2, Date, c("Year", "Month"), sep = " ", remove = TRUE) %>%
          #mutate(Day = "01") %>%
          #mutate(newDate = paste(Year, Month, Day)) %>%
          #mutate(dateFormatted = as.Date(newDate, '%Y %B %d'))
    
# Add the percentage of each type of expense for each month
data3 <- ddply(data2, "newDate", transform, PercentType = round((Amount / sum(Amount) * 100) , 1))

# Re-arrange the order of columns ta make the table more user-friendly
data3 <- data3[, c(5, 6, 4, 2, 3, 7)]

# Define stiles for the stacked bar
datebreaks.month <- seq(as.Date("2013-01-01"), as.Date("2015-12-01"), by = "1 month")
cbPalette <- c("#FFCC00", "#990066", "#56B4E9", "#99CC33", "#FF3366")

# GRAPH USING AND EXTENDED X-AXIS FOR THE THREE YEARS
plot1 <- ggplot(data3, aes(x = newDate, y = PercentType, fill = ExpenseType)) +
                geom_bar(stat = "identity") +
                geom_bar(stat = "identity", colour="black", show_guide = FALSE) +
                scale_x_date(breaks = datebreaks.month, label = date_format("%b\n%Y")) +
                scale_y_continuous(breaks = seq(0, 101, by = 10),
                                   limits = c(0, 101),
                                   name = "Expenses (£)",
                                   expand = c(0, 0)) +
                scale_fill_manual(values = cbPalette, name = "Expense type") +
                guides(fill = guide_legend(reverse = TRUE))

plot1 <- plot1 + facet_wrap(facets = ~ Year, nrow = 3, ncol = 1, as.table = TRUE)

# GRAPH USING A SHORT X-AXIS FOR THE THREE YEARS
plot2 <- ggplot(data3, aes(x = Month, y = PercentType,  fill = ExpenseType)) +
              geom_bar(stat = "identity", colour="black", show_guide = TRUE) +
              scale_x_discrete() +
              scale_y_continuous(breaks = seq(0, 101, by = 10),
                                 limits = c(0, 101),
                                 name = "Expenses (£)",
                                 expand = c(0, 0)) +
              guides(fill = guide_legend(reverse = TRUE))  
plot2 <- plot2 + facet_wrap(facets = ~ Year, nrow = 3, ncol = 1, as.table = TRUE)

plot1
plot2