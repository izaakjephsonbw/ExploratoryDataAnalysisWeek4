## Import libraries
library(tidyverse)

## Import data
if(!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if(!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

## Filter data by city for Balitmore
baltimore <- NEI %>% filter(fips == "24510")

## Group and summarise data by total emissions
baltimoreemissions <- baltimore %>% group_by(year) %>% summarise(sum(Emissions))
colnames(baltimoreemissions)[2] <- "total"

## Create plot 2 to show total emissions from all sources in Baltimore over time
png(file = "plot2.png")
with(baltimoreemissions, barplot(total, ylim = c(0,4000), main = "total emissions from all sources in Baltimore", names.arg = year))
dev.off()