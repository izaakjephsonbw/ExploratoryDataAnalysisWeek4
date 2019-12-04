## Import libraries
library(tidyverse)

## Import data
if(!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if(!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

## Group and summarise data by total emissions
totalemissions <- NEI %>% group_by(year) %>% summarise(sum(Emissions))
colnames(totalemissions)[2] <- "total"

## create plot 1 to show total emissions fom all sources over time
png(file = "plot1.png")
with(totalemissions, barplot(total, ylim = c(0,8000000), main = "total emissions from all sources over all US", names.arg = year))
dev.off()
