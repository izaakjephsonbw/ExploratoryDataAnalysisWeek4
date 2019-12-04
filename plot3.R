## Import libraries
library(tidyverse)

## Import data
if(!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if(!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

## Filter data by city for Balitmore
baltimore <- NEI %>% filter(fips == "24510")

## Group and summarise data by total emissions
baltimorebytype <- baltimore %>% group_by(type,year)%>% summarise(sum(Emissions))
colnames(baltimorebytype)[3] <- "total"

## Create plot 3 to show total emissions in Baltimore over time, split by type
png(file = "plot3.png")
g <- ggplot(data = baltimorebytype, mapping = aes(year,total))
g + geom_line() + geom_point() + facet_wrap(.~type)+ ylab("baltimore emissions")
dev.off()
