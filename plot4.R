## Import libraries
library(tidyverse)

## Import data
if(!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if(!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

## Filter data by city for coal sources
colnames(NEI)[2] <- "SCCode"
coal <- NEI %>% filter(SCCode %in% as.vector(SCC$SCC[str_which(SCC$Short.Name,"Coal|coal")]))

## Group and summarise data by total emissions
coaltotal <- coal %>% group_by(year)%>% summarise(sum(Emissions))
colnames(coaltotal)[2] <- "total"

## Create plot 4 to show total coal emissions over time
png(file = "plot4.png")
g <- ggplot(data = coaltotal, mapping = aes(year,total))
g + geom_line() + geom_point() + scale_y_continuous(limits= c(0,700000))+ ylab("total coal source emissions")
dev.off()
