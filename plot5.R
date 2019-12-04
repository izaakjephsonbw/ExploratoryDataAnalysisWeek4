## Import libraries
library(tidyverse)

## Import data
if(!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if(!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

## Filter data by city for motor sources in Baltimore
colnames(NEI)[2] <- "SCCode"
baltimore <- NEI %>% filter(fips == "24510")
baltimoremotor <- baltimore %>% filter(SCCode %in% as.vector(SCC$SCC[str_which(SCC$Short.Name,"Vehicle|vehicle")]))

## Group and summarise data by total emissions
bmmotortotal <- baltimoremotor %>% group_by(year)%>% summarise(sum(Emissions))
colnames(bmmotortotal)[2] <- "bmtotal"

## Create plot 5 to show total motor emissions in Baltimore over time
png(file = "plot5.png")
g <- ggplot(data = bmmotortotal, mapping = aes(year,bmtotal))
g + geom_line() + geom_point() + ylab("baltimore motor vehicle emissions")
dev.off()
