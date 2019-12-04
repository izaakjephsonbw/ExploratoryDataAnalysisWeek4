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

## Filter data by city for motor sources in LA
LA <- NEI %>% filter(fips == "06037")
LAmotor <- LA %>% filter(SCCode %in% as.vector(SCC$SCC[str_which(SCC$Short.Name,"Vehicle|vehicle")]))

## Group and summarise data by total emissions
LAmotortotal <- LAmotor %>% group_by(year)%>% summarise(sum(Emissions)) 
colnames(LAmotortotal)[2] <- "LAtotal"

## Calculate % change in emissions since 1999
LAmotortotal <- LAmotortotal %>% mutate(LA = LAtotal/LAtotal[1])
bmmotortotal <- bmmotortotal %>% mutate(baltimore = bmtotal/bmtotal[1])

## Join and gather data into a single tall data frame
motortotal <- left_join(LAmotortotal,bmmotortotal)
gatheredmotortotal <- gather(motortotal, "LA","baltimore", key = "city", value = "ratio")

## Create plot 6 to show change in emissions for LA and Baltimore from motor sources over time
png(file = "plot6.png")
g <- ggplot(data = gatheredmotortotal, mapping = aes(year, ratio,colour = city)) 
g + geom_line() + geom_point() + ylab("percent change in motor emissions since 1999")
dev.off()
