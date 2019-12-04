## Import libraries
library(tidyverse)

## Import data
if(!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if(!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

## Group and summarise data by total emissions
totalemissions <- NEI %>% group_by(year) %>% summarise(sum(Emissions))
colnames(totalemissions)[2] <- "total"

## create plot 1 to show total emissions from all sources over time
png(file = "plot1.png")
with(totalemissions, barplot(total, ylim = c(0,8000000), main = "total emissions from all sources over all US", names.arg = year))
dev.off()

## Filter data by city for Balitmore
baltimore <- NEI %>% filter(fips == "24510")

## Group and summarise data by total emissions
baltimoreemissions <- baltimore %>% group_by(year) %>% summarise(sum(Emissions))
colnames(baltimoreemissions)[2] <- "total"

## Create plot 2 to show total emissions from all sources in Baltimore over time
png(file = "plot2.png")
with(baltimoreemissions, barplot(total, ylim = c(0,4000), main = "total emissions from all sources in Baltimore", names.arg = year))
dev.off()

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
