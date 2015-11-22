gdp <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(gdp, destfile="GDP.csv", method = "curl")
gdpData <- read.csv("GDP.csv")
gdpData2 <- read.csv("GDP.csv", skip = 4, nrows = 214)
gdpData3 <- gdpData2[,c(1,2,4,5)]
gdpData3[,4] <- as.numeric(gsub(",","",gsub("[..]","",gdpData2[,5])))
colnames(gdpData3) <- c("CountryCode","Rank","CountryName","GDP")

# Sorting
gdpData3[order(gdpData3$GDP),]


fedstatsFile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fedstatsFile, destfile = "FEDSTATS_Country.csv", method = "curl")
fedstatsData <- read.csv("FEDSTATS_Country.csv")


combinedData <- merge(gdpData3, fedstatsData, by.x = 'CountryCode', by.y = "CountryCode")
sorted <- combinedData[order(combinedData$GDP),]


#  What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
highIncomeNonOECD <- sorted[ which(sorted$Income.Group == "High income: nonOECD"),]
mean(highIncomeNonOECD$Rank, na.rm = TRUE)
highIncomeOECD <- sorted[ which(sorted$Income.Group == "High income: OECD"),]


incomeGroup <- sorted[which(is.na(sorted$Rank) == FALSE),c(1:4,6)]

#divisions <- quantile(incomeGroup$GDP, probs = seq(0,1,0.2))

divisions <- quantile(incomeGroup$Rank, probs = seq(0,1,0.2))
with(incomeGroup, table(cut(Rank, divisions), Income.Group))
