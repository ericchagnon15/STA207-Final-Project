## Cleaning to make the 3 datasets have the same country names
# for example 2 data sets have the country labeled "Antigua and Barbados"
# and the other dataset has it labeled as "Antigua & Barbados"


library(tidyverse)
library(dplyr)

#setwd("~/Documents/UCDavis/Winter2022/STA207/project/")

gni_raw = read.csv("data/gni.csv", sep=",", header = TRUE)
country_gni = gni_raw[,c(1,65)]
colnames(country_gni) = c("Country", "GNI") #266 rows

who = read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
who_last_day = who[which(who$Date_reported == as.Date("2022-03-02", "%Y-%d-%m")),]
who_last_day = who_last_day[,c(3,8)] #237 rows

country_data = read.csv("data/CIA_Country_Facts.csv")
country_data = country_data[,c(1,3,5,10)] #227 rows


#####

who_last_day$Country %in% country_data$Country #indices of countries in WHO not in CIA data


## AFter looking at the differences by hand, row by row, in the 3 datasets the following countries had differences in naming conventions

conflicting_country_names = c("Antigua & Barbuda", "Bahamas, The", "Bolivia (Plurinational State of)", "Bosnia & Herzegovina", "British Virgin Is.", "Brunei",
                              "Central African Rep.", "Democratic Republic of the Congo", "Congo, Dem. Rep.", "Congo, Repub. of the", "Congo, Rep.", "Gambia, The", "Iran (Islamic Republic of)",
                              "Korea, North", "Korea, Dem. People's Rep.", "Micronesia (Federated States of)", "Northern Mariana Islands (Commonwealth of the)",
                              "N. Mariana Islands", "Republic of Korea", "Russian Federation", "Saint Kitts & Nevis", "St Pierre & Miquelon")

#the naming conventions were normalized througout the 3 datasets (there are potentially some misses due to human error)
country_data[8, 1] = "Antigua and Barbuda"
country_data[15, 1] = "Bahamas"
country_gni[24,1] = "Bahamas"
who_last_day[25,1] = "Bolivia"
country_data[26,1] = "Bosnia and Herzegovina"
country_data[29,1] = "British Virgin Islands"
country_data[30,1] = "Brunei Darussalam"
country_data[40,1] = "Central African Republic"
country_data[46,1] = "Democratic Republic of the Congo"
country_gni[44,1] = "Democratic Republic of the Congo"
country_data[47,1] = "Congo"
country_gni[45,1] = "Congo"
who_last_day[44,1] = "Czech Republic"
country_gni[87,1] = "Gambia"
country_data[74,1] = "Gambia"
who_last_day[99,1] = "Iran"
country_gni[113,1] = "Iran"
country_data[110,1] = "North Korea"
country_data[111,1] = "South Korea"
who_last_day[55,1] = "North Korea"
country_gni[194,1] = "North Korea"
country_gni[127,1] = "South Korea"
who_last_day[172,1] = "South Korea"
who_last_day[136,1] = "Federated States of Micronesia"
country_data[137,1] = "Federated States of Micronesia"
country_gni[80,1] = "Federated States of Micronesia"
who_last_day[155,1] = "Northern Marina Islands"
country_data[154,1] = "Northern Marina Islands"
who_last_day[173,1] = "Moldova"
who_last_day[176,1] = "Russia"
who_last_day[227,1] = "United States"



country_data_gni = inner_join(country_data, country_gni, "Country")

all_data = inner_join(country_data_gni, who_last_day, "Country")
death_percent = all_data$Cumulative_deaths / all_data$Population
all_data = cbind(all_data, death_percent)

## Final dataset with variables in numeric form
final_numeric_data = all_data[,c(-2,-6)]
final_numeric_data = na.omit(final_numeric_data) 




## Based on the research the GNI values have the following Levels
# Low <1046
# Medium - Low between 1046 and 4095
# Medium - High between 4096 and 12695
# High greater than 12695

gni.factor = c()
gni.factor[final_numeric_data$GNI < 1046] <- "Low"
gni.factor[final_numeric_data$GNI > 1045 & final_numeric_data$GNI < 4096] <- "Medium_Low"
gni.factor[final_numeric_data$GNI > 4095 & final_numeric_data$GNI < 12696] <- "Medium_High"
gni.factor[final_numeric_data$GNI > 12695] <- "High"
gni.factor = as.factor(gni.factor)

x =  data.frame(num = final_numeric_data$GNI, factor = gni.factor)

## Based on the research the Literacy Rate values have the following Levels
# Low < 65%
# Medium between 65% and 95%
# High ggreater than 95%

literacy.factor = c()
literacy.factor[final_numeric_data$Literacy.... < 65] <- "Low"
literacy.factor[final_numeric_data$Literacy.... > 64 & final_numeric_data$Literacy.... < 96] <- "Medium"
literacy.factor[final_numeric_data$Literacy.... > 95] <- "High"
literacy.factor = as.factor(literacy.factor)

#final dataset with treatments in factor form with appropriate levels

final_data = data.frame(country = final_numeric_data$Country,
                        percentDead = final_numeric_data$death_percent,
                        literacy = literacy.factor,
                        gni = gni.factor)

# Save the datasets
#write.csv(final_data, "final_data.csv")
#write.csv(final_numeric_data, "data/final_numeric_data.csv")


## Counts of variables in each level
table(final_data$gni)
table(final_data$literacy)


## SIDE BY SIDE BOX PLOTS
ggplot(data=final_data, aes(x = gni, y = percentDead, fill = gni)) + geom_boxplot() + ggtitle("Percentage Dead by GNI")

ggplot(data=final_data, aes(x = literacy, y = percentDead, fill = literacy)) + geom_boxplot() + ggtitle("Percentage Dead by Literacy Rate")

## HISTOGRAMS
par(mfrow=c(2,2))

ggplot(data=final_data[which(final_data$gni == "Low"),], aes(x = percentDead, fill=gni)) + geom_histogram() + 
  ggtitle("Distribution of Percentage Dead for Low GNI")

ggplot(data=final_data[which(final_data$gni == "Medium_Low"),], aes(x = percentDead, fill=gni)) + geom_histogram() + 
  ggtitle("Distribution of Percentage Dead for Medium Low GNI")

ggplot(data=final_data[which(final_data$gni == "Medium_High"),], aes(x = percentDead, fill=gni)) + geom_histogram() + 
  ggtitle("Distribution of Percentage Dead for Medium High GNI")

ggplot(data=final_data[which(final_data$gni == "High"),], aes(x = percentDead, fill=gni)) + geom_histogram() + 
  ggtitle("Distribution of Percentage Dead for High GNI")



par(mfrow=c(1,1))

## INTERACTION PLOTS
interaction.plot(x.factor = final_data$gni, trace.factor = final_data$literacy, response = final_data$percentDead)

######## Model Building
model = aov(data = final_data, percentDead ~ literacy + gni)
summary(model)
par(mfrow=c(2,2))

plot(model)


model2 = aov(data = final_data, percentDead ~ gni)
plot(model2)

# weighted least squares

s2_low = var(final_data[which(final_data$gni == "Low"),]$percentDead)
s2_mediumLow = var(final_data[which(final_data$gni == "Medium_Low"),]$percentDead)
s2_mediumHigh = var(final_data[which(final_data$gni == "Medium_High"),]$percentDead)
s2_high = var(final_data[which(final_data$gni == "High"),]$percentDead)

w=c()

# apply the weights to each observation
for (i in c(1:nrow(final_data))){
  if (final_data[i,]$gni == "Low"){
    w[i] = s2_low
  }
  else if (final_data[i,]$gni == "Medium_Low"){
    w[i] = s2_mediumLow
  }
  else if (final_data[i,]$gni == "Medium_High"){
    w[i] = s2_mediumHigh
  }
  else{
    w[i] = s2_high
  }
}

model3 = aov(data=final_data, percentDead~gni, weights = w)
plot(model3)



# non-parametric (replace observations with the rank)
final_data$rank.perc = rank(final_data$percentDead)
model4 = aov(data=final_data, rank.perc~gni)
plot(model4)

# Tests for equal variance and normality
library(car)
leveneTest(data=final_data, percentDead ~ gni)

shapiro.test(model2$residuals)
