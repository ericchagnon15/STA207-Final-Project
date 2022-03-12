library(tidyverse)
library(dplyr)

setwd("~/Downloads")

gni_raw = read.csv("gni.csv", sep=",", header = TRUE)
country_gni = gni_raw[,c(1,65)]
colnames(country_gni) = c("Country", "GNI") #266 rows


who = read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
who_last_day = who[which(who$Date_reported == as.Date("2022-03-02", "%Y-%d-%m")),]
who_last_day = who_last_day[,c(3,8)] #237 rows

country_data = read.csv("CIA_Country_Facts.csv")
country_data = country_data[,c(1,3,5,10)] #227 rows

data2 = inner_join(country_data, country_gni, "Country")

data = inner_join(data2, who_last_day, "Country")
death_percent = data$Cumulative_deaths / data$Population
data = cbind(data, death_percent)

final_numeric_data = data[,c(-2,-6)]
final_numeric_data = na.omit(final_numeric_data)      

set

# rules for GNI
# Low < 1046
# Medium-Low 1046-4095
# Medium-High 4096 - 12695
# High > 12695

# rules for literacy
# Low < 65
# Medium 65-95
# High > 95

gni.factor = c()
gni.factor[final_numeric_data$GNI < 1046] <- "Low"
gni.factor[final_numeric_data$GNI > 1045 & final_numeric_data$GNI < 4096] <- "Medium_Low"
gni.factor[final_numeric_data$GNI > 4095 & final_numeric_data$GNI < 12696] <- "Medium_High"
gni.factor[final_numeric_data$GNI > 12695] <- "High"
gni.factor = as.factor(gni.factor)

x =  data.frame(num = final_numeric_data$GNI, factor = gni.factor)

literacy.factor = c()
literacy.factor[final_numeric_data$Literacy.... < 65] <- "Low"
literacy.factor[final_numeric_data$Literacy.... > 64 & final_numeric_data$Literacy.... < 96] <- "Medium"
literacy.factor[final_numeric_data$Literacy.... > 95] <- "High"
literacy.factor = as.factor(literacy.factor)

final_data = data.frame(country = final_numeric_data$Country,
                        percentDead = final_numeric_data$death_percent,
                        literacy = literacy.factor,
                        gni = gni.factor)

model = aov(data = final_data, percentDead ~ literacy + gni)
summary(model)

plot(model)
