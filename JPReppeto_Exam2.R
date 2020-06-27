#Clear the environment
rm(list=ls(all=TRUE))

#setting the working directory
setwd("/Users/jp/R")

#activating rio
library(rio)

#importing inequality data
inequality_data <- import("inequality.xlsx")
ineqdata <- import("inequality.xlsx")

#subsetting Denmark, Sweden, and Brazil
subset(inequality_data, country == "Sweden")
subset(inequality_data, country == "Denmark")
subset(inequality_data, country == "Brazil")

#changing encoding before removing accents
#head
head(inequality_data)

#remove the accent function
remove.accents <- function(s) {
  #single letter
  old1 <- "ú"
  new1 <- "u"
  s1 <- chartr(old1, new1,s)
}

inequality_data$country <- remove.accents(inequality_data$country)

#checking head
head(inequality_data)
#nice

#sorting by low ineq_gni
lowineq <- inequality_data[order(inequality_data$inequality_gini),]

#head of new data
head(lowineq)

#mean function and mean gini
mean_function <- function(x)
{sum(x) / length(x)}

mean_function(inequality_data$inequality_gini)

#ifelse dummy variables
inequality_data$high_inequality <- ifelse(inequality_data$inequality_gini >= 36.81, 1, 0)
inequality_data$low_inequality <- ifelse(inequality_data$inequality_gini < 36.81, 1, 0)

#crosstabbing our variables
library(doBy)

summaryBy(high_inequality ~ low_inequality, 
          data = inequality_data, FUN= c(mean, length))

#for loop printer
for

#importing wdi data
remotes::install_github('vincentarelbundock/WDI')
library(WDI)

adult_literacy = WDI(country = "all", 
                     indicator = c("SE.ADT.LITR.ZS"),
                     start = 2015, end = 2015, extra = FALSE, cache = NULL)

#renaming

library(data.table)

setnames(adult_literacy, "SE.ADT.LITR.ZS", "Literacy rate, adult total")
      
#merging the variable to ineq dataset
library(tidyverse)

merged_df = left_join(inequality_data, adult_literacy)
by= c("iso2c", "country", "year")
         