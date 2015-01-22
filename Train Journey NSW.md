---
title: "Train Journey Stats for Sydney & Around"
output: html_document
---

## EXECUTIVE SUMMARY

This document records my Shiny application for Coursera Subject - Developing Data Products. I've chosen NSW Transport data to analyze some statistical facts regards to the journeys made from various train lines near Sydney.


## OVERVIEW

An interactive interface is created from shiny package of R, which allows customerised input with three different formats:

* a slider to adjust range of years

* a dropdown to select a single or multiple lines

* a check box to choose display mode

Whilst on the output side there are

* comparision result of highest year/month

* number of total journeys

* graphical view by line (or combined)


## USER MANUAL

The shinyApp deployed online can be found from: <https://helenh2.shinyapps.io/Project>

Input required:

* year range slider: mandatory but default to between 2001 and 2013, which is the range of dataset

* dropdown for lines selection: default to no selection. There will be no output when this is left empty. Support multiple selections; for example, if selecting "Airport" & "South", statistics will be based data from this two lines within the year range

* check box: default to un-ticked which means graphs will be displayed by data of individual lines. If ticked, only one graph will be displayed with combining all data from selected lines.

Output should be intepreted as:

* Highest Year from Selected Input: return the year with highested total journeys within the input year range & for input line(s)

* Highest Month from Selected Input: return the month with highested total journeys within the input year range & for input line(s) 

* Total Journeys from Selected Input: return the total number of journeys within the input year range & for input line(s)

* Historical Graphics: return either a split view or a combinged view (based on check box selection) of total journeys throughout year(s) by lines or in total

## DEMO


For example, if select for year 2003 - 2009 and few lines ('Western','CBD','Blue Mountains','South'), when display them in separate views, they will look like below.

```{r echo=FALSE,message=FALSE}

library(ggplot2)
library(reshape2)
library(shiny)
library(plyr)
library(gsubfn) #strapply

#   Read in character appearances CSV
Transport <- read.csv("Transport NSW.csv", stringsAsFactors = FALSE)
# Relabelling
colnames(Transport) <- c('line','month','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013')
# Melt table
mdata <- melt(Transport, id=c("line","month"))
# Relabelling
colnames(mdata) <- c('line','month','year','count')
# Convert factor (from melt) to int
mdata$year <- as.numeric(as.character(mdata$year))

# for QA
line <- c('Western','CBD','Blue Mountains','South')
year1 <- as.numeric(2003)
year2 <- as.numeric(2009)
 
separatePlot <- function(line,year1,year2){

#   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
#   aggr by year/line
  rpt_year <- ddply(rpt_data,.(line,year),summarize,total_count=sum(count))
#   plot
  sp <- ggplot(rpt_year, aes(x=year, y=total_count,fill=line))
  sp <- sp + geom_bar(stat="identity", position=position_dodge())
  sp <- sp + facet_wrap(~line,ncol=3)
  sp <- sp + geom_smooth()
#   print(sp)

  return(sp)

}

combinePlot <- function(line,year1,year2){
  
#   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
#   aggr by year
  rpt_year_c <- ddply(rpt_data,.(year),summarize,total_count_c=sum(count))
#   plot
  cp <- ggplot(rpt_year_c, aes(x=year, y=total_count_c))
  cp <- cp + geom_bar(stat="identity", position=position_dodge(),fill = rainbow(n=length(rpt_year_c$year)))
  cp <- cp + geom_smooth()
#   print(cp)
  
  return(cp)

}

highYr <- function(line,year1,year2){
  
  #   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
  #   aggr by year
  rpt_year_cal <- ddply(rpt_data,.(year),summarize,total_count_cal=sum(count))
  #   order by total count per year
  order <- rpt_year_cal[order(-rpt_year_cal[,2]),]
  year <- order[1,"year"]
  return(year)

}

highMth <- function(line,year1,year2){
  
  #   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
  #   aggr by month
  rpt_month_cal <- ddply(rpt_data,.(month),summarize,total_count_cal=sum(count))
  #   order by total count per month
  order_m <- rpt_month_cal[order(-rpt_month_cal[,2]),]
  month <- order_m[1,"month"]
  return(month)
  
}

totalJourney <- function(line,year1,year2){
  
  #   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
  #   aggr to overall count
  rpt_total_cal <- ddply(rpt_data,.(),summarize,total_count_cal=sum(count))
  all <- rpt_total_cal[,2]
  return(all)
  
}

print("Highest Year from Selected Input:") 
print(highYr(line,year1,year2))


print("Highest Month from Selected Input:")
print(highMth(line,year1,year2))

print("Total Journeys from Selected Inputt:")
print(totalJourney(line,year1,year2))

separatePlot(line,year1,year2)

```

With the same years and lines selection, but tick check box for display combined view, text and numeric values returned will be the same as they based on unchanged logic, yet graphic view will only show one instead of four.


```{r echo=FALSE,message=FALSE}

library(ggplot2)
library(reshape2)
library(shiny)
library(plyr)
library(gsubfn) #strapply

#   Read in character appearances CSV
Transport <- read.csv("Transport NSW.csv", stringsAsFactors = FALSE)
# Relabelling
colnames(Transport) <- c('line','month','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013')
# Melt table
mdata <- melt(Transport, id=c("line","month"))
# Relabelling
colnames(mdata) <- c('line','month','year','count')
# Convert factor (from melt) to int
mdata$year <- as.numeric(as.character(mdata$year))

# for QA
line <- c('Western','CBD','Blue Mountains','South')
year1 <- as.numeric(2003)
year2 <- as.numeric(2009)
 
separatePlot <- function(line,year1,year2){

#   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
#   aggr by year/line
  rpt_year <- ddply(rpt_data,.(line,year),summarize,total_count=sum(count))
#   plot
  sp <- ggplot(rpt_year, aes(x=year, y=total_count,fill=line))
  sp <- sp + geom_bar(stat="identity", position=position_dodge())
  sp <- sp + facet_wrap(~line,ncol=3)
  sp <- sp + geom_smooth()
#   print(sp)

  return(sp)

}

combinePlot <- function(line,year1,year2){
  
#   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
#   aggr by year
  rpt_year_c <- ddply(rpt_data,.(year),summarize,total_count_c=sum(count))
#   plot
  cp <- ggplot(rpt_year_c, aes(x=year, y=total_count_c))
  cp <- cp + geom_bar(stat="identity", position=position_dodge(),fill = rainbow(n=length(rpt_year_c$year)))
  cp <- cp + geom_smooth()
#   print(cp)
  
  return(cp)

}

highYr <- function(line,year1,year2){
  
  #   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
  #   aggr by year
  rpt_year_cal <- ddply(rpt_data,.(year),summarize,total_count_cal=sum(count))
  #   order by total count per year
  order <- rpt_year_cal[order(-rpt_year_cal[,2]),]
  year <- order[1,"year"]
  return(year)

}

highMth <- function(line,year1,year2){
  
  #   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
  #   aggr by month
  rpt_month_cal <- ddply(rpt_data,.(month),summarize,total_count_cal=sum(count))
  #   order by total count per month
  order_m <- rpt_month_cal[order(-rpt_month_cal[,2]),]
  month <- order_m[1,"month"]
  return(month)
  
}

totalJourney <- function(line,year1,year2){
  
  #   get just the data cut within input range
  rpt_data <- mdata[mdata$line %in% line&mdata$year>=year1&mdata$year<=year2,]
  #   aggr to overall count
  rpt_total_cal <- ddply(rpt_data,.(),summarize,total_count_cal=sum(count))
  all <- rpt_total_cal[,2]
  return(all)
  
}

print("Highest Year from Selected Input:") 
print(highYr(line,year1,year2))


print("Highest Month from Selected Input:")
print(highMth(line,year1,year2))

print("Total Journeys from Selected Inputt:")
print(totalJourney(line,year1,year2))

combinePlot(line,year1,year2)

```

This is only a quick demo to show how results look like. Results actually differ quite a lot based on the years and lines seleced. For example, it's not necessray that the most recent year is the highest year. Highest month may also differ a lot based on the line such as Olympic Park line will be very high around April due to Easter Shown around the month. Lines in CBD or high density residential areas are popular throughout the year, while remote destinations (i.e.Blue mountains) would be extrememly low compared to them.


## REFERENCES

Dataset from: <http://www.bts.nsw.gov.au/Statistics/Train>

Dataset name: **Summary of Train Ticket Issues**
