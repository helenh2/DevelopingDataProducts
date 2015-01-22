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
    line <- c('Airport','South')
    year1 <- as.numeric(2009)
    year2 <- as.numeric(2013)
 
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

shinyServer(
  function(input,output){
    
    output$text1 <- renderText({print(if (is.null(input$line)) {" "}
                                      else highYr(input$line,
                                                  input$year[1],
                                                  input$year[2]))})
    
    output$text2 <- renderText({print(if (is.null(input$line)) {" "}
                                          else highMth(input$line,
                                                      input$year[1],
                                                      input$year[2]))})
    
    output$text3 <- renderText({print(if (is.null(input$line)) {0}
                                      else totalJourney(input$line,
                                                   input$year[1],
                                                   input$year[2]))})
    
    output$plot <- renderPlot({print(if (is.null(input$line)) {" "}
                                         else
                                          {if (input$combine == TRUE) {combinePlot(input$line,
                                                                             input$year[1],
                                                                             input$year[2])}
                                            else {separatePlot(input$line,
                                                              input$year[1],
                                                              input$year[2])}
                                          }
                                )},
                               width = 800, height = 600)
    }
)
