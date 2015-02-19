### Version 14.02.2015
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library("shiny")
library("shinythemes")
library("graphics")
library("ggplot2")
library("lattice")
library("plotrix")
library("psych")
library("RColorBrewer")
library("lubridate")
library("plyr")
library("GGally")
library("e1071")
library("Hmisc")
library("effects")
library("car")
library("relaimpo")
library("car")
library("ROCR")
library("fpc")
library("randomForest")
library("maptree")
library("nlme")
library("mgcv")
library("htmltools")



# load helpers.R file
source("helpers.R")

# load dataset 
#Datamining281114 <- read.delim("Datamining281114.csv", header = TRUE, sep = ";", dec=".")

Dataset <- read.delim("DMFeb.csv", header = TRUE, sep = ";", dec=".")
# Call function to create the dataset for analysis
data <- create_dataset(Dataset)
#

#----------------------------------------------------------------------------------
# the histogram plot function
#  
histPlot <- function( ds, x, nbins, group_var ){  
  range_var = diff(range(as.numeric(ds[,x])))/nbins 

  if (group_var!="None")
  {
    cdf <- ddply( ds, group_var, function(df) mean(df[,x]) )
    colnames(cdf)<-c("gv", "x.means")
  
    h <- ggplot(ds, aes_string(x=x, fill=group_var)) +
              geom_histogram( aes( y=..density.. ), binwidth=range_var, alpha=.5, position="identity" ) +  
              scale_x_continuous(limits=c(min(as.numeric(ds[,x])),max(as.numeric(ds[,x])))) +
              geom_vline(data=cdf, aes_string(xintercept="x.means", colour="gv"), linetype="dashed", size=1)
              
  }else{
    h <- ggplot(ds, aes_string(x=x)) + geom_histogram( aes( y=..density..,fill=..count.. ), binwidth=range_var ) +
              scale_x_continuous(limits=c(min(as.numeric(ds[,x])),max(as.numeric(ds[,x])))) +
              geom_vline(aes_string(xintercept=mean(ds[,x], na.rm=T)), color="red", linetype="dashed", size=1) 
  }
    
}
#----------------------------------------------------------------------------------
# the density plot function
# 
densityPlot <- function( ds, x, group_var ){  
 
  if (group_var!="None")
  {
    cdf <- ddply( ds, group_var, function(df)mean(df[,x]) )
    colnames(cdf)<-c("gv", "x.means")
    
    d <- ggplot(ds, aes_string(x=x, color=group_var)) + geom_density(size=1,alpha=0.8) + 
              geom_vline(data=cdf, aes_string(xintercept="x.means", colour="gv"), linetype="dashed", size=0.5)
  }else{
    
    d <- ggplot(ds, aes_string(x=x)) + geom_density(size=1,color="blue") +
             geom_vline( aes_string(xintercept=mean(ds[,x], na.rm=T)), linetype="dashed", size=0.5 )
  }
}

#----------------------------------------------------------------------------------
# the Boxplot function
# 
boxPlots <- function( ds, x, group_var ){  
  if (group_var!="None")
  {
    d <- ggplot(ds, aes_string(x=group_var, y=x, fill=group_var)) + 
                geom_boxplot(notch=TRUE, width=0.5, outlier.size=1.5) +
                stat_summary(fun.y=mean, geom="point", shape=5, size=4)
  }else{
    d <- ggplot(ds, aes_string(x=1, y=x)) + 
                geom_boxplot(notch=TRUE, width=0.5, outlier.size=1.5) +
                stat_summary(fun.y=mean, geom="point", shape=5, size=4)
  }
  
  r <- max(ds[,x]) - min(ds[,x])
  d <- d # + scale_y_continuous(breaks=seq(min(ds[,x]), max(ds[,x]), round(r/5)))  # todo not working in some cases
 
}

#----------------------------------------------------------------------------------
# the scatter plot function
#
scatterPlot <- function(ds, x, y, colour, size, regr.method)
{
  if (colour!="None")
  {
      p <- ggplot(ds, aes_string(x=x, y=y)) 
      p <- p + aes_string(color=colour, size=size) + geom_point()
      p <- p + geom_smooth(method = regr.method, size = 1)  
      p <- p + scale_color_brewer(type="qual",palette='Set1') + scale_fill_brewer()
  }else{
      p <- ggplot(ds, aes_string(x=x, y=y)) 
      p <- p + geom_point()
      p <- p + geom_smooth(method = regr.method, size = 1)  
      p <- p + scale_color_brewer(type="qual",palette='Set1') + scale_fill_brewer() 
  }
}

#----------------------------------------------------------------------------------
# the scatter matrix plot function
# 
scatterMatrixPlot <- function(ds, dim_vars, group_by_var)
{
  if ( group_by_var != "None"){
    ds <- ds[,c(dim_vars,group_by_var)]
    p <- ggpairs(ds, columns=1:length(dim_vars), 
                 upper = list(continuous='cor'),
                 lower = list(continuous = "smooth"), color = group_by_var,params=c(size=3),
                 axisLabels='internal', title = "Matrix Scatter Plot")
  }else{
    ds <- ds[,dim_vars]
    p <- ggpairs(ds, columns=1:length(dim_vars), 
                 upper = list(continuous='cor'),
                 lower = list(continuous = "smooth"),params=c(size=3),
                 axisLabels='internal', title = "Matrix Scatter Plot")
  }
    
}  
##----------------------------------------------------------------------------------
##                 Summary Mutlivariate Statistics function
##
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
sum_stats <- function(data, measurevar, groupvars, na.rm=FALSE, conf.interval=.95, .drop=TRUE)
{
 
if ( groupvars != "None" ){
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, min, max, mean, median, sd, CV, kurtosis, skewness, Q1, Q3, IR, se and ci
  data_stats <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N      = length2(xx[[col]], na.rm=na.rm),
                     min    = min(xx[[col]], na.rm=na.rm),
                     max    = max(xx[[col]], na.rm=na.rm),
                     mean   = mean(xx[[col]], na.rm=na.rm),
                     median = median(xx[[col]], na.rm=na.rm),
                     sd     = sd(xx[[col]], na.rm=na.rm),
                     CV     = ( sd(xx[[col]], na.rm=na.rm)/mean(xx[[col]], na.rm=na.rm) )*100,
                     kurtosis = kurtosis(xx[[col]], na.rm=na.rm),
                     skewness = skewness(xx[[col]], na.rm=na.rm),
                     Q1       = quantile(xx[[col]], 1/4, na.rm=na.rm, names=FALSE),
                     Q3       = quantile(xx[[col]], 3/4, na.rm=na.rm, names=FALSE),
                     IR       = IQR(xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  data_stats$se <- data_stats$sd / sqrt(data_stats$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, data_stats$N-1)
  data_stats$ci <- data_stats$se * ciMult
  
}else{
  
  # This does the summary for all records of the data set at the variable measurevar 
  # N, min, max, mean, median, sd, CV, kurtosis, skewness, Q1, Q3, IR, se and ci
  
  ds <- data[,measurevar]
  data_stats <- data.frame(N     = length(ds),
                           min   = min(ds, na.rm=na.rm),
                           max    = max(ds, na.rm=na.rm),
                           mean   = mean(ds, na.rm=na.rm),
                           median = median(ds, na.rm=na.rm),
                           sd     = sd(ds, na.rm=na.rm),
                           CV     = ( sd(ds, na.rm=na.rm)/mean(ds, na.rm=na.rm) )*100,
                           kurtosis = kurtosis(ds, na.rm=na.rm),
                           skewness = skewness(ds, na.rm=na.rm),
                           Q1       = quantile(ds, 1/4, na.rm=na.rm, names=FALSE),
                           Q3       = quantile(ds, 3/4, na.rm=na.rm, names=FALSE),
                           IR       = IQR(ds, na.rm=na.rm)
                         )
                      
  
  data_stats$se <- data_stats$sd / sqrt(data_stats$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, data_stats$N-1)
  data_stats$ci <- data_stats$se * ciMult
  
  rownames(data_stats)<-"Total"
  
}

return(data_stats)

}
##----------------------------------------------------------------------------------

#---------------------------------------------------------------------------------- shinyServer....
#
shinyServer(function(input, output, session){
 
  #---------------------------------------------------------------------------------------------------
  #     Subset of Dataset 
  #---------------------------------------------------------------------------------------------------
  passData <- reactive({
       
    if (input$groupOrientation != "All"){ 
      data <- subset(data, Orientation %in% c(input$groupOrientation)) 
    }
    if (input$groupSystem != "All"){ 
      data <- subset(data, System %in% c(input$groupSystem))
    }
    if (input$groupBatch != "All"){ 
      data <- subset(data, Batch %in% c(input$groupBatch))
    }
    if (input$groupSection != "All"){ 
      data <- subset(data, Section %in% c(input$groupSection))
    }
    if (input$groupHatchery != "All"){ 
      data <- subset(data, Hatchery %in% c(input$groupHatchery))
    }
    if (input$groupOriginMonth != "All"){ 
      data <- subset(data, Origin.Month %in% c(input$groupOriginMonth)) 
    }
    if (input$groupOriginYear != "All"){ 
      data <- subset(data, Origin.Year %in% c(input$groupOriginYear)) 
    }
    if (input$groupFood != "All"){ 
      data <- subset(data, Actual.Feed %in% c(input$groupFood))
    }
    if (input$groupFood.Category != "All"){ 
      data <- subset(data, Feed.Category %in% c(input$groupFood.Category))
    }
    if (input$groupSupplier != "All"){ 
      data <- subset(data, Supplier %in% c(input$groupSupplier))
    }
    if (input$groupStartAvWeightBioCat != "All"){ 
      data <- subset(data, Start.Av.Weight.BioCat %in% c(input$groupStartAvWeightBioCat))
    }
    if (input$groupEndAvWeightBioCat != "All"){ 
      data <- subset(data, End.Av.Weight.BioCat %in% c(input$groupEndAvWeightBioCat))
    }
    
    data <- data[ data$End.Av.Weight >= as.numeric(input$rangeAvWeight[1]) & data$End.Av.Weight <= as.numeric(input$rangeAvWeight[2]) 
               & data$Av.Weight.Deviation >= as.numeric(input$rangeAvWeightDev[1])
               & data$Av.Weight.Deviation <= as.numeric(input$rangeAvWeightDev[2]) 
               & data$Econ.FCR.Period >= as.numeric(input$rangePeriod.FCR[1]) & data$Econ.FCR.Period <= as.numeric(input$rangePeriod.FCR[2]) 
               & data$LTD.Econ.FCR >= as.numeric(input$rangeLTD.Econ.FCR[1]) & data$LTD.Econ.FCR <= as.numeric(input$rangeLTD.Econ.FCR[2])  
               & data$SGR.Period >= as.numeric(input$rangePeriod.SGR[1]) & data$SGR.Period <= as.numeric(input$rangePeriod.SGR[2]) 
               & data$SFR.Period >= as.numeric(input$rangePeriod.SFR[1]) & data$SFR.Period <= as.numeric(input$rangePeriod.SFR[2]) 
               & data$LTD.Mortality >= as.numeric(input$rangeLTD.Mortality[1]) & data$LTD.Mortality <= as.numeric(input$rangeLTD.Mortality[2])
               & data$Period.Day.Degrees >= as.numeric(input$rangePeriod.Day.Degrees[1]) & 
                      data$Period.Day.Degrees <= as.numeric(input$rangePeriod.Day.Degrees[2])
               & data$Avg.Temperature >= as.numeric(input$rangeAvg.Temp[1]) & 
                      data$Avg.Temperature <= as.numeric(input$rangeAvg.Temp[2])
               & (data$From >= ymd(input$dateRangeFrom[1]) & data$From <= ymd(input$dateRangeFrom[2])) 
               & (data$To >= ymd(input$dateRangeTo[1]) & data$To <= ymd(input$dateRangeTo[2])) 
               & data$Period.Feed.Qty >= as.numeric(input$rangePeriod.Feed.Qty[1]) & 
                 data$Period.Feed.Qty <= as.numeric(input$rangePeriod.Feed.Qty[2])
              , ]
    
    class(data$ProductionTimeDays)
    
#   For debugging  
# View(data)
#   str(data)
#   print(nrow(data))
    return(data)
  })  
  
  
#---------------------------------------------------------------------------------------------------
#     Histograms
#---------------------------------------------------------------------------------------------------
#
#...................................................... H1
output$histPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="End.Av.Weight", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H2
output$histPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Econ.FCR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H3
output$histPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="LTD.Econ.FCR", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H4
output$histPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="SFR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H5
output$histPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="SGR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H6
output$histPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="LTD.Mortality", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H7
output$histPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Period.Day.Degrees", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H8
output$histPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Avg.Temperature", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#---------------------------------------------------------------------------------------------------
#     Density Plots
#---------------------------------------------------------------------------------------------------
#
#...................................................... D1
output$densPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- densityPlot( graphData, x="End.Av.Weight", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D2
output$densPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData() 
      theGraph <- densityPlot(graphData, x="Econ.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D3
output$densPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData() 
      theGraph <- densityPlot(graphData, x="LTD.Econ.FCR", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D4
output$densPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="SFR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D5
output$densPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="SGR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D6
output$densPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="LTD.Mortality", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D7
output$densPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Period.Day.Degrees", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D8
output$densPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Avg.Temperature", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#---------------------------------------------------------------------------------------------------
#     BoxPlots
#---------------------------------------------------------------------------------------------------
#
#...................................................... B1
output$boxPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="End.Av.Weight", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B2
output$boxPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Econ.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B3
output$boxPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="LTD.Econ.FCR", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B4
output$boxPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="SFR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B5
output$boxPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="SGR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B6
output$boxPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="LTD.Mortality", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B7
output$boxPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Period.Day.Degrees", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B8
output$boxPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Avg.Temperature", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#---------------------------------------------------------------------------------------------------
#     Summary Univariate Statistics
#---------------------------------------------------------------------------------------------------
output$summary_stats_EndAvWeight <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="End.Av.Weight", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
})  
output$summary_stats_PeriodFCR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Econ.FCR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_EconFCR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="LTD.Econ.FCR", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_PeriodSFR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="SFR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_PeriodSGR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="SGR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_Mortality <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="LTD.Mortality", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
})  
output$summary_stats_Period.Day.Degrees <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Period.Day.Degrees", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_Avg.Temp <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Avg.Temperature", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 



#---------------------------------------------------------------------------------------------------
#     Dislpay dataset (Data)
#---------------------------------------------------------------------------------------------------
# Dislpay dataset
output$dataset <- renderDataTable({
  data <- passData() 
})

#---------------------------------------------------------------------------------------------------
#     Scatter Matrix Plots & Scatter Plots
#---------------------------------------------------------------------------------------------------
output$scatterMatrixPlot <- renderPlot({
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      
      dim_vars = c("End.Av.Weight", "Econ.FCR.Period", "SFR.Period", "SGR.Period", "LTD.Econ.FCR", 
                   "LTD.Mortality", "Avg.Temperature", "Period.Day.Degrees")
      group_by_var = input$radioDimMulti
      theGraph <- scatterMatrixPlot(graphData, dim_vars, group_by_var)
      print(theGraph)
   })
  }
})
 
#...................................................... S1
output$scatterPlot.EndAvWeight.PeriodFCR <- renderPlot({ 
#Re-run when button is clicked
if (input$goMultiPlot == 0){ 
  return() }
else{ 
  isolate({    
    graphData <- passData()
    p <- scatterPlot(graphData, x="End.Av.Weight", y="Econ.FCR.Period", colour=input$radioDimMulti,
                     size = "Closing.Biomass", regr.method="loess") 
   print(p)
  })
 }
})
output$cor.stats.EndAvWeight.PeriodFCR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
          data <- passData()
          if ( input$radioDimMulti != "None"){
            d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=Econ.FCR.Period))
          }else{
            d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$Econ.FCR.Period))
          }
          return( d ) 
    })  
  }
})  
#.......................................................... S2
output$scatterPlot.EndAvWeight.PeriodSFR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EndAvWeight.PeriodSFR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=SFR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$SFR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S3
output$scatterPlot.EndAvWeight.PeriodSGR <- renderPlot({ 
#Re-run when button is clicked
if (input$goMultiPlot == 0){ 
    return() }
else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="SGR.Period", colour=input$radioDimMulti,
                  size = "Closing.Biomass", regr.method="loess") 
      print(p)
      })
    }
})
output$cor.stats.EndAvWeight.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$SGR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S4
output$scatterPlot.EndAvWeight.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="End.Av.Weight", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EndAvWeight.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=End.Av.Weight))
      }else{
      d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$End.Av.Weight))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S5
output$scatterPlot.PeriodEcon.FCR.PeriodSFR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodEcon.FCR.PeriodSFR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=SFR.Period))
      }else{
          d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$SFR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S6
output$scatterPlot.PeriodEcon.FCR.PeriodSGR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodEcon.FCR.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$SGR.Period))
      }
      return( d )
    })  
  }
})
#.......................................................... S7
output$scatterPlot.PeriodFCR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="Econ.FCR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodFCR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=Econ.FCR.Period))
      }else{
          d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$Econ.FCR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S8
output$scatterPlot.PeriodSFR.PeriodSGR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SFR.Period", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSFR.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SFR.Period, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SFR.Period, y=data$SGR.Period))
      }
      return( d )      
    })  
  }
})
#.......................................................... S9
output$scatterPlot.PeriodSFR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSFR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=SFR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$SFR.Period))
      }
      return( d )      
    })  
  }
})
#.......................................................... S10
output$scatterPlot.PeriodSGR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSGR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$SGR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S11
output$scatterPlot.EconFCR.EndAvWeight <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.EndAvWeight <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S12
output$scatterPlot.EconFCR.FCRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.FCRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S13
output$scatterPlot.EconFCR.SFRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SFR.Period", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.SFRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SFR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SFR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S14
output$scatterPlot.EconFCR.SGRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SGR.Period", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.SGRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SGR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SGR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S15
output$scatterPlot.EconFCR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})

#---------------------------------------------------------------------------------------------------
#     Multidimensional Dashboard
#---------------------------------------------------------------------------------------------------

datasetMD <- reactive({
  data <- passData()
#  data <- data[sample(nrow(data), input$sampleSize),]
  data <- data[  (data$From >= ymd(input$MD.dateRangeFrom[1]) & data$From <= ymd(input$MD.dateRangeFrom[2])) 
               & (data$To >= ymd(input$MD.dateRangeTo[1]) & data$To <= ymd(input$MD.dateRangeTo[2])) , ]
  
})

output$plotDashboard <- renderPlot({
 
  dsMD <- datasetMD()
  p <- ggplot(dsMD, aes_string(x=input$x, y=input$y)) + geom_point(size=2.5) 
  
  if (input$color != 'None')
    p <- p + aes_string(color=input$color)
  
  facets <- paste(input$facet_row, '~', input$facet_col)
  if (facets != '. ~ .')
    p <- p + facet_grid(facets)

  if (input$xmeans)
      p <- p + geom_line( stat = "vline", xintercept="mean")
  
  if (input$ymeans)
      p <- p + geom_line( stat = "hline", yintercept="mean")
  
  if (input$total.xmeans)
  { 
    avgx = mean(as.numeric(dsMD[,input$x]))
    p <- p + geom_vline( xintercept=avgx, color="darkred", linetype="dashed", size=1.5)
  }
  if (input$total.ymeans)
  { 
    avgy = mean(as.numeric(dsMD[,input$y]))
    p <- p + geom_hline( yintercept=avgy, color="darkred", linetype="dashed", size=1.5)
  }
  if (input$smooth)
      p <- p + geom_smooth()

  if ( input$comp.ranges)
  {   
    smtr <- stat_summary(fun.data ="mean_cl_boot", geom="crossbar", conf.int=0.95, width=0.3, B=1000, na.rm=T, reps=F) 
    p <- p + smtr
  }    
  
  if ( input$benchmarker)  
  {
    p <- p + geom_abline(intercept=0, slope=1)
  }
  print(p)
  
})


#---------------------------------------------------------------------------------------------------
#     Multidimensional Interactive Dashboard
#---------------------------------------------------------------------------------------------------

#  datasetMD <- reactive({
#   data <- passData()
#   #  data <- data[sample(nrow(data), input$sampleSize),]
#   data <- data[  (data$From >= ymd(input$MD.dateRangeFrom[1]) & data$From <= ymd(input$MD.dateRangeFrom[2])) 
#                  & (data$To >= ymd(input$MD.dateRangeTo[1]) & data$To <= ymd(input$MD.dateRangeTo[2])) , ]
#  })
# 
# 
#  output$plot.Interactive.Dashboard <- renderMetricsgraphics({
#     
#     dsMD <- datasetMD()
#     
#     if (input$color != 'None'){
#       dsMD %>% 
#           mjs_plot(x= input$x, y=input$y) %>% 
#             mjs_point(point_size = 1.5, color_accessor=input$color) -> plot.dashboard
#     }else{
#       dsMD %>% 
#         mjs_plot(x= input$x, y=input$y) %>% 
#         mjs_point(point_size = 1.5) -> plot.dashboard
#     }
#     
#     print(plot.dashboard)
#     
#  })
# 
# #  print(plot.dashboard)
# #    mjs_point(point_size = 1.5, color_accessor=input$color, color_type="category", 
# #               size_accessor=Biomass, least_squares=TRUE) %>% mjs_labs(x="Average Weight", y="FCR")
# #   
#   
#   
# ##})



#---------------------------------------------------------------------------------------------------
#     Regression
#---------------------------------------------------------------------------------------------------
# Tab: Build
#

output$explanatoryVar <- renderUI({
  if (is.null(input$responseVar)) { return() }
  regressors <- list("End.Av.Weight", "Start.Av.Weight", "Days", "Period.Feed.Qty",
                     "Suggested.Feed.Qty", "Econ.FCR.Period", "SFR.Period", "SGR.Period",
                     "LTD.Mortality", "Avg.Temperature", "Age")
  regressors <- regressors[ regressors != input$responseVar ]   
  checkboxGroupInput(inputId='explanatory.Variables', label=h3('Explanatory Variable(s):'), 
                     choices=regressors, selected=regressors[1])
})


runRegression <- reactive({
        if (is.null(input$explanatory.Variables)) return()
        regres <- list(input$responseVar,input$explanatory.Variables)
        
        # "data": dataset that based on the user choices in the first page
        data <- passData()  
        ds <- data[ , names(data) %in% unlist(regres) ]
        
        if ( input$radioModel!=3 ){
            fmla = as.formula(paste(input$responseVar," ~ ",paste(input$explanatory.Variables, collapse="+")))
            if ( input$radioModel==1 ) # "Linear"
            {
              model <- lm(formula=fmla, data=ds )
              return(model)
            }else if (input$radioModel==2 )  # "Generalized Linear Models"
            {
              model <- glm(formula=fmla, family=gaussian(link='identity'), data=ds, control=glm.control(maxit=1000,trace=FALSE), model=TRUE)
              return(model)
            }
        }else if ( input$radioModel==3 ){ # "Generalized Additive Models"
            fmla = as.formula(paste(input$responseVar,"~",paste("s(", input$explanatory.Variables, ", bs= \"cr\" )", collapse="+") )) 
            model <- gam(formula=fmla, data=ds)
            return(model)
        } 
})

output$fmla <- renderText({
  if (input$goRegression == 0){
    return() }
  else{ 
   isolate({    
     if ( input$radioModel!=3 ){
        paste( as.character(input$responseVar), paste(input$explanatory.Variables, collapse=" + "), sep=" ~ ")  
     }else{
        paste(input$responseVar," ~ ",paste("s(", input$explanatory.Variables, ", bs=\"cr\" )", collapse="+"))
     }  
  })  # end isolate
  } # end if...else
})

output$regression_R <- renderPrint({
  
  if (input$goRegression == 0){
    return() }
  else{ 
     isolate({ 
  
        if( !is.null(input$responseVar)){
          
          mod <- runRegression()
          s<-summary(mod)
          
          if ( input$radioModel==1 ){ # "Linear"
            res.cor <- matrix( c( s$r.squared*100, s$adj.r.squared*100, AIC(mod) ), c(1,3) )
            colnames(res.cor) <- c( " R-Squared ",  " Adjusted R-Squared ", "AIC" )
            print(res.cor[1,], row.names=FALSE, digits=5)
          }else if (input$radioModel==2 ){  # "GLM"
            res.cor <- matrix( c( s$aic, s$deviance), c(1,2) )
            colnames(res.cor) <- c("AIC", "Residuals Deviance")
            print(res.cor[1,], row.names = FALSE, justify='centred', nsmall=3 )
          }else if (input$radioModel==3 ){  # "GAM"
            if (mod$converged == TRUE){
                  res.cor <- matrix( c( s$r.sq*100, mod$aic, mod$gcv.ubre ), c(1,3) )
                  colnames(res.cor) <- c(" Adjusted R-Squared ", "AIC", "Generalized Cross Validation - GCV")
                  res.cor.df <- as.data.frame(res.cor) 
                  print(res.cor.df[1,], row.names = FALSE, justify='centred', nsmall=3 )
            }else{
                  print(data.frame(Warning="The GAM method could not converged. Try again using another model."))
            }  
          }
        } 
        else
        {
          print(data.frame(Warning="Please select Model Parameters."))
        }
  
    }) # end isolate
  } # end if...else
  
})

output$regression_Table_residuals <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
        if( !is.null(input$responseVar)){
          resids<-summary(runRegression()$residuals)
          print(resids)
        } else {
          print(data.frame(Warning="Please select Model Parameters."))
        }
      }) # end isolate
    } # end if...else
})

output$plot_lm_13 <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      mod <- runRegression()
      if ( input$radioModel==1 || input$radioModel==2 ){ # "Linear" or "GLM"  
          par(mfrow=c(1,2))
          plot(mod, which = c(1,3), id.n=10)
      }
      else if ( input$radioModel==3 ){ # "GAM"
        if (mod$converged == TRUE){
          if ( length(input$explanatory.Variables)%%2 == 0 )
          {
              par(mfrow=c(length(input$explanatory.Variables)/2, 2),oma=c(1,1,1,0), mar=c(2,1,1,2), tcl=-0.1, mgp=c(2,1,0) )
              plot.gam(mod, residuals=TRUE, pch=19, height="800px")
          }
          else{
              par(mfrow=c( length(input$explanatory.Variables)%/%2 + 1, 2),oma=c(1,1,1,0), mar=c(2,1,1,2),tcl=-0.1,mgp=c(2,1,0) )
              plot.gam(mod, residuals=TRUE, pch=19, height="800px")
          }
        }
      }
    }) # end isolate
  } # end if...else
})

output$plot_lm_24 <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
     isolate({  
       fit <- runRegression()
       if ( input$radioModel==1 || input$radioModel==2 ){ # "Linear" or "GLM"  
          par(mfrow=c(1,2))
          plot(fit, which = 2, id.n=10)
          cutoff <- 4/(nrow(fit$model)-length(fit$coefficients)-2)
          plot(fit, which=4, cook.levels=cutoff)
          abline(h=cutoff, lty=2, col="blue")
       }else if ( input$radioModel==3 ){ # "GAM" 
         if (fit$converged == TRUE){
           par(mfrow=c(2,2))
           gam.check(fit)
         }
       }
    }) # end isolate
  } # end if...else
})

# output$regression_Table_coeff <- renderPrint({
#   if( !is.null(input$responseVar)){
#     coefs<-coef(runRegression())
#     print(coefs)
#   } else {
#     print(data.frame(Warning="Please select Model Parameters."))
#   }
# }) 

output$regression_Table_sign_coeff <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
        if( !is.null(input$responseVar)){
          mod <- runRegression()
          s<-summary(mod)
          print(s$coefficients)
        }else{
          print(data.frame(Warning="Please select Model Parameters."))
        }
    }) # end isolate
  } # end if...else
})


output$regression_Table_sign_param_gam <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      if( !is.null(input$responseVar)){
        mod <- runRegression()
        s<-summary(mod)
        if (mod$converged == TRUE){ 
          print(s$p.table)
        }
      }else{
          print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})

output$regression_Table_sign_coeff_gam <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      if( !is.null(input$responseVar)){
        mod <- runRegression()
        s<-summary(mod)
        if (mod$converged == TRUE){ 
            print(s$s.table)
        }
      }else{
            print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})

output$regression_CI <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      
      if( !is.null(input$responseVar)){
        ci <- confint(runRegression())
        print(ci)
      } else {
        print(data.frame(Warning="Please select Model Parameters."))
      }
  }) # end isolate
  } # end if...else
})

output$regression_Anova <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      if( !is.null(input$responseVar)){
        anv <- anova(runRegression())
        print(anv)
      } else {
        print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})

output$regression_outliers <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      if( !is.null(input$responseVar)){
        outliers <- outlierTest(runRegression())
        print(outliers)
      } else {
        print(data.frame(Warning="Please select Model Parameters."))
      }
  }) # end isolate
  } # end if...else
})

output$plot_Infl <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      influencePlot(runRegression(),  main="Influence Plot", scale=5, id.n=10, id.cex=.75, id.col='blue',
                    sub="Circle size is proportional to Cook’s distance")
  }) # end isolate
  } # end if...else
})

output$table_Infl <- renderDataTable({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      p<-influencePlot(runRegression(), id.n=10)
  return(p) 
  }) # end isolate
  } # end if...else
})

output$gam.mod.coeffs <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      if( !is.null(input$responseVar)){
        mod <- runRegression()
        if (mod$converged == TRUE){ 
          cof.df <- data.frame(mod$coefficients)
          names(cof.df)<- "Coefficients"
          print(cof.df)
        }
      }else{
        print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})

run_Relative.Importance <- reactive({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      if (is.null(input$explanatory.Variables) || length(input$explanatory.Variables) <2) return()
      fit <- runRegression()
      metrics <- calc.relimp(fit, type = "car")
      return(metrics) 
    }) # end isolate
  } # end if...else
})


output$Rel.Impo <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
        if (is.null(input$explanatory.Variables) || length(input$explanatory.Variables) <2) 
          return(print(data.frame(Warning="Please select more Regressors Parameters.")))
        metrics <- run_Relative.Importance()
        m<-metrics$car*100
        print(round(m, digits = 2) )
  }) # end isolate
  } # end if...else
})  

output$bar.Rel.Impo <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      if (is.null(input$explanatory.Variables) || length(input$explanatory.Variables) <2) 
        return(print(data.frame(Warning="Please select more Regressors Parameters.")))
      metrics <- run_Relative.Importance()
      m<-metrics$car*100
      barplot(m, xlab='% of Responce Variance', main=paste('Relative Importances for', input$responseVar, sep=' '), 
              horiz=TRUE, las=1,cex.names=0.8, col='blue')
    }) # end isolate
  } # end if...else
})

#---------------------------------------------------------------------------------------------------
# Tab: Predict with Regression Model
#
# output$inpts_explanatoryVar <- renderUI({
#                 lapply(1:length(input$explanatory.Variables), function(i) {
#                   name <- paste0('num_', input$explanatory.Variables[i]) 
#                   numericInput(name, label = h4(paste0("Numeric input for ",  
#                                       input$explanatory.Variables[i])), value = NA) 
#                 })
# })
  

output$pred.actual.plot <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      fit <- runRegression()
      data <- passData()
      newds <- subset(data, select=input$explanatory.Variables)

      preds <- predict(fit,newdata = newds)
      
      df <- data.frame( subset(data,select=input$responseVar), preds ) 
      names(df) <- c( "Actual", "Prediction" )
      View(df)
      ggplot(df, aes_string(x="Actual", y="Prediction") ) + geom_point() + geom_abline(col="red")
    })
  }
})


output$value <- renderPrint({ 
  
  if (input$goPredict == 0){
    return() }
  else{ 
    isolate({
        fit <- runRegression()
      
        if (!is.null(input$num_FCR)){
           newdata <- data.frame('Econ.FCR.Period'= input$num_FCR)
        }
        if (!is.null(input$num_Days)){
           newdata <- cbind(newdata, 'Days'=input$num_Days)
        }
        if (!is.null(input$num_Start.Av.Weight)){
          newdata <- cbind(newdata, "Start.Av.Weight"=input$num_Start.Av.Weight)
        }
        if (!is.null(input$num_Period.Feed.Qty)){
          newdata <- cbind(newdata, 'Period.Feed.Qty'=input$num_Period.Feed.Qty)
        }
        if (!is.null(input$num_Suggested.Feed.Qty)){
          newdata <- cbind(newdata,  'Suggested.Feed.Qty'=input$num_Suggested.Feed.Qty)
        }
        if (!is.null(input$num_SFR.Period)){
          newdata <- cbind(newdata, "SFR.Period"=input$num_SFR.Period)
        }
        if (!is.null(input$num_SGR.Period)){
          newdata <- cbind(newdata, "SGR.Period"=input$num_SGR.Period)
        }
        if (!is.null(input$num_End.Av.Weight)){
          newdata <- cbind(newdata,  "End.Av.Weight"=input$num_End.Av.Weight)
        }
        if (!is.null(input$num_LTD.Mortality)){
          newdata <- cbind(newdata,"LTD.Mortality"=input$num_LTD.Mortality)
        }
        if (!is.null(input$num_Avg.Temperature)){
          newdata <- cbind(newdata,"Avg.Temperature"=input$num_Avg.Temperature)
        }
        pred_val <- predict(fit, newdata)
        names(pred_val) <- as.character(input$responseVar)
        return( pred_val )
    })
  }
  
})


#---------------------------------------------------------------------------------------------------
#     Analysis of Variance (ANOVA, Factorial ANOVA, MANOVA)
#---------------------------------------------------------------------------------------------------

 runANOVA <- reactive({
  if (is.null(input$Ind.Vars)){ return() }
  aov.list.vars <- list(input$Dep.Var, input$Ind.Vars)
  
  # "data": dataset that based on the user choices in the first page
  data <- passData()  
  ds <- data[ , names(data) %in% unlist(aov.list.vars) ]
 
  if ( input$anova.test==1 || input$anova.test==2 ){
       fmla = paste( paste(as.character(input$Dep.Var),collapse=" + "), paste(as.character(input$Ind.Vars), collapse=" + "), sep=" ~ " ) 
  }else{
       fmla = paste( paste(input$Dep.Var,collapse=" + "), paste(as.character(input$Ind.Vars), collapse=" * "), sep=" ~ " )
  }
  
  model.aov <- aov(formula=as.formula(fmla), data=ds)
 
  return(model.aov)

})
  
output$fmla.aov <- renderText({
  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({   
      
      if ( input$anova.test==1 || input$anova.test==2 ){
        fmla = paste( paste(as.character(input$Dep.Var),collapse=" + "), paste(as.character(input$Ind.Vars), collapse=" + "), sep=" ~ " )
      }else{
        fmla = paste( paste(as.character(input$Dep.Var),collapse=" + "), paste(as.character(input$Ind.Vars), collapse=" * "), sep=" ~ " )
      }
      
    })  # end isolate
  } # end if...else
})


output$summary.aov <- renderPrint({
  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({   
    if ( !is.null(input$Dep.Var) ){
         mod <- runANOVA()
         s <- summary(mod)
         print(s)
     }
     else{ 
         print(data.frame(Warning="Please select Model Parameters."))
     }
    }) # end isolate
  } # end if...else
})    

output$plot.aov <- renderPlot({
  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({ 
      mod <- runANOVA()
      par(mfrow=c(2,2))
      plot(mod)
    })
  }
})

output$sign.diffs.Tukey <- renderPrint({
  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({ 
      mod <- runANOVA()
      sign.diffs.Tukey <- TukeyHSD(mod)
      print(sign.diffs.Tukey)
    })
  }
})

output$plot.TukeyHSD <- renderPlot({

  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({ 
      mod <- runANOVA()
      par(las=1)
      plot(TukeyHSD(mod))
    })
  }  
})


  
}) # end shinyServer