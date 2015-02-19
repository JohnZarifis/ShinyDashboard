### Version 14.02.2015
#
# ------------------------
# Create the dataset
# ------------------------
create_dataset <- function(dataset){
  
  data <- data.frame('Orientation' = substr(dataset$Unit,1,1), 
                     'System' = substr(dataset$Unit,2,2),
                     'Cage' = substr(dataset$Unit,nchar(as.character(dataset$Unit))-1,nchar(as.character(dataset$Unit))-1),
                     'Section' = substr(dataset$Unit,nchar(as.character(dataset$Unit)),nchar(as.character(dataset$Unit))),
                     'Unit' = dataset$Unit,
                     'Batch' = dataset$Batch, 
                     "Hatchery" = dataset$Hatchery, 
                     "Origin.Year" = as.character(dataset$Origin.Year), 
                     "Origin.Month" = dataset$Origin.Month,
                     "From" = dmy(dataset$From), 
                     "To" = dmy(dataset$To), 
                     "Month.Sampling" = month(dataset$To,label = TRUE),
                     "Start.Av.Weight" = dataset$Start.Av..Wt., 
                     "End.Av.Weight" = dataset$End.Av.Wt.,
                     "Model.End.Av.Weight.Act.Feed" = dataset$Model.End.Av..Wt..Act..Feed, 
                     "Av.Weight.Deviation" = dataset$Av..Wt..Deviation...., 
                     "Av.Weight.Before.Sampling" = dataset$Av..Wt..Before.Sampl., 
                     "Model.End.Av.Weight.Suggested.Feed" = dataset$Model.End.Av..Wt..Sugg..Feed, 
                     "Actual.Feed" = dataset$Actual.Feed, 
                     "Feed.Category" = dataset$Feed.Category, 
                     "Supplier" = dataset$Supplier, 
                     "Period.Feed.Qty" = dataset$Period.Feed.Qty,
                     "Suggested.Feed.Qty" = dataset$Suggested.Feed.Qty,
                     "Opening.Fish.No" = dataset$Opening.Fish.No, 
                     "Opening.Biomass" = dataset$Opening.Biomass,
                     "Closing.Fish.No" = dataset$Closing.Fish.No, 
                     "Closing.Biomass" = dataset$Closing.Biomass,      
                     "Harvest.Biomass" = dataset$Harvest.Biomass, 
                     "Biomass.Produced" = dataset$Biomass.Produced,     
                     "Biomass.Produced.Before.Sampling" = dataset$Biomass.Produced.Before.Sampl., 
                     "Econ.FCR.Period" = dataset$Econ..FCR.Period,
                     "FCR.Before.Sampling" = dataset$Econ.FCR.Period.Before.Sampl., 
                     "Mortality.No" = dataset$Mortality.No, 
                     "Model.Mortality.No" = dataset$Model.Mortality.No, 
                     "Mortality.Deviation" = dataset$Mortality.Deviation....,
                     "SFR.Period" = dataset$SFR.Period...., 
                     "SFR.Period.Before.Sampling" = dataset$SFR.Period.....Before.Sampl.,     
                     "SGR.Period" = dataset$SGR.Period...., 
                     "Max.Food.Qty" = dataset$Max.Feed.Qty,
                     "Food.Price" = dataset$Food.Price, 
                     "LTD.Econ.FCR" = dataset$LTD.Econ..FCR, 
                     "LTD.Mortality" = dataset$LTD.Mortality..,  
                     "LTD.Mortality.No" = dataset$LTD.Mortality.No, 
                     "Avg.Oxygene" = dataset$Avg..Oxygene, 
                     "Avg.Temperature" = dataset$Avg..Temp., 
                     "Feeding.Policy" = dataset$Feeding.Policy, 
                     "Period.Day.Degrees" = dataset$Period.Day.Degrees,
                     "Start.Av.Weight.Category" =  dataset$Start.Av..Weight.Category,
                     "End.Av.Weight.Category" = dataset$End.Av..Weight.Category,
                     "Final.Harvest" = dataset$Final.Harvest,
                     "Start.Av.Weight.BioCat" = dataset$Start.Av.Weight.BioCat,
                     "End.Av.Weight.BioCat" = dataset$End.Av.Weight.BioCat,
                     "Age" = dataset$AGE, 
                     "Batch.Start.Date" = dmy(dataset$BATCH.START.DATE),
                     "Days" = interval( dmy(dataset$From), dmy(dataset$To) )%/%days(1)
                    )
  
  #   For debugging  
  #  View(data)
  #   str(data)
  # print(nrow(data))
  
  return(data)
  
  #"ProductionTimeDays" = paste(01,dataset$Origin.Month, dataset$Origin.Year, sep="-" )
}



#-----------------------------------------------------------------------------------------------------
##   Function that Summarizes data.
##   Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE,w,quant) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col, w) {
                   c(N         = length2(xx[[col]], na.rm=na.rm),
                     mean      = mean   (xx[[col]], na.rm=na.rm),
                     sd        = sd     (xx[[col]], na.rm=na.rm),
                     w.mean    = weighted.mean(xx[[col]],xx[[w]]),
                     sum.w     = sum(xx[[w]]),
                     min       = min(xx[[col]]),
                     max       = max(xx[[col]]),
                     quant     = quantile(xx[[col]],quant)
                   )
                 },
                 measurevar,w
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval  ?? t or z for us??: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  # Todo create function for percentiles...
  return(datac)
}

######-------end of function-----------####



