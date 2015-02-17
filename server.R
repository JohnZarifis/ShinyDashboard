
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinydashboard)
library(RODBC)
library(dygraphs)

functionThatGetsData <- function(){
  ch <- odbcConnect("TestRODBC",uid='sa',pwd = "1q2w3e!Q@W#E") #("SEN_DB", uid = "nireus", pwd = "nireus")
  DataSet<- as.data.frame(sqlQuery(ch, 
                                      paste("select CostDate,FishNo,c.MAB,DayTemp,g.Designation,l.Designation,s.Designation from costmain c
                                      inner join cagelot t
                                      on c.cagelotid = t.cagelotid
                                      inner join cage g
                                      on g.cageid = t.cageid
                                      inner join lot l
                                      on l.lotid = t.lotid
                                      inner join site s 
                                      on s.siteid = g.siteid")),stringsAsFactors = FALSE)
  odbcClose(ch)
  return(DataSet)
  
}


shinyServer(function(input, output, session){
  
  output$orderNum <- renderText({sourceData()[1,1]})#renderText({ 10*2 })
  
  output$progress <- renderText({ "70%" })
  
  output$progressIcon <- renderUI({ icon("list") })
  
  output$approvalBox <- renderUI({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  sourceData <- reactive({
    invalidateLater(100000,session)
    
    functionThatGetsData()
    
  })
  
#   pollData <- reactivePoll(4000, session,
#                            # This function returns the time that the logfile was last
#                            # modified
#                            checkFunc = function() {
#                              if (file.exists(logfilename))
#                                file.info(logfilename)$mtime[1]
#                              else
#                                ""
#                            },
#                            # This function returns the content of the logfile
#                            valueFunc = function() {
#                              readLines(logfilename)
#                            }
# )
  
  
  
})

