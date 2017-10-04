# SERVER ----
# forecast_Q, V September 2017
# tobias siegfried, hydrosolutions Ltd., 2017

shinyServer(function(input,output,session) {
  
  # STATIONS OVERVIEW PAGE ---------------------------------------------------------------
  # REACTIVE INPUT LOCATION
  textbits <- reactive({
    textbits <- locsUnique[locsUnique$loc==input$location,]
    return(textbits)
  })
  # TEXT OUTPUT
  output$rbinfo <- renderText({
    paste("River Basin: ", textbits()$river)
  })
  output$stationName <- renderText({
    paste("Station Name: ", textbits()$loc)
  })
  output$stationType <- renderText({
    paste("Station Type: ",  textbits()$type)
  })
  output$stationCode <- renderText({
    paste("Station Code: ",  textbits()$code)
  })
  output$locText <- renderText({
    paste("Location (lat,lon): ", textbits()$lat, textbits()$lon)
  })
  output$firstData <- renderText({
    paste("First Data: ", data()$firstData)
  })
  output$lastData <- renderText({
    paste("Last Data: ", data()$lastData)
  })
  output$currentDate <- renderText({
    paste("Current Date: ",as.character(Sys.time()))
  })
  output$stationNameEdit <- renderText({ # data edit page
    paste("Station: ", textbitsEdit()$loc)
  })
  # DECADE CALCULATOR
  output$decadeText <- renderText({
    today <- Sys.time()
    doy <- strftime(today, format = "%j")
    doy <- floor(as.numeric(doy)/10 + 1)
    paste("Current Decade: ", as.character(doy))
  })
  output$decadeNumber <- reactive({
    today <- Sys.time()
    doy <- strftime(today, format = "%j")
    doy <- floor(as.numeric(doy)/10 + 1)
    list("decadeNumber"=doy,"year"=substring(today,1,4))
  })
  
  
  # FORECAST PAGE ------------------------------------------------------------------------
  # DECADAL
  output$fc521400dec <- renderText({
    idxFC <- which(!is.na(forecast.dec[[1]][,3])==TRUE) %>% max
    target <- coredata(forecast.dec[[1]][idxFC,1])
    targetY <- substring(time(forecast.dec[[1]][idxFC,1]),1,4) %>% as.numeric
    fcValue <- coredata(forecast.dec[[1]][idxFC,3])
    doy <- strftime(Sys.time(), format = "%j")
    currD <- floor(as.numeric(doy)/10 + 1)
    currY <- as.numeric(substring(Sys.time(),1,4))
    compNorm <- normsQ.dec[1,target]
    if (currD<36){
      if ((target==(currD+1))&(targetY==currY)){ # VALID
        fcStr <- paste("[Valid]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else { # OUTDATED
        fcStr <- paste("[Outdated]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    } else {
      if (currD==1){
        fcStr <- paste("[Valid]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else {
        fcStr <- paste("[Outdated]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""),
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    }
  })
  output$obsFcComp521400dec <- renderPlot({
    idx <- 1
    idxFC <- which(!is.na(forecast.dec[[1]][,3])==TRUE) %>% max
    fcDec <- forecast.dec[[idx]]
    tsN <- normsQ.dec[idx,(-37:-38)]
    tsN <- cbind(tsN,tsN) %>% t
    fcDec$decade <- tsN
    latestFC <- fcDec[idxFC,3]
    fcDec <- merge(fcDec,latestFC)
    names(fcDec) <- c("Norm","Q","Forecast","Latest Forecast")
    autoplot.zoo(fcDec[,-4],facets = NULL) + scale_colour_manual(values=c("green","blue","red","red")) + labs(x = "Date", y = "Q [m^3/s]")
    #last_plot() + geom_point(aes(x = Index, y = Value), data = fortify(fcDec[idxFC,4], melt = TRUE)) + labs(x = "Date", y = "Q [m^3/s]")
  })
  output$fc520800dec <- renderText({
    idxFC <- which(!is.na(forecast.dec[[2]][,3])==TRUE) %>% max
    target <- coredata(forecast.dec[[2]][idxFC,1])
    targetY <- substring(time(forecast.dec[[2]][idxFC,1]),1,4) %>% as.numeric
    fcValue <- coredata(forecast.dec[[2]][idxFC,3])
    doy <- strftime(Sys.time(), format = "%j")
    currD <- floor(as.numeric(doy)/10 + 1)
    currY <- as.numeric(substring(Sys.time(),1,4))
    compNorm <- normsQ.dec[2,target]
    if (currD<36){
      if ((target==(currD+1))&(targetY==currY)){ # VALID
        fcStr <- paste("[Valid]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else { # OUTDATED
        fcStr <- paste("[Outdated]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    } else {
      if (currD==1){
        fcStr <- paste("[Valid]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else {
        fcStr <- paste("[Outdated]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""),
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    }
  }) 
  output$obsFcComp520800dec <- renderPlot({
    idx <- 2
    idxFC <- which(!is.na(forecast.dec[[1]][,3])==TRUE) %>% max
    fcDec <- forecast.dec[[idx]]
    tsN <- normsQ.dec[idx,(-37:-38)]
    tsN <- cbind(tsN,tsN) %>% t
    fcDec$decade <- tsN
    latestFC <- fcDec[idxFC,3]
    fcDec <- merge(fcDec,latestFC)
    names(fcDec) <- c("Norm","Q","Forecast","Latest Forecast")
    autoplot.zoo(fcDec[,-4],facets = NULL) + scale_colour_manual(values=c("green","blue","red","red")) + labs(x = "Date", y = "Q [m^3/s]")
  })
  output$fc520400dec <- renderText({
    idxFC <- which(!is.na(forecast.dec[[3]][,3])==TRUE) %>% max
    target <- coredata(forecast.dec[[3]][idxFC,1])
    targetY <- substring(time(forecast.dec[[3]][idxFC,1]),1,4) %>% as.numeric
    fcValue <- coredata(forecast.dec[[3]][idxFC,3])
    doy <- strftime(Sys.time(), format = "%j")
    currD <- floor(as.numeric(doy)/10 + 1)
    currY <- as.numeric(substring(Sys.time(),1,4))
    compNorm <- normsQ.dec[3,target]
    if (currD<36){
      if ((target==(currD+1))&(targetY==currY)){ # VALID
        fcStr <- paste("[Valid]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else { # OUTDATED
        fcStr <- paste("[Outdated]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    } else {
      if (currD==1){
        fcStr <- paste("[Valid]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else {
        fcStr <- paste("[Outdated]: Latest forecast available: Decade", 
                       target, paste("(", targetY, ")", sep=""),
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    }
  })
  output$obsFcComp520400dec <- renderPlot({
    idx <- 3
    idxFC <- which(!is.na(forecast.dec[[1]][,3])==TRUE) %>% max
    fcDec <- forecast.dec[[idx]]
    tsN <- normsQ.dec[idx,(-37:-38)]
    tsN <- cbind(tsN,tsN) %>% t
    fcDec$decade <- tsN
    latestFC <- fcDec[idxFC,3]
    fcDec <- merge(fcDec,latestFC)
    names(fcDec) <- c("Norm","Q","Forecast","Latest Forecast")
    autoplot.zoo(fcDec[,-4],facets = NULL) + scale_colour_manual(values=c("green","blue","red","red")) + labs(x = "Date", y = "Q [m^3/s]")
  })
# MONTHLY
  output$fc521400mon <- renderText({
    idxFC <- which(!is.na(forecast.mon[[1]][,3])==TRUE) %>% max
    target <- mod(coredata(forecast.mon[[1]][idxFC,1])+1,36) # this +1 is vital here since it determines properly the first taret decade of the forecast month
    targetY <- substring(time(forecast.mon[[1]][idxFC,1]),1,4) %>% as.numeric
    fcValue <- coredata(forecast.mon[[1]][idxFC,3])
    doy <- strftime(Sys.time(), format = "%j")
    currD <- floor(as.numeric(doy)/10 + 1)
    currY <- as.numeric(substring(Sys.time(),1,4))
    compNorm <- normsQ.dec[1,target]
    if (currD<36){
      if ((target==(currD+1))&(targetY==currY)){ # VALID
        fcStr <- paste("[Valid]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else { # OUTDATED
        fcStr <- paste("[Outdated]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    } else {
      if (currD==1){
        fcStr <- paste("[Valid]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else {
        fcStr <- paste("[Outdated]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""),
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    }
  })
  output$obsFcComp521400mon <- renderPlot({
    idx <- 1
    idxFC <- which(!is.na(forecast.mon[[1]][,3])==TRUE) %>% max
    fcMon <- forecast.mon[[idx]]
    tsN <- normsQ.mon[idx,(-13:-14)]
    tsN <- cbind(tsN,tsN) %>% t
    fcMon$decade <- tsN
    latestFC <- fcMon[idxFC,3]
    fcMon <- merge(fcMon,latestFC)
    names(fcMon) <- c("Norm","Q","Forecast","Latest Forecast")
    autoplot.zoo(fcMon[,-4],facets = NULL) + scale_colour_manual(values=c("green","blue","red","red")) + labs(x = "Date", y = "Q [m^3/s]")
    #last_plot() + geom_point(aes(x = Index, y = Value), data = fortify(fcDec[idxFC,4], melt = TRUE)) + labs(x = "Date", y = "Q [m^3/s]")
  })
  output$fc520800mon <- renderText({
    idxFC <- which(!is.na(forecast.mon[[2]][,3])==TRUE) %>% max
    target <-  mod(coredata(forecast.mon[[2]][idxFC,1])+1,36)
    targetY <- substring(time(forecast.mon[[2]][idxFC,1]),1,4) %>% as.numeric
    fcValue <- coredata(forecast.mon[[2]][idxFC,3])
    doy <- strftime(Sys.time(), format = "%j")
    currD <- floor(as.numeric(doy)/10 + 1)
    currY <- as.numeric(substring(Sys.time(),1,4))
    compNorm <- normsQ.dec[1,target]
    if (currD<36){
      if ((target==(currD+1))&(targetY==currY)){ # VALID
        fcStr <- paste("[Valid]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else { # OUTDATED
        fcStr <- paste("[Outdated]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    } else {
      if (currD==1){
        fcStr <- paste("[Valid]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else {
        fcStr <- paste("[Outdated]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""),
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    }
  })
  output$obsFcComp520800mon <- renderPlot({
    idx <- 2
    idxFC <- which(!is.na(forecast.mon[[1]][,3])==TRUE) %>% max
    fcMon <- forecast.mon[[idx]]
    tsN <- normsQ.mon[idx,(-13:-14)]
    tsN <- cbind(tsN,tsN) %>% t
    fcMon$decade <- tsN
    latestFC <- fcMon[idxFC,3]
    fcMon <- merge(fcMon,latestFC)
    names(fcMon) <- c("Norm","Q","Forecast","Latest Forecast")
    autoplot.zoo(fcMon[,-4],facets = NULL) + scale_colour_manual(values=c("green","blue","red","red")) + labs(x = "Date", y = "Q [m^3/s]")
    #last_plot() + geom_point(aes(x = Index, y = Value), data = fortify(fcDec[idxFC,4], melt = TRUE)) + labs(x = "Date", y = "Q [m^3/s]")
  })
  output$fc520400mon <- renderText({
    idxFC <- which(!is.na(forecast.mon[[3]][,3])==TRUE) %>% max
    target <-  mod(coredata(forecast.mon[[3]][idxFC,1])+1,36)
    targetY <- substring(time(forecast.mon[[3]][idxFC,1]),1,4) %>% as.numeric
    fcValue <- coredata(forecast.mon[[3]][idxFC,3])
    doy <- strftime(Sys.time(), format = "%j")
    currD <- floor(as.numeric(doy)/10 + 1)
    currY <- as.numeric(substring(Sys.time(),1,4))
    compNorm <- normsQ.dec[3,target]
    if (currD<36){
      if ((target==(currD+1))&(targetY==currY)){ # VALID
        fcStr <- paste("[Valid]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else { # OUTDATED
        fcStr <- paste("[Outdated]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    } else {
      if (currD==1){
        fcStr <- paste("[Valid]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""), 
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      } else {
        fcStr <- paste("[Outdated]: Latest forecast available: Month", 
                       ceil(target/3), paste("(", targetY, ")", sep=""),
                       "- mean discharge:", round(fcValue,digits = 0), "[m^3/s], Norm:", round(compNorm,digits=1), "[m^3/s]")
      }
    }
  })
  output$obsFcComp520400mon <- renderPlot({
    idx <- 3
    idxFC <- which(!is.na(forecast.mon[[1]][,3])==TRUE) %>% max
    fcMon <- forecast.mon[[idx]]
    tsN <- normsQ.mon[idx,(-13:-14)]
    tsN <- cbind(tsN,tsN) %>% t
    fcMon$decade <- tsN
    latestFC <- fcMon[idxFC,3]
    fcMon <- merge(fcMon,latestFC)
    names(fcMon) <- c("Norm","Q","Forecast","Latest Forecast")
    autoplot.zoo(fcMon[,-4],facets = NULL) + scale_colour_manual(values=c("green","blue","red","red")) + labs(x = "Date", y = "Q [m^3/s]")
    #last_plot() + geom_point(aes(x = Index, y = Value), data = fortify(fcDec[idxFC,4], melt = TRUE)) + labs(x = "Date", y = "Q [m^3/s]")
  })
  
  # DATA PAGE ------------------------------------------------------------------------
  # REACTIVE INPUT SELECTION
  textbitsEdit <- reactive({ # for data edit page
    textbitsEdit <- locsUnique[locsUnique$loc==input$location_edit,]
    return(textbitsEdit)
  })
  
  output$dataTableEdit1TypeText <- renderText({
    if (typeEdit() == "gauge"){
      "Discharge Q [m3/s]"
    } else
    {
      "Precipitation [mm] and Temperature [deg. C]"
    }
  })
  
  values <- reactiveValues()
  observeEvent(input$location_edit,{
    tabLength <- model.par$editTableL
    if (typeEdit() == "gauge"){
      tab <- tail(ts[,colnames(ts)==input$location_edit],tabLength) # alternative data subsetting ts[time(ts)>as.Date("2000-01-01"),]
      # the way it is implemented now should show always the full year... testing now...
      tt <- zoo(as.numeric(substring(time(tab),1,4)),time(tab))
      tab <- merge(tt,decadeMaker(head(time(tab),1),tail(time(tab),1)),tab)
      names(tab) <- cbind("Year","Decade","Q")
      values[[input$location_edit]] <- tab %>% as.data.frame
    } else {
      tab <- tail(ts[,colnames(ts)==paste(input$location_edit,".P",sep="")],tabLength)
      tab1 <- tail(ts[,colnames(ts)==paste(input$location_edit,".T",sep="")],tabLength)
      tt <- zoo(as.numeric(substring(time(tab),1,4)),time(tab))
      tab <- merge(tt,decadeMaker(head(time(tab),1),tail(time(tab),1)),tab,tab1)
      names(tab) <- cbind("Year","Decade","P","T")
      values[[input$location_edit]] <- tab %>% as.data.frame
    }
  })
  observeEvent(input$submitData,{ # listening to 'Submit New Data' button
    temp <- hot_to_r(input$editTable1) %>% as.data.frame()
    values[[input$location_edit]] <- temp
    dimM <- dim(values[[input$location_edit]])[1]
    todayY <- as.numeric(substring(Sys.time(),1,4))
    
    if (typeEdit() == "gauge"){ # UPDATE GOOGLE SHEETS - Q
      
      statSelected <- textbitsEdit()$loc
      statID <- which(match(names(Q),statSelected)==1)
      
      rStartIdxQ <- temp[1,]$Year
      newDataQ <- Reshape(temp$Q,36,2) %>% t %>% as.data.frame()
      newDataQ["YEAR"] <- c(rStartIdxQ,rStartIdxQ+1)
      newDataQ <- newDataQ[c(37,1:36)]
      names(newDataQ) <- gsub("V", "D", names(newDataQ))
      df2update <- Q[[statID]]$data.df %>% as.data.frame()
      df2update[match(newDataQ$YEAR,df2update$YEAR),] <- newDataQ
      
      if (remote==1){
        putdata(input$location_edit,"Q",df2update,1) # UPDATE GOOGLE SHEETS
      } else {
        write.table(df2update,paste("data/",input$location_edit,"_Q.csv",sep=""),append=FALSE,dec = ".",sep="\t",row.names = FALSE)
        prepareDataFun(stationsLocN,modelListN,modelParN,gaugePMN,remote) # THIS FUNCTION NEEDS TO BE WRITTEN FIRST!
        browser()
      }
      
      # UPDATE WORKSPACE (QUESTION: CAN WE AVOID TO RELOAD ALL DATA AS DONE IN global.R?)
      ts[(dimM-model.par$editTableL+1):dimM,colnames(ts)==input$location_edit] <- temp[,colnames(temp)=="Q"] # desktop variable update!
      
      
    } else {  # UPDATE GOOGLE SHEETS - P,T
      
      statID_PT <- which(match(names(P),input$location_edit)==1)

      rStartIdxP <- temp[1,]$Year
      rStartIdxT <- temp[1,]$Year
      newDataP <- Reshape(temp$P,36,2) %>% t %>% as.data.frame()
      newDataT <- Reshape(temp$T,36,2) %>% t %>% as.data.frame()
      newDataT["YEAR"] <- c(rStartIdxT,rStartIdxT+1)
      newDataP["YEAR"] <- c(rStartIdxP,rStartIdxP+1)
      newDataT <- newDataT[c(37,1:36)]
      newDataP <- newDataP[c(37,1:36)]
      names(newDataT) <- gsub("V", "D", names(newDataT))
      names(newDataP) <- gsub("V", "D", names(newDataP))
      
      df2updateT <- T[[statID_PT]]$data.df %>% as.data.frame()
      df2updateP <- P[[statID_PT]]$data.df %>% as.data.frame()
      df2updateT[match(newDataT$YEAR,df2updateT$YEAR),] <- newDataT
      df2updateP[match(newDataP$YEAR,df2updateP$YEAR),] <- newDataP
      
      if (remote==1){
        putdata(input$location_edit,"P",df2updateP,1) # UPDATE GOOGLE SHEETS - P
        putdata(input$location_edit,"T",df2updateT,1) # UPDATE GOOGLE SHEETS - T
      } else {
        # local desktop saving and then reload all data!
      }
      # UPDATE WORKSPACE - WORK IN PROGRESS!!!!
      ts[(dimM-model.par$editTableL+1):dimM,colnames(ts)==paste(input$location_edit,".P",sep="")] <- temp[,colnames(temp)=="P"] # desktop variable update!
      ts[(dimM-model.par$editTableL+1):dimM,colnames(ts)==paste(input$location_edit,".T",sep="")] <- temp[,colnames(temp)=="T"] # further dependencies also need to be updated!
    }
  })
  
  # REACTIVE VALUES (OUTPUT FOR VISUALIZATION/X-CHECKING)
  df <- reactive({
    df <- values[[input$location_edit]]
  })
  
  # OUTPUT OF EDITTABLE1
  output$editTable1 <- renderRHandsontable({
    hot <- rhandsontable(df(), width = 400, height = 800, rowHeaderWidth = 100)
    hot <- hot %>% hot_col("Decade", format = "0") %>% hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  output$plotEditData <- renderPlot({
    if (typeEdit()=="gauge"){
      ts1 <- zoo(df()[,3],order.by = as.Date(rownames(df())))
      idxN <- which(normsQ.dec$loc == input$location_edit)
      tsN <- normsQ.dec[idxN,(-37:-38)]
      tsN <- cbind(tsN,tsN) %>% t
      ts2 <- zoo(coredata(tsN),order.by = as.Date(rownames(df())))
      ts <- cbind(ts1,ts2)
      names(ts) <- cbind("Q","Discharge Norm")
      autoplot.zoo(ts,facets = NULL)
    } else {
      #ttt <- 2
      ts1 <- zoo(df()[,3:4],order.by = as.Date(rownames(df())))
      names(ts1) <- cbind("P","T")
      autoplot.zoo(ts1,facets = Series ~ .)
    }
  })
  
  output$testTable <- renderTable({
    df()
  })
  
  # STATIONS OVERVIEW PAGE ---------------------------------------------------------------
  tsQdata <- reactive({tsQ[,colnames(tsQ)==input$location]})
  tsPdata <- reactive({tsP[,colnames(tsP)==paste(input$location,".P",sep="")]})
  tsTdata <- reactive({tsT[,colnames(tsT)==paste(input$location,".T",sep="")]})
  normsQdata <- reactive({
    normsQdata <- normsQ.dec[normsQ.dec$loc==input$location,]
    normsQdata <- subset(normsQdata, select = -c(loc,var))
    return(normsQdata)
  })
  normsPdata <- reactive({
    normsPdata <- normsP[normsP$loc==input$location,]
    normsPdata <- subset(normsPdata, select = -c(loc,var))
    return(normsPdata)
  })
  normsTdata <- reactive({
    normsPdata <- normsP[normsP$loc==input$location,]
    normsPdata <- subset(normsPdata, select = -c(loc,var))
    normsTdata <- normsT[normsT$loc==input$location,]
    normsTdata <- subset(normsTdata, select = -c(loc,var))
    normsPdata1 <- normsPdata
    normsPdata1[1,] <- as.numeric(normsTdata)
    normsTdata <- normsPdata1
    return(normsTdata)
  })
  
  # GAUGE SWITCHER 
  type <- reactive({
    type <- locsUnique[locsUnique$loc==input$location,]$type
    return(type)
  })
  typeEdit <- reactive({
    typeEdit <- locsUnique[locsUnique$loc==input$location_edit,]$type
    return(typeEdit)
  })
  
  code <- reactive({
    code <- locsUnique[locsUnique$loc==input$location,]$code
    return(code) # this is the code of the selected station, i.e either gauge or meteo station!
  })
  
  codeEdit <- reactive({
    codeEdit <- locsUnique[locsUnique$loc==input$location_edit,]$code
    return(codeEdit)
  })
  
  output$timeseriesGraphs <- renderDygraph({
    if (type() == "gauge") {
      dygraph(tsQdata(), main="Discharge Time Series",xlab="time",ylab="Q [m^3/s]") %>% dyRangeSelector()
    } else {
      toPlot <- cbind(tsPdata(),tsTdata())
      dygraph(toPlot, main = "Meteorological Station Time Series") %>% 
        dyAxis("y", label = "Precipitation [mm/decade]") %>%
        dyAxis("y2", label = "Temperature [deg. C]", independentTicks = FALSE) %>%
        dySeries("tsTdata()", axis = 'y2') %>%
        dyRangeSelector()
    }
  })
  
  output$NormsGraphs <- renderPlot({
    if (type() == "gauge") {
      actData <- tail(dfQ[dfQ$id==code(),],2)
      actData <- subset(actData, select = -c(YEAR,id))
      normsQdata <- rbind(normsQdata(),actData)
      normsQdata$id <- c(1,2,3)
      plot_normsQdata <- melt(normsQdata,id.var="id")
      ggplot(plot_normsQdata,aes(x=variable,y=value,group=id,color=factor(id))) + 
        geom_line() + 
        geom_point() +
        xlab("Decade") + ylab("Discharge [m^3/s]") +
        scale_colour_discrete(name = "Data", labels=c("Norm","Last Year Data","Current Year Data")) +
        theme(legend.position="top") +
        ggtitle("Discharge Norm and Current Data")
    } else {
      actDataP <- tail(dfP[dfP$id==code(),],2)
      actDataT <- tail(dfT[dfT$id==code(),],2)
      actDataP <- subset(actDataP, select = -c(YEAR,id))
      actDataT <- subset(actDataT, select = -c(YEAR,id))
      normsPdata <- rbind(normsPdata(),actDataP)
      normsTdata <- rbind(normsTdata(),actDataT)
      normsPdata$id <- c(1,2,3)
      normsTdata$id <- c(1,2,3)
      plot_normsPdata <- melt(normsPdata,id.var="id")
      plot_normsTdata <- melt(normsTdata,id.var="id")
      pT <- ggplot(plot_normsTdata,aes(x=variable,y=value,group=id,color=factor(id))) +
        geom_line() +
        geom_point() +
        xlab("Decade") + ylab("Temperature [deg. C]") +
        scale_colour_discrete(name = "Data", labels=c("Norm","Last Year Data","Current Year Data")) +
        theme(legend.position="top") +
        ggtitle("Temperature Norm and Current Data")
      pP <- ggplot(plot_normsPdata,aes(x=variable,y=value,group=id,color=factor(id))) +
        geom_line() +
        geom_point() +
        xlab("Decade") + ylab("Cumm. Precipitation [mm]") +
        scale_colour_discrete(name = "Data", labels=c("Norm","Last Year Data","Current Year Data")) +
        theme(legend.position="top") +
        ggtitle("Precipitation Norm and Current Data")
      multiplot(pT,pP,cols=1)
    }
  })
  
  output$forecastQual_16279_dec <- renderPlot({print(model.dec.qual[[1]][[1]])})
  output$forecastQual_16290_dec <- renderPlot({print(model.dec.qual[[2]][[1]])})
  output$forecastQual_16294_dec <- renderPlot({print(model.dec.qual[[3]][[1]])})
  output$forecastQual_16279_mon <- renderPlot({print(model.mon.qual[[1]][[1]])})
  output$forecastQual_16290_mon <- renderPlot({print(model.mon.qual[[2]][[1]])})
  output$forecastQual_16294_mon <- renderPlot({print(model.mon.qual[[3]][[1]])})
  
  output$model_16279_dec <- renderText({tt <- capture.output(summary(model.dec[[1]]))
  return(tt[1:4])
  })
  output$model_16290_dec <- renderText({tt <- capture.output(summary(model.dec[[2]]))
  return(tt[1:4])
  })
  output$model_16294_dec <- renderText({tt <- capture.output(summary(model.dec[[3]]))
  return(tt[1:4])
  })
  output$model_16279_mon <- renderText({tt <- capture.output(summary(model.mon[[1]]))
  return(tt[1:4])
  })
  output$model_16290_mon <- renderText({tt <- capture.output(summary(model.mon[[2]]))
  return(tt[1:4])
  })
  output$model_16294_mon <- renderText({tt <- capture.output(summary(model.mon[[3]]))
  return(tt[1:4])
  })
  
  output$model_16279_decEnsemble <- renderTable({tt <- summary(model.dec[[1]])
  return(tt)
  })
  output$model_16290_decEnsemble <- renderTable({tt <- summary(model.dec[[2]])
  return(tt)
  })
  output$model_16294_decEnsemble <- renderTable({tt <- summary(model.dec[[3]])
  return(tt)
  })
  output$model_16279_monEnsemble <- renderTable({tt <- summary(model.mon[[1]])
  return(tt)
  })
  output$model_16290_monEnsemble <- renderTable({tt <- summary(model.mon[[2]])
  return(tt)
  })
  output$model_16294_monEnsemble <- renderTable({tt <- summary(model.mon[[3]])
  return(tt)
  })
  
  # GRAPHICAL OUTPUT ----
  output$dygraphFullTimeSeriesQ <- 
    renderDygraph({dygraph(tsQdata(), main="Discharge Time Series",xlab="time",ylab="Q [m^3/s]") %>% dyRangeSelector() }) 
  output$dygraphFullTimeSeriesT <- 
    renderDygraph({dygraph(tsTdata(), main = "Temperature Time Series",xlab="time",ylab="T [deg C]") %>% dyRangeSelector() })
  output$dygraphFullTimeSeriesP <- 
    renderDygraph({dygraph(tsPdata(), main = "Precipitation Time Series",xlab="time",ylab="P [mm]") %>% dyRangeSelector() })
  
  output$normActualQ <- 
    renderDygraph({
      data2render <- as.data.frame(t(rbind("decade" = as.vector(1:36), "normQ"=data()$normQ,
                                           "previousYQ" = data()$tsQtable[nrow(data()$tsQtable)-1,-1],
                                           "currentYQ" = data()$tsQtable[nrow(data()$tsQtable),-1])))
      dygraph(data2render, main = tstitle()$normstitleQ,xlab="decade",ylab="Q [m^3/s]")
    })
  
  # HELP PAGE ----
  ############# GENERAL DEFINITIONS ############# 
  #SMTP Server Detail: list(host.name = "aspmx.l.google.com", port = 25) works for gmail recipient only. Check the spam folder.
  #Other options: list(host.name = "smtp.gmail.com", port = 465, user.name = "gmail_username", passwd = "password", ssl = TRUE)
  #See mailR for reference: https://github.com/rpremraj/mailR 
  smtp_setting = list(host.name = "aspmx.l.google.com", port = 25)
  
  #Stores the captcha text and result
  captcha <- reactiveValues()
  
  #Updates the captcha
  changecaptcha <- function() {
    set.seed(as.integer((as.double(Sys.time()) * 8764 + Sys.getpid()) %% 2 ^ 31))
    n1 = sample(c(1:10), 1)
    set.seed(as.integer((as.double(Sys.time()) * 345 + Sys.getpid()) %% 2 ^ 26))
    n2 = sample(c(1:10), 1)
    
    captcha$text = paste(n1, " + ", n2, " = ", sep = "")
    captcha$result = n1 + n2
  }
  
  #checks if the string in argument "x" is in the format of a valid e-mail adress
  isValidEmail <- function(x) {
    grepl(
      "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
      as.character(x),
      ignore.case = TRUE
    )
  }
  
  
  ############# BUILD DYNAMIC UI #############
  #Initialise Feedback Form UI Elements
  output$feedbackform <- renderUI({
    changecaptcha()
    tagList(
      textInput("sender", "your e-mail", value = ""),
      textInput("subject", "subject", value = ""),
      textAreaInput("message",
                    label = "your message",
                    value = ""),
      numericInput("captcha", label = captcha$text, value = 1),
      actionButton("send", "submit"),
      textOutput("value")
    )
  })
  
  ############# REACTIVE FUNCTION ############# 
  
  #Sends e-mail when "Send"-Button is pressed and e-mail&Captcha have been validated. Otherwise, error messages are shown.
  observeEvent(input$send, {
    msg <- isolate(input$message)
    if (!isValidEmail(input$sender)) {
      output$value <-renderText("e-mail is not valid!")
    } else if (input$captcha != captcha$result) {
      output$value <- renderText("Are you a robot?")
    } else {
      tryCatch({
        send.mail(
          from = input$sender,
          to = "tobuli@gmail.com",
          subject = paste("Feedback from forecastQ Shiny Application",isolate(input$subject)),
          body = paste(input$sender, msg),
          smtp = smtp_setting,
          authenticate = FALSE,
          send = TRUE
        )
        output$value <-renderText("Thank you for your feedback!")
        updateTextInput(session, "sender", "your e-mail", "")
        updateTextInput(session, "subject", "subject", "")
        updateTextInput(session, "message", "your message", "")
        changecaptcha()},
        error = function(e) {
          output$value <-renderText("Could not send message. Please try again later.")
        })
    }
  })
#}
  
  
  # MAPPING STUFF ----
  # MAPPING FUNCTIONS
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, 
                                                       color="black", fillColor="orange", fillOpacity=1, 
                                                       opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  
  # @knitr server02remainder1
  output$Map <- renderLeaflet({
    leaflet() %>% setView(lon, lat, zoom = 8) %>% addTiles() %>%
      addProviderTiles("OpenTopoMap",options = providerTileOptions(noWrap = TRUE)) %>% 
      addCircleMarkers(data=locsUnique[locsUnique$type=="gauge",], radius=6, color="blue", weight=2,stroke=TRUE, fillOpacity=0.9, group="locations", layerId = ~loc) %>% 
      addCircleMarkers(data=locsUnique[locsUnique$type=="meteo",], radius=6, color="red", weight=2,stroke=TRUE, fillOpacity=0.9, group="locations", layerId = ~loc)
    
  })
  
  # @knitr server02remainder2
  observe({ # show or hide location markers
    proxy <- leafletProxy("Map")
    if (input$show_stations) {
      proxy %>% showGroup("locations")
    } else {
      updateSelectInput(session, "location", selected="")
      proxy %>% hideGroup("locations") %>% removeMarker(layerId="Selected")
    }
  })
  
  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  observeEvent(input$Map_marker_click, { # update the location selectInput on map clicks
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })
  
  observeEvent(input$location, { # update the map markers and view on location selectInput changes
    p <- input$Map_marker_click
    p2 <- subset(locsUnique, loc==input$location)
    proxy <- leafletProxy("Map")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
    } else if(length(p$id) && input$location!=p$id){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    } else if(!length(p$id)){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    }
  })
  
})