# PACKAGES ----
library(shiny)
library(shinyBS)
library(shinythemes)
library(rgdal)
library(raster)
library(plyr) # for renaming data.frame columns
library(dplyr)
library(leaflet)
library(googlesheets)
library(data.table)
library(dygraphs)
library(zoo)
library(lubridate)
library(rhandsontable)
library(rminer)
library(reshape)
library(pracma)
library(DT) # load fancy version of data tables
library(caret)
library(caretEnsemble)
library(ggplot2)
library(doParallel)
library(mlbench)
library(rpart)
library(gdata)
library(pROC)
library(rdrop2)
library(rhandsontable)

# FUNCTIONS ----
getdata <- function(path,wsName){
  
  if (wsName == "") {
    data <- read.csv2(path,TRUE,dec = ".",sep=",")
  } else {
    #upload new data
    dataS <- gs_title(path)
    data <- dataS %>% gs_read(ws=wsName)
    rowYearIdx <- dataS %>% gs_read(ws=wsName,range=cell_cols("A")) %>% as.data.frame()
  }
  
  ystart = paste(toString(data[1,1]),"-01-01",sep="")
  data.tab <- data
  data.tab.sum <- do.call(cbind, lapply(data.tab, summary))
  data.tab.norm <- data.tab.sum["Mean",]
  data.tab.norm <- data.tab.norm[-1]
  data.tab.norm1 <- data.frame("January"=numeric(3),"February"=numeric(3),"March"=numeric(3),"April"=numeric(3),
                               "May"=numeric(3),"June"=numeric(3),"July"=numeric(3),"August"=numeric(3),
                               "September"=numeric(3),"October"=numeric(3),"November"=numeric(3),"December"=numeric(3))
  data.tab.norm1[,1] <- data.tab.norm[c("D1","D2","D3")]
  data.tab.norm1[,2] <- data.tab.norm[c("D4","D5","D6")]
  data.tab.norm1[,3] <- data.tab.norm[c("D7","D8","D9")]
  data.tab.norm1[,4] <- data.tab.norm[c("D10","D11","D12")]
  data.tab.norm1[,5] <- data.tab.norm[c("D13","D14","D15")]
  data.tab.norm1[,6] <- data.tab.norm[c("D16","D17","D18")]
  data.tab.norm1[,7] <- data.tab.norm[c("D19","D20","D21")]
  data.tab.norm1[,8] <- data.tab.norm[c("D22","D23","D24")]
  data.tab.norm1[,9] <- data.tab.norm[c("D25","D26","D27")]
  data.tab.norm1[,10] <- data.tab.norm[c("D28","D29","D30")]
  data.tab.norm1[,11] <- data.tab.norm[c("D31","D32","D33")]
  data.tab.norm1[,12] <- data.tab.norm[c("D34","D35","D36")]
  
  data <- t(data)
  data <- as.vector(unname(data[-1,]))
  temp <- zooreg(data,frequency=36,start=as.yearmon(ystart))
  temp.Date <- as.Date(time(temp)) + c(5,15,25) - 1
  data <- zoo(data,temp.Date)
  out <- list("data" = data, "data.df" = data.tab,  "data.norm" = data.tab.norm, "data.norm.Table" = data.tab.norm1, "rowYearIdx" = rowYearIdx)
  return(out)
} # loading available data from Google Sheets
putdata <- function(path,wsName,newData,rStartIDX){ 
  ss <- gs_title(path)
  gs_edit_cells(ss,ws=wsName,input = newData, anchor = paste("B",(rStartIDX+1),sep="")) # the +1 is because of the header row...
} # writes data back to Google Sheets
forecastprep <- function(data,decade,lag) {
  # ==== NEEDS REWRITE
  le <- 72 # nicely and easily solves the new year problem!!! - NOT!
  currYDates <- tail(decade,le) %>% time() %>% as.Date() 
  currYDecades <- tail(decade,le)
  lastDataDate <- tail(data,1) %>% time() %>% as.Date()
  idx <- which(currYDates == lastDataDate)
  if (idx < le) {
    idx <- idx + 1
  } else {
    idx <- 1
  } # Change of year needs to be dealt with carefully!
  # ==== 
  forecastDate <- currYDates[idx]
  forecastDecade <- coredata(currYDecades[idx])
  # now assemble colums - problem here is renaming the columns
  # lData <- dim(data)[1]
  P <- tail(coredata(data$P),lag)
  T <- tail(coredata(data$T),lag)
  Q <- tail(coredata(data$Q),lag+1)
  dfData <- c(forecastDecade,P,T,Q)
  laggedpred <- rbind.data.frame(dfData)
  colnames(laggedpred) <- c("decade","Plag5","Plag4","Plag3","Plag2","Plag1","Tlag5","Tlag4","Tlag3","Tlag2","Tlag1",
                            "Qlag5","Qlag4","Qlag3","Qlag2","Qlag1","y")
  rownames(laggedpred) <- forecastDate
  laggedpred <- list("laggedpred"=laggedpred, "forecastDate"=forecastDate,"forecastDecade"=forecastDecade)
  return(laggedpred)
} # currently used for one shot forecast once models are trained.
reshapeDecadal <- function(df){
  data.tab.norm1 <- data.frame("January"=numeric(3),"February"=numeric(3),"March"=numeric(3),"April"=numeric(3),
                               "May"=numeric(3),"June"=numeric(3),"July"=numeric(3),"August"=numeric(3),
                               "September"=numeric(3),"October"=numeric(3),"November"=numeric(3),"December"=numeric(3))
  data.tab.norm1[,1] <- c(df$D1,df$D2,df$D3)
  data.tab.norm1[,2] <- c(df$D4,df$D5,df$D6)
  data.tab.norm1[,3] <- c(df$D7,df$D8,df$D9)
  data.tab.norm1[,4] <- c(df$D10,df$D11,df$D12)
  data.tab.norm1[,5] <- c(df$D13,df$D14,df$D15)
  data.tab.norm1[,6] <- c(df$D16,df$D17,df$D18)
  data.tab.norm1[,7] <- c(df$D19,df$D20,df$D21)
  data.tab.norm1[,8] <- c(df$D22,df$D23,df$D24)
  data.tab.norm1[,9] <- c(df$D25,df$D26,df$D27)
  data.tab.norm1[,10] <- c(df$D28,df$D29,df$D30)
  data.tab.norm1[,11] <- c(df$D31,df$D32,df$D33)
  data.tab.norm1[,12] <- c(df$D34,df$D35,df$D36)
  return(data.tab.norm1)
}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
rmse <- function(error){
  sqrt(mean(error^2))
}
decadeMaker <- function(tstart,tend){
  decade <- 1:36 # Preparation of decade indicators
  ydiff <- year(strptime(tend, format = "%Y-%m-%d")) - year(strptime(tstart, format = "%Y-%m-%d")) [1] + 1
  decade <- as.vector(repmat(decade, 1, ydiff))
  temp <- zooreg(decade, frequency = 36, start=as.yearmon(tstart))
  temp.Date <- as.Date(time(temp)) + c(5,15,25) - 1
  decade <- zoo(decade, temp.Date) %>% return
}
allowedError <- function(model.fit,model.Q,station,fType){
  # generate obs and predict
  newdata <- model.Q$model.test
  model.ensemblePred <- predict(model.fit, newdata = newdata)
  obs <- zoo(model.Q$model.test$y, model.Q$time.test)
  pred <- zoo(model.ensemblePred,model.Q$time.test)
  # plots abs(err)/sd
  ltData <- rbind(model.Q$model.train,model.Q$model.test)
  if (fType=="mon"){
    sdQ <- model.Q$decSD
  } else if (fType=="dec"){
    sdQ <- model.Q$decDeltaSD
  }
  # 2. Calculate RMSE
  error <- abs(obs-pred)
  dec.Q <- decadeMaker(model.Q$time.test[1],tail(model.Q$time.test,1))
  error <- merge(decade=dec.Q, obs=obs, pred=pred, error=error)
  # now, we need to reshape this...
  tend <- tail(model.Q$model.test,1) %>% row.names
  tstart <- model.Q$model.test[1,] %>% row.names
  nYTest <- year(strptime(tend, format = "%Y-%m-%d")) - year(strptime(tstart, format = "%Y-%m-%d")) [1] + 1
  error1 <- matrix(error$error,ncol = nYTest,nrow=36) %>% t %>% as.data.frame
  vStandard <- error1/t(replicate(nYTest,sdQ))
  vStandard1 <- vStandard
  vStandard$id <- 1:nrow(vStandard)
  plot_vStandard <- melt(vStandard,id.vars = "id")
  
  p <- ggplot(plot_vStandard, aes(x=variable,y=value)) +
    geom_point() +
    geom_boxplot(aes(fill=value)) +
    geom_hline(yintercept = 0.674, color="red") +
    geom_hline(yintercept = 0.8, color="blue") +
    labs(y="Decadal Quality Criterion [Error / SD]") +
    labs(x="decade") +
    labs(title=paste("Assessment of Forecast Quality", station)) +
    ylim(0,1.5) +
    scale_alpha(guide='none')
  #print(p)
  vStandard <- rapply(na.omit(vStandard1),mean)
  output <- list(p,vStandard)
  return(output)
}
lagPredictors <- function(data,lag, switchM){
  if (switchM == "dec"){
    lagQ <- CasesSeries(data$Q,c(1:lag))
    names(lagQ) <- gsub("l","Ql",names(lagQ))
    lagP <- CasesSeries(data$P,c(1:lag))
    lagP <- rename(lagP,c("y"="lag0"))
    names(lagP) <- gsub("l","Pl",names(lagP))
    lagP <- subset(lagP,select = -c(Plag0)) # remove Plag0
    lagT <- CasesSeries(data$T,c(1:lag))
    lagT <- rename(lagT,c("y"="lag0"))
    names(lagT) <- gsub("l","Tl",names(lagT))
    lagT <- subset(lagT,select = -c(Tlag0)) # remove Tlag0
  } else {
    lagQ <- CasesSeries(data$Q,c(0:(lag-1)))
    names(lagQ) <- gsub("l","Ql",names(lagQ))
    lagQ$y <- coredata(data$Y[(-1:-(lag-1))]) # this is the replacement of the monthly values at the corresponding slots
    lagP <- CasesSeries(data$P,c(1:(lag-1)))
    lagP <- rename(lagP,c("y"="lag0"))
    names(lagP) <- gsub("l","Pl",names(lagP)) 
    lagT <- CasesSeries(data$T,c(1:(lag-1)))
    lagT <- rename(lagT,c("y"="lag0"))
    names(lagT) <- gsub("l","Tl",names(lagT)) 
  }
  laggedpred <- list("lagQ" = lagQ, "lagP" = lagP, "lagT" = lagT)
  return(laggedpred)
}
modelpreparation <- function(modelType,data,lagpred,maxLag.de,timeTrainEnd,timeTestStart,par,switchM){
  if (switchM=="dec"){
    lag <- maxLag.de
  } else {lag <- maxLag.de - 1}
  if (modelType == "Q") {
    model <- cbind.data.frame(decade=data$decade[-1:-lag],lagpred$lagQ)
  } else if (modelType == "PQ") {
    model <- cbind.data.frame(decade=data$decade[-1:-lag],lagpred$lagP,lagpred$lagQ)
  } else if (modelType == "TQ") {
    model <- cbind.data.frame(decade=data$decade[-1:-lag],lagpred$lagT,lagpred$lagQ)
  } else if (modelType == "PTQ") {
    model <- cbind.data.frame(decade=data$decade[-1:-lag],lagpred$lagP,lagpred$lagT,lagpred$lagQ)
  } else if (modelType == "PTgradQQ") {
    model <- cbind.data.frame(decade=data$decade[-1:-lag],lagpred$lagP,lagpred$lagT,lagpred$lagGradQ,lagpred$lagQ)
  }
  model.time <- time(data[-1:-lag])
  time.train.id <- model.time < as.Date(timeTrainEnd)
  time.test.id <- model.time > as.Date(timeTestStart) 
  model.train <- model[time.train.id,]
  model.test <- model[time.test.id,]
  model.test <- cbind.data.frame(t=model.time[time.test.id],model.test) # just to get times correct later (due to NAs)
  model.train <- na.omit(model.train)
  model.test <- na.omit(model.test)
  time.test <- model.test$t
  model.test <- subset(model.test, select = - t) # kicked out date again
  out <- list("model.train" = model.train, "model.test" = model.test, "time.test" = time.test)
  return(out)
}
modelfitting <- function(model,model.par,modelList){
  model.trainControl <- trainControl(
    method="timeslice",
    initialWindow=model.par$iniWindow,
    horizon = model.par$horizon,
    fixedWindow = TRUE,
    savePredictions="final",
    index=createResample(model$model.train$y, model.par$nresamples),
    allowParallel = TRUE
  )
  model.list <- caretList(
    y~., data=model$model.train,
    trControl=model.trainControl,
    methodList = modelList
  )
  model.ensemble <- caretEnsemble(
    model.list, 
    metric="RMSE",
    trControl=trainControl(
      number=5,
      allowParallel = TRUE
    ))
  summary.ensemble <- summary(model.ensemble)
  summary.ensemble
  results <- list(model.ensemble,summary.ensemble)
  return(model.ensemble)
}
modelquality <- function(model.fit,testdata){
  #newdata <- subset(testdata,select=-y)
  newdata <- testdata
  model.pred.mean <- predict(model.fit, newdata = newdata)
  error.mean <- mmetric(model.pred.mean,testdata$y,metric = c("SAE","MAE","RMSE","R2"))
  model.quality <- list("error.mean" = error.mean)
  model.quality <- as.data.frame(model.quality)
  return(model.quality)
}
prepareSD <- function(model,q){
  # DECADAL FACTORS FOR MONTHLY FORECASTS
  model$decSD <- apply(q$data.df[,-1],2,sd,na.rm=TRUE)
  # DECADAL FACTORS FOR DECADAL FORECASTS
  q <- q$data
  dq <- c(NA,diff(q))
  dqR <- Reshape(dq,36,length(dq)/36) %>% as.data.frame %>% t
  model$decDeltaSD <- apply(dqR,2,sd,na.rm=TRUE)
  return(model)
}
forecastRunoff <- function(model,data,gaugeN,precipN,tempN,decade,par){
  pred <- cbind(decade = decade, P = data[,colnames(data)==precipN],
                T = data[,colnames(data)==tempN], Q = data[,colnames(data)==gaugeN])
  pred <- pred %>% na.omit
  pred <- tail(pred,par$forecastPrepL)
  dataPred <- forecastprep(pred,decade,par$lag)
  forecastQ1 <- predict(model,newdata=dataPred$laggedpred)
  tVec <- rownames(dataPred$laggedpred) %>% as.Date
  forecastQ1 <- zoo(round(forecastQ1,digits=2),tVec)
  result <- list(value = forecastQ1, forecastTarget = dataPred$forecastDecade, forecastTargetY = as.numeric(substring(dataPred$forecastDate,1,4)))
  return(result)
} # OBSOLETE

# ----- TEST CODE ------
# forecast.dec[[idx]] <- forecastRunoffNew(data.dec[[idx]],model.dec[[idx]], model.data.dec[[idx]],1,ts,pred.dec[[idx]],decade,model.par,"dec")
# forecast.mon[[idx]] <- forecastRunoffNew(data.mon[[idx]],model.mon[[idx]], model.data.mon[[idx]],1,ts,pred.mon[[idx]],decade,model.par,"mon")
#idx <- 2
#data <- data.mon[[idx]]
#model <- model.mon[[idx]]
#model.data <- model.data.mon[[idx]]
#pred <- pred.mon[[idx]]
#switchM <- "mon"
#par <- model.par
#windowL <- 100
# ----

forecastRunoffNew <- function(data,model,model.data,windowL,ts,pred,decade,par,switchM){ # should be ok...
  if (switchM=="dec"){
    predictors <- cbind(coredata(decade[-(par$lag:1),]),pred$lagP,pred$lagT,pred$lagQ[,-(par$lag+1)]) %>% as.zoo()
    colnames(predictors) <- c("decade","Plag5","Plag4","Plag3","Plag2","Plag1","Tlag5","Tlag4","Tlag3","Tlag2","Tlag1",
                              "Qlag5","Qlag4","Qlag3","Qlag2","Qlag1")
    ll <- par$lag
  } else {
    predictors <- cbind(coredata(decade[-((par$lag-1):1),]),pred$lagP,pred$lagT,pred$lagQ[,-(par$lag+1)]) %>% as.zoo()
    colnames(predictors) <- c("decade","Plag4","Plag3","Plag2","Plag1","Plag0","Tlag4","Tlag3","Tlag2","Tlag1","Tlag0",
                              "Qlag4","Qlag3","Qlag2","Qlag1","Qlag0")
    ll <- (par$lag-1)
    target <- CasesSeries(data$Y,1)
    target <- target$lag1 %>% as.zoo()
    time(target) <- time(data$Y[-1,])
    #target <- tail(target,windowL)
  }
  time(predictors) <- time(ts[(-1:-ll),])
  predictors <- tail(predictors,dim(model.data$model.test)[1])
  predictorsNNA <- predictors %>% na.omit
  predictedQ <- predict(model,newdata=predictorsNNA) %>% as.zoo()
  # ====== We are actually predicting the next decade, so be careful with slotting!
  time(predictedQ) <- time(predictorsNNA)
  if (switchM =="mon"){
    te <- merge(decade,predictedQ)
    te$predictedQ <- c(NA, coredata(te$predictedQ[-dim(te)[1]]))
    predictedQ <- te$predictedQ 
  }
  # ======
  if (switchM=="dec"){
    target <- pred$lagQ$y %>% as.zoo()
    time(target) <- time(decade[-1:-ll])
  }
  result <- merge(predictors$decade,target,predictedQ,fill=NA)
  colnames(result) <- cbind("decade","observed","predicted")
  if (switchM=="dec"){
    iplot <- 1:dim(result)[1] # plot all!
  } else {
    iplot <- !(mod(result$decade,3)-1) # plot only months!
  }
  result <- result[iplot,]
  # Now, here we have to be carefull with the na.omit since with it, we also remove the actual 
  if (switchM=="dec"){
    result <- tail(result,par$editTableL)
  } else {
    result <- tail(result,(par$editTableL/3)) # accounting for monthly length!
  }
  return(result)
}
stripRF <- function(cm) {
  cm$finalModel$predicted <- NULL 
  cm$finalModel$oob.times <- NULL 
  cm$finalModel$y <- NULL
  cm$finalModel$votes <- NULL
  cm$control$indexOut <- NULL
  cm$control$index    <- NULL
  cm$trainingData <- NULL
  attr(cm$terms,".Environment") <- c()
  attr(cm$formula,".Environment") <- c()
  return(cm)
}

# CLUMSY REGION SELECTOR (IMPLEMENTING BETTER SWITCH TO BE DONE IN THE FUTURE...)
uz <- 0
cn <- 1

if (cn==1){
  stationsLoc <- "stationsLoc_CN"
  modelList <- "modelList_CN"
  modelPar <- "modelParameters_CN"
  gaugePM <- "gaugePredictorsMapping_CN"
} else if (uz==1) {
  stationsLoc <- "stationsLoc_UZ"
  modelList <- "modelList_UZ"
  modelPar <- "modelParameters_UZ"
  gaugePM <- "gaugePredictorsMapping_UZ"
} else {
  disp("Region not available!")
}

## LOAD DATA ----
remote <- 1
remoteFC <- 0
saveFCremote <- 0
save.data <- 1

if (remote==1){
  
  locs <- gs_title(stationsLoc)
  locs <- locs %>% gs_read(ws="loc")
  lat <- mean(locs$lat)
  lon <- mean(locs$lon)
  
  locsUnique <- locs
  locsUnique <- locsUnique[c("loc","code","lat","lon","river","type")]
  locsUnique <- unique(locsUnique)
  locsUnique.gauges <- locsUnique[locsUnique$type=="gauge",]
  locsUnique.meteo <- locsUnique[locsUnique$type=="meteo",]
  
  code.Q <- locs$code[locs$var=="Q"]
  code.P <- locs$code[locs$var=="P"]
  code.T <- locs$code[locs$var=="T"]
  
  # LOAD GOOGLE SHEETS
  Q.names <- locsUnique[locsUnique$type=="gauge",]$loc
  P.names <- locs[locs$var=="P",]$loc
  T.names <- locs[locs$var=="T",]$loc
  
  Q <- list()
  for (idx in 1:length(Q.names)){
    Q[[idx]] <- getdata(Q.names[idx],"Q")
  }
  names(Q) <- Q.names
  
  P <- list()
  for (idx in 1:length(P.names)){
    P[[idx]] <- getdata(P.names[idx],"P")
  }
  names(P) <- P.names
  
  T <- list()
  for (idx in 1:length(T.names)){
    T[[idx]] <- getdata(T.names[idx],"T")
  }
  names(T) <- T.names
  
  predictorMapping <- gs_title(gaugePM) %>% gs_read(ws="predictorMapping")
  # List of models used
  modelList <- gs_title(modelList) %>% gs_read(ws="list",stringsAsFactors = FALSE)  %>% as.list
  # Model parameters
  model.par <- gs_title(modelPar) %>% gs_read(ws="par",stringsAsFactors = FALSE)  %>% as.data.frame
  model.par$value[1:7] <- as.integer(model.par$value[1:7])
  model.par1 <- as.data.frame(t(model.par$value))
  colnames(model.par1) <- model.par$parameter
  lags <- model.par[1,]
  lags <- subset(lags,select=-parameter)
  lags <- c(lags$value,lags$value2,lags$value3) %>% as.numeric
  model.par <- model.par1
  rm(model.par1)
  model.par$lag <- lags
  model.par$nTrees <- model.par$nTrees %>% levels %>% as.numeric
  model.par$tuneL <- model.par$tuneL %>% levels %>% as.numeric
  model.par$seedN <- model.par$seedN %>% levels %>% as.numeric
  model.par$iniWindow <- model.par$iniWindow %>% levels %>% as.numeric
  model.par$horizon <- model.par$horizon %>% levels %>% as.numeric
  model.par$nresamples <- model.par$nresamples %>% levels %>% as.numeric
  model.par$forecastPrepL <- model.par$forecastPrepL %>% levels %>% as.numeric
  model.par$editTableL <- model.par$editTableL %>% levels %>% as.numeric
  
  # CREATE BIG DATA TABLES WITH ALL AVAILABLE DATA INSIDE! ----
  ts <- zoo()
  normsQ.dec <- data.frame()
  normsQ.mon <- data.frame()
  tsQ <- zoo()
  for (idx in 1:length(Q.names)){
    Q[[idx]]$data.norm$loc <- Q.names[idx]
    Q[[idx]]$data.norm$var <- "Q"
    Q[[idx]]$data.norm <- Q[[idx]]$data.norm %>% as.data.frame
    normsQ.dec <- rbind(normsQ.dec,Q[[idx]]$data.norm)
    normsQ <- rollapply(Q[[idx]]$data.df[,-1] %>% t,list(0:2),mean,fill=NA,by=3) %>% t
    normsQ <- normsQ[,cbind(1,4,7,10,13,16,19,22,25,28,31,34)]
    normsQ.mon <- rbind(normsQ.mon %>% as.data.frame,colMeans(normsQ, na.rm = TRUE, dims = 1))
    names(normsQ.mon) <- cbind("J","F","M","A","M","J","J","A","S","O","N","D")
  }
  normsQ.mon <- cbind(normsQ.mon,normsQ.dec[,-1:-36])
  
  normsP <- data.frame()
  tsP <- zoo()
  for (idx in 1:length(P.names)){
    P[[idx]]$data.norm$loc <- P.names[idx]
    P[[idx]]$data.norm$var <- "P"
    P[[idx]]$data.norm <- P[[idx]]$data.norm %>% as.data.frame
    normsP <- rbind(normsP,P[[idx]]$data.norm)
  }
  normsT <- data.frame()
  tsT <- zoo()
  for (idx in 1:length(T.names)){
    T[[idx]]$data.norm$loc <- T.names[idx]
    T[[idx]]$data.norm$var <- "T"
    T[[idx]]$data.norm <- T[[idx]]$data.norm %>% as.data.frame
    normsT <- rbind(normsT,T[[idx]]$data.norm)
  }
  for (idx in 1:length(Q.names)){
    if (idx==1){
      tsQ <- Q[[idx]]$data
    } else {
      tsQ <- merge(tsQ,Q[[idx]]$data,fill=NA)
    }
  }
  names(tsQ) <- paste(Q.names)
  for (idx in 1:length(P.names)){
    if (idx==1){
      tsP <- P[[idx]]$data
    } else {
      tsP <- merge(tsP,P[[idx]]$data,fill=NA)
    }
  }
  names(tsP) <- paste(P.names,".P",sep="")
  for (idx in 1:length(T.names)){
    if (idx==1){
      tsT <- T[[idx]]$data
    } else {
      tsT <- merge(tsT,T[[idx]]$data,fill=NA)
    }
  }
  names(tsT) <- paste(T.names,".T",sep="")
  ts <- merge(tsQ,tsP,fill=NA)
  ts <- merge(ts,tsT,fill=NA) # all is stored here! so that simplifies stuff later on in the reactive server part!
  
  dfQ <- data.frame()
  dfQcurrent.dec <- dfQ
  for (idx in 1:length(Q.names)){
    if (idx==1){
      dfQ <- Q[[idx]]$data.df %>% as.data.frame
      dfQ['id'] <- code.Q[idx]
      dfQcurrent.dec <- tail(dfQ,1)
    } else {
      te <- Q[[idx]]$data.df %>% as.data.frame
      te['id'] <- code.Q[idx]
      tecurrent <- tail(te,1)
      dfQ <- rbind(dfQ,te)
      dfQcurrent.dec <- rbind(dfQcurrent.dec,tecurrent)
    }
  }
  
  dfP <- data.frame()
  for (idx in 1:length(P.names)){
    if (idx==1){
      dfP <- P[[idx]]$data.df %>% as.data.frame
      dfP['id'] <- code.P[idx]
    } else{
      te <- P[[idx]]$data.df %>% as.data.frame
      te['id'] <- code.P[idx]
      dfP <- rbind(dfP,te)
    }
  }
  
  dfT <- data.frame()
  for (idx in 1:length(T.names)){
    if (idx==1){
      dfT <- T[[idx]]$data.df %>% as.data.frame
      dfT['id'] <- code.T[idx]
    } else{
      te <- T[[idx]]$data.df %>% as.data.frame
      te['id'] <- code.T[idx]
      dfT <- rbind(dfT,te)
    }
  }
  
  # Decadal and Monthly Model Data Assembly
  tstart <- head(tsQ,1) %>% time %>% as.Date 
  tend <- tail(tsQ,1) %>% time %>% as.Date 
  decade <- decadeMaker(tstart,tend) 
  
  data.dec <- list()
  data.mon <- data.dec
  tsQ.mon <- rollapplyr(tsQ,list((1:3)),mean,fill=NA) # monthly targets
  for (idx in 1:length(Q.names)){
    Qstat <- tsQ[,idx]
    Ystat <- Qstat
    Ymonstat <- tsQ.mon[,idx]
    Pname <- paste(predictorMapping$station_name[idx],".P",sep="")
    i <- which(names(tsP)==Pname)
    Pstat <- tsP[,i]
    Tname <- paste(predictorMapping$station_name[idx],".T",sep="")
    i <- which(names(tsT)==Tname) 
    Tstat <- tsT[,i]
    data.dec[[idx]] <- cbind(Q=Qstat,P=Pstat,T=Tstat,decade=decade,Y=Ystat) 
    data.mon[[idx]] <- cbind(Q=Qstat,P=Pstat,T=Tstat,decade=decade,Y=Ymonstat) 
  }
  names(data.dec) <- Q.names
  names(data.mon) <- Q.names
  
  dfQ.mon <- data.frame()
  dfQcurrent.mon <- dfQ.mon
  for (idx in 1:length(Q.names)){
    if (idx==1){
      dfMon <- Reshape(coredata(data.mon[[idx]]$Y),36,length(data.mon[[1]]$Y)/36) %>% as.data.frame %>% t
      dfQ.mon <- dfMon %>% as.data.frame
      dfQ.mon['id'] <- code.Q[idx]
      dfQcurrent.mon <- tail(dfQ.mon,1)
    } else {
      dfMon <- Reshape(coredata(data.mon[[idx]]$Y),36,length(data.mon[[1]]$Y)/36) %>% as.data.frame %>% t
      te <- dfMon %>% as.data.frame
      te['id'] <- code.Q[idx]
      tecurrent <- tail(te,1)
      dfQ.mon <- rbind(dfQ.mon,te)
      dfQcurrent.mon <- rbind(dfQcurrent.mon,tecurrent)
    }
  }
  
  dfQcurrent.mon <- cbind(dfQcurrent.dec$YEAR,dfQcurrent.mon)
  names(dfQcurrent.mon) <- colnames(dfQcurrent.dec)
  
  # load latest forecasts
  if (remoteFC==1){
    forecast.dec <- list()
    forecast.mon <- list()
    forecast.issueDate.dec <- list()
    forecast.issueDate.mon <- list()
    for (idx in 1:length(Q.names)){
      fileNRemoteMon <- gs_title(paste("forecast_MON_",Q.names[[idx]],sep=""))
      fileNRemoteDec <- gs_title(paste("forecast_DEC_",Q.names[[idx]],sep=""))
      forecast.dec[[idx]] <- fileNRemoteDec %>% gs_read(ws="Sheet1",range=cell_rows(1:(model.par$editTableL+1))) %>% as.zoo()
      forecast.issueDate.dec[[idx]] <- 
        fileNRemoteDec %>% gs_read(ws="Sheet1",range=cell_limits(c(model.par$editTableL+2,2),c(model.par$editTableL+2,2)))
      forecast.mon[[idx]] <- fileNRemoteMon %>% gs_read(ws="Sheet1",range=cell_rows(1:(model.par$editTableL+1))) %>% as.zoo()
      forecast.issueDate.mon[[idx]] <- fileNRemoteMon %>% gs_read(ws="Sheet1",range=cell_rows(model.par$editTableL+2))
    }
  }
  
  # save data
  if (save.data==1){
    save.image(file="data/allData.RData")
    disp("FINISHED LOADING REMOTE DATA - STORED LOCALLY")
  } else {
    disp("FINISHED LOADING REMOTE DATA")
  }
  
} else {
  #token <- readRDS("droptoken.rds")
  #~/Dropbox (hydrosolutions)/iMoMoTOOLS/RRM_Heihe_decadal/forecastQ/modelsDec.RData
  #drop_get(path='imomotools/rrm_heihe_decadal/forecastQ/data/allData.RData',overwrite=TRUE,dtoken=token)
  disp("LOADING LOCAL DATA")
  load('data/allData.RData')
  disp("SUCCESSFULLY LOADED LOCAL DATA")
}

# INITIAL MODEL FITTNG ----
fitModelDec <- 1 # Choose carefully since it takes a LONG time to fit these models -> should become user choice!
fitModelMon <- 1
qualAssess.dec <- 1 # Assess quality of models - always on for display on 'Assessment of Forecast Quality" page
qualAssess.mon <- 1 # Assess quality of models - always on for display on 'Assessment of Forecast Quality" page

# DECADAL MODELS ----
if (fitModelDec+fitModelMon==0){
  disp("LOADING ALL MODELS")
  #load("data/allModels.RData")
  model.data.dec <- readRDS("data/modelDataDec.rds")
  disp("read data/modelDataDec.rds")
  model.data.mon <- readRDS("data/modelDataMon.rds")
  disp("read data/modelDataMon.rds")
  pred.dec <- readRDS("data/predDec.rds")
  disp("read data/predDec.rds")
  pred.mon <- readRDS("data/predMon.rds")
  disp("read data/predMon.rds")
  disp("SUCCESSFULLY LOADED ALL DATA")
  #if (!remoteFC){
  model.dec <- readRDS("data/modelDec.rds")
  disp("read data/modelDec.rds")
  model.mon <- readRDS("data/modelMon.rds")
  disp("read data/modelMon.rds")
  disp("SUCESSFULLY LOADED ALL MODELS")
  #}
} else {
  if (fitModelDec==1){
    disp("FITTING DECADAL MODELS")
    # Lagging predictors
    pred.dec <- list()
    for (idx in 1:length(data.dec)){
      pred.dec[[idx]] <- lagPredictors(data.dec[[idx]],lags,"dec")
    }
    names(pred.dec) <- names(data.dec)
    # Model preparation
    model.data.dec <- list()
    for (idx in 1:length(pred.dec)){
      model.data.dec[[idx]] <- modelpreparation("PTQ",data.dec[[idx]],pred.dec[[idx]],lags,model.par$tTrainEnd,model.par$tTestStart,model.par,"dec")
    }
    names(model.data.dec) <- names(data.dec)
    # Fitting of ensemble models
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    model.dec <- list()
    for (idx in 1:length(pred.dec)){
      model.dec[[idx]] <- modelfitting(model.data.dec[[idx]],model.par,modelList$models)
    }
    names(model.dec) <- names(data.dec)
    stopCluster(cl)
    # reduce size of models
    for (i in 1:length(model.dec)){
      model.dec[[i]]$models$parRF <- stripRF(model.dec[[i]]$models$parRF)
    }
    # Save models
    disp("SAVING DECADAL MODELS")
    saveRDS(pred.dec,file="data/predDec.rds")
    saveRDS(model.data.dec,file="data/modelDataDec.rds")
    saveRDS(model.dec,file="data/modelDec.rds")
  } 
  # MONTLY MODELS ----
  if (fitModelMon==1){
    disp("FITTING MONTHLY MODELS")
    # Lagging predictors
    pred.mon <- list()
    for (idx in 1:length(data.mon)){
      pred.mon[[idx]] <- lagPredictors(data.mon[[idx]],lags,"mon")
    }
    names(pred.mon) <- names(data.mon)
    # Model preparation
    model.data.mon <- list()
    for (idx in 1:length(pred.mon)){
      model.data.mon[[idx]] <- modelpreparation("PTQ",data.mon[[idx]],pred.mon[[idx]],lags,model.par$tTrainEnd,model.par$tTestStart,model.par,"mon")
    }
    names(model.data.mon) <- names(data.mon)
    # Fitting of ensemble models
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    model.mon <- list()
    for (idx in 1:length(pred.mon)){
      model.mon[[idx]] <- modelfitting(model.data.mon[[idx]],model.par,modelList$models)
    }
    names(model.mon) <- names(data.mon)
    stopCluster(cl)
    # reduce size of models
    for (i in 1:length(model.dec)){
      model.mon[[i]]$models$parRF <- stripRF(model.mon[[i]]$models$parRF)
    }
    # Save models
    disp("SAVING MONTHLY MODELS")
    saveRDS(pred.mon,file="data/predMon.rds")
    saveRDS(model.data.mon,file="data/modelDataMon.rds")
    saveRDS(model.mon,file="data/modelMon.rds")
  }
}

# MODEL QUALITY ASSESSMENT
# SIMPLE ERROR METRICS $ AND RUSSIAN QUALITY STANDARD----
if (qualAssess.dec==1){
  for (idx in 1:length(data.dec)){
    model.data.dec[[idx]] <- prepareSD(model.data.dec[[idx]],Q[[i]])
  }
  # RMSE/SD decadal quality criterion
  model.dec.qual <- list()
  for (idx in 1:length(model.dec)){
    model.dec.qual[[idx]] <- allowedError(model.dec[[idx]],model.data.dec[[idx]],Q.names[idx],"dec")
  }
  names(model.dec.qual) <- Q.names
}
if (qualAssess.mon==1){
  for (idx in 1:length(data.mon)){
    model.data.mon[[idx]] <- prepareSD(model.data.mon[[idx]],Q[[i]])
  }
  # RMSE/SD decadal quality criterion
  model.mon.qual <- list()
  for (idx in 1:length(model.mon)){
    model.mon.qual[[idx]] <- allowedError(model.mon[[idx]],model.data.mon[[idx]],Q.names[idx],"dec")
  }
  names(model.mon.qual) <- Q.names
}

# RUN FORECASTS----
if (!remoteFC){
  forecast.dec <- list()
  forecast.issueDate.dec <- list()
  for (idx in 1:length(model.dec)){
    forecast.dec[[idx]] <- forecastRunoffNew(data.dec[[idx]],model.dec[[idx]], model.data.dec[[idx]],1,ts,pred.dec[[idx]],decade,model.par,"dec")
    forecast.issueDate.dec[[idx]] <- Sys.time()
    towrite <- rbind(forecast.dec[[idx]] %>% as.data.frame(),c("Forecast Issue Time",toString(Sys.time()),"Status: Normal"))
    fileNLocal <- paste("data/forecastDec_",Q.names[[idx]],".csv",sep="")
    fileNRemote <- gs_title(paste("forecast_DEC_",Q.names[[idx]],sep=""))
    write.csv(towrite, file = fileNLocal)
    if (saveFCremote==1){
      gs_upload(fileNLocal,fileNRemote$sheet_key)
      fileNRemote %>% gs_edit_cells(ws="Sheet1",input=towrite)
    }
  }
  names(forecast.dec) <- Q.names
  names(forecast.issueDate.dec) <- Q.names
  
  forecast.mon <- list()
  forecast.issueDate.mon <- list()
  for (idx in 1:length(model.dec)){
    forecast.mon[[idx]] <- forecastRunoffNew(data.mon[[idx]],model.mon[[idx]], model.data.mon[[idx]],1,ts,pred.mon[[idx]],decade,model.par,"mon")
    forecast.issueDate.mon[[idx]] <- Sys.time()
    towrite <- rbind(forecast.mon[[idx]] %>% as.data.frame(),c("Forecast Issue Time",toString(Sys.time()),"Status: Normal"))
    fileNLocal <- paste("data/forecastMon_",Q.names[[idx]],".csv",sep="")
    fileNRemote <- gs_title(paste("forecast_MON_",Q.names[[idx]],sep=""))
    write.csv(towrite, file = fileNLocal)
    if (saveFCremote==1){
      gs_upload(fileNLocal,fileNRemote$sheet_key)
      fileNRemote %>% gs_edit_cells(ws="Sheet1",input=towrite)
    }
  }
  names(forecast.mon) <- Q.names
  names(forecast.issueDate.mon) <- Q.names
  disp("FORECASTS RECOMPUTED FOR CURRENT (LATEST) DECADE / MONTH")
}
# ====================================================================================
disp("FINISHED INITIALIZATION")
# ====================================================================================
# ====================================================================================