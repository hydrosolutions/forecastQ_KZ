# GLOBAL ----
# forecast_Q, V September 2017
# tobias siegfried, hydrosolutions Ltd., 2017
# Note: Google Sheets integration awefully slow! Work on local file storage on DB integration


# PACKAGES ----
library('install.load')
install_load("shiny")
install_load("shinyBS")
install_load('shinythemes')
install_load('rgdal')
install_load('raster')
install_load('plyr') # for renaming data.frame columns
install_load('dplyr')
install_load('leaflet')
install_load('googlesheets')
install_load('data.table')
install_load('dygraphs')
install_load('zoo')
install_load('lubridate')
install_load('rhandsontable')
install_load('rminer')
install_load('reshape')
install_load('pracma')
install_load('DT') # load fancy version of data tables
install_load('caret')
install_load('caretEnsemble')
install_load('ggplot2')
install_load('doParallel')
install_load('mlbench')
install_load('rpart')
install_load('gdata')
install_load('pROC')
install_load('rdrop2')
install_load('MBA')
install_load('RPMG')
install_load('XML')
install_load('dotCall64')
install_load('fields')
install_load('rNOMADS')
install_load('rvest')
install_load('scrapeR')
#install_load("rJava")
#install_load("mailR")
install_load('devtools')

if (Sys.info()["sysname"]=="Darwin"){ # local deployment
  load_all("/Users/tobiassiegfried/Dropbox (hydrosolutions)/R_Packages/forecastQ")
}else{
  load_all("/srv/shiny-server/forecastQ_CN/forecastQ") # macMini deployment
}
  
  
# OPTIONS AND HELPER FUNCTIONS FOR DEBUGGING ----
options(shiny.error=browser)
options(httr_oob_default=TRUE) # required to resolve some google_sheets issues on the Unix Server

# INITIAL CONFIGURATIONS ----
uz <- 0
cn <- 1

if (cn==1){ # LOCATION SELECTOR ----
  stationsLoc <- "stationsLoc_CN"
  modelList <- "modelList_CN"
  modelPar <- "modelParameters_CN"
  gaugePM <- "gaugePredictorsMapping_CN"
} else if (uz==1) 
{
  stationsLoc <- "stationsLoc_UZ"
  modelList <- "modelList_UZ"
  modelPar <- "modelParameters_UZ"
  gaugePM <- "gaugePredictorsMapping_UZ"
} else {
  disp("Region not available!")
}

# LOAD / SAVE SCHEMES ----
remote <- 0
remoteFC <- 0
saveFCremote <- 0
save.data <- 0


# INITIAL MODEL FITTNG ----
fitModelDec <- 0 # Choose carefully since it takes a LONG time to fit these models -> should become user choice!
fitModelMon <- 0
qualAssess.dec <- 1 # Assess quality of models - always on for display on 'Assessment of Forecast Quality" page
qualAssess.mon <- 1 # Assess quality of models - always on for display on 'Assessment of Forecast Quality" page

# LOAD DATA ----
# GET STATIONS
setupStat <- funSetupStations(stationsLoc,remote)
locs <- setupStat$locs
locsUnique <- setupStat$locsUnique
code <- setupStat$code
lat <- setupStat$lat
lon <- setupStat$lon


# LOAD QPT GOOGLE SHEETS
qpt <- funQPT(locs,locsUnique,remote)
Q <- qpt$Q
P <- qpt$P
T <- qpt$T


# GET ALL PARAMETERS SETUP
par <- funModelPar(gaugpePM,modelList,modelPar,remote)
model.par <- par$model.par
modelList <- par$modelList
predictorMapping <- par$predictorMapping


# CREATE NORMS
norms <- funCalcNorms(Q,P,T)
normsQ.dec <- norms$normsQ.dec
normsQ.mon <- norms$normsQ.mon
normsP <- norms$normsP
normsT <- norms$normsT


# GEN. TIMESERIES
timeS <- funGenTimeSeries(Q,P,T)
ts <- timeS$ts
tsQ <- timeS$tsQ
tsP <- timeS$ts
tsT <- timeS$tsT


# GEN. DATA COLLECTIONS
dataCont <- funData(ts,Q,P,T,tsQ,tsP,tsT,tsQ.mon,predictorMapping)
data.dec <- dataCont$data.dec
data.mon <- dataCont$data.mon
decade <- dataCont$decade


# GEN. DATAFRAMES
df <- funDF(Q,P,T,data.mon)
dfQ <- df$dfQ
dfQ.mon <- df$dfQ.mon
dfQcurrent.dec <- df$dfQcurrent.dec
dfQcurrent.mon <- df$dfQcurrent.mon
dfP <- df$dfP
dfT <- df$dfT


# LOAD LATEST FORECASTS? AUX. FUNCTION ----
if (remoteFC==1){
  forecast.dec <- list()
  forecast.mon <- list()
  forecast.issueDate.dec <- list()
  forecast.issueDate.mon <- list()
  for (idx in 1:length(names(Q))){
    fileNRemoteMon <- gs_title(paste("forecast_MON_",names(Q)[[idx]],sep=""))
    fileNRemoteDec <- gs_title(paste("forecast_DEC_",names(Q)[[idx]],sep=""))
    forecast.dec[[idx]] <- fileNRemoteDec %>% gs_read(ws="Sheet1",range=cell_rows(1:(model.par$editTableL+1))) %>% as.zoo()
    forecast.issueDate.dec[[idx]] <- 
      fileNRemoteDec %>% gs_read(ws="Sheet1",range=cell_limits(c(model.par$editTableL+2,2),c(model.par$editTableL+2,2)))
    forecast.mon[[idx]] <- fileNRemoteMon %>% gs_read(ws="Sheet1",range=cell_rows(1:(model.par$editTableL+1))) %>% as.zoo()
    forecast.issueDate.mon[[idx]] <- fileNRemoteMon %>% gs_read(ws="Sheet1",range=cell_rows(model.par$editTableL+2))
  }
}



# SAVE DATA I/0 ----
if (save.data==1){
  save.image(file="data/allData.RData")
  disp("FINISHED LOADING REMOTE DATA - STORED LOCALLY")
} else {
  disp("FINISHED UPDTATING DATA")
}



# DECADAL MODELS ----
if (fitModelDec+fitModelMon==0){
  disp("LOADING ALL MODELS")
  model.data.dec <- readRDS("data/modelDataDec.rds")
  disp("read data/modelDataDec.rds")
  model.data.mon <- readRDS("data/modelDataMon.rds")
  disp("read data/modelDataMon.rds")
  pred.dec <- readRDS("data/predDec.rds")
  disp("read data/predDec.rds")
  pred.mon <- readRDS("data/predMon.rds")
  disp("read data/predMon.rds")
  disp("SUCCESSFULLY LOADED ALL DATA")
  model.dec <- readRDS("data/modelDec.rds")
  disp("read data/modelDec.rds")
  model.mon <- readRDS("data/modelMon.rds")
  disp("read data/modelMon.rds")
  disp("SUCESSFULLY LOADED ALL MODELS")
} else {
  if (fitModelDec==1){
    # DECADAL MODELS ----
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



# MODEL QUALITY ASSESSMENT ----
# SIMPLE ERROR METRICS AND RUSSIAN QUALITY STANDARD
if (qualAssess.dec==1){
  for (idx in 1:length(data.dec)){
    model.data.dec[[idx]] <- prepareSD(model.data.dec[[idx]],Q[[idx]])
  }
  # RMSE/SD decadal quality criterion
  model.dec.qual <- list()
  for (idx in 1:length(model.dec)){
    model.dec.qual[[idx]] <- allowedError(model.dec[[idx]],model.data.dec[[idx]],names(Q)[idx],"dec",5)
  }
  names(model.dec.qual) <- names(Q)
}
if (qualAssess.mon==1){
  for (idx in 1:length(data.mon)){
    model.data.mon[[idx]] <- prepareSD(model.data.mon[[idx]],Q[[idx]])
  }
  # RMSE/SD decadal quality criterion
  model.mon.qual <- list()
  for (idx in 1:length(model.mon)){
    model.mon.qual[[idx]] <- allowedError(model.mon[[idx]],model.data.mon[[idx]],names(Q)[idx],"dec",5)
  }
  names(model.mon.qual) <- names(Q)
}



# RUN FORECASTS ----
if (!remoteFC){
  # Decadal
  forecast.dec <- list()
  forecast.issueDate.dec <- list()
  for (idx in 1:length(model.dec)){
    forecast.dec[[idx]] <- forecastRunoffNew(data.dec[[idx]],model.dec[[idx]], model.data.dec[[idx]],1,ts,pred.dec[[idx]],decade,model.par,"dec")
    forecast.issueDate.dec[[idx]] <- Sys.time()
    towrite <- rbind(forecast.dec[[idx]] %>% as.data.frame(),c("Forecast Issue Time",toString(Sys.time()),"Status: Normal"))
    fileNLocal <- paste("data/forecastDec_",names(Q)[[idx]],".csv",sep="")
    write.csv(towrite, file = fileNLocal)
    if (saveFCremote==1){
      fileNRemote <- gs_title(paste("forecast_DEC_",names(Q)[[idx]],sep=""))
      gs_upload(fileNLocal,fileNRemote$sheet_key)
      fileNRemote %>% gs_edit_cells(ws="Sheet1",input=towrite)
    }
  }
  names(forecast.dec) <- names(Q)
  names(forecast.issueDate.dec) <- names(Q)
  
  # Monthly
  forecast.mon <- list()
  forecast.issueDate.mon <- list()
  for (idx in 1:length(model.dec)){
    forecast.mon[[idx]] <- forecastRunoffNew(data.mon[[idx]],model.mon[[idx]], model.data.mon[[idx]],1,ts,pred.mon[[idx]],decade,model.par,"mon")
    forecast.issueDate.mon[[idx]] <- Sys.time()
    towrite <- rbind(forecast.mon[[idx]] %>% as.data.frame(),c("Forecast Issue Time",toString(Sys.time()),"Status: Normal"))
    fileNLocal <- paste("data/forecastMon_",names(Q)[[idx]],".csv",sep="")
    write.csv(towrite, file = fileNLocal)
    if (saveFCremote==1){
      fileNRemote <- gs_title(paste("forecast_MON_",names(Q)[[idx]],sep=""))
      gs_upload(fileNLocal,fileNRemote$sheet_key)
      fileNRemote %>% gs_edit_cells(ws="Sheet1",input=towrite)
    }
  }
  names(forecast.mon) <- names(Q)
  names(forecast.issueDate.mon) <- names(Q)
  disp("FORECASTS RECOMPUTED FOR CURRENT (LATEST) DECADE / MONTH")
}

# ====================================================================================
# ====================================================================================
disp("FINISHED INITIALIZATION")
# ====================================================================================
# ====================================================================================
