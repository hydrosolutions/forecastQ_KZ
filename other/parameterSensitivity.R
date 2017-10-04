# temp file for parameter sensitivity runs
# to be used on the run with the app.R initialization file!
# tobias siegfried, Nov 2016

nruns <- 5
modelList <- c("rf", "cubist","earth","xgbTree")
cl <- makeCluster(detectCores())
registerDoParallel(cl)
iniWindow <- c(1300,1320,1340,1360,1380,1400,1420,1440,1460,1480,1500)
resIniW <- iniWindow
resIniW <- matrix(resIniW,nruns,length(resIniW),byrow = F)
lags <- 5
for (r in 1:nruns){
  disp("===RUN===")
  disp(r)
  disp("=========")
  for (i in 1:length(iniWindow)){
    
    fitModel <- 1 # Choose carefully since it takes a LONG time to fit these models
    qualAssess <- 1 # Assess quality of models
    nBootstraps <- 10
    lagCur <- lags # just choose good lag
    
    tstart <- head(tsQ)[1] %>% time %>% as.Date
    tend <- tail(tsQ)[6] %>% time %>% as.Date
    decade <- decadeMaker(tstart,tend)
    
    # ATTENTION: HARDCODING!
    tTrainEnd <- "1999-12-31"
    tTestStart <- "2000-01-05"
    
    modelList <- c("rf", "cubist","xgbTree")
    
    if (fitModel==1){
      # model data assembly
      Chatkal_Khudaydod_16279.model.data <- cbind(Q=tsQ$Chatkal_Khudaydod_16279,P=tsP$Chatkal_Station_38471,
                                                  T=tsT$Chatkal_Station_38471,decade=decade,Y=tsQ$Chatkal_Khudaydod_16279)
      # lagging predictors
      Chatkal_Khudaydod_16279.model.pred <- laggingpredictors(Chatkal_Khudaydod_16279.model.data,lagCur)
      # model preparation
      Chatkal_Khudaydod_16279.model <- modelpreparation("PTQ",Chatkal_Khudaydod_16279.model.data,
                                                        Chatkal_Khudaydod_16279.model.pred,lagCur,tTrainEnd,tTestStart)
      
      Chatkal_Khudaydod_16279.model.trainControl <- trainControl(
        method="timeslice",
        initialWindow=iniWindow[i],
        horizon = model.par$horizon,
        fixedWindow = TRUE,
        savePredictions="final",
        index=createResample(Chatkal_Khudaydod_16279.model$model.train$y, nBootstraps),
        allowParallel = TRUE
      )
      # NEW MODEL FITTING WITH CARET ENSEMBLE ====
      Chatkal_Khudaydod_16279.model.list <- caretList(
        y~., data=Chatkal_Khudaydod_16279.model$model.train,
        trControl=Chatkal_Khudaydod_16279.model.trainControl,
        methodList = modelList
      )
      Chatkal_Khudaydod_16279.model.ensemble <- caretEnsemble(
        Chatkal_Khudaydod_16279.model.list, 
        metric="RMSE",
        trControl=trainControl(
          number=5,
          allowParallel = TRUE
        ))
      summary(Chatkal_Khudaydod_16279.model.ensemble)
      resIniW[r,i] <- Chatkal_Khudaydod_16279.model.ensemble$error$RMSE
    } else {
      getwd()
      list.files()
      disp("LOADING EXISTING MODELS")
      load("desktop.RData")
    }
  }
}
stopCluster(cl)
