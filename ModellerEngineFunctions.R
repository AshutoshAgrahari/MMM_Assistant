#############################################################################
#######################   AutoModeller functions   ##########################
#############################################################################

applyModellingPeriod <- function(df,startDate,endDate){
  dfDateSubset <- subset(df, df$period >= ymd(startDate) & df$period <= ymd(endDate))
  return(dfDateSubset)
}

#Changine alpha function for automodeller 
getAlpha <- function(df_lagged,df,alpha,beta,df_variable){
  df_laggedDT <- as.data.table(df_lagged)
  dfDT <- as.data.table(df)
  alpha <- alpha[complete.cases(alpha)]
  
  for(name in names(alpha)){
    if(max(dfDT[[name]],na.rm = T) != 0){
      set(x = df_laggedDT,j = name,value = (as.numeric(unname(beta[name]))/(10^10))^((as.numeric(unname(alpha[name]))^((as.numeric(df_laggedDT[[name]])/max(as.numeric(dfDT[[df_variable]]),na.rm = T))*100))))
      #Refresnce formula
      #(1/(10^10))^(as.numeric(unname(alpha[name]))^((df_lagged[,name]/max(df_lagged[,name]))*100))
    } else{
      df_laggedDT[[name]]  <- 0 
    }
  }
  df <- as.data.frame(df_laggedDT)
  return(df)
}

# lag for complete tranformation
applyDfLag <- function(df,lag){
  
  df_Lag <- as.data.table(df)
  for(name in names(lag)){
    if(!is.na(lag[name])) {
      df_Lag[,(name):=shift(df_Lag[[name]],as.numeric(unname(lag[name])),fill = 0,type = "lag")]
    }
  }
  df_lagged <- as.data.frame(df_Lag)
  return(df_lagged)
}

#power 
getPower <- function(df,powerRange){
  dfPowerDt <- as.data.table(df)
  powerSeries <- powerRange[complete.cases(powerRange)]
  for(name in names(powerSeries)) {
    set(dfPowerDt,j = name,value=dfPowerDt[[name]]^as.numeric(unname(powerSeries[name])))
  }
  df <-  as.list.data.frame(dfPowerDt)
  return(df)
}

#Decay
getDecay <- function(df,decay){
  
  df_DecayDT <- as.data.table(df)
  decay <- decay[complete.cases(decay)]
  calcDecay <- function(col,decay){
    for(i in 1:length(col)){
      if(i ==1){
        col[i] <- as.numeric(col[i])
      } else if(!is.na(col[i - 1])){
        col[i] <- as.numeric(col[i])+ as.numeric(col[i - 1]*(1-decay))
      }
    }
    return(col)
  }
  
  for(name in names(decay)) {
    set(df_DecayDT,j = name,value=calcDecay(df_DecayDT[[name]],as.numeric(unname(decay[name]))))
  }
  df <-  as.data.frame(df_DecayDT)
  return(df)
}

# transform the data as per bucket
CreateAllTransformations <- function(df, trnsList, amInputBucketList){
  colNames <- names(df)
  transVar <- as.character(unlist(amInputBucketList[names(amInputBucketList) %in% trnsList$selectedTransBucket]))
  dfToNonTrans <- df[,-which(names(df) %in% c("Period",transVar))]
  dfToTrans <- data.frame(df[,which(names(df) %in% transVar)],stringsAsFactors = F)
  names(dfToTrans) <- names(df)[which(names(df) %in% transVar)]
  if(length(dfToTrans)==1){
    dfToTrans[,1] <- as.numeric(dfToTrans[,1])
  }else{
    dfToTrans <- as.data.frame(apply(dfToTrans,2,as.numeric))
  }
  
  transformedDf  <- list()
  for (name in names(dfToNonTrans)) {
    transformedDf[[name]][[name]] <- as.numeric(dfToNonTrans[,name])
  }
  
  for(name in names(dfToTrans)){
    # Transforming the data as per the bucket selection.
    transformedDf[[name]]<- list()
    lagTrans <- as.data.frame(replicate(as.numeric(as.character(dfToTrans[,name])), n = (trnsList$getLagMax-trnsList$getLagMin)+1),stringsAsFactors = F)
    lagSeries <- as.numeric(trnsList$getLagMin:trnsList$getLagMax)
    names(lagSeries) <- paste0(name,"_L",trnsList$getLagMin:trnsList$getLagMax)
    names(lagTrans) <- names(lagSeries)
    lagTrans <- applyDfLag(df = lagTrans,lag = lagSeries)

    if(trnsList$decaySelection == "Alpha Decay"){
      alphaDecayTransList <- alphaDecayTrans(dfToTrans,lagTrans,name,trnsList)
      transformedDf[[name]] <- append(transformedDf[[name]],values = alphaDecayTransList) 
    }else if(trnsList$decaySelection == "Power Decay"){
      powerDecayTransList <- powerDecayTrans(dfToTrans,lagTrans,name,trnsList)
      transformedDf[[name]] <- append(transformedDf[[name]],values = powerDecayTransList)
    }else if(trnsList$decaySelection == "Decay Power"){
      decayPowerTransList <- decayPowerTrans(dfToTrans,lagTrans,name,trnsList)
      transformedDf[[name]] <- append(transformedDf[[name]],values = decayPowerTransList) 
    }else if(trnsList$decaySelection == "Decay Alpha"){
      decayAlphaTransList <- decayAlphaTrans(dfToTrans,lagTrans,name,trnsList)
      transformedDf[[name]] <- append(transformedDf[[name]],values = decayAlphaTransList) 
    }
  }
  
  return(transformedDf)
}

#capturing Alpha Decay data
alphaDecayTrans <- function(dfToTrans,lagTrans,name,trnsList){
  alphaTransformedList <- list()
  for(lagName in names(lagTrans)){
    AlphaSeries <- as.numeric(seq(from=trnsList$getAlphaMin,to=trnsList$getAlphaMax,by=trnsList$getAlphaSteps))
    lagTransAlpha <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])),n = length(AlphaSeries)),stringsAsFactors = F)
    names(AlphaSeries) <- paste0(lagName,"_A",seq(from=trnsList$getAlphaMin,to=trnsList$getAlphaMax,by=trnsList$getAlphaSteps))
    names(lagTransAlpha) <- names(AlphaSeries)
    betaSeries <- rep(1,times=length(AlphaSeries))
    names(betaSeries) <-  names(AlphaSeries)
    df <- as.data.frame(dfToTrans[,name])
    colnames(df) <- name
    lagTransAlpha <- getAlpha(df_lagged = lagTransAlpha,df = df,alpha = AlphaSeries,beta = betaSeries,df_variable = name)
    
    for(alphaName in names(lagTransAlpha)){
      decaySeries <- as.numeric(seq(from=trnsList$getDecayMin,to=trnsList$getDecayMax,by=trnsList$getDecaySteps))
      lagTransAlphaDecay <- as.data.frame(replicate(as.numeric(as.character(lagTransAlpha[,alphaName])),n = length(decaySeries)),stringsAsFactors = F)
      names(decaySeries) <- paste0(alphaName,"_D",seq(from=trnsList$getDecayMin,to=trnsList$getDecayMax,by=trnsList$getDecaySteps))
      names(lagTransAlphaDecay) <- names(decaySeries)
      lagTransAlphaDecay <- getDecay(lagTransAlphaDecay,decaySeries)
      
      alphaTransformedList <- c(alphaTransformedList,lagTransAlphaDecay)
    }
  }
  return(alphaTransformedList)
}

#capturing Power Decay data
powerDecayTrans <- function(dfToTrans,lagTrans,name,trnsList){
  powerTransformedList <- list()
  for(lagName in names(lagTrans)){
    powerSeries <- as.numeric(seq(from=trnsList$getPowerMin,to=trnsList$getPowerMax,by=trnsList$getPowerSteps))
    lagTransPower <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])), n = length(powerSeries)),stringsAsFactors = F)
    names(powerSeries) <- paste0(lagName,"_P",seq(from=trnsList$getPowerMin,to=trnsList$getPowerMax,by=trnsList$getPowerSteps))
    names(lagTransPower) <- names(powerSeries)
    lagTransPower <- as.data.frame.list(getPower(lagTransPower,powerSeries))
    for(powerName in names(lagTransPower)){
      decaySeries <- as.numeric(seq(from=trnsList$getDecayMin,to=trnsList$getDecayMax,by=trnsList$getDecaySteps))
      lagTransPowerDecay <- as.data.frame(replicate(as.numeric(as.character(lagTransPower[,powerName])),n = length(decaySeries)),stringsAsFactors = F)
      names(decaySeries) <- paste0(powerName,"_D",seq(from=trnsList$getDecayMin,to=trnsList$getDecayMax,by=trnsList$getDecaySteps))
      names(lagTransPowerDecay) <- names(decaySeries)
      lagTransPowerDecay <- getDecay(lagTransPowerDecay,decaySeries)
      
      powerTransformedList <- c(powerTransformedList,lagTransPowerDecay)
    }
  }
  return(powerTransformedList)
}

#capturing Decay Power data
decayPowerTrans <- function(dfToTrans,lagTrans,name,trnsList){
  powerTransformedList <- list()
  for(lagName in names(lagTrans)){
    decaySeries <- as.numeric(seq(from=trnsList$getDecayMin,to=trnsList$getDecayMax,by=trnsList$getDecaySteps))
    lagTransDecay <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])), n = length(decaySeries)),stringsAsFactors = F)
    names(decaySeries) <- paste0(lagName,"_D",seq(from=trnsList$getDecayMin,to=trnsList$getDecayMax,by=trnsList$getDecaySteps))
    names(lagTransDecay) <- names(decaySeries)
    lagTransDecay <- getDecay(lagTransDecay,decaySeries)
    for(powerName in names(lagTransDecay)){
      powerSeries <- as.numeric(seq(from=trnsList$getPowerMin,to=trnsList$getPowerMax,by=trnsList$getPowerSteps))
      lagTransDecayPower <- as.data.frame(replicate(as.numeric(as.character(lagTransDecay[,powerName])), n = length(powerSeries)),stringsAsFactors = F)
      names(powerSeries) <- paste0(powerName,"_P",seq(from=trnsList$getPowerMin,to=trnsList$getPowerMax,by=trnsList$getPowerSteps))
      names(lagTransDecayPower) <- names(powerSeries)
      lagTransDecayPower <- getPower(lagTransDecayPower,powerSeries)
      
      powerTransformedList <- c(powerTransformedList,lagTransDecayPower)
    }
  }
  return(powerTransformedList)
}

#capturing Decay Alpha data
decayAlphaTrans <- function(dfToTrans,lagTrans,name,trnsList){
  alphaTransformedList <- list()
  for(lagName in names(lagTrans)){
    decaySeries <- as.numeric(seq(from=trnsList$getDecayMin,to=trnsList$getDecayMax,by=trnsList$getDecaySteps))
    lagTransDecay <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])), n = length(decaySeries)),stringsAsFactors = F)
    names(decaySeries) <- paste0(lagName,"_D",seq(from=trnsList$getDecayMin,to=trnsList$getDecayMax,by=trnsList$getDecaySteps))
    names(lagTransDecay) <- names(decaySeries)
    lagTransDecay <- getDecay(lagTransDecay,decaySeries)
    for(alphaName in names(lagTransDecay)){
      AlphaSeries <- as.numeric(seq(from=trnsList$getAlphaMin,to=trnsList$getAlphaMax,by=trnsList$getAlphaSteps))
      lagTransDecayAlpha <- as.data.frame(replicate(as.numeric(as.character(lagTransDecay[,alphaName])),n = length(AlphaSeries)),stringsAsFactors = F)
      names(AlphaSeries) <- paste0(alphaName,"_A",seq(from=trnsList$getAlphaMin,to=trnsList$getAlphaMax,by=trnsList$getAlphaSteps))
      names(lagTransDecayAlpha) <- names(AlphaSeries)
      betaSeries <- rep(1,times=length(AlphaSeries))
      names(betaSeries) <-  names(AlphaSeries)
      df <- as.data.frame(dfToTrans[,name])
      colnames(df) <- alphaName
      lagTransDecayAlpha <- getAlpha(df_lagged = lagTransDecayAlpha,df = df,alpha = AlphaSeries,beta = betaSeries,df_variable = alphaName)
      
      alphaTransformedList <- c(alphaTransformedList,lagTransDecayAlpha)
    }
  }
  return(alphaTransformedList)
}

getTransformedVariables <- function(amTransDataList,parametersDf){
  
  transformedVariablesIndexDf  <- parametersDf[which(parametersDf$Transformation != "Linear"),]
  transformedVariablesIndexDf$Bucket <- as.character(transformedVariablesIndexDf$Bucket)
  transformedVariablesDf <- data.frame()
  variables <- NULL
  for(i in 1:nrow(transformedVariablesIndexDf)){
    if(transformedVariablesIndexDf$Transformation[i] == "Decay Power"){
    variables <- sapply(paste0(transformedVariablesIndexDf$VariableName[i],"_L",as.numeric(as.character(transformedVariablesIndexDf$LagMin[i])):as.numeric(as.character(transformedVariablesIndexDf$LagMax[i]))),FUN = function(x)sapply(paste0(x,"_D",paste0(seq(from=as.numeric(as.character(transformedVariablesIndexDf$DecayMin[i])),to=as.numeric(as.character(transformedVariablesIndexDf$DecayMax[i])),by=as.numeric(as.character(transformedVariablesIndexDf$DecaySteps[i]))))),FUN = function(y)paste0(y,"_P",paste0(seq(from=as.numeric(as.character(transformedVariablesIndexDf$PowerMin[i])),to=as.numeric(as.character(transformedVariablesIndexDf$PowerMax[i])),by=as.numeric(as.character(transformedVariablesIndexDf$PowerSteps[i]))))),simplify = T),simplify = T)
    
    } else if (transformedVariablesIndexDf$Transformation[i] == "Alpha Decay"){
      variables <- sapply(paste0(transformedVariablesIndexDf$VariableName[i],"_L",as.numeric(as.character(transformedVariablesIndexDf$LagMin[i])):as.numeric(as.character(transformedVariablesIndexDf$LagMax[i]))),FUN = function(x)sapply(paste0(x,"_A",paste0(seq(from=as.numeric(as.character(transformedVariablesIndexDf$AlphaMin[i])),to=as.numeric(as.character(transformedVariablesIndexDf$AlphaMax[i])),by=as.numeric(as.character(transformedVariablesIndexDf$AlphaSteps[i]))))),FUN = function(y)paste0(y,"_D",paste0(seq(from=as.numeric(as.character(transformedVariablesIndexDf$DecayMin[i])),to=as.numeric(as.character(transformedVariablesIndexDf$DecayMax[i])),by=as.numeric(as.character(transformedVariablesIndexDf$DecaySteps[i]))))),simplify = T),simplify = T)
      
    } else if(transformedVariablesIndexDf$Transformation[i] == "Power Decay"){
      variables <- sapply(paste0(transformedVariablesIndexDf$VariableName[i],"_L",as.numeric(as.character(transformedVariablesIndexDf$LagMin[i])):as.numeric(as.character(transformedVariablesIndexDf$LagMax[i]))),FUN = function(x)sapply(paste0(x,"_P",paste0(seq(from=as.numeric(as.character(transformedVariablesIndexDf$PowerMin[i])),to=as.numeric(as.character(transformedVariablesIndexDf$PowerMax[i])),by=as.numeric(as.character(transformedVariablesIndexDf$PowerSteps[i]))))),FUN = function(y)paste0(y,"_D",paste0(seq(from=as.numeric(as.character(transformedVariablesIndexDf$DecayMin[i])),to=as.numeric(as.character(transformedVariablesIndexDf$DecayMax[i])),by=as.numeric(as.character(transformedVariablesIndexDf$DecaySteps[i]))))),simplify = T),simplify = T)
      
    } else if (transformedVariablesIndexDf$Transformation[i] == "Decay Alpha"){
      variables <- sapply(paste0(transformedVariablesIndexDf$VariableName[i],"_L",as.numeric(as.character(transformedVariablesIndexDf$LagMin[i])):as.numeric(as.character(transformedVariablesIndexDf$LagMax[i]))),FUN = function(x)sapply(paste0(x,"_D",paste0(seq(from=as.numeric(as.character(transformedVariablesIndexDf$DecayMin[i])),to=as.numeric(as.character(transformedVariablesIndexDf$DecayMax[i])),by=as.numeric(as.character(transformedVariablesIndexDf$DecaySteps[i]))))),FUN = function(y)paste0(y,"_A",paste0(seq(from=as.numeric(as.character(transformedVariablesIndexDf$AlphaMin[i])),to=as.numeric(as.character(transformedVariablesIndexDf$AlphaMax[i])),by=as.numeric(as.character(transformedVariablesIndexDf$AlphaSteps[i]))))),simplify = T),simplify = T)
    }
     
    tempDf <- as.data.frame.list(amTransDataList[[transformedVariablesIndexDf$Bucket[i]]][[transformedVariablesIndexDf$VariableName[i]]][names(amTransDataList[[transformedVariablesIndexDf$Bucket[i]]][[transformedVariablesIndexDf$VariableName[i]]]) %in% variables])
    
    if(ncol(transformedVariablesDf) ==0 ){
      transformedVariablesDf <- tempDf
    }else{
      transformedVariablesDf <- cbind(transformedVariablesDf,tempDf)  
    }
  }
  return(transformedVariablesDf)
}


buildFormulaList <- function(amTransDataList,modelScopeDf,bucketData,parametersDf){
  
  linearNames <-  names(amTransDataList)[names(amTransDataList) %in% parametersDf$Bucket[parametersDf$Transformation == "Linear" & parametersDf$ModellingFlag == "Yes" & parametersDf$Bucket != "Dependent"]]
  nonLinearNames <-  names(amTransDataList)[names(amTransDataList) %in% parametersDf$Bucket[parametersDf$Transformation != "Linear" & parametersDf$ModellingFlag == "Yes"]]
  
  # calculating the range of combination for each bucket
  bucketData[,-1] <- apply(bucketData[,-1],2,as.numeric)
  bucketRangeList <- lapply(bucketData$Bucket, function(x) bucketData$MinVariables[bucketData$Bucket == x]:bucketData$Max[bucketData$Bucket == x])
  names(bucketRangeList) <- bucketData$Bucket
  
  # generating bucket wise combination for formula building
  bucketComb <- data.frame(expand.grid(bucketRangeList),stringsAsFactors = F)
  
  # getting all variables by bucket.
  bucketVarList <- list()
  bucketVarList[["Dependent"]] <- as.character(parametersDf$VariableName[which(parametersDf$Bucket == "Dependent")])
  # collecting all linear variables by bucket
  if(length(linearNames)!= 0){
    bucketVarListLr <- lapply(linearNames, function(x) {names(amTransDataList[[x]])[names(amTransDataList[[x]]) %in% parametersDf$VariableName[parametersDf$Bucket==x & parametersDf$ModellingFlag == "Yes"]]})
    names(bucketVarListLr) <- linearNames
    bucketVarList <- append(bucketVarList, bucketVarListLr)
  }
  
  # collecting all nonlinear variables by bucket
  if(length(nonLinearNames)!= 0){
    bucketVarListNLr <- lapply(nonLinearNames, function(x) {names(amTransDataList[[x]])[names(amTransDataList[[x]]) %in% parametersDf$VariableName[parametersDf$Bucket==x & parametersDf$ModellingFlag == "Yes"]]})
    names(bucketVarListNLr) <- nonLinearNames
    for(name in names(bucketVarListNLr)){
      tempList <- lapply(bucketVarListNLr[[name]], function(x){names(amTransDataList[[name]][[x]])})
      names(tempList) <- bucketVarListNLr[[name]]
      bucketVarListNLr[[name]] <- tempList
    }
    bucketVarList <- append(bucketVarList, bucketVarListNLr)
  }
  
  # generating all possible combination of each bucket by min and max. (by default generating NULL for min 0)
  bucketVarComb <- list()
  for(name in names(bucketRangeList)){
    # name <- names(bucketRangeList)[1]
    range <- bucketRangeList[[name]]
    if(name %in% linearNames){
      bucketVarComb[[name]] <- lapply(range, function(x){
        if(x!=0){
          unlist(combn(bucketVarList[[name]],x,simplify = F,FUN = function(x){paste0(x,collapse = " + ")}))
        }
      })
    }else{
      bucketVarComb[[name]] <- lapply(range, function(x){
        if(x!=0){
          varCombList <- as.list(as.data.frame(combn(names(bucketVarList[[name]]),x)))
          apply(varList <- sapply(data.frame(rbindlist(lapply(varCombList, function(x){
            data.frame(do.call(expand.grid,bucketVarList[[name]][names(bucketVarList[[name]]) %in% unlist(x)]),stringsAsFactors = F)
          }))), as.character),1,FUN = function(x){paste(x,collapse = "+")})
        }
      })
    }
	  names(bucketVarComb[[name]])<- range
  }
  
  # remove all NULL from nested bucketvVarComb list
  bucketVarComb <- rlist::list.clean(bucketVarComb,fun = is.null, recursive = T)
  
  # baseformula with dependent only
  baseformula <- paste0(bucketVarList$Dependent," ~ ")
  
  # building formula by bucketComb row wise
  # remove 0 from bucketComb after comnverting into list.
  formulaList <- NULL
  for(i in 1:nrow(bucketComb)){
    if(length(formulaList) < 200000){
      bucket <- as.list(bucketComb[i,])
      bucket[bucket == 0] <- NULL
      if(length(bucket)!= 0){
        formulaList <- c(formulaList, paste0(baseformula, apply(expand.grid(lapply(names(bucket), function(x){bucketVarComb[[x]][[as.character(bucket[[x]])]]}),stringsAsFactors = F),1,paste0, collapse = " + ")))
      }
    }
  }
  
  return(as.list(formulaList))
}

getIterCount <- function(RegDataTemp,amTransDataList, bucketData,parametersDf,startDate, endDate){
  modelScopeDf <- getRegDataTable(RegDataTemp,amTransDataList, parametersDf,startDate, endDate)
  baseFormula <- buildFormulaList(amTransDataList,modelScopeDf,bucketData,parametersDf)
  return(baseFormula)
}

getActualVsPredictedDf <- function(modelScopeDf,model){
  data <- modelScopeDf
  actPred <- cbind.data.frame(Period =data[,"period"], Actual = model$model[,1], Predicted = fitted(model), Residual = residuals(model))
  return(actPred)
}

getElasticity  <- function(model,parametersDf){
  
  modelScopeMean <- colMeans(model$model)
  modelScopeMean_12 <- colMeans(tail(model$model,n = 12))
  depVar <- parametersDf$VariableName[parametersDf$Bucket=="Dependent"]
  df <- NULL
  contribution <- NULL
  elasticity  <- NULL
  elasticity_12 <- NULL
  
  for(name in names(model$coefficients)){
    
    if(name == "(Intercept)"){
      contribution[name] <- (model$coefficients[name] *100)/modelScopeMean[depVar]
    } else {
      
      contribution[name] <- model$coefficients[name]*(modelScopeMean[name]/modelScopeMean[depVar])* 100
      
      if(length(parametersDf$Transformation[parametersDf$VariableName == name]) != 0 && parametersDf$Transformation[parametersDf$VariableName == name] == "Linear" || grepl("Dummy",name)){
        elasticity[name] <- 5*model$coefficients[name]*(modelScopeMean[name]/modelScopeMean[depVar])
        
        elasticity_12[name] <- 5*model$coefficients[name]*(modelScopeMean_12[name]/modelScopeMean_12[depVar])
        
        
      } else {
        decayValue <- as.numeric(gsub("D","",str_extract(name,"D\\d+.\\d+")))
        powerValue <- as.numeric(gsub("P","",str_extract(name,"P\\d+.\\d+")))
        
        elasticity[name] <- (((1.01^(powerValue))-1)*100*(model$coefficients[name])*(modelScopeMean[name])/(modelScopeMean[depVar]))*5
        
        elasticity_12[name] <- (((1.01^(powerValue))-1)*100*(model$coefficients[name])*(modelScopeMean_12[name])/(modelScopeMean_12[depVar]))*5
      } 
    }  
  }
  parameterDetails <- cbind(tidy(model),contribution = contribution,VIF=c(0,vif(model)),Elasticity_Modelling_Period = c(0,elasticity),Elasticity_L12_Modelling_Period = c(0,elasticity_12))
  
  return(parameterDetails)
}

#get data for transformation
getDataForTransformation <-function(RegDataTemp,amTransDataList,parametersDf,bucketData,startDate,endDate, baseFormula, rankType){
  modelScopeDf <- getRegDataTable(RegDataTemp,amTransDataList, parametersDf,startDate, endDate)
  # Ashutosh: passing parameterDF to allPossibleRegressions() to get T-stat for each variable given by user
  system.time(resultList <- allPossibleRegressions(modelScopeDf = modelScopeDf,baseFormula, parametersDf))
  
  result <- resultList$result
  # if there is no model, then message will display to change parameter.
  if(length(result) == 1){
    resultData <- list()
    resultData[["Models"]] <- result
    resultData[["ModelScopeDf"]] <- modelScopeDf
    resultData[["finalDf"]] <- modelScopeDf
    resultData[["modelList"]] <- resultList$ModelList
    return(resultData)
  }else {
    rankResultList <- list()
    rankResultList <- rankModels(data = result,rankType)
    resultData <- list()
    if(length(rankResultList) != 1){
      ranked_result <- cbind(result, Model_Rank = rankResultList$rank, Model_Score = rankResultList$score)
      ranked_result$Model_Score <- round(ranked_result$Model_Score,digits = 2)
      resultData[["Models"]] <- ranked_result
      resultData[["ModelScopeDf"]] <- modelScopeDf
      resultData[["finalDf"]] <- modelScopeDf
      resultData[["modelList"]] <- resultList$ModelList
      return(resultData)
    }else{
      resultData[["Models"]] <- result
      resultData[["ModelScopeDf"]] <- modelScopeDf
      resultData[["finalDf"]] <- modelScopeDf
      resultData[["modelList"]] <- resultList$ModelList
      return(resultData)
    }
  }
}

# Function to get data table for linear regression input with specified period range.
getRegDataTable <- function(RegDataTemp,amTransDataList,parametersDf,startDate,endDate){
  linearVariables  <- parametersDf$VariableName[which(parametersDf$Transformation == "Linear")]
  linearVariablesDf <- as.data.frame(RegDataTemp[-1,][,linearVariables])
  names(linearVariablesDf) <- linearVariables
  if(any(as.character(parametersDf$Transformation)!= "Linear")){
    transformedVariableDf <- getTransformedVariables(amTransDataList,parametersDf)
    finalDf  <- cbind(period=lubridate::dmy(RegDataTemp[-1,1]),linearVariablesDf,transformedVariableDf)
  } else {
    finalDf  <- cbind(period=lubridate::dmy(RegDataTemp[-1,1]),linearVariablesDf)
  }
  modelScopeDf <- applyModellingPeriod(finalDf,startDate,endDate)
  return(modelScopeDf)
}

getDummyModelResult <- function(model, modelScopeDummyTable, RegDataTemp, modelScopeDf){
  if(grepl("Intercept",names(coef(model)))){
    candidateModelVar <- names(coef(model))[-1]
  }else{
    candidateModelVar <- names(coef(model))
  }
  
  candidateModelDf <- as.data.frame(modelScopeDf[which(modelScopeDf$period >= min(modelScopeDummyTable$Period) & modelScopeDf$period <= max(modelScopeDummyTable$Period)),which(names(modelScopeDf) %in% as.character(candidateModelVar))])
  names(candidateModelDf) <- as.character(candidateModelVar)
  
  dummyDFTable <- as.data.frame(modelScopeDummyTable[,which(names(modelScopeDummyTable) %in% names(which(apply(modelScopeDummyTable[,-1],2,sum)!=0)))])
  names(dummyDFTable) <- names(which(apply(modelScopeDummyTable[,-1],2,sum)!=0))
  
  modelScopeDfDepVar <- names(unlist(sapply(RegDataTemp[1,], function(x) which(x == "Dependent"))))
  modelScopeDfDep <- data.frame(modelScopeDf[which(modelScopeDf$period >= min(modelScopeDummyTable$Period) & modelScopeDf$period <= max(modelScopeDummyTable$Period)), names(modelScopeDf) %in% modelScopeDfDepVar], stringsAsFactors = FALSE)
  colnames(modelScopeDfDep) <- modelScopeDfDepVar
  
  candidateModelScopeDf <- cbind(modelScopeDfDep, candidateModelDf, dummyDFTable)
  candidateModelScopeDf <- as.data.frame(lapply(candidateModelScopeDf, function(x) as.numeric(as.character(x))))
  
  candidateModelVarList <- list()
  NonDependent <- names(candidateModelDf)
  DummyVar <- names(dummyDFTable)
  NonDependent <- append(NonDependent, DummyVar)
  candidateModelVarList[["NonDependent"]] <- NonDependent
  candidateModelVarList[["Dependent"]] <- names(modelScopeDfDep)
  
  baseFormula <- as.formula(paste0(candidateModelVarList$Dependent," ~ ", paste0(unlist(candidateModelVarList$NonDependent),collapse = "+")))
  modelDummy <- lm(formula = baseFormula, data = candidateModelScopeDf)
  return(modelDummy)
}


updateAllModelsResults <- function(allModelsResults,model.index,resultTable, allModelsList, rankType){
  result <- as.data.frame(t(extractModelParameterValue(allModelsResults[[length(allModelsResults)]])))
  modelNumber <- strsplit(as.character(resultTable$`Model No`[model.index]), split = "_")
  modelNumber <- as.numeric(modelNumber[[1]][2])
  allModelsList[[modelNumber]] <- allModelsList[[modelNumber]]+1
  
  result <- cbind(nrow(resultTable)+1 ,Model_No = paste0("CANDIDATE_",modelNumber,"_Dummy_",as.numeric(allModelsList[[modelNumber]])), result)
  colnames(result) <- c("Index","Model No","%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","F_Stat","MAPE")
  result$`%R2` <- sapply(result$`%R2`, function(x) x <- round((x * 100),digits = 2))
  result$`%R2.adj` <- sapply(result$`%R2.adj`, function(x) x <- round((x * 100),digits = 2))
  
  if(length(resultTable)==8){
    result <- rbind(resultTable, result)
  } else {
    result <- rbind(resultTable[,!names(resultTable) %in% c("Model_Rank","Model_Score")], result)
  }
  rankResultList <- rankModels(result, rankType)
  ranked_result <- cbind(result, Model_Rank = rankResultList$rank, Model_Score = rankResultList$score)
  ranked_result$Model_Score <- round(ranked_result$Model_Score,digits = 2)
  resultList <- as.list(NULL)
  resultList[["ranked_result"]] <- ranked_result
  resultList[["allModelsList"]] <- allModelsList
  
  return(resultList)
}

allPossibleRegressions <- function(modelScopeDf, baseFormula, parametersDf){
  n <- nrow(modelScopeDf)
  modelScopeDfFinal <- modelScopeDf
  modelScopeDfFinal$period <- NULL
  modelScopeDfFinal <- as.data.frame(lapply(modelScopeDfFinal, function(x) as.numeric(as.character(x))))
  allModelsResults <<- lapply(baseFormula,function(x, data) lm(x, data=modelScopeDfFinal),data=modelScopeDfFinal)
  allModelsResults1 <- modelFilterByTStat(allModelsResults, parametersDf)
  allModelsResults <- allModelsResults1
  result <- NULL
  result[["ModelList"]] <- allModelsResults
  result[["result"]] <- extractModelParameter(allModelsResults)
  
  return(result)
}

# Function to extract model parameters to display on screen
extractModelParameter <- function(allModelsResults){
  #allModelsResults <- dummyModel
  # calculating number of models
  n.models <- length(allModelsResults)
  
  if(n.models == 0){
    # if there is no model, then message will display to change parameter.
    return(0)
  }else{
    # calling the function to extract the parameter of each model to rank.
    result <- lapply(allModelsResults, extractModelParameterValue)
    result <- as.data.frame(matrix(unlist(result), nrow=n.models, byrow=T))
    result <- cbind(index = c(1:nrow(result)),paste0("CANDIDATE_",1:nrow(result)), result)
    
    rownames(result) <- NULL
    colnames(result) <- c("Index","Model No","%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","F_Stat","MAPE")
    result$`%R2` <- sapply(result$`%R2`, function(x) x <- round((x * 100),digits = 2))
    result$`%R2.adj` <- sapply(result$`%R2.adj`, function(x) x <- round((x * 100),digits = 2))
    
    return(result)
  }
}

# fucntion to extract model parameter to rank the model
extractModelParameterValue <- function(fit) {
  R2 <- summary(fit)$r.squared
  R2.adj <- summary(fit)$adj.r.squared
  dw <- abs(2-durbinWatsonTest(fit)[[2]])
  model_t_stat_avg <- mean(abs(tidy(fit)$statistic))
  VIF.Avg <- mean(abs(vif(fit)))
  RootMSE <- sqrt(mean(fit$residuals^2))
  F_Stat <- round(summary(fit)$fstatistic[1],digits = 5)
  MAPE <- MAPE(fit$fitted.values, fit$model[,1])
  out <- data.frame(R2=R2, R2.adj=R2.adj,DurbinWatson=dw, T.Stat.Avg = model_t_stat_avg, VIF.Avg = VIF.Avg,RootMSE = RootMSE, F_Stat = F_Stat, MAPE = MAPE)
  
  out <- sapply(out,function(x) if(!is.nan(x)) {x <- x}
    else{x <- 0}
    )
  
  return(out)
}


# check the model tstat of tstat dir variable to filter out others models from allmodelresults.
modelFilterByTStat <- function(allModelsResults,parametersDf){
  # extarcting variable name with tStatDir from parameterDF by only taking only non zero tStatDir variables to filter the models.
  tstatParameterDF <- parametersDf[which(parametersDf$TstatDir != 0) ,c(1,which(colnames(parametersDf)=="TstatDir"))]
  
  # Function to check the model if tstat of model term should be greater than the tStatDir value provided by user for positive tstat direction or if tstat of model term should be lesser than the tStatDir value provided by user for negative tstat direction.
  tStatCheck <- function(modelIndex, tstatParam){
    modelData <- allModelsResults[[modelIndex]]
    modelDf <- tidy(modelData)[c(1,4)]
    modelTerm <- gsub("_L+[0-9].*", "", modelDf$term) 
    flag <- 0
    if(all(tstatParam$VariableName %in% modelTerm)){
      for (i in 1:nrow(tstatParam)) {
        if(tstatParam$TstatDir[i] < 0 & round(modelDf[grep(tstatParam$VariableName[i], modelTerm),2],5) < 0  &  abs(round(modelDf[grep(tstatParam$VariableName[i], modelTerm),2],5)) > abs(round(as.numeric(as.character(tstatParam$TstatDir[i])),2))){
          flag <- flag + 1
        }else if(tstatParam$TstatDir[i] > 0 & round(modelDf[grep(tstatParam$VariableName[i], modelTerm),2],5) > round(as.numeric(as.character(tstatParam$TstatDir[i])),2)){
          flag <- flag + 1
        }
      }
      if(flag == nrow(tstatParam)){
        return(modelIndex)
      }
    }else {
      # here return modelindex which doesn't have tsat derived variable.
      return(modelIndex)
    }
  }
  
  if(nrow(tstatParameterDF) >= 1){
    tstatModel <- lapply(1:nrow(tstatParameterDF),function(y){
      unlist(sapply(1:length(allModelsResults), function(x, tstatParam){tStatCheck(x,tstatParam)}, tstatParam = tstatParameterDF[y,]))
    })
    
    tstatFilteredModelIndex <- Reduce(intersect,  tstatModel) 
    tstatFinalModel <- allModelsResults[tstatFilteredModelIndex]
    return(tstatFinalModel)
  }else{
    return(allModelsResults)
  }
}

#Function definition to Ranking the Model based on score
rankModels <- function(data, rankType) {
  
  if(nrow(data) == 1 |nrow(data) == 0){
    return(list(0))
  }else{
    if(rankType == "Ranking1"){
      
      #Ranking formula is used by Nimish's team
      factors_for_ranking <- data
      factors_for_ranking$RankAverage <- factors_for_ranking$`%R2` - factors_for_ranking$`%R2.adj` 
      factors_for_ranking <- factors_for_ranking[order(factors_for_ranking$RankAverage,-factors_for_ranking$F_Stat),]
      factors_for_ranking <- within(factors_for_ranking, FinalRankForModels <- rank(order(factors_for_ranking$RankAverage,-factors_for_ranking$F_Stat), ties.method='average'))
      factors_for_ranking <- factors_for_ranking[order(factors_for_ranking$Index),]
      
      return(list(rank=factors_for_ranking$FinalRankForModels,score =factors_for_ranking$RankAverage))
      
    }else if(rankType == "Ranking2"){
      # Ranking formula is used by Sounava's team
      factors_for_ranking <- data
      # creating ranks
      # ranks based on R Square, Adjusted R square and Tstat Average in descending order
      RankedModels_based_on_Parameters <- as.data.frame(
        apply(cbind(factors_for_ranking$R2,factors_for_ranking$R2.adj,factors_for_ranking$T.stat.avg),
              2,FUN=function(x){
                rank(-x,ties.method = "average")
              }))
      
      #Normalizing DW statistic by negating all the values from 2.
      factors_for_ranking$DW_normalized <- ave(factors_for_ranking$DW,FUN=function(y){2-y})
      
      # Ranks based on DW and RMSE in ascending order
      ReverseRanking_Models <- as.data.frame(
        apply(cbind(factors_for_ranking$RootMSE,factors_for_ranking$DW_normalized),2,
              FUN=function(z){
                rank(z,ties.method = "average")
              }))
      
      # Final ranks in a dataframe
      FinaldataforRanking <- cbind(RankedModels_based_on_Parameters,ReverseRanking_Models)
      
      #Averaging the ranks across parameters for models
      factors_for_ranking$RankAverage <- apply(FinaldataforRanking,1,mean)
      
      #Final Ranks for Models based on the average of all the statistical parameters
      factors_for_ranking <- transform(
        factors_for_ranking,FinalRankForModels = rank(factors_for_ranking$RankAverage,ties.method = "average")
      )
      
      # rounding off model rank to it's floor value.
      factors_for_ranking$FinalRankForModels <- floor(factors_for_ranking$FinalRankForModels)
      return(list(rank=factors_for_ranking$FinalRankForModels,score =factors_for_ranking$RankAverage))
    }
    
  }
}

sortModelResult <- function(modelResult,flag){
  if(flag == TRUE){
    resultTableDF <- arrange(modelResult, Model_Rank)
  }else{
    resultTableDF <- modelResult
  }
  return(resultTableDF)
}

# Function to create bucket wise variable name in data frame to filter the model based on variables.
createBucketVarData <- function(amInputBuckets){ 
  amInputBuckets <- amInputBuckets[!which(amInputBuckets$bucket=="Dependent"),]
  
  bucketList <- split(amInputBuckets, by = "bucket",keep.by = FALSE)
  ## Compute maximum length of each bucket
  bucketVarLength <- as.vector(NULL)
  for (i in 1:length(bucketList)) {
    bucketVarLength <- append(bucketVarLength, length(bucketList[[i]]$variableName))
  }
  max.bucketLen <- max(bucketVarLength)
  ## Add NA values to list elements
  for (i in 1:length(bucketList)) {
    bucketList[[i]] <- lapply(bucketList[[i]], function(v) { c(v, rep(NA, max.bucketLen-length(v)))})
  }
  bucketVar <- as.data.frame.list(bucketList)
  colnames(bucketVar) <- names(bucketList)
  
  return(bucketVar)
}

# Function to extract variable name from all generated models through regression.
extractModelVarName <- function(allModelsResults){
  
  modelVarNames <- list()
  
  # extracting model variable with model index from all model results.
  for (i in 1:length(allModelsResults)) {
    test <- allModelsResults[[i]]
    temp <- names(test$model)
    temp[grep("Dummy_Var",temp)] <- NA
    #modelVarNames[[i]] <- c(i,names(test$model))
    modelVarNames[[i]] <- c(i,temp)
  }
  
  ## Compute maximum length
  max.length <- max(sapply(modelVarNames, length))
  ## Add NA values to model variable list elements to make same length 
  modelVarNames <- lapply(modelVarNames, function(v) { c(v, rep(NA, max.length-length(v)))})
  
  modelVar <- do.call(rbind.data.frame, modelVarNames)
  colnames(modelVar) <- paste("Var", 1:ncol(modelVar), sep="")
  
  if(length(allModelsResults)== 1){
    return(modelVar)
    
  }else {
    # removing lag, decay parts from model variable names.
    modelVar <- as.data.frame(apply(modelVar, 2, function(x){
      x <- gsub("_L.*","",as.character(x))
    }))
    return(modelVar)
  }
}

# Function to extract allModelResults filtered by variable given by user.
extractFilterModelResults <- function(bucketSelectedVarName, modelVar, allModelsResults, resultTable){
  test <- bucketSelectedVarName
  
  filterResultTable <- data.frame()
  
  if(length(test)< length(modelVar) & length(test)>=2){
    testResult <- as.data.frame(t(apply(modelVar[,2:(length(test)+1)], 1, function(x){
      x %in% test
    })))
    
    testResult <- cbind(ModelIndex = modelVar$Var1, testResult)
    
    testResult$TrueNumber <- apply(testResult[,2:(length(test)+1)],1,function(x){
      length(which(x==TRUE))
    })
    
    modelFilter <- as.numeric(as.character(testResult$ModelIndex[which(testResult$TrueNumber == length(test))]))
    
    if(length(modelFilter) == 0){
      return(filterResultTable)
    }else{
      filterResultTable <- resultTable[modelFilter, ]
      filterResultTable[,3:7] <- sapply(filterResultTable[,3:7], function(x) round(x, digits = 5))
      return(filterResultTable)
    }
  }else{
    return(filterResultTable)
  }
}

# Function to extract top model for each variable combination.
extractVarCombModel <- function(modelVarCombList, modelVarCombIndex, rankType){
  
  n.models <- length(modelVarCombList)
  
  # calling the function to extract the parameter of each model to rank.
  result <- lapply(modelVarCombList, extractModelParameterValue)
  result <- as.data.frame(matrix(unlist(result), nrow=n.models, byrow=T))
  result <- cbind(paste("CANDIDATE_",modelVarCombIndex), result)
  rownames(result) <- NULL
  colnames(result) <- c("Model No","%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE", "F_Stat","MAPE")
  result$`%R2` <- sapply(result$`%R2`, function(x) x <- round((x * 100),digits = 2))
  result$`%R2.adj` <- sapply(result$`%R2.adj`, function(x) x <- round((x * 100),digits = 2))
  
  if(nrow(result)<=1){
    ranked_result = result
  }else {
    rankResult <- rankModels(result, rankType)
    ranked_result <- cbind(result, Model_Rank = rankResult$rank, Model_Score = rankResult$score)
  }
  
  
  if(nrow(ranked_result) >= 2){
    topModelVarCombResult <- arrange(ranked_result, desc(Model_Score))
    return(topModelVarCombResult[1,])
  }else{
    return(ranked_result)
  }
}

# function to get the unique variable combination table.
getModelVarTable <- function(allModelsResults){
  
  modelVar <- extractModelVarName(allModelsResults)
  
  # to remove the duplicate combination
  modelVarComb <- modelVar[,-1]
  modelVarComb <- modelVarComb[!duplicated(modelVarComb),]
  if(nrow(modelVarComb) == 1){
    modelVarComb <- as.data.frame(t(apply(modelVarComb, 2, as.character)),stringsAsFactors = FALSE)
  }else {
    modelVarComb <- as.data.frame(apply(modelVarComb, 2, as.character),stringsAsFactors = FALSE)
  }
  
  return(modelVarComb)
}

# function to extract Top model for exch variable combination in list.
extractTopModelVariableCombResult <- function(allModelsResults, resultTableDetail, resultTable){
  modelVarComb <- getModelVarTable(allModelsResults)
  modelVar <- extractModelVarName(allModelsResults)
  
  varCombList <- list()
  for (i in 1:nrow(modelVarComb)) {
    varCombList[[i]] <- modelVarComb[i,]
    varCombList[[i]] <- unlist(lapply(varCombList[[i]], na.omit))
  }
  
  varCombList <<- varCombList
  varCombModelList <- list()
  for (i in 1:length(varCombList)) {
    test <- varCombList[[i]]
    testResult <- as.data.frame(t(apply(modelVar[,2:(length(test)+1)], 1, function(x){
      x %in% test
    })))
    testResult <- cbind(ModelIndex = modelVar$Var1, testResult)
    testResult$TrueNumber <- apply(testResult[,2:(length(test)+1)],1,function(x){
      length(which(x==TRUE))
    })
    varCombModelList[[i]] <- as.numeric(as.character(testResult$ModelIndex[which(testResult$TrueNumber == length(test))]))
  }
  
  varCombModelList
  modelCombList <- list()
  for (i in 1:length(varCombModelList)) {
    if(length(varCombModelList[[i]])>1){
      modelCombList[i] <- sortModelResult(resultTableDetail[varCombModelList[[i]],],flag = 1)[1,1]
    }else {
      modelCombList[i] <- resultTable[varCombModelList[[i]][1],1]
    }
  }
  
  return(modelCombList)
}

# function to compare model.
compareModelResult <- function(s, allModelsResults, resultTableDetail, parametersDf){
  models <- allModelsResults[s]
  compareModel <- NULL
  for (i in 1:length(models)) {
    temp <- data.frame(getElasticity(models[[i]],parametersDf = parametersDf),row.names = NULL)
    temp <- temp[,-c(3,5,6)]
    temp$Model <- resultTableDetail[s[i],c("Model No")]
    temp<- temp[,c(7,1:6)]
    if(i==1){
      compareModel <- temp
    }else {
      compareModel <- rbind(compareModel, temp)
    }
  }
  return(compareModel)
}

extractModelDetail <- function(model, modelScopeDf, parametersDf, modelResult, RegDataTemp, dummyModelScopeDf){
  tmpModelScopeDf <- modelScopeDf
  if(any(grepl("Dummy",names(model$coefficients)))){
    dummyModelIndex <- which(as.character(dummyModelScopeDf$Model_No) == modelResult[,2])
    tmpModelScopeDf <- subset(tmpModelScopeDf, period >= dummyModelScopeDf[dummyModelIndex,"Start_Date"] & period <= dummyModelScopeDf[dummyModelIndex,"End_Date"])
  }
  
  modelParameters <- getElasticity(model,parametersDf)
  output <- NULL
  output <- c(output,"The REG Procedure")
  output <- c(output,"\n\n")
  output <- c(output,paste("Model:",modelResult$`Model No`))
  output <- c(output,paste("Dependant Variable:",names(model$model[1])))
  output <- c(output,"\n\n")
  output <- c(output,paste("Number of Observations Read:",nrow(RegDataTemp[-1,])))
  output <- c(output,paste("Number of Observations Used:",nrow(tmpModelScopeDf)))
  output <- c(output,"\n\n")
  output <- c(output,noquote(capture.output(write.csv(modelResult,stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  output <- c(output,noquote(capture.output(write.csv(modelParameters,file = stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  output <- as.data.frame(output,quote=F)
  colnames(output) <- "output"
  
  resultOutput <- list()
  resultOutput[["modelDetails"]] <- cSplit(output,"output",sep = ",",type.convert = F)
  resultOutput[["actPredData"]] <- getActualVsPredictedDf(tmpModelScopeDf,model)
  
  return(resultOutput)
}

extractModelData <- function(model,modelScopeDummyTable, modelScopeDf, parametersDf,dummyModelScopeDf,modelResult){
  
  if(any(grepl("Dummy",names(model$coefficients)))){
    dummyModelIndex <- which(as.character(dummyModelScopeDf$Model_No) == modelResult[,2])
    Period <- modelScopeDf[which(modelScopeDf$period >= dummyModelScopeDf[dummyModelIndex,"Start_Date"] & modelScopeDf$period <= dummyModelScopeDf[dummyModelIndex,"End_Date"]),"period"]
  }else{
    Period <- modelScopeDf$period
  }
  modelData <- cbind(Period,model$model)
  return(modelData)
}


#############################################################################
#####################   Variable Console functions   ########################
#############################################################################

#............................................................................
# Function to process upload file to variable console
#............................................................................

variableConsoleFile <- function(file, actionData, varTag){
  if(actionData == "inputFile"){
    varDataCSV <- file
    varDataCSV <- varDataCSV[-1, ]
    varDataCSV[,2:ncol(varDataCSV)] <- apply(varDataCSV[,2:ncol(varDataCSV)], 2, as.numeric)
    return(varDataCSV)
  }
  else if(actionData == "outputFile"){
    varTagDownload <- rbind(varTag,data.table(variableName = names(file)[-c(1:nrow(varTag))],
      bucket = rep("NewVariable", ncol(file)-nrow(varTag)),
      stringsAsFactors = F
    ))  
    
    varNewTag <- data.table(t(varTagDownload$bucket))
    colnames(varNewTag) <- varTagDownload$variableName
    downloadFile <- file
    downloadFile <- rbind(downloadFile, varNewTag)
    n.var <- nrow(downloadFile)
    downloadFile <- downloadFile[c(n.var,1:(n.var-1))]
    return(downloadFile)
  }
}

################################################################################
#####################   OLS manual process Acquire   ###########################
################################################################################

##################### Function related to Model Manager ########################
olsm_createModelManagerData <- function(olsm_SelectedVar){
  df <- data.frame(
    VariableName = olsm_SelectedVar,
    #Variable type
    Type = factor(
      rep("Not in Model", times = length(olsm_SelectedVar)),
      levels = c("DepVar", "Manual No Trans", "Outside No Trans","Fixed Var No Trans","Manual TOF", "Outside TOF","Fixed Var TOF","Not in Model")
    ), 
    
    #Variable transformation
    Transformation = factor(
      rep("Linear", times = length(olsm_SelectedVar)),
      levels = c("Linear", "S-Shaped","Power","Decay","S-Shaped_Decay_Capped", "S-Shaped_New")
    ),
    
    #Lag minimum
    LagMin = rep(as.integer(0), times = length(olsm_SelectedVar)),
    
    #Lag maximum
    LagMax = rep(as.integer(0), times = length(olsm_SelectedVar)),
    
    #decay steps
    DecaySteps = rep(as.integer(0), times = length(olsm_SelectedVar)),
    
    #decay minimum
    DecayMin = rep(as.numeric(1), times = length(olsm_SelectedVar)),
    
    #decay maximum
    DecayMax = rep(as.numeric(1), times = length(olsm_SelectedVar)),
    
    #alpha steps
    AlphaSteps = rep(as.integer(0), times = length(olsm_SelectedVar)),
    
    #alpha minimum
    AlphaMin = rep(as.numeric(0), times = length(olsm_SelectedVar)),
    
    #alpha maximum
    AlphaMax = rep(as.numeric(0), times = length(olsm_SelectedVar)),
    
    #alpha minimum
    BetaMin = rep(as.numeric(1), times = length(olsm_SelectedVar)),
    
    #Series Multipler
    BetaMultiplier = rep(as.integer(0), times = length(olsm_SelectedVar)),
    
    #alpha steps
    BetaSteps = rep(as.integer(0), times = length(olsm_SelectedVar)),
    
    #Series maximum
    SeriesMax = rep(as.numeric(1), times = length(olsm_SelectedVar)),
    
    # Normalization
    Normalization = factor(
      rep("None", times = length(olsm_SelectedVar)),
      levels = c("None", "Division","Subtraction")
    ),
    
    # Min Max Adjustment
    Min_Max_Adjustment = factor(
      rep("None", times = length(olsm_SelectedVar)),
      levels = c("None","Min","Max")
    ),
    
    # Fixed Coefficient
    Fixed_Coefficient = rep(as.numeric(0), times = length(olsm_SelectedVar)),
    
    # Combined Column
    Combined_Column = rep(as.integer(0), times = length(olsm_SelectedVar)),
    
    # Mixed Effect
    Random_Effect = factor(
      rep(0, times = length(olsm_SelectedVar)),
      levels = c(0,1)
    ), 
    
    stringsAsFactors = F
  )
  
  return(df)
}


##################### Function related to Transformation #######################
createOlsmTransformation <- function(olsm_RegDataModelDF, olsm_parametersDF,adStockChoice,startDate,endDate){
  #Not in the model dropping
  varsToBeDropped <- NULL
  varsToBeDropped <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "Not in Model"]
  
  if(length(varsToBeDropped)!=0){
    tempData <- olsm_RegDataModelDF[,-which(colnames(olsm_RegDataModelDF) %in% varsToBeDropped)]    
  }else {
    tempData <- olsm_RegDataModelDF
  }
  
  TransformedDf <- tempData[,which(names(tempData) %in% c("Geography","Period"))]
  
  for(i in 1:nrow(olsm_parametersDF)){
    # i = 4
    name <- olsm_parametersDF[i,"VariableName"]
    olsm_varDetails <- as.list(olsm_parametersDF[i,])
    
    # S-Shaped_New --- Currently ME will not do any transformation for S-Shaped_New. Not implemented Yet.
    if(olsm_parametersDF[i,"Type"] %in% c("DepVar","Fixed Var No Trans","Manual No Trans","Outside No Trans", "S-Shaped_New")){
      transVec <- as.data.frame(tempData[,colnames(tempData) %in% olsm_parametersDF[i,"VariableName"]])
      colnames(transVec) <- name
      TransformedDf<-cbind(TransformedDf,transVec)
    } else 
      if(olsm_parametersDF[i,"Type"] %in% c("Manual TOF","Fixed Var TOF")){
        lagMin <- as.numeric(as.character(olsm_parametersDF$LagMin[i]))
        alphaMin <- as.numeric(as.character(olsm_parametersDF$AlphaMin[i]))
        decayMin <- as.numeric(as.character(olsm_parametersDF$DecayMin[i]))
        
        if(olsm_parametersDF[i,"Transformation"] == "S-Shaped" | grepl("S-Shaped_Decay_Capped",olsm_parametersDF$Transformation[i])){
          betaMin <- as.numeric(as.character(olsm_parametersDF$BetaMin[i]))
          seriesMax <- as.numeric(as.character(olsm_parametersDF$SeriesMax[i]))
          if(seriesMax == 0){
            seriesMax = 0.01
          }
          
          dfDT <- data.table(name = tempData[, name])
          colnames(dfDT) <- name
          dfDTOrig <- data.table(name = tempData[, name])
          colnames(dfDTOrig) <- name
          df_laggedDT <- dfDT[,(name):=shift(dfDT[[name]],lagMin,fill = 0,type = "lag")]
          
          if(adStockChoice == "AdStock First"){
            df_laggedDT <- olsmCalcDecay(df_laggedDT,decayMin)
            if(max(dfDTOrig[[name]],na.rm = T) != 0){
              set(x = df_laggedDT,j = name,value = (betaMin/(10^10))^(alphaMin^((as.numeric(df_laggedDT[[name]])/(max(as.numeric(dfDTOrig[[name]]),na.rm = T)*seriesMax))*100)))
            }else {
              df_laggedDT[[name]]  <- 0
            }
          }else 
            if(adStockChoice == "AdStock Last"){
              if(max(dfDTOrig[[name]],na.rm = T) != 0){
                set(x = df_laggedDT,j = name,value = (betaMin/(10^10))^(alphaMin^((as.numeric(df_laggedDT[[name]])/(max(as.numeric(dfDTOrig[[name]]),na.rm = T)*seriesMax))*100)))
              }else {
                df_laggedDT[[name]]  <- 0
              }
              df_laggedDT <- olsmCalcDecay(df_laggedDT,decayMin)
            }
          
          # Updating S-Shaped_Decay_Capped transformation value with cap of 1 incase it is greater than 1.
          if(olsm_parametersDF$Transformation[i] == "S-Shaped_Decay_Capped"){
            df_laggedDT[df_laggedDT > 1] <- 1
          }
          TransformedDf<-cbind(TransformedDf,as.data.frame(df_laggedDT))
          
        }else 
          if (olsm_parametersDF[i,"Transformation"] == "Power"){
            
            dfDT <- data.table(name = tempData[, name])
            colnames(dfDT) <- name
            df_laggedDT <- dfDT[,(name):=shift(dfDT[[name]],lagMin,fill = 0,type = "lag")]
            if(adStockChoice == "AdStock First"){
              df_laggedDT <- olsmCalcDecay(df_laggedDT,decayMin)
              set(df_laggedDT,j = name,value=df_laggedDT[[name]]^as.numeric(alphaMin))
            }else 
              if(adStockChoice == "AdStock Last"){
                set(df_laggedDT,j = name,value=df_laggedDT[[name]]^as.numeric(alphaMin))
                df_laggedDT <- olsmCalcDecay(df_laggedDT,decayMin)
              }
            TransformedDf<-cbind(TransformedDf,as.data.frame(df_laggedDT))
            
          }else 
            if(olsm_parametersDF[i,"Transformation"] == "Linear"){
              transVec <- as.data.frame(tempData[,colnames(tempData) %in% olsm_parametersDF[i,"VariableName"]])
              colnames(transVec) <- name
              TransformedDf<-cbind(TransformedDf,transVec)
            }else 
              if(olsm_parametersDF[i,"Transformation"] == "Decay"){
                #capturing Decay data
                dfDT <- data.table(name = tempData[, name])
                colnames(dfDT) <- name
                df_laggedDT <- dfDT[,(name):=shift(dfDT[[name]],olsm_varDetails$LagMin,fill = 0,type = "lag")]
                df_laggedDT <- olsmCalcDecay(df_laggedDT,olsm_varDetails$DecayMin)
                TransformedDf<-cbind(TransformedDf,as.data.frame(df_laggedDT))
              }
      } else 
        if(olsm_parametersDF[i,"Type"] == "Outside TOF"){
          if(olsm_parametersDF[i,"Transformation"] == "Linear"){
            transVec <- as.data.frame(tempData[,colnames(tempData) %in% olsm_parametersDF[i,"VariableName"]])
            colnames(transVec) <- name
            TransformedDf<-cbind(TransformedDf,transVec)
          }else {
            TransformedDf<-cbind(TransformedDf,createOlsmTransOutsideTOF(olsm_RegDataTemp = tempData, olsm_parametersDF = olsm_parametersDF,name = name,adStockChoice = adStockChoice,olsm_varDetails = olsm_varDetails))
          }
        } 
  }
  return(TransformedDf)
}

olsmCalcDecay <- function(col,decay){
  name <- colnames(col)
  col = unlist(col,use.names = FALSE)
  for(i in 1:length(col)){
    if(i ==1){
      col[i] <- as.numeric(col[i])
    } else if(!is.na(col[i - 1])){
      col[i] <- as.numeric(col[i])+ as.numeric(col[i - 1]*(1-decay))
    }
  }
  col <- data.table(col,stringsAsFactors = FALSE)
  colnames(col)<- name
  return(col)
}

olsmgetAlpha <- function(df_lagged,df,alpha,beta,df_variable, seriesMax){
  df_laggedDT <- as.data.table(df_lagged)
  dfDT <- as.data.table(df)
  beta <- beta[complete.cases(beta)]
  if(seriesMax == 0){
    seriesMax = 0.01
  }
  for(name in names(beta)){
    #name <- names(beta)[1]
    if(max(dfDT[[df_variable]],na.rm = T) != 0){
      set(x = df_laggedDT,j = name,value = (as.numeric(unname(beta[name]))/(10^10))^((as.numeric(unname(alpha))^((as.numeric(df_laggedDT[[name]])/(max(as.numeric(dfDT[[df_variable]]),na.rm = T)*seriesMax))*100))))
      #Refresnce formula
      #(1/(10^10))^(as.numeric(unname(alpha[name]))^((df_lagged[,name]/max(df_lagged[,name]))*100))
    } else{
      df_laggedDT[[name]]  <- 0 
    }
  }
  df <- as.data.frame(df_laggedDT)
  return(df)
}

# lag for complete tranformation
olsmapplyDfLag <- function(df,lag){
  
  df_Lag <- as.data.table(df)
  for(name in names(lag)){
    
    if(!is.na(lag[name])) {
      df_Lag[,(name):=shift(df_Lag[[name]],as.numeric(unname(lag[name])),fill = 0,type = "lag")]
    }
  }
  df_lagged <- as.data.frame(df_Lag)
  return(df_lagged)
}

#power 
olsmgetPower <- function(df,powerRange){
  dfPowerDt <- as.data.table(df)
  powerSeries <- powerRange[complete.cases(powerRange)]
  for(name in names(powerSeries)) {
    set(dfPowerDt,j = name,value=dfPowerDt[[name]]^as.numeric(unname(powerSeries[name])))
  }
  df <-  as.list.data.frame(dfPowerDt)
  return(df)
}

#Decay
olsmgetDecay <- function(df,decay){
  df_DecayDT <- as.data.table(df)
  decay <- decay[complete.cases(decay)]
  calcDecay <- function(col,decay){
    for(i in 1:length(col)){
      if(i ==1){
        col[i] <- as.numeric(col[i])
      } else if(!is.na(col[i - 1])){
        col[i] <- as.numeric(col[i])+ as.numeric(col[i - 1]*(1-decay))
      }
    }
    return(col)
  }  
  
  for(name in names(decay)) {
    set(df_DecayDT,j = name,value=calcDecay(df_DecayDT[[name]],as.numeric(unname(decay[name]))))
  }
  df <-  as.data.frame(df_DecayDT)
  return(df)
}

olsmCreateLagSeries <- function(name, df, lagMin, lagMax){
  lagTrans <- as.data.frame(replicate(as.numeric(as.character(df)), n = (as.numeric(as.character(lagMax))-as.numeric(as.character(lagMin)))+1),stringsAsFactors = F)
  lagSeries <- as.numeric(as.numeric(as.character(lagMin)):as.numeric(as.character(lagMax)))
  names(lagSeries) <- paste0(name,"_L",as.numeric(as.character(lagMin)):as.numeric(as.character(lagMax)))
  names(lagTrans) <- names(lagSeries)
  return(olsmapplyDfLag(df = lagTrans,lag = lagSeries))
}

#capturing Alpha Decay data   
olsmalphaDecayTrans <- function(olsm_RegDataTemp,lagTrans,name, olsm_varDetails){  
  alphaTransformedList <- list()
  # Alpha
  for(lagName in names(lagTrans)){
    if(as.numeric(as.character(olsm_varDetails$AlphaSteps)) == 0 | as.numeric(as.character(olsm_varDetails$AlphaSteps)) == 1){
      alphaSteps <- 1
      AlphaSeries <- as.numeric(as.character(olsm_varDetails$AlphaMin))
    }else {
      alphaSteps <- (as.numeric(as.character(olsm_varDetails$AlphaMax))-as.numeric(as.character(olsm_varDetails$AlphaMin)))/(as.numeric(as.character(olsm_varDetails$AlphaSteps))-1)
      AlphaSeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$AlphaMin)),to=as.numeric(as.character(olsm_varDetails$AlphaMax)),by=alphaSteps))
    }
    
    lagTransAlpha <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])),n = length(AlphaSeries)),stringsAsFactors = F)
    names(AlphaSeries) <- paste0(lagName,"_A",seq(from=as.numeric(as.character(olsm_varDetails$AlphaMin)),to=as.numeric(as.character(olsm_varDetails$AlphaMax)),by=alphaSteps))
    names(lagTransAlpha) <- names(AlphaSeries)
    
    # Beta
    for (alphaName in names(lagTransAlpha)) {
      if(olsm_varDetails$BetaSteps==0){
        BetaSeries <- olsm_varDetails$BetaMin
      }else {
        if(olsm_varDetails$BetaMultiplier==0){
          BetaSeries <- olsm_varDetails$BetaMin  
        }else{
          BetaSeries <- rep(olsm_varDetails$BetaMin*(olsm_varDetails$BetaMultiplier^(0:(olsm_varDetails$BetaSteps-1))))
        }
      }
      lagTransAlphaBeta <- as.data.frame(replicate(as.numeric(as.character(lagTransAlpha[,alphaName])),n = length(BetaSeries)),stringsAsFactors = F)
      names(BetaSeries) <- paste0(alphaName,"_B",BetaSeries)
      names(lagTransAlphaBeta) <-  names(BetaSeries)
      df <- as.data.frame(olsm_RegDataTemp[,name])
      colnames(df) <- name
      lagTransAlphaBeta <- olsmgetAlpha(df_lagged = lagTransAlphaBeta,df = df,alpha = AlphaSeries[[alphaName]],beta = BetaSeries,df_variable = name, seriesMax = olsm_varDetails$SeriesMax)
      
      #Decay
      for(betaName in names(lagTransAlphaBeta)){
        if(as.numeric(as.character(olsm_varDetails$DecaySteps)) == 0 | as.numeric(as.character(olsm_varDetails$DecaySteps)) == 1){
          decaySteps <- 1
          decaySeries <- as.numeric(as.character(olsm_varDetails$DecayMin))
        }else {
          decaySteps <- (as.numeric(as.character(olsm_varDetails$DecayMax))-as.numeric(as.character(olsm_varDetails$DecayMin)))/(as.numeric(as.character(olsm_varDetails$DecaySteps))-1)
          decaySeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
        }
        lagTransAlphaBetaDecay <- as.data.frame(replicate(as.numeric(as.character(lagTransAlphaBeta[,betaName])),n = length(decaySeries)),stringsAsFactors = F)
        names(decaySeries) <- paste0(betaName,"_D",seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
        names(lagTransAlphaBetaDecay) <- names(decaySeries)
        lagTransAlphaBetaDecay <- olsmgetDecay(lagTransAlphaBetaDecay,decaySeries)
        alphaTransformedList <- c(alphaTransformedList,lagTransAlphaBetaDecay)
      }
    }
  }
  return(as.data.frame.list(alphaTransformedList))
}

#capturing Decay Alpha data
olsmdecayAlphaTrans <- function(olsm_RegDataTemp,lagTrans,name, olsm_varDetails){
  alphaTransformedList <- list()
  # Decay
  for(lagName in names(lagTrans)){
    if(as.numeric(as.character(olsm_varDetails$DecaySteps)) == 0 | as.numeric(as.character(olsm_varDetails$DecaySteps)) == 1){
      decaySteps <- 1
      decaySeries <- as.numeric(as.character(olsm_varDetails$DecayMin))
    }else {
      decaySteps <- (as.numeric(as.character(olsm_varDetails$DecayMax))-as.numeric(as.character(olsm_varDetails$DecayMin)))/(as.numeric(as.character(olsm_varDetails$DecaySteps))-1)
      decaySeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
    }
    
    lagTransDecay <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])), n = length(decaySeries)),stringsAsFactors = F)
    names(decaySeries) <- paste0(lagName,"_D",seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
    names(lagTransDecay) <- names(decaySeries)
    lagTransDecay <- olsmgetDecay(lagTransDecay,decaySeries)
    
    #Alpha
    for(alphaName in names(lagTransDecay)){
      if(as.numeric(as.character(olsm_varDetails$AlphaSteps)) == 0 | as.numeric(as.character(olsm_varDetails$AlphaSteps)) == 1){
        alphaSteps <- 1
        AlphaSeries <- as.numeric(as.character(olsm_varDetails$AlphaMin))
      }else {
        alphaSteps <- (as.numeric(as.character(olsm_varDetails$AlphaMax))-as.numeric(as.character(olsm_varDetails$AlphaMin)))/(as.numeric(as.character(olsm_varDetails$AlphaSteps))-1)
        AlphaSeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$AlphaMin)),to=as.numeric(as.character(olsm_varDetails$AlphaMax)),by=alphaSteps))
      }
      
      lagTransDecayAlpha <- as.data.frame(replicate(as.numeric(as.character(lagTransDecay[,alphaName])),n = length(AlphaSeries)),stringsAsFactors = F)
      names(AlphaSeries) <- paste0(alphaName,"_A",seq(from=as.numeric(as.character(olsm_varDetails$AlphaMin)),to=as.numeric(as.character(olsm_varDetails$AlphaMax)),by=alphaSteps))
      names(lagTransDecayAlpha) <- names(AlphaSeries)
      
      
      # Beta
      for (betaName in names(lagTransDecayAlpha)) {
        if(olsm_varDetails$BetaSteps==0){
          BetaSeries <- olsm_varDetails$BetaMin
        }else {
          if(olsm_varDetails$BetaMultiplier==0){
            BetaSeries <- olsm_varDetails$BetaMin  
          }else{
            BetaSeries <- rep(olsm_varDetails$BetaMin*(olsm_varDetails$BetaMultiplier^(0:(olsm_varDetails$BetaSteps-1))))
          }
        }
        lagTransDecayAlphaBeta <- as.data.frame(replicate(as.numeric(as.character(lagTransDecayAlpha[,betaName])),n = length(BetaSeries)),stringsAsFactors = F)
        names(BetaSeries) <- paste0(betaName,"_B",BetaSeries)
        names(lagTransDecayAlphaBeta) <-  names(BetaSeries)
        
        df <- as.data.frame(olsm_RegDataTemp[,name])
        colnames(df) <- name
        lagTransDecayAlphaBeta <- olsmgetAlpha(df_lagged = lagTransDecayAlphaBeta,df = df,alpha = AlphaSeries[[betaName]],beta = BetaSeries,df_variable = name, seriesMax = olsm_varDetails$SeriesMax)
        alphaTransformedList <- c(alphaTransformedList,lagTransDecayAlphaBeta)
      }
      
    }
  }
  return(as.data.frame.list(alphaTransformedList))
}

#capturing Power Decay data
olsmpowerDecayTrans <- function(olsm_RegDataTemp,lagTrans,name, olsm_varDetails){
  powerTransformedList <- list()
  # Power
  for(lagName in names(lagTrans)){
    if(as.numeric(as.character(olsm_varDetails$AlphaSteps)) == 0 | as.numeric(as.character(olsm_varDetails$AlphaSteps)) == 1){
      alphaSteps <- 1
      AlphaSeries <- as.numeric(as.character(olsm_varDetails$AlphaMin))
    }else {
      alphaSteps <- (as.numeric(as.character(olsm_varDetails$AlphaMax))-as.numeric(as.character(olsm_varDetails$AlphaMin)))/(as.numeric(as.character(olsm_varDetails$AlphaSteps))-1)
      AlphaSeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$AlphaMin)),to=as.numeric(as.character(olsm_varDetails$AlphaMax)),by=alphaSteps))
    }
    
    lagTransPower <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])),n = length(AlphaSeries)),stringsAsFactors = F)
    names(AlphaSeries) <- paste0(lagName,"_P",seq(from=as.numeric(as.character(olsm_varDetails$AlphaMin)),to=as.numeric(as.character(olsm_varDetails$AlphaMax)),by=alphaSteps))
    names(lagTransPower) <- names(AlphaSeries)
    lagTransPower <- as.data.frame.list(olsmgetPower(lagTransPower,AlphaSeries))
    
    # Decay
    for(powerName in names(lagTransPower)){
      if(as.numeric(as.character(olsm_varDetails$DecaySteps)) == 0 | as.numeric(as.character(olsm_varDetails$DecaySteps)) == 1){
        decaySteps <- 1
        decaySeries <- as.numeric(as.character(olsm_varDetails$DecayMin))
      }else {
        decaySteps <- (as.numeric(as.character(olsm_varDetails$DecayMax))-as.numeric(as.character(olsm_varDetails$DecayMin)))/(as.numeric(as.character(olsm_varDetails$DecaySteps))-1)
        decaySeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
      }
      lagTransPowerDecay <- as.data.frame(replicate(as.numeric(as.character(lagTransPower[,powerName])), n = length(decaySeries)),stringsAsFactors = F)
      names(decaySeries) <- paste0(powerName,"_D",seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
      names(lagTransPowerDecay) <- names(decaySeries)
      lagTransPowerDecay <- olsmgetDecay(lagTransPowerDecay,decaySeries)
      powerTransformedList <- c(powerTransformedList,lagTransPowerDecay)
    }
  }
  return(as.data.frame.list(powerTransformedList))
}

#capturing Decay Power data
olsmdecayPowerTrans <- function(olsm_RegDataTemp,lagTrans,name, olsm_varDetails){
  powerTransformedList <- list()
  #Decay
  for(lagName in names(lagTrans)){
    if(as.numeric(as.character(olsm_varDetails$DecaySteps)) == 0 | as.numeric(as.character(olsm_varDetails$DecaySteps)) == 1){
      decaySteps <- 1
      decaySeries <- as.numeric(as.character(olsm_varDetails$DecayMin))
    }else {
      decaySteps <- (as.numeric(as.character(olsm_varDetails$DecayMax))-as.numeric(as.character(olsm_varDetails$DecayMin)))/(as.numeric(as.character(olsm_varDetails$DecaySteps))-1)
      decaySeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
    }
    
    lagTransDecay <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])), n = length(decaySeries)),stringsAsFactors = F)
    names(decaySeries) <- paste0(lagName,"_D",seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
    names(lagTransDecay) <- names(decaySeries)
    lagTransDecay <- olsmgetDecay(lagTransDecay,decaySeries)
    #Power
    for(decayName in names(lagTransDecay)){
      if(as.numeric(as.character(olsm_varDetails$AlphaSteps)) == 0 | as.numeric(as.character(olsm_varDetails$AlphaSteps)) == 1){
        alphaSteps <- 1
        AlphaSeries <- as.numeric(as.character(olsm_varDetails$AlphaMin))
      }else {
        alphaSteps <- (as.numeric(as.character(olsm_varDetails$AlphaMax))-as.numeric(as.character(olsm_varDetails$AlphaMin)))/(as.numeric(as.character(olsm_varDetails$AlphaSteps))-1)
        AlphaSeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$AlphaMin)),to=as.numeric(as.character(olsm_varDetails$AlphaMax)),by=alphaSteps))
      }
      
      lagTransDecayPower <- as.data.frame(replicate(as.numeric(as.character(lagTransDecay[,decayName])),n = length(AlphaSeries)),stringsAsFactors = F)
      names(AlphaSeries) <- paste0(decayName,"_P",seq(from=as.numeric(as.character(olsm_varDetails$AlphaMin)),to=as.numeric(as.character(olsm_varDetails$AlphaMax)),by=alphaSteps))
      names(lagTransDecayPower) <- names(AlphaSeries)
      lagTransDecayPower <- as.data.frame.list(olsmgetPower(lagTransDecayPower,AlphaSeries))
      powerTransformedList <- c(powerTransformedList,lagTransDecayPower)
    }
  }
  return(as.data.frame.list(powerTransformedList))
}

olsmDecayTrans <- function(olsm_RegDataTemp,lagTrans,name, olsm_varDetails){
  decayTransformedList <- list()
  # Decay
  if(as.numeric(as.character(olsm_varDetails$DecaySteps)) == 0 | as.numeric(as.character(olsm_varDetails$DecaySteps)) == 1){
    decaySteps <- 1
    decaySeries <- as.numeric(as.character(olsm_varDetails$DecayMin))
  }else {
    decaySteps <- (as.numeric(as.character(olsm_varDetails$DecayMax))-as.numeric(as.character(olsm_varDetails$DecayMin)))/(as.numeric(as.character(olsm_varDetails$DecaySteps))-1)
    decaySeries <- as.numeric(seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
  }
  
  for(lagName in names(lagTrans)){
    lagTransDecay <- as.data.frame(replicate(as.numeric(as.character(lagTrans[,lagName])), n = length(decaySeries)),stringsAsFactors = F)
    names(decaySeries) <- paste0(lagName,"_D",seq(from=as.numeric(as.character(olsm_varDetails$DecayMin)),to=as.numeric(as.character(olsm_varDetails$DecayMax)),by=decaySteps))
    names(lagTransDecay) <- names(decaySeries)
    lagTransDecay <- olsmgetDecay(lagTransDecay,decaySeries)
    decayTransformedList <- c(decayTransformedList,lagTransDecay)
  }
  return(as.data.frame.list(decayTransformedList))
}

createOlsmTransOutsideTOF <- function(olsm_RegDataTemp, olsm_parametersDF, name, adStockChoice, olsm_varDetails){
  TransformedOutsideTOF_Df <- NULL
  lagTrans <- olsmCreateLagSeries(name, olsm_RegDataTemp[,name], olsm_varDetails$LagMin, olsm_varDetails$LagMax)
  
  if(as.character(olsm_varDetails$Transformation) == "S-Shaped" | as.character(olsm_varDetails$Transformation) == "S-Shaped_Decay_Capped"){
    if(adStockChoice == "AdStock First"){
      TransformedOutsideTOF_Df <- olsmdecayAlphaTrans(olsm_RegDataTemp,lagTrans,name, olsm_varDetails)
    }else if (adStockChoice == "AdStock Last"){
      TransformedOutsideTOF_Df <- olsmalphaDecayTrans(olsm_RegDataTemp,lagTrans,name, olsm_varDetails)
    }
    
    # Updating S-Shaped_Decay_Capped transformation value with cap of 1 incase it is greater than 1.
    if(as.character(olsm_varDetails$Transformation) == "S-Shaped_Decay_Capped"){
      TransformedOutsideTOF_Df[TransformedOutsideTOF_Df > 1] <- 1
    }
  }else 
    if (as.character(olsm_varDetails$Transformation) == "Power"){
      if(adStockChoice == "AdStock First"){
        TransformedOutsideTOF_Df <- olsmdecayPowerTrans(olsm_RegDataTemp,lagTrans,name, olsm_varDetails)
      }else if (adStockChoice == "AdStock Last"){
        TransformedOutsideTOF_Df <- olsmpowerDecayTrans(olsm_RegDataTemp,lagTrans,name, olsm_varDetails)
      }
    }else 
      if(as.character(olsm_varDetails$Transformation) == "Decay"){
        TransformedOutsideTOF_Df<-olsmDecayTrans(olsm_RegDataTemp,lagTrans,name, olsm_varDetails)
      }
  return(as.data.frame(TransformedOutsideTOF_Df))
}

createOlsmNormalization <- function(olsmFinalRegDf,olsm_parametersDF){
  addGeo <- FALSE
  if(any(names(olsmFinalRegDf) %in% "Geography")){
    addGeo <- TRUE
    geoName <- olsmFinalRegDf$Geography
    olsmFinalRegDf <- olsmFinalRegDf[,which(names(olsmFinalRegDf)!= "Geography")]
  }
  
  for(name in names(olsmFinalRegDf)[-1]){
    if(as.character(olsm_parametersDF$Normalization[which(grepl(gsub("_L+[0-9].*","",name), olsm_parametersDF$VariableName))]) == "Division"){
      #division
      if(mean(olsmFinalRegDf[,which(names(olsmFinalRegDf) == name)],na.rm = T)==0){
        # divide by zero case handle
        olsmFinalRegDf[name]<-olsmFinalRegDf[name]
      }else{
        olsmFinalRegDf[,which(names(olsmFinalRegDf) == name)] <- olsmFinalRegDf[,which(names(olsmFinalRegDf) == name)]/mean(olsmFinalRegDf[,which(names(olsmFinalRegDf) == name)],na.rm = T)
        
      }
    }else if(as.character(olsm_parametersDF$Normalization[which(grepl(gsub("_L+[0-9].*","",name), olsm_parametersDF$VariableName))]) == "Subtraction"){
      #Subtraction
      olsmFinalRegDf[,which(names(olsmFinalRegDf) == name)] <- olsmFinalRegDf[,which(names(olsmFinalRegDf) == name)]-mean(olsmFinalRegDf[,which(names(olsmFinalRegDf) == name)],na.rm = T)
    }else{
      # None
      olsmFinalRegDf[name]<-olsmFinalRegDf[name]
    }
  }
  
  if(addGeo == TRUE){
    olsmFinalRegDf <- cbind("Geography" = geoName, olsmFinalRegDf)
    return(olsmFinalRegDf)
  }else{
    return(olsmFinalRegDf[,-1])
  }
  
}


##################### Function related to Regression & Model Result ############

olsmGetFixedEffectDF <- function(olsmFinalRegDf, olsm_parametersDF){
  df <- olsmFinalRegDf # copying the olsmFinalRegDf for making fixed df file.
  fixedVar <- olsm_parametersDF[grepl("Fixed Var",olsm_parametersDF$Type),1]
  if(length(fixedVar)!=0){
    depVar <- olsm_parametersDF[grepl("DepVar",olsm_parametersDF$Type),1]
    # subtracting fixed df value from dependent variable.
    fixedVarCoef <- olsm_parametersDF[grepl("Fixed Var",olsm_parametersDF$Type),c("VariableName","Fixed_Coefficient")]
    for (i in 1:nrow(fixedVarCoef)) {
      df[,which(names(df) %in% fixedVarCoef$VariableName[i])]<- df[,which(names(df) %in% fixedVarCoef$VariableName[i])]* fixedVarCoef$Fixed_Coefficient[i]
    }
    
    if(length(fixedVar)==1){
      df[,which(names(df)==depVar)] <- df[,which(names(df)==depVar)]- df[,which(names(df) %in% fixedVar)]
    }else{
      df[,which(names(df)==depVar)] <- df[,which(names(df)==depVar)]- rowSums(df[,which(names(df) %in% fixedVar)])
    }
    
    # removing fixed var from olsmFinalFixedRegDf
    df <- df[,-which(names(df) %in% fixedVar)]
    return(df)
  }else {
    return(df)
  }
}

olsmCreateCombinedColumn <- function(df,combinedCol){
  combinedColumns <- combinedCol[which(combinedCol$Combined_Column != 0),]
  if(nrow(combinedColumns)!=0){
    columnsToBeDeleted <- combinedColumns$VariableName
    
    combinedColumnsList <<- split(combinedColumns$VariableName,combinedColumns$Combined_Column)
    
    combinedColumnsList <<- setNames(combinedColumnsList,paste0("Combined_",names(combinedColumnsList)))
    
    combinedColumnsdf <- do.call("cbind",lapply(combinedColumnsList,function(x){data.frame(rowSums(df[,x]))}))
    
    colnames(combinedColumnsdf) <- names(combinedColumnsList)
    
    finalCombined <- cbind(df,combinedColumnsdf)
    
    finalCombined <- finalCombined[ , !(names(finalCombined) %in% columnsToBeDeleted)]
    return(finalCombined)
  }else {
    return(df)
  }
}

olsmExtractOutsideVar <- function(fit, modelManager){
  olsm_parametersDF <- modelManager
  modelVar <- names(fit$coefficients)
  outsideVar <- olsm_parametersDF$VariableName[grep("Outside",olsm_parametersDF$Type)]
  if(any(gsub("_L+[0-9].*","", modelVar) %in% outsideVar)){
    return(as.character(modelVar[gsub("_L+[0-9].*","", modelVar) %in% outsideVar]))
  }else{
    return("NO Outside (Base Model)")
  }
}

olsmExtractOutsideTstat <- function(model, outsideVar){
  if(any(gsub("_L+[0-9].*","",names(model$coefficients)) %in% outsideVar)){
    test <- as.data.frame(t(tidy(model)[-1]))[3,]
    colnames(test) = tidy(model)[,1]
    i <- unlist(sapply(outsideVar, function(x) grep(x, colnames(test))))
    tstat <- round(test[,i],digits = 5)
  }else{
    #tstat <- "No Outside Variable"
    tstat <- 0
  }
  return(tstat)
}

#Rebuilding estimates for combined column by split
olsmSplitCombinedEstimateData <- function(parameterDetails, parametersDf){
  index <- grep("Combined",parameterDetails$term)
  df <- parameterDetails[-index,]
  temp <- NULL
  combinedColumns <- parametersDf[which(parametersDf$Combined_Column != 0),]
  combinedColumnsList <- split(combinedColumns$VariableName,combinedColumns$Combined_Column)
  combinedColumnsList <- setNames(combinedColumnsList,paste0("Combined_",names(combinedColumnsList)))
  
  for (i in 1:length(index)) {
    temp <- parameterDetails[rep(index[i], each=length(combinedColumnsList[[as.character(parameterDetails$term[index[i]])]])),]
    temp$term <- combinedColumnsList[[as.character(parameterDetails$term[index[i]])]]
    df <- rbind(df, temp)
  }
  return(df)
}


olsmGetContribution <- function(olsmFullDecomp, depVar, unrolled){
  # function for calculating contribution.
  # If olsmFullDecomp, depVar present with NULL unrolled then it will calculate for nonstacked model and appended up stacked model.
  # if unrolled is not NULL then contribution will be calculated by geography.
  if(is.null(unrolled)){
    if(length(olsmFullDecomp)==2){
      olsmFullDecomp <- olsmFullDecomp$FulldecompUnRolledDf
    }else{
      olsmFullDecomp <- olsmFullDecomp$Fulldecomposition_BaseModel
    }
  }
  fullDecompAvg <- colMeans(olsmFullDecomp[,!names(olsmFullDecomp) %in% c("Period", "Geography",depVar)])
  fullDecompAvg <- t(as.data.frame.list(fullDecompAvg/sum(fullDecompAvg)*100))
  contributionDF <- data.frame(row.names(fullDecompAvg),fullDecompAvg, row.names = NULL)
  if(is.null(unrolled)){
    names(contributionDF) <- c("term","Contribution%")
  }else{
    names(contributionDF) <- c("term",unrolled)
  }
  return(contributionDF)
}

# New elasticity calculation need to build here by remove above function. Please Don't delete below commented section.
olsmGetElasticity  <- function(model,parametersDf,hasIntercept,olsmFinalTransRegDf){
  
  transData <- olsmFinalTransRegDf
  if(any(grepl("Dummy",names(model$coefficients)))){
    transData <- cbind(transData,model$model[grep("Dummy",names(model$coefficients))])
  }
  
  VIF <- data.frame(term = names(vif(model)), VIF = vif(model),row.names = NULL)
  
  if(any(grepl("Combined",names(model$coefficients)))){
    modelParam <- merge(tidy(model),VIF,by = "term",all = T)
    parameterDetails <- olsmSplitCombinedEstimateData(modelParam, parametersDf)
  }else{
    parameterDetails <- merge(tidy(model),VIF,by = "term",all = T)
  }
  
  if(any(grepl("Fixed",parametersDf$Type))){
    fixedVar <- cbind(parametersDf[grep("Fixed",parametersDf$Type),c("VariableName","Fixed_Coefficient")],matrix(NA,length(grep("Fixed",parametersDf$Type)),length(parameterDetails)-2))
    names(fixedVar) <- names(parameterDetails)
    parameterDetails <- rbind(parameterDetails,fixedVar)
  }
  
  modelCoef <- parameterDetails$estimate
  names(modelCoef)<- parameterDetails$term
  depVar <- parametersDf$VariableName[parametersDf$Type=="DepVar"]
  transDataMean <- colMeans(transData[,names(transData) %in% c(depVar,parameterDetails$term)])
  transDataMean_12 <- colMeans(transData[transData$Period >= max(transData$Period)-365,names(transData) %in% c(depVar,parameterDetails$term)])
  
  # Denormalizing estimate when depvar is normalized by division. It is not implemented for Substraction.
  if(parametersDf$Normalization[parametersDf$Type == "DepVar"] == "Division"){
    for(name in names(modelCoef)){
      if(name == "(Intercept)"){
        modelCoef[name] <- (modelCoef[name] * transDataMean[depVar])
      }else{
        modelCoef[name] <- (modelCoef[name] * transDataMean[depVar])/transDataMean[name]
      }
    }
  }

  elasticity  <- NULL
  elasticity_12 <- NULL
  
  for(name in names(modelCoef)){
    #name = names(modelCoef)[5]
    mmVarName <- gsub("_L+[0-9].*","",name)
    if(name == "(Intercept)"){
      elasticity[name] <- NA
      elasticity_12[name] <- NA 
    }else if(grepl("Dummy",name)){
      powerValue <- 1
      elasticity[name] <- (modelCoef[name] * transDataMean[name]/transDataMean[depVar])*((1.05^powerValue)-1)*100
      elasticity_12[name] <- (modelCoef[name] * transDataMean_12[name]/transDataMean_12[depVar])*((1.05^powerValue)-1)*100
    }else if(parametersDf$Transformation[parametersDf$VariableName == mmVarName] %in% c("Linear","Decay","S-Shaped_New")){
      powerValue <- 1
      elasticity[name] <- (modelCoef[name] * transDataMean[name]/transDataMean[depVar])*((1.05^powerValue)-1)*100
      elasticity_12[name] <- (modelCoef[name] * transDataMean_12[name]/transDataMean_12[depVar])*((1.05^powerValue)-1)*100
    }else if(parametersDf$Transformation[parametersDf$VariableName == mmVarName] %in% c("Power")){
      if(parametersDf$Type[parametersDf$VariableName == mmVarName] %in% c("Manual TOF","Fixed Var TOF")){
        powerValue <- parametersDf$AlphaMin[parametersDf$VariableName == mmVarName]
      }else if(parametersDf$Type[parametersDf$VariableName == mmVarName] %in% c("Outside TOF")){
        powerValue <- as.numeric(gsub("P","",str_extract(name,"P\\d+.\\d+")))
      }
      elasticity[name] <- ((modelCoef[name] * transDataMean[name])/transDataMean[depVar])*((1.05^powerValue)-1)*100
      elasticity_12[name] <- ((modelCoef[name] * transDataMean_12[name])/transDataMean_12[depVar])*((1.05^powerValue)-1)*100
    }else if(parametersDf$Transformation[parametersDf$VariableName == mmVarName] %in% c("S-Shaped","S-Shaped_Decay_Capped")){
      elasticity[name] <- NA
      elasticity_12[name] <- NA 
    }
  }
  
  parameterDetails <- cbind(parameterDetails,Elasticity_Modelling_Period = elasticity,Elasticity_L12_Modelling_Period = elasticity_12)
  
  if(nrow(tidy(model)) != length(names(model$coefficients))){
    # here is handling multicollinearity in model result.
    multiCorVar <- names(model$coefficients)[!names(model$coefficients) %in% tidy(model)$term]
    multiCorVar <- data.frame(multiCorVar,matrix(0, nrow = length(multiCorVar), ncol = 1),matrix(NA, nrow = length(multiCorVar), ncol = length(parameterDetails)-2))
    names(multiCorVar) <- names(parameterDetails)
    parameterDetails <- rbind(parameterDetails, multiCorVar)
  }
  
  
  # reordering parameterDetails as per model manager
  varRowName <- parameterDetails$term
  varOrderMM <- parametersDf$VariableName
  if(any(varRowName %in% "(Intercept)")){
    varRowName[varRowName %in% "(Intercept)"] <- depVar
  }
  rownames(parameterDetails) <- as.character(sapply(varRowName, function(x) gsub("_L+[0-9].*","",x)))
  varOrderMM <- varOrderMM[varOrderMM %in% row.names(parameterDetails)]
  parameterDetails <- data.frame(parameterDetails[order(match(row.names(parameterDetails),varOrderMM)),],row.names = NULL)

  return(parameterDetails)
}

olsmGetModelParameter <- function(model, olsm_parametersDF,hasIntercept, modelContribution, olsmFinalTransRegDf){
  olsmResult <- data.frame(olsmGetElasticity(model,olsm_parametersDF,hasIntercept, olsmFinalTransRegDf), row.names = NULL)
  colnames(olsmResult) <- c("term", "estimate", "std.error", "statistic", "p.value","VIF", "Elasticity_Modelling_Period", "Elasticity_L12_Modelling_Period")
  
  olsmResult$term[grep("Intercept",olsmResult$term)] <- gsub("\\(|\\)","",olsmResult$term[grep("Intercept",olsmResult$term)])
  olsmResult <- merge(olsmResult, modelContribution, by = "term",sort = F)
  olsmResult <- olsmResult[,c("term","estimate","std.error","statistic","p.value","VIF","Contribution%","Elasticity_Modelling_Period","Elasticity_L12_Modelling_Period")]
  return(olsmResult)
}

# Function to extract model parameters to display on screen
olsmExtractModelParameter <- function(olsmAllModelList, olsm_parametersDF){
  # calculating number of models
  n.models <- length(olsmAllModelList)
  if(n.models == 0){
    # if there is no model, then message will display to change parameter.
    return(0)
  }else{
    # calling the function to extract the parameter of each model to rank.
    result <- lapply(olsmAllModelList, olsmExtractModelParameterValue)
    result <- as.data.frame(matrix(unlist(result), nrow=n.models, byrow=T))
    modelOutside <- unlist(lapply(olsmAllModelList, function(x, modelManager) olsmExtractOutsideVar(x,modelManager = olsm_parametersDF),modelManager=olsm_parametersDF))
    olsmModelResult <- cbind(paste0("Model_",1:nrow(result)), modelOutside, result)
    rownames(olsmModelResult) <- NULL
    colnames(olsmModelResult) <- c("Model_No", "Outside_Variable","%R2","%R2.adj","DW","RootMSE")
    olsmModelResult$`%R2` <- sapply(olsmModelResult$`%R2`, function(x) x <- round((x * 100),digits = 2))
    olsmModelResult$`%R2.adj` <- sapply(olsmModelResult$`%R2.adj`, function(x) x <- round((x * 100),digits = 2))
    return(olsmModelResult)
  }
}

# fucntion to extract model parameter to rank the model
olsmExtractModelParameterValue <- function(fit) {
  R2 <- summary(fit)$r.squared
  R2.adj <- summary(fit)$adj.r.squared
  dw <- durbinWatsonTest(fit)[[2]]
  RootMSE <- sqrt(mean(fit$residuals^2))
  out <- data.frame(R2=R2, R2.adj=R2.adj,DurbinWatson=dw, RootMSE = RootMSE)
  out <- sapply(out,function(x) if(!is.nan(x)) {x <- x}
                else{x <- 0}
  )
  return(out)
}

olsm_getActualVsPredictedDf <- function(model, olsm_parametersDF, regDf){
  data <- regDf
  olsm_actPred <- cbind.data.frame(Period =data[,"Period"], Actual = data[,olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]], Predicted = fitted(model), Residual = residuals(model))
  
  # code to add fixed effect value to predData.
  if(any(grepl("Fixed",olsm_parametersDF$Type))){
    fixedVarCoef <- olsm_parametersDF[grepl("Fixed Var",olsm_parametersDF$Type),c("VariableName","Fixed_Coefficient")]
    df <- as.data.frame(regDf[,which(names(regDf) %in% fixedVarCoef$VariableName)])
    names(df) <- fixedVarCoef$VariableName
    for (i in 1:nrow(fixedVarCoef)) {
      df[,which(names(df) %in% fixedVarCoef$VariableName[i])]<- df[,which(names(df) %in% fixedVarCoef$VariableName[i])]* fixedVarCoef$Fixed_Coefficient[i]
    }
    predDf <- cbind(olsm_actPred$Predicted,df)
    olsm_actPred$Predicted <- rowSums(predDf)
  }
  
  if(any(names(data) %in% "Geography")){
    olsm_actPred <- cbind(Geography = data$Geography,olsm_actPred)
  }
  
  return(olsm_actPred)
}

##################### Function related to Model Result Download ################

olsmExtractModelDetail <- function(model, modelResult,olsm_parametersDF, obsCount,olsmResult){
  
  output <- NULL
  output <- c(output,"The REG Procedure")
  output <- c(output,"\n\n")
  output <- c(output,paste("Model:",modelResult[1,1]))
  output <- c(output,paste("Dependant Variable:",names(model$model[1])))
  output <- c(output,"\n\n")
  output <- c(output,paste("Number of Observations Used in Model:",obsCount))
  output <- c(output,"\n\n")
  output <- c(output,noquote(capture.output(write.csv(modelResult,stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  output <- c(output,noquote(capture.output(write.csv(olsmResult,file = stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  
  return(output)
}

olsmExtractAllModelData <- function(olsmAllModelList, olsm_parametersDF,hasIntercept){
  olsmModelDataList <- lapply(olsmAllModelList, function(x) as.data.frame(tidy(x)))
  olsmModelDataDfFinal <- NULL
  parColName <- names(olsm_parametersDF)[which(!names(olsm_parametersDF) %in% c("VariableName","Type" ))]
  fixedVar <- olsm_parametersDF[which(olsm_parametersDF$Type %in% c("Fixed Var No Trans","Fixed Var TOF")),c("VariableName", "Fixed_Coefficient")]
  
  for (i in 1:length(olsmModelDataList)) {
    ModelDf <- as.data.frame(tidy(olsmAllModelList[[i]]))
    outsideVarfull <- ModelDf$term[gsub("_L+[0-9].*","",ModelDf$term) %in% olsm_parametersDF$VariableName[grep("Outside",olsm_parametersDF$Type)]]
    outsideVar <- gsub("_L+[0-9].*","",outsideVarfull[!is.na(outsideVarfull)])
    term <- list()
    
    if(length(outsideVar)==0){
      olsmModelDataDf <- cbind(rep(paste0("Model_",i), times = nrow(ModelDf)),rep("No OutsideVar", times = nrow(ModelDf)),ModelDf)
      if(any(grepl("Combined",olsmModelDataDf$term))==TRUE){
        olsmModelDataDf <- olsmSplitCombinedEstimateData(olsmModelDataDf, olsm_parametersDF)
      }
      colnames(olsmModelDataDf) <- c("Model_Number", "Outside_Variable","Model Terms", "Estimate", "Std.Error", "Statistic","p.Value")
      
      if(nrow(fixedVar)!=0){
        fixedVarDf <- data.frame(rep(olsmModelDataDf$Model_Number[1],nrow(fixedVar)), olsmModelDataDf$Outside_Variable[1],fixedVar$VariableName, fixedVar$Fixed_Coefficient, NA, NA, NA)
        colnames(fixedVarDf) = c("Model_Number","Outside_Variable","Model Terms","Estimate","Std.Error","Statistic","p.Value")
        olsmModelDataDf <- rbind(olsmModelDataDf, fixedVarDf)
      }
      orderModelTerm <- olsmModelDataDf$`Model Terms`
      linearDecayVarPos <- grep("_L+[0-9].*", orderModelTerm)
      orderTerm <- gsub("_L+[0-9].*","",orderModelTerm)
      if(hasIntercept == "Yes"){
        term[[orderModelTerm[1]]] <- olsm_parametersDF[which(olsm_parametersDF$Type == "DepVar"),] 
        term$`(Intercept)`[1] <- "(Intercept)"
        term <- append(term,sapply(orderTerm[-1], function(x) term[[x]] <- olsm_parametersDF[which(olsm_parametersDF$VariableName == x),],simplify = FALSE))
      }else {
        term <- append(term,sapply(orderTerm, function(x) term[[x]] <- olsm_parametersDF[which(olsm_parametersDF$VariableName == x),],simplify = FALSE))
      }
      
      parDf <- as.data.frame(rbindlist(term))
      parDf$VariableName[linearDecayVarPos] <- olsmModelDataDf$`Model Terms`[linearDecayVarPos]
      
      olsmModelDataDf <- merge(olsmModelDataDf, parDf, by.x = "Model Terms", by.y = "VariableName",all.x = TRUE)
      olsmModelDataDf[which(olsmModelDataDf$Type == "DepVar"),parColName] <- NA
      olsmModelDataDf[which(olsmModelDataDf$Type %in% c("Fixed Var No Trans")),parColName[-which(parColName=="Fixed_Coefficient")]] <- NA
      olsmModelDataDf[which(olsmModelDataDf$Type %in% c("Fixed Var TOF")),parColName[-which(parColName %in% c("Transformation","DecayMin","AlphaMin","BetaMin","Normalization","Fixed_Coefficient"))]] <- NA
      olsmModelDataDf <- olsmModelDataDf[,c("Model_Number","Outside_Variable","Model Terms","Estimate","Std.Error","Statistic","p.Value",names(parDf)[-1])]
      olsmModelDataDf <- olsmModelDataDf[match(orderModelTerm, olsmModelDataDf$`Model Terms`),]
      
      if(i > 1){
        olsmModelDataDfFinal <- rbind(olsmModelDataDfFinal, olsmModelDataDf)
      }else {
        olsmModelDataDfFinal <- olsmModelDataDf
      }
      
    }else{
      
      df <- cbind(rep(paste0("Model_",i), times = nrow(ModelDf)),rep(outsideVar, times = nrow(ModelDf)),ModelDf)
      if(any(grepl("Combined",df$term))==TRUE){
        df <- olsmSplitCombinedEstimateData(df, olsm_parametersDF)
      }
      colnames(df) <- c("Model_Number", "Outside_Variable","Model Terms", "Estimate", "Std.Error", "Statistic","p.Value")
      df$`Model Terms` <- gsub("_L+[0-9].*","",df$`Model Terms`)
      
      if(nrow(fixedVar)!= 0){
        fixedVarDf <- data.frame(rep(df$Model_Number[1],nrow(fixedVar)), df$Outside_Variable[1],fixedVar$VariableName, fixedVar$Fixed_Coefficient, NA, NA, NA)
        colnames(fixedVarDf) = c("Model_Number","Outside_Variable","Model Terms","Estimate","Std.Error","Statistic","p.Value")
        df <- rbind(df, fixedVarDf)
      }
      
      orderTerm <- df$`Model Terms`
      indvar <- orderTerm[!orderTerm %in% outsideVarfull]
      if(hasIntercept == "Yes"){
        term[[orderTerm[1]]] <- olsm_parametersDF[which(olsm_parametersDF$Type == "DepVar"),] 
        term$`(Intercept)`[1] <- "(Intercept)"
        indvar <- indvar[-1]
      }
      term <- append(term,sapply(indvar, function(x) term[[x]] <- olsm_parametersDF[which(olsm_parametersDF$VariableName == x),],simplify = FALSE))
      term <- append(term,sapply(outsideVar, function(x) term[[x]] <- olsm_parametersDF[which(olsm_parametersDF$VariableName == x),],simplify = FALSE))
      parDf <- as.data.frame(rbindlist(term))
      
      if(any(grepl("Dummy",names(term)))){
        dummyDf <- as.data.frame(t(sapply(names(term)[grep("Dummy",names(term))], function(x) c(x,rep(NA,length(parDf)-1)))))
        names(dummyDf) <- names(parDf)
        parDf <- rbind(parDf,dummyDf)
      }
      
      df <- merge(df, parDf, by.x = "Model Terms", by.y = "VariableName")
      df <- df[,c("Model_Number","Outside_Variable","Model Terms","Estimate","Std.Error","Statistic","p.Value",names(parDf)[-1])]
      df$`Model Terms`[df$`Model Terms` %in% outsideVar] <- outsideVarfull
      orderTerm[orderTerm == outsideVar] <- outsideVarfull
      df <- df[match(orderTerm, df$`Model Terms`),]
      df[which(df$Type == "DepVar"),parColName] <- NA
      df[which(df$Type %in% c("Fixed Var No Trans")),parColName[-which(parColName=="Fixed_Coefficient")]] <- NA
      df[which(df$Type %in% c("Fixed Var TOF")),parColName[-which(parColName %in% c("Transformation","DecayMin","AlphaMin","BetaMin","Normalization","Fixed_Coefficient"))]] <- NA
      olsmModelDataDfFinal <- rbind(olsmModelDataDfFinal, df)
    }
  }
  return(olsmModelDataDfFinal)
}

olsmExtractModelData <- function(model, olsm_parametersDF, olsmFinalTransRegDf, olsmFinalRegDf,regDf,olsmDummyModelDateScope,modelResult){

  olsm_modelData <- NULL
  modelDf <- names(model$coefficients)

  # get combined column var
  combinedIndex <- grep("Combined",modelDf)
  combinedVar <- NULL
  if(length(combinedIndex) >= 1){
    combinedColumns <- olsm_parametersDF[which(olsm_parametersDF$Combined_Column != 0),]
    combinedColumnsList <- split(combinedColumns$VariableName,combinedColumns$Combined_Column)
    combinedColumnsList <- setNames(combinedColumnsList,paste0("Combined_",names(combinedColumnsList)))
    combinedVar <- as.character(unlist(lapply(combinedIndex, function(x) combinedColumnsList[[modelDf[x]]])))
    modelDf <- modelDf[-c(combinedIndex)] # removing intercept and combined columns
  }

  # get fixed var
  fixedVar <- NULL
  fixedVar <- olsm_parametersDF[which(olsm_parametersDF$Type %in% c("Fixed Var No Trans","Fixed Var TOF")),c("VariableName")]

  # get depvar
  depVar <- as.character(olsm_parametersDF$VariableName[which(olsm_parametersDF$Type == "DepVar")])

  # remove intercept
  if(any(grepl("Intercept", modelDf))){
    modelDf <- modelDf[-grep("Intercept", modelDf)]
  }

  # get indepVar
  indepVar <- modelDf[!modelDf %in% depVar]


  if(grepl("Dummy",modelResult[,"Model_No"])){
    dummyScope <- olsmDummyModelDateScope[[modelResult[,"Model_No"]]]
    regDf <- data.frame(subset(regDf, dmy(regDf$Period) >= dummyScope$dummyStartDate & dmy(regDf$Period) <= dummyScope$dummyEndDate),row.names = NULL)
    olsmFinalTransRegDf <- data.frame(subset(olsmFinalTransRegDf, dmy(olsmFinalTransRegDf$Period) >= dummyScope$dummyStartDate & dmy(olsmFinalTransRegDf$Period) <= dummyScope$dummyEndDate),row.names = NULL)
    olsmFinalRegDf <- data.frame(subset(olsmFinalRegDf, dmy(olsmFinalRegDf$Period) >= dummyScope$dummyStartDate & dmy(olsmFinalRegDf$Period) <= dummyScope$dummyEndDate),row.names = NULL)
  }

  olsm_modelData <- cbind(olsmFinalTransRegDf[, which(colnames(olsmFinalTransRegDf)== "Period")],
                          regDf[,which(names(regDf)==depVar)],
                          olsmFinalRegDf[,which(names(olsmFinalRegDf) %in% indepVar)],
                          olsmFinalTransRegDf[,which(names(olsmFinalTransRegDf) %in% combinedVar)],
                          regDf[,which(names(regDf) %in% fixedVar)])


  if(grepl("Dummy",modelResult[,"Model_No"])){
    colnames(olsm_modelData) <- c("Period", depVar, as.character(indepVar[-grep("Dummy",indepVar)]),as.character(combinedVar),as.character(fixedVar))
    if(any(names(olsmFinalRegDf) %in% "Geography")){
      olsm_modelData <- cbind(Geography = olsmFinalRegDf$Geography, olsm_modelData)
    }

    if(any(names(olsmFinalRegDf) %in% "weight")){
      olsm_modelData <- cbind(olsm_modelData, Weight = olsmFinalRegDf[,"weight"])
    }

    dummyScope <- olsmDummyModelDateScope[[modelResult[,"Model_No"]]]
    olsm_modelData <- data.frame(subset(olsm_modelData, dmy(olsm_modelData$Period) >= dummyScope$dummyStartDate & dmy(olsm_modelData$Period) <= dummyScope$dummyEndDate),row.names = NULL)

    df <- cbind(olsm_modelData,model$model[,grep("Dummy",names(model$model))])
    names(df) <- c(names(olsm_modelData),names(model$model)[grep("Dummy",names(model$model))])
    olsm_modelData <- df

  }else{

    colnames(olsm_modelData) <- c("Period", depVar, indepVar,as.character(combinedVar),as.character(fixedVar))
    if(any(names(olsmFinalRegDf) %in% "Geography")){
      olsm_modelData <- cbind(Geography = olsmFinalRegDf$Geography, olsm_modelData)
    }
    if(any(names(olsmFinalRegDf) %in% "weight")){
      olsm_modelData <- cbind(olsm_modelData, Weight = olsmFinalRegDf[,"weight"])
    }
  }

  return(olsm_modelData)
}

olsmExtractMixedModel <- function(model, modelParam,olsmFinalNormRegDf,modelFeatureList,olsm_parametersDF,olsm_RegDataTemp){
  # Model Features
  modelFeature <- data.frame(Method = model$method,AIC = summary(model)["AIC"],BIC = summary(model)["BIC"],logLik = -2*model$logLik)
  names(modelFeature)[names(modelFeature) %in% "logLik"] <- "-2logLik"
  
  # Rolled Up Mixed Model Result
  rolledEstimate <-  data.frame(summary(model)$tTable)[c("Value","Std.Error","t.value","p.value")]
  names(rolledEstimate) <- c("Rolledup_Estimate","Rolledup_Std.Error","Rolledup_t.value","Rolledup_p.value")
  rolledEstimate <- data.frame(term = rownames(rolledEstimate), rolledEstimate, row.names = NULL,stringsAsFactors = F)
  if(any(grepl("Combined",rolledEstimate$term))){
    rolledEstimate <- data.frame(olsmSplitCombinedEstimateData(rolledEstimate, olsm_parametersDF),row.names = NULL)
  }
  if(any(grepl("(Intercept)",rolledEstimate$term))){
    rolledEstimate$term[grep("(Intercept)",rolledEstimate$term)] <- "Intercept"
  }
  ## calculating Contribution
  olsmModelData <- olsmFinalNormRegDf
  olsmModelData$Period <- lubridate::dmy(olsmModelData$Period)
  olsmFullDecomp <- olsmExtractFullDecomp(model, olsm_parametersDF, olsmModelData,modelFeatureList, olsm_RegDataTemp)
  depVar <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]
  rolledEstimate <- merge(rolledEstimate,olsmGetContribution(olsmFullDecomp, depVar, unrolled = NULL),by = "term",sort = F)
  
  # Unrolled Up Mixed Model Result
  unrolledEstimate <- data.frame(term = rownames(t(coef(model))),data.frame(t(coef(model)),row.names = NULL))
  if(any(grepl("Combined",unrolledEstimate$term))){
    unrolledEstimate <- data.frame(olsmSplitCombinedEstimateData(unrolledEstimate, olsm_parametersDF),row.names = NULL)
  }
  
  
  
  # Unrolled Contribution by Geography
  olsmFullDecompList <-  split(olsmFullDecomp$FulldecompUnRolledDf,olsmFullDecomp$FulldecompUnRolledDf$Geography)
  geoContrList <- lapply(names(olsmFullDecompList), function(x) olsmGetContribution(olsmFullDecompList[[x]], depVar,x))
  geoContrTable <- Reduce(function(x, y) merge(x, y, all=TRUE), geoContrList)
  
  # Random Effect by geography
  randomEffect <- data.frame(Term = rownames(t(random.effects(model))),data.frame(t(random.effects(model)),row.names = NULL))
  
  output <- NULL
  output <- c(output,"The Mixed Model Result")
  output <- c(output,"\n\n")
  output <- c(output,paste("Dependant Variable:",modelParam$Depvar))
  output <- c(output,"\n\n")
  output <- c(output,paste("Model Statistics:"))
  output <- c(output,noquote(capture.output(write.csv(modelFeature,stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  output <- c(output,paste("Number of Observations Read:",modelParam$TotalDataCount))
  output <- c(output,paste("Number of Observations Used:",nrow(model$data)))
  output <- c(output,"\n\n")
  output <- c(output,paste("Class Level Information:"))
  output <- c(output,noquote(capture.output(write.csv(data.frame(Class = "Geography",Levels = length(modelParam$SelectedGeos), Values = paste(modelParam$SelectedGeos,collapse = "-")),stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  output <- c(output,paste("Rolled-up Estimate with Contribution:"))
  output <- c(output,noquote(capture.output(write.csv(rolledEstimate,stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  output <- c(output,paste("Unrolled Estimate by Geography:"))
  output <- c(output,noquote(capture.output(write.csv(unrolledEstimate,stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  output <- c(output,paste("Unrolled Contribution% by Geography:"))
  output <- c(output,noquote(capture.output(write.csv(geoContrTable,stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  output <- c(output,paste("Random Effect for Each Variable within Geography:"))
  output <- c(output,noquote(capture.output(write.csv(randomEffect,stdout(),row.names = F,quote = F))))
  output <- c(output,"\n\n")
  
  return(output)
}

olsmExtractFullDecomp <- function(model, olsm_parametersDF, olsmModelData,modelFeatureList, olsm_RegDataTemp){
  
  olsmDenormbyDep <- function(olsmFulldecompDf, depAvg, ModelDf, modelFeatureList, olsm_parametersDF, denormType){
    # This function will call if Depvar is Normalized.
    # And IndepVar may or may not be normalized.
    # IF IndepVar is Normalized so denormalized indepVar with its estimate and depAvg, 
    # otherwise denormalized indepVar with its estimate only.
    # DepVar will denormalized by depAvg.
    # IF Intercept is present then it will get denormalized by depAvg.
    for (j in 1:length(olsmFulldecompDf)) {
      if(any(ModelDf$term == names(olsmFulldecompDf)[j])){
        if(denormType == "Division"){
          olsmFulldecompDf[,j] <- olsmFulldecompDf[,j]* ModelDf[which(ModelDf$term == names(olsmFulldecompDf)[j]),2]* depAvg
        }else if(denormType == "Subtraction"){
          olsmFulldecompDf[,j] <- olsmFulldecompDf[,j]* (ModelDf[which(ModelDf$term == names(olsmFulldecompDf)[j]),2]+ depAvg)
        }else {
          # indepVar is not normalized.
          olsmFulldecompDf[,j] <- olsmFulldecompDf[,j]* ModelDf[which(ModelDf$term == names(olsmFulldecompDf)[j]),2]
        }
      }else if(names(olsmFulldecompDf)[j] == modelFeatureList$depVar){
        if(denormType == "Division"){
          olsmFulldecompDf[,j] <- olsmFulldecompDf[,j]  * depAvg
        }else if(denormType == "Subtraction"){
          olsmFulldecompDf[,j] <- olsmFulldecompDf[,j]  + depAvg
        }
      }
    }
    
    if(modelFeatureList$hasIntercept == "Yes"){
      if(denormType == "Division"){
        intercept <- ModelDf[grep("Intercept",ModelDf$term),2] * depAvg
      }else if(denormType == "Subtraction"){
        intercept <- ModelDf[grep("Intercept",ModelDf$term),2] + depAvg
      }
      olsmFulldecompDf <- cbind(olsmFulldecompDf, intercept)
      names(olsmFulldecompDf)[length(names(olsmFulldecompDf))] <- "Intercept"
    }
    
    return(olsmFulldecompDf)
  }
  olsmDenormWithoutDep<- function(olsmFulldecompDf, ModelDf, modelFeatureList, olsm_parametersDF){
    # This function will call if Depvar is not Normalized.
    # And IndepVar may or may not be normalized, and It will get denormalized with its estimate only.
    # IF Intercept is present then it will just add to data.
    
    for (j in 1:length(olsmFulldecompDf)) {
      if(any(ModelDf$term == names(olsmFulldecompDf)[j])){
        olsmFulldecompDf[,j] <- olsmFulldecompDf[,j]* ModelDf[which(ModelDf$term == names(olsmFulldecompDf)[j]),2]
      }
    }
    if(modelFeatureList$hasIntercept == "Yes"){
      olsmFulldecompDf <- cbind(olsmFulldecompDf, ModelDf[grep("Intercept",ModelDf$term),2])
      names(olsmFulldecompDf)[length(names(olsmFulldecompDf))] <- "Intercept"
    }
    return(olsmFulldecompDf)
  }
  olsmMinMaxAdjust <- function(df, modelFeatureList){
    tmp <- NULL
    min_max_var <- modelFeatureList$min_max_var
    if(nrow(min_max_var)!= 0){
      for(i in 1:nrow(min_max_var)){
        if(any(grepl(min_max_var$VariableName[i],names(df)))){
          varTmp <- df[,which(grepl(min_max_var$VariableName[i],names(df)))]
          if(min_max_var$Min_Max_Adjustment[i] == "Min"){
            df[,which(grepl(min_max_var$VariableName[i],names(df)))] <- varTmp-min(varTmp)
            tmp <- min(varTmp)
          }else if(min_max_var$Min_Max_Adjustment[i] == "Max"){
            df[,which(grepl(min_max_var$VariableName[i],names(df)))] <- varTmp-max(varTmp)
            tmp <- max(varTmp)
          }
          
          if(modelFeatureList$hasIntercept == "Yes"){
            df[,which(grepl("Intercept",names(df)))] <- df[,which(grepl("Intercept",names(df)))] + tmp
          }
        }
      }
    }
    
    return(df)
  }
  olsmExtractFullDecompRolledUp <- function(olsmFulldecompUnRolledDf){
    olsmFulldecompRolledDf <- olsmFulldecompUnRolledDf
    olsmFulldecompRolledDf$Geography <- NULL
    if(!is.Date(olsmFulldecompRolledDf$Period)){
      olsmFulldecompRolledDf$Period <- lubridate::dmy(olsmFulldecompRolledDf$Period)
    }
    olsmFulldecompRolledDf <- aggregate(olsmFulldecompRolledDf[,-1],by = list(olsmFulldecompRolledDf$Period),sum)
    names(olsmFulldecompRolledDf)[1] <- "Period"
    return(olsmFulldecompRolledDf)
  }
  
  if(modelFeatureList$mixedModelChioce == "Yes"){
    ModelDf <- data.frame(term = rownames(t(coef(model))), t(coef(model)), row.names = NULL)
    names(ModelDf)[-1] <- modelFeatureList$selectedGeos
  }else {
    ModelDf <- tidy(model)
  }
  olsmFullDecomp <- NULL
  
  if(any(grepl("Combined",ModelDf$term))){
    combinedDf <- cbind(rep(paste0("Model_",1), times = nrow(ModelDf)),rep("No OutsideVar", times = nrow(ModelDf)),ModelDf)
    combinedDf <- olsmSplitCombinedEstimateData(combinedDf, olsm_parametersDF)
    if(modelFeatureList$mixedModelChioce == "Yes"){
      ModelDf <- data.frame(combinedDf[,-c(1,2)],row.names = NULL)
      names(ModelDf)[-1] <- modelFeatureList$selectedGeos
    }else {
      ModelDf <- data.frame(combinedDf[,which(names(combinedDf) %in% c("term","estimate"))],row.names = NULL)
    }
  }
  
  if(any(grepl("Fixed",olsm_parametersDF$Type))){
    fixedDf <- data.frame(olsm_parametersDF[grep("Fixed",olsm_parametersDF$Type),names(olsm_parametersDF) %in% c("VariableName","Fixed_Coefficient")],row.names = NULL)
    names(fixedDf) <- c("term","estimate")
    if(modelFeatureList$mixedModelChioce == "Yes"){
      fixedDf <- cbind(fixedDf, matrix(rep(fixedDf$estimate,each= length(ModelDf)-2), ncol=length(ModelDf)-2, byrow=TRUE))
      names(fixedDf) <- names(ModelDf)
      ModelDf <- rbind(ModelDf,fixedDf)
    }else {
      fixedVarDf <- data.frame(fixedDf$term, fixedDf$estimate, matrix(NA, nrow = nrow(fixedDf), ncol = ncol(ModelDf)-ncol(fixedDf)))
      names(fixedVarDf) <- names(ModelDf)
      ModelDf <- rbind(ModelDf,fixedVarDf)
    }
  }
  
  modelFeatureList[["depVar"]] <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]
  indepVar <- as.character(ModelDf$term[!ModelDf$term %in% modelFeatureList$depVar])
  df <- olsm_RegDataTemp[which(olsm_RegDataTemp$Period %in% modelFeatureList$modellingPeriod),]
  modelFeatureList[["min_max_var"]] <- olsm_parametersDF[which(olsm_parametersDF$Min_Max_Adjustment != "None"),c("VariableName","Min_Max_Adjustment")]
  
  if(modelFeatureList$stackedModel == TRUE){
    geoDepMean <- df[df$Geography %in% modelFeatureList$selectedGeos,names(df) %in% c("Geography", modelFeatureList$depVar)]
    geoDepMean <- aggregate(geoDepMean[,which(names(geoDepMean) %in% modelFeatureList$depVar)], by = list(geoDepMean$Geography), FUN=mean)
    names(geoDepMean) <- c("Geography", "DepMean")
    olsmFulldecompDf <- olsmModelData[,names(olsmModelData)%in% c("Geography", "Period", modelFeatureList$depVar,indepVar)]
    olsmFulldecompList <- split(olsmFulldecompDf, olsmFulldecompDf$Geography)  
    olsmFulldecompList <- lapply(olsmFulldecompList, function(x){if(nrow(x) == 0){return(NULL)}else {return(x)}})
    olsmFulldecompList <- olsmFulldecompList[!sapply(olsmFulldecompList,is.null)]
    
    if(olsm_parametersDF$Normalization[olsm_parametersDF$Type == "DepVar"] != "None"){
      
      denormType <- as.character(olsm_parametersDF$Normalization[olsm_parametersDF$Type == "DepVar"])
      
      if(modelFeatureList$mixedModelChioce == "No"){
        
        olsmFulldecompList <- lapply(names(olsmFulldecompList), function(x) olsmDenormbyDep(olsmFulldecompList[[x]], geoDepMean[geoDepMean$Geography == x,"DepMean"], ModelDf, modelFeatureList, olsm_parametersDF, denormType))
        
      }else if(modelFeatureList$mixedModelChioce == "Yes"){
        olsmFulldecompList <- lapply(names(olsmFulldecompList), function(x){
          olsmDenormbyDep(olsmFulldecompDf = olsmFulldecompList[[x]], depAvg = geoDepMean[geoDepMean$Geography == x,"DepMean"], ModelDf[,names(ModelDf) %in% c("term", x)], modelFeatureList, olsm_parametersDF, denormType)
        } )
      }
    }else 
      if(olsm_parametersDF$Normalization[olsm_parametersDF$Type == "DepVar"] == "None"){
        if(modelFeatureList$mixedModelChioce == "No"){
          olsmFulldecompList <- lapply(names(olsmFulldecompList), function(x) olsmDenormWithoutDep(olsmFulldecompList[[x]], ModelDf, modelFeatureList, olsm_parametersDF))
        }else if(modelFeatureList$mixedModelChioce == "Yes"){
          olsmFulldecompList <- lapply(names(olsmFulldecompList), function(x) olsmDenormWithoutDep(olsmFulldecompList[[x]], ModelDf[,names(ModelDf) %in% c("term", x)], modelFeatureList, olsm_parametersDF))
          
        }
      }
    
    olsmFullDecomp[["FulldecompUnRolledDf"]] <- as.data.frame(rbindlist(lapply(olsmFulldecompList, function(x) olsmMinMaxAdjust(x, modelFeatureList))))
    olsmFullDecomp[["FulldecompRolledDf"]] <- olsmExtractFullDecompRolledUp(olsmFullDecomp[["FulldecompUnRolledDf"]])
    
  }else 
    if(modelFeatureList$stackedModel == FALSE){
      
      olsmFulldecompDf <- olsmModelData[,names(olsmModelData)%in% c("Period",modelFeatureList$depVar,indepVar)]
      if(olsm_parametersDF$Normalization[olsm_parametersDF$Type == "DepVar"] != "None"){
        depAvg <- mean(df[,which(names(df) == modelFeatureList$depVar)])
        denormType <- as.character(olsm_parametersDF$Normalization[olsm_parametersDF$Type == "DepVar"])
        olsmFullDecomp[["Fulldecomposition_BaseModel"]] <- olsmDenormbyDep(olsmFulldecompDf, depAvg, ModelDf, modelFeatureList, olsm_parametersDF, denormType)
        
      }else 
        if(olsm_parametersDF$Normalization[olsm_parametersDF$Type == "DepVar"] == "None"){
        olsmFullDecomp[["Fulldecomposition_BaseModel"]] <- olsmDenormWithoutDep(olsmFulldecompDf, ModelDf, modelFeatureList, olsm_parametersDF)
        }
      
      olsmFullDecomp[["Fulldecomposition_BaseModel"]] <- olsmMinMaxAdjust(df = olsmFullDecomp$Fulldecomposition_BaseModel, modelFeatureList)
  }
  return(olsmFullDecomp)
}

# Function to denorm actual vs Predcited of OLSM Model.
olsmDenormActvsPred <- function(modelParam, actPredData, olsm_parametersDF, olsm_RegDataTemp ){
  actPredDenormList <- list()
  if(modelParam$stackedModel == TRUE){
    actPredDataRolled <- actPredData
    actPredDataRolled$Geography <- NULL
    if(class(actPredDataRolled$Period) != "Date"){
      actPredDataRolled$Period <- lubridate::dmy(actPredDataRolled$Period)
    }
    actPredDataRolled <- aggregate(actPredDataRolled[,-1],by = list(actPredDataRolled$Period),sum)
    names(actPredDataRolled)[1] <- "Period"
    actPredDenormList[["actPredDataUnRolled"]] <- actPredData
    actPredDenormList[["actPredDataRolled"]] <- actPredDataRolled
    
    if(olsm_parametersDF$Normalization[which(olsm_parametersDF$Type == "DepVar")] != "None"){
      
      depVar <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]
      df <- olsm_RegDataTemp[which(olsm_RegDataTemp$Period %in% modelParam$modellingPeriod),]
      geoDepMean <- df[df$Geography %in% modelParam$selectedGeos,names(df) %in% c("Geography", depVar)]
      geoDepMean <- aggregate(geoDepMean[,which(names(geoDepMean) %in% depVar)], by = list(geoDepMean$Geography), FUN=mean)
      names(geoDepMean) <- c("Geography", "DepMean")
      actPredList <- split(actPredData, actPredData$Geography)  
      
      actPredList <- lapply(actPredList, function(x){if(nrow(x) == 0){return(NULL)}else {return(x)}})
      actPredList <- actPredList[!sapply(actPredList,is.null)]
      
      for(i in 1:length(actPredList)){
        depAvg <- geoDepMean$DepMean[geoDepMean$Geography == names(actPredList)[i]]
        if(olsm_parametersDF$Normalization[which(olsm_parametersDF$Type == "DepVar")] == "Division"){
          actPredList[[i]][,-c(1,2)] <- actPredList[[i]][,-c(1,2)]  *  depAvg
        }else if(olsm_parametersDF$Normalization[which(olsm_parametersDF$Type == "DepVar")] == "Subtraction"){
          if(modelParam$MixedModelChoice == "No"){
            actPredList[[i]][,-c(1,2,5)] <- actPredList[[i]][,-c(1,2,5)]  + depAvg
          }else if(modelParam$MixedModelChoice == "Yes"){
            actPredList[[i]][,!names(actPredList[[i]]) %in% c("Geography","Period", names(actPredList[[i]])[grep("Residuals",names(actPredList[[i]]))])] <- actPredList[[i]][,!names(actPredList[[i]]) %in% c("Geography","Period", names(actPredList[[i]])[grep("Residuals",names(actPredList[[i]]))])]  + depAvg
          }
        }  
      }
      
      actPredDataUnRolled <- as.data.frame(rbindlist(actPredList))
      actPredDataRolled <- actPredDataUnRolled
      actPredDataRolled$Geography <- NULL
      if(class(actPredDataRolled$Period) != "Date"){
        actPredDataRolled$Period <- lubridate::dmy(actPredDataRolled$Period)
      }
      actPredDataRolled <- aggregate(actPredDataRolled[,-1],by = list(actPredDataRolled$Period),sum)
      names(actPredDataRolled)[1] <- "Period"
      actPredDenormList[["actPredDataUnRolled"]] <- actPredDataUnRolled
      actPredDenormList[["actPredDataRolled"]] <- actPredDataRolled
    }
    
  }else if(modelParam$stackedModel == FALSE){
    
    actPredDenormList[["actPredData"]] <- actPredData
    if(olsm_parametersDF$Normalization[which(olsm_parametersDF$Type == "DepVar")] != "None"){
      
      depVar <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]
      
      depAvg <- mean(olsm_RegDataTemp[which(olsm_RegDataTemp$Period %in% modelParam$modellingPeriod),depVar])
      
      if(olsm_parametersDF$Normalization[which(olsm_parametersDF$Type == "DepVar")] == "Division"){
        actPredData[,-1] <- apply(actPredData[,-1],2,function(x) return(x * depAvg))
      }else if(olsm_parametersDF$Normalization[which(olsm_parametersDF$Type == "DepVar")] == "Subtraction"){
        actPredData[,-c(1,4)] <- apply(actPredData[,-c(1,4)],2,function(x) return(x + depAvg))
      }
      actPredDenormList[["actPredData"]] <- actPredData
    }
  }
  
  return(actPredDenormList)
}


##################### OLSM Modelling Functions ##########################
# Non Stacked OLS model
olsmGetNonStackedOLSModel <- function(olsm_RegDataTemp,olsm_parametersDF,modelFeatureList){
  # write.csv(olsmFinalTransRegDftmp,file = "C:/Users/ashutosh.agrahari/Downloads/transData_ADFirst.csv",row.names = F)
  olsmFinalTransRegDftmp <- NULL
  olsm_RegDataModelDF <- olsm_RegDataTemp[,which(colnames(olsm_RegDataTemp) %in% c("Period",olsm_parametersDF$VariableName))]
  # Calling a function for transformation
  olsmFinalTransRegDftmp <- createOlsmTransformation(olsm_RegDataModelDF, olsm_parametersDF,modelFeatureList$adStockChoice,modelFeatureList$startDate,modelFeatureList$endDate)
  names(olsmFinalTransRegDftmp)[1] <- "Period"
  olsmFinalTransRegDftmp <- data.frame(olsmFinalTransRegDftmp,stringsAsFactors = FALSE)
  
  olsmFinalTransRegDftmp <- olsmFinalTransRegDftmp[as.character(olsmFinalTransRegDftmp$Period) %in% modelFeatureList$modellingPeriod,]
  olsmFinalTransRegDf <- olsmFinalTransRegDftmp
  
  # Calling a function for normalization
  olsmFinalNormRegDf <- createOlsmNormalization(olsmFinalTransRegDf,olsm_parametersDF)
  olsmFinalRegDf <- olsmGetFixedEffectDF(olsmFinalNormRegDf, olsm_parametersDF)
  olsmFinalRegDf <- olsmCreateCombinedColumn(olsmFinalRegDf, olsm_parametersDF[,c("VariableName","Combined_Column")])
  formulaList <-  olsmBuildFormula(olsmFinalRegDf, olsm_parametersDF, modelFeatureList$hasIntercept)
  
  modelParamList <- c("OLS", list(formulaList))
  names(modelParamList) <- c("type", "formulaList")
  olsmAllModelList <- olsmAllPossibleRegressions(modelParamList,olsmFinalRegDf)
  olsmModelResult <- olsmExtractModelParameter(olsmAllModelList, olsm_parametersDF)
  olsmResult <- NULL
  olsmResult[["olsmFinalTransRegDf"]] <- olsmFinalTransRegDf
  olsmResult[["olsmFinalNormRegDf"]] <- olsmFinalNormRegDf
  olsmResult[["olsmFinalRegDf"]] <- olsmFinalRegDf
  olsmResult[["formulaList"]] <- formulaList
  olsmResult[["olsmAllModelList"]] <- olsmAllModelList
  olsmResult[["olsmModelResult"]] <- olsmModelResult
  
  return(olsmResult)
  
}

# Stacked OLS model (OLS + WLS)
olsmGetStackedOLSModel <- function(olsm_RegDataTemp,olsm_parametersDF,olsm_SplitByGeoList,modelFeatureList,type){
  olsm_splitByGeoSubset <- olsm_SplitByGeoList[which(names(olsm_SplitByGeoList) %in% modelFeatureList$selectedGeos)]
  splitDfList <- olsm_splitByGeoSubset
  olsmFinalTransRegList <- NULL
  splitDf <- NULL
  for (name in names(splitDfList)) {
    #name <- names(splitDfList)[2]
    df <- splitDfList[[name]]
    df <- createOlsmTransformation(olsm_RegDataModelDF = df, olsm_parametersDF,adStockChoice =modelFeatureList$adStockChoice,  startDate = modelFeatureList$startDate,endDate = modelFeatureList$endDate)
    df <- data.frame(df,stringsAsFactors = FALSE)
    df <- df[as.character(df$Period) %in% modelFeatureList$modellingPeriod,]
    olsmFinalTransRegList[[name]] <- df
    df <-  createOlsmNormalization(df,olsm_parametersDF)
    splitDf[[name]]<- df
  }
  
  olsmFinalTransRegDf <- as.data.frame(rbindlist(olsmFinalTransRegList))
  olsmFinalNormRegDf <- as.data.frame(rbindlist(splitDf))
  olsmFinalRegDf <- olsmGetFixedEffectDF(olsmFinalNormRegDf, olsm_parametersDF)
  olsmFinalRegDf <- olsmCreateCombinedColumn(olsmFinalRegDf, olsm_parametersDF[,c("VariableName","Combined_Column")])
  
  modelParamList <- list()
  
  if(type == "OLS"){
    # OLS stacked modelling 
    formulaList <-  olsmBuildFormula(olsmFinalRegDf, olsm_parametersDF,modelFeatureList$hasIntercept, mixed = FALSE)
    modelParamList <- c("OLS", list(formulaList))
    names(modelParamList) <- c("type", "formulaList")
    
    olsmAllModelList <- olsmAllPossibleRegressions(modelParamList,olsmFinalRegDf)
    olsmModelResult <- olsmExtractModelParameter(olsmAllModelList, olsm_parametersDF)
  }else if(type == "WLS"){
    depVar <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]
    regDF <- olsm_RegDataTemp[as.character(olsm_RegDataTemp$Period) %in% modelFeatureList$modellingPeriod,]
    geoMean <- aggregate(regDF[, which(names(regDF) %in% depVar)], list(regDF$Geography), mean)
    olsmFinalRegDf <- merge(olsmFinalRegDf, geoMean, by.x = "Geography", by.y = "Group.1")
    colnames(olsmFinalRegDf)[names(olsmFinalRegDf)%in% "x"] <- "weight"
    formulaList <-  olsmBuildFormula(olsmFinalRegDf, olsm_parametersDF,modelFeatureList$hasIntercept, mixed = FALSE)
    
    modelParamList <- c("WLS", list(formulaList))
    names(modelParamList) <- c("type", "formulaList")
    olsmAllModelList <- olsmAllPossibleRegressions(modelParamList,olsmFinalRegDf)
    olsmModelResult <- olsmExtractModelParameter(olsmAllModelList, olsm_parametersDF)
  }else if(type == "Mixed"){
    # Mixed Modelling
    formulaList <-  olsmBuildFormula(olsmFinalRegDf, olsm_parametersDF,modelFeatureList$hasIntercept, mixed = TRUE)
    # Formula Building for Mixed Model
    randomVar <- paste0(olsm_parametersDF$VariableName[olsm_parametersDF$VariableName != olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"] & olsm_parametersDF$Random_Effect == 1],collapse = "+")

    if(modelFeatureList$hasIntercept=="No"){
      randomVar <- paste0("0 + ",randomVar)
    }else if(modelFeatureList$hasIntercept=="Yes"){
      randomVar <- paste0("1 + ",randomVar)
    }
    
    modelParamList <- c("Mixed", formulaList, randomVar)
    names(modelParamList) <- c("type", "formulaList", "randomVar")
    
    if(modelFeatureList$wLSChoice == "No"){
      modelParamList[["weight"]] <- FALSE
      olsmAllModelList <- olsmAllPossibleRegressions(modelParamList,olsmFinalRegDf)
    }else if(modelFeatureList$wLSChoice == "Yes"){
      modelParamList[["weight"]] <- TRUE
      modelParamList[["depVar"]] <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]
      geoMean <- aggregate(olsmFinalTransRegDf[, which(names(olsmFinalTransRegDf) %in% modelParamList$depVar)], list(olsmFinalTransRegDf$Geography), mean)
      olsmFinalRegDf <- merge(olsmFinalRegDf, geoMean, by.x = "Geography", by.y = "Group.1")
      colnames(olsmFinalRegDf)[names(olsmFinalRegDf)%in% "x"] <- "Geoweight"
      
      olsmAllModelList <- olsmAllPossibleRegressions(modelParamList,olsmFinalRegDf)
    } 
    
  }
  olsmResult <- NULL
  olsmResult[["olsmFinalTransRegDf"]] <- olsmFinalTransRegDf
  olsmResult[["olsmFinalNormRegDf"]] <- olsmFinalNormRegDf
  olsmResult[["olsmFinalRegDf"]] <- olsmFinalRegDf
  olsmResult[["olsmAllModelList"]] <- olsmAllModelList
  olsmResult[["formulaList"]] <- formulaList
  if(type != "Mixed"){
    olsmResult[["olsmModelResult"]] <- olsmModelResult
  }
  
  return(olsmResult)
}


olsmBuildFormula <- function(olsmFinalRegDf, olsm_parametersDF, hasIntercept, mixed){
  varName <- names(olsmFinalRegDf)
  
  olsm_varTypeDf <- olsm_parametersDF[,c("VariableName","Type","Combined_Column")]
  uniqueCombValue <- plyr::count(as.factor(olsm_varTypeDf$Combined_Column))
  combinedColumns <- olsm_varTypeDf[-c(which(olsm_varTypeDf$Combined_Column==uniqueCombValue[which(uniqueCombValue$freq <= 1),1]),which(olsm_varTypeDf$Combined_Column==uniqueCombValue[which(uniqueCombValue$x == 0),1])),]
  
  formulaList <- list()
  depVar <- as.character(olsm_parametersDF$VariableName[which(olsm_parametersDF$Type == "DepVar")])
  baseFormula <- paste0(depVar," ~ ")
  
  # generating formula without intercept.
  if(hasIntercept=="No"){
    baseFormula <- paste0(depVar," ~ ","0 +")
  }
  
  IndepVariable <- olsm_varTypeDf$VariableName[which(olsm_varTypeDf$Type %in% c("Manual No Trans","Manual TOF"))]
  
  if(nrow(combinedColumns)!=0){
    IndepVariable <- IndepVariable[-which(IndepVariable %in% combinedColumns$VariableName)]
    IndepVariable <- c(IndepVariable, names(combinedColumnsList))
  }
  
  firstFormula <- paste0(paste0(baseFormula,paste(IndepVariable[-length(IndepVariable)],"+",collapse = " " ),collapse = " ")," ",IndepVariable[length(IndepVariable)])
  formulaList[[1]]<- firstFormula
  baseFormula <- paste0(baseFormula,paste(IndepVariable,"+",collapse = " " ),collapse = " ")
  
  linearDecayList <- NULL
  
  outsideLinear <- olsm_varTypeDf$VariableName[which(olsm_varTypeDf$Type == "Outside No Trans")]
  outsideTOF <- olsm_varTypeDf$VariableName[which(olsm_varTypeDf$Type == "Outside TOF")]
  
  if(any(grepl("Outside",olsm_parametersDF$Type))){
    if(length(outsideLinear)>0){
      for (i in 1:length(outsideLinear)) {
        formulaCount <- length(formulaList)
        formulaList[[formulaCount+1]] <- paste0(baseFormula," ",outsideLinear[i])
      }
    }
    
    if(length(outsideTOF)>0){
      for (i in 1:length(outsideTOF)) {
        outsideTOFVar <- outsideTOF[i]
        varTOF <- varName[grep(outsideTOFVar,varName)]
        for (j in 1:length(varTOF)) {
          formulaCount <- length(formulaList)
          formulaList[[formulaCount+1]] <- paste0(baseFormula," ",varTOF[j])
        }
      }
    }
  }
  
  return(formulaList)
}

# Generate Dummy Model
olsmGetDummyModelResult <- function(olsmAllModelList, olsmModelResult,olsmModelScopeDummyTable, finalDf, olsm_parametersDF,dummyModelProp){
  
  if(grepl("Dummy",olsmModelResult$Model_No[as.numeric(dummyModelProp$olsm.model.index)])){
    baseModel <- as.numeric(gsub("Model_|_Dummy_+[0-9]*","",olsmModelResult$Model_No[as.numeric(dummyModelProp$olsm.model.index)]))
    model <- olsmAllModelList[[baseModel]]
  }else {
    model <- olsmAllModelList[[as.numeric(dummyModelProp$olsm.model.index)]]
  }
  
  # Dummy model Data
  if(dummyModelProp$stackedModel == FALSE){
    dummyModelData <- data.frame(lubridate::dmy(finalDf$Period),model$model)
    names(dummyModelData) <- c("Period",names(model$model))
  }else if(dummyModelProp$stackedModel == TRUE){
    dummyModelData <- data.frame(finalDf$Geography,lubridate::dmy(finalDf$Period),model$model)
    if(any(grepl("weights",names(dummyModelData)))){
      names(dummyModelData) <- c("Geography","Period",names(model$model)[-length(model$model)],"weight")
    }else{
      names(dummyModelData) <- c("Geography","Period",names(model$model))
    }
  }
  
  dummyModelData  <- subset(dummyModelData, Period >= min(olsmModelScopeDummyTable$Period) & Period <= max(olsmModelScopeDummyTable$Period))
  
  dummyDFTable <- as.data.frame(olsmModelScopeDummyTable[,which(names(olsmModelScopeDummyTable) %in% names(which(apply(olsmModelScopeDummyTable[,!names(olsmModelScopeDummyTable) %in% c("Geography","Period")],2,sum)!=0)))])
  names(dummyDFTable) <- names(which(apply(olsmModelScopeDummyTable[,!names(olsmModelScopeDummyTable) %in% c("Geography","Period")],2,sum)!=0))
  
  if(dummyModelProp$stackedModel == FALSE){
    dummyModelData <- cbind(dummyModelData,dummyDFTable)[,-1]
  }else if(dummyModelProp$stackedModel == TRUE){
    dummyModelData <- cbind(dummyModelData,dummyDFTable)
    dummyModelData <- dummyModelData[,!names(dummyModelData) %in% c("Geography","Period")]
  }
  
  depVar <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]
  if(dummyModelProp$WLSChoice == "No"){
    indepVar <- names(dummyModelData)[!names(dummyModelData) %in% depVar]  
  }else if(dummyModelProp$WLSChoice == "Yes"){
    indepVar <- names(dummyModelData)[!names(dummyModelData) %in% c(depVar,"weight")]
  }
  
  if(dummyModelProp$hasIntercept == "No"){
    baseFormula <- paste0(depVar," ~ ", paste0(c(indepVar,0),collapse = "+"))
  }else if(dummyModelProp$hasIntercept == "Yes"){
    baseFormula <- paste0(depVar," ~ ", paste0(indepVar,collapse = "+"))
  }
  
  dummyFormula <- list()
  dummyFormula[[1]] <- baseFormula
  modelParamList <- list()
  
  if(dummyModelProp$WLSChoice == "No"){
    # Stacked OLS Model
    modelParamList <- c("OLS", as.list(dummyFormula))
    names(modelParamList) <- c("type", "formulaList")
    modelDummy <- olsmAllPossibleRegressions(modelParamList,olsmFinalRegDf = dummyModelData)
  }else if(dummyModelProp$WLSChoice == "Yes"){
    # Stacked WLS Model
    modelParamList <- c("WLS", dummyFormula)
    names(modelParamList) <- c("type", "formulaList")
    modelDummy <- olsmAllPossibleRegressions(modelParamList,dummyModelData)
  }
  
  return(modelDummy)
}

olsmAllPossibleRegressions <- function(modelParamList,olsmFinalRegDf){
  
  # write.csv(reshape2::melt(olsmFinalRegDf, id=c("Geography","Period")),file = "C:/Users/ashutosh.agrahari/Downloads/WLS_Model_Data.csv",row.names = F)
  if(modelParamList$type == "OLS"){
    olsmFinalRegDf <- as.data.frame(lapply(olsmFinalRegDf, function(x) as.numeric(as.character(x))))
    olsmModelsResults <- lapply(modelParamList$formulaList,function(x, data) lm(x, data=olsmFinalRegDf,na.action = na.exclude),data=olsmFinalRegDf)
  }else 
    if(modelParamList$type == "WLS"){
    if(any(names(olsmFinalRegDf) %in% c("Geography","Period"))){
      modelScopeDfFinal <- olsmFinalRegDf[,-which(names(olsmFinalRegDf) %in% c("Geography","Period"))]
    }else{
      modelScopeDfFinal <- olsmFinalRegDf
    }
    modelScopeDfFinal <- as.data.frame(lapply(modelScopeDfFinal, function(x) as.numeric(as.character(x))))
    olsmModelsResults <- lapply(modelParamList$formulaList,function(x, data) lm(x, data=modelScopeDfFinal, weights = weight,na.action = na.exclude),data=modelScopeDfFinal)
    
  }else 
    if(modelParamList$type == "Mixed"){
    
    # function to generate Mixed model and remove sign flipage iteratively.
    randModelFunction <- function(fixedFormula,randFormula,nlmeData, modelParamList){
      
      if(modelParamList$weight == FALSE){
        
        mixedeffectmodel <- lme(fixed = as.formula(fixedFormula),random = list(Geography = pdDiag(randFormula)),data = nlmeData, method = "REML", correlation = NULL,weights = NULL,contrasts = NULL,
                                control = lmeControl(maxIter = 50000,msMaxIter = 50000,tolerance = 1e-6,niterEM = 25,msMaxEval = 200,msTol = 1e-10,msVerbose = F,returnObject = TRUE,gradHess = TRUE,apVar = TRUE,minAbsParApVar = 0.05,opt = "nlminb",optimMethod = "BFGS"))
        
      }else if(modelParamList$weight == TRUE){
        #varFixed(value = eval(parse(text = paste0("~ 1/Geoweight"))))
        mixedeffectmodel <- lme(fixed = as.formula(fixedFormula),random = list(Geography = pdDiag(randFormula)),data = nlmeData, method = "REML", correlation = NULL,weights = varFixed(value = ~ 1/Geoweight),contrasts = NULL,
                                control = lmeControl(maxIter = 50000,msMaxIter = 50000,tolerance = 1e-6,niterEM = 25,msMaxEval = 200,msTol = 1e-10,msVerbose = F,returnObject = TRUE,gradHess = TRUE,apVar = TRUE,minAbsParApVar = 0.05,opt = "nlminb",optimMethod = "BFGS"))
      }
      
      estimateDf <- as.data.frame.list(coef(mixedeffectmodel))
      fixedDf <- fixef(mixedeffectmodel)
      if(any(grepl("Intercept", names(estimateDf)))){
        names(estimateDf)[1] <- "Intercept"
        names(fixedDf)[1] <- "Intercept"
      }
      estimateDfRatio <- NULL
      for(name in names(fixedDf)){
        estimateDfRatio <- estimateDf[,name]* fixedDf[name]
        estimateDf[,name] <- estimateDfRatio
      }
      
      getcolnameList <- as.list(NULL)
      getcolnameList <- apply(estimateDf,2,function(x){
        if(!any(x < 0)){
          return(NULL)
        }else{
          as.vector(which(x < 0))
        }
      })
      
      if(any(grepl("Intercept",names(getcolnameList)))){
        getcolnameList[[grep("Intercept",names(getcolnameList))]] <- NULL
      }
      
      if(any(unlist(lapply(getcolnameList,FUN = function(x)  length(x))))){
        print("Flipped")
        splitData_geography <- split(nlmeData,nlmeData[,"Geography"])
        for(name in names(getcolnameList)){
          if(length(getcolnameList[[name]]) != 0){
            for(i in 1:length(getcolnameList[[name]])){
              splitData_geography[[getcolnameList[[name]][i]]][,name] <- 0
            }
          }
        }
        nlmeData <- as.data.frame(rbindlist(splitData_geography,fill = T))
        randModelFunction(fixedFormula,randFormula,nlmeData, modelParamList)
      }else{
        return(mixedeffectmodel)
      }
    }
    
    modelScopeDfFinal <- olsmFinalRegDf[,-which(names(olsmFinalRegDf) %in% c("Period"))]
    modelScopeDfFinal[,-which(names(modelScopeDfFinal) %in% c("Geography"))] <- as.data.frame(lapply(modelScopeDfFinal[,-which(names(modelScopeDfFinal) %in% c("Geography"))], function(x) as.numeric(as.character(x))))
    
    # getting random part of lme formula.
    randFormula <- eval(parse(text = as.character(paste0("~ ",modelParamList$randomVar))))
    
    # checking sign flipage in Mixed Model.
    olsmModelsResults <- lapply(modelParamList$formulaList, function(x,randFormula, data, modelParamList) randModelFunction(fixedFormula = x,randFormula = randFormula,nlmeData = data, modelParamList = modelParamList), randFormula = randFormula, data = modelScopeDfFinal, modelParamList = modelParamList)
  }
  return(olsmModelsResults)
}


