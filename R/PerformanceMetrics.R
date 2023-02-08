# Calculates a set of metrics (RMSE, MAE, and Theil)
calculateMetrics = function(results_df){ 
  metrics = c('RMSE', 'MAE', "Theil")
  metrics_df = data.frame(matrix(nrow = (length(results_df)-1), ncol = length(metrics)))
  rownames(metrics_df) = names(results_df)[2:(length(results_df))]
  colnames(metrics_df) = metrics
  
  for (i in 2:length(results_df)){
    metrics_df[i-1, c("RMSE")] = getRMSE(results_df$Target, results_df[[i]])
    metrics_df[i-1, c("MAE")] = getMAE(results_df$Target, results_df[[i]])
    metrics_df[i-1, c("Theil")] = getTheil(results_df$Target, results_df[[i]])
  }
  return(metrics_df)  
}

# Mean absolute error
getMAE = function(target,forecast){ # target = a; forecast = b
  values = na.omit(data.frame(target = target, forecast = forecast))
  MAE = sum(abs(values$target - values$forecast))/length(values$target)
  return(MAE)
}

# Mean squared error
getMSE = function(target,forecast){
  SE = getSE(target,forecast)
  MSE=mean(SE, na.rm=TRUE)
  MSE
}

# Theil's U2 Statistic
getTheil = function(target,forecast){
  seriesSize = length(target)
  squaredSumTF = 0
  squaredSumTT = 0
  i=2
  while(i<=seriesSize){
    squaredSumTF = squaredSumTF + (target[i]-forecast[i])^2
    squaredSumTT = squaredSumTT + (target[i]-target[i-1])^2
    #valor.theil[i]=((target[i]-forecast[i])^2)/(sum((target[i]-target[i+1])^2))
    i=i+1
  }
  Theil = squaredSumTF/squaredSumTT
  Theil
}

# WRONG Prediction on Change of Direction
getWPOCID = function(target,forecast){
  Dt = 0
  i=1
  seriesSize = length(target)
  while(i<seriesSize){
    TT = (target[i+1]-target[i])
    FF = (forecast[i+1]-forecast[i])
    #if((target[i+1]-target[i])*(forecast[i+1]-forecast[i])>0){
    if(TT*FF>0 | (TT==0 & FF==0)){
      Dt= Dt + 1 
    }
    i=i+1
    #print(i)
  }
  WPOCID=1-(Dt/(seriesSize-1))
  WPOCID
}

# Root Mean Squared Error (RMSE)
getRMSE = function(target,forecast){
  MSE = getMSE(target,forecast)
  RMSE = sqrt(MSE)
  return(RMSE)
}

# R2
getRegressionMetrics = function(target,forecast){
  linearModel = lm(target~forecast)
  coefficients = linearModel$coefficients
  anova = summary(linearModel)
  R2 = anova$r.squared
  ret = list()
  ret$Intercept = coefficients[1]
  ret$Slope = coefficients[2]
  ret$WR2 = 1-R2
  ret$R2 = R2
  return(ret)
}

getSE = function(target,forecast){
  SE=(target-forecast)^2
  return(SE)
}

