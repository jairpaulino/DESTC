# Creates a matrix based on the sliding window technique (w=lag) 
# and classifies the trend of each instance based on mk.test().
getSlidingWindows_wTrendAnalysis = function(timeSeries_ts, w, alpha = 0.1
                              , nStepAhead, running_mean = 1){

  len = length(timeSeries_ts)
  trend_df = data.frame(matrix(ncol = (w+5), nrow = (len-w)))
  names_trend = NULL
  for(i in 1:w){names_trend[i] = paste("x", i, sep="")}
  names(trend_df)[w:1] = names_trend #rev(names_trend)
  names(trend_df)[(w+1):(w+5)] = c("A", "B", "mk_pvalue", "S", "Class")
  
  for (i in 1:(len-w)){#i=1
    mkTest = mk.test(getRunningMean(timeSeries_ts[i:(i+w-1)], n=running_mean))
    pvalue = mkTest$p.value
    trend_df$mk_pvalue[i] = pvalue
    S = mkTest$estimates[1]
    trend_df$S[i] = S
    
    trend_df[i,1:w] = timeSeries_ts[i:(i+w-1)]
    trend_df$nStepAhead[i] = timeSeries_ts[i+w-1+nStepAhead]
    
    lm = lm(timeSeries_ts[i:(i+w-1)] ~ seq(1, w))
    # lm = lm(log(timeSeries_ts[i:(i+w-1)]) ~ seq(1, w))
    
    trend_df$A[i] = lm$coefficients[2]
    trend_df$B[i] = lm$coefficients[1]
    
    if(pvalue > alpha | is.na(pvalue)){
      trend_df$Class[i] = "None"
    }else{
      if(S >= 0){
        trend_df$Class[i] = "Positive"
      }else{
        trend_df$Class[i] = "Negative"
      }
    }
  }#View(trend_df)
  trend_df = trend_df %>% drop_na(nStepAhead)
  return(trend_df) 
}

# Calculates the running mean of a time series and returns a vector
getRunningMean = function(timeSeries, n){
  #timeSeries = normTest; n = 1
  runningMean = NULL
  for(i in (n+1):(length(timeSeries)+1)){
    runningMean[i] = mean(timeSeries[(i-n):(i-1)])
  }
  return(as.numeric(na.omit(runningMean)))
}
