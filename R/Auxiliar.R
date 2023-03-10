#devtools::document
#cat("Version: 1.0.0\n", file = "DESCRIPTION", append = TRUE)

#' getSlidingWindows_wTrendAnalysis
#' 
#' Creates a matrix based on the sliding window technique (w=lag) 
#' and classifies the trend of each instance based on mk.test().
#'
#' @param time_series a 'ts' or 'numeric' object.
#' @param w Int (default = 10). The sliding window lag size. 
#' @param alpha Float (default = 0.1). The significance level (alpha) of MK trend test.
#' @param nStepAhead Int (default = 1). Number o steps ahead .
#' @param running_mean Int (default = 1). If running_mean>1, a running mean function is applied. 
#'
#' @return A data frame with sliding windows and trend analysis values.
#'  A and B are Ax+B values from a linear regression, mk_pvalue is the MK trend test
#'  p-value, S is the MK statists, and Class is the trend classification.
#' @export
#' @examples
#' ts = AirPassengers
#' getSlidingWindows_wTrendAnalysis(ts)
#' 
getSlidingWindows_wTrendAnalysis = function(time_series, w = 10, alpha = 0.1
                              , nStepAhead = 1, running_mean = 1){

  len = length(time_series)
  trend_df = data.frame(matrix(ncol = (w+5), nrow = (len-w)))
  names_trend = NULL
  for(i in 1:w){names_trend[i] = paste("x", i, sep="")}
  names(trend_df)[w:1] = names_trend #rev(names_trend)
  names(trend_df)[(w+1):(w+5)] = c("A", "B", "mk_pvalue", "S", "Class")
  
  for (i in 1:(len-w)){#i=1
    mkTest = mk.test(getRunningMean(time_series[i:(i+w-1)], n=running_mean))
    pvalue = mkTest$p.value
    trend_df$mk_pvalue[i] = pvalue
    S = mkTest$estimates[1]
    trend_df$S[i] = S
    
    trend_df[i,1:w] = time_series[i:(i+w-1)]
    trend_df$nStepAhead[i] = time_series[i+w-1+nStepAhead]
    
    lm = lm(time_series[i:(i+w-1)] ~ seq(1, w))
    # lm = lm(log(time_series[i:(i+w-1)]) ~ seq(1, w))
    
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

#' getRunningMean
#' 
#' Calculates the running mean of a time series
#'
#' @param time_series a 'ts' or 'numeric' object.
#' @param n Int (default = 1). If $running_mean>1$, a running mean function is applied.
#'
#' @return returns a running mean numeric vector.
#' @export
#'
#' @examples
#' ts = AirPassengers
#' plot.ts(getRunningMean(ts, 30))
getRunningMean = function(time_series, n){
  #time_series = normTest; n = 1
  runningMean = NULL
  for(i in (n+1):(length(time_series)+1)){
    runningMean[i] = mean(time_series[(i-n):(i-1)])
  }
  return(as.numeric(na.omit(runningMean)))
}
