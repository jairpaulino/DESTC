#' getSlidingWindows_wTrendAnalysis
#'
#' Creates a matrix based on the sliding window technique \(w=lag\)
#' and classifies the trend of each instance based on mk.test().
#'
#' @param time_series a 'ts' or 'numeric' object.
#' @param w Int (default = 10). The sliding window lag size.
#' @param alpha Float (default = 0.1). The significance level (alpha) of MK trend test.
#' @param nStepAhead Int (default = 1). Number o steps ahead.
#' @param running_mean Int (default = 1). If running_mean>1, a running mean function is applied.
#'
#' @return A data frame with sliding windows and trend analysis values.
#'  A and B are Ax+B values from a linear regression, mk_pvalue is the MK trend test
#'  p-value, S is the MK statists, and Class is the trend classification.
#' @export
#' @importFrom trend mk.test
#' @import tidyverse
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
  names(trend_df)[w:1] = names_trend
  names(trend_df)[(w+1):(w+5)] = c("A", "B", "mk_pvalue", "S", "Class")

  for (i in 1:(len-w)){
    mkTest = mk.test(getRunningMean(time_series[i:(i+w-1)], n=running_mean))
    pvalue = mkTest$p.value
    trend_df$mk_pvalue[i] = pvalue
    S = mkTest$estimates[1]
    trend_df$S[i] = S

    trend_df[i,1:w] = time_series[i:(i+w-1)]
    trend_df$nStepAhead[i] = time_series[i+w-1+nStepAhead]

    lm = lm(time_series[i:(i+w-1)] ~ seq(1, w))

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
  }
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
  runningMean = NULL
  for(i in (n+1):(length(time_series)+1)){
    runningMean[i] = mean(time_series[(i-n):(i-1)])
  }
  return(as.numeric(na.omit(runningMean)))
}

#' getBestInstanceModel
#'
#' Identifies the best model for each of the instances
#'
#' @param df A data frame whose first column contains "Target" values
#' and other columns are models predictions.
#' @return a vector with the best model to each instance
#' @export
#'
#' @examples
#' test_df = data.frame(matrix(ncol=4, nrow=5))
#' colnames(test_df) = c("Target", "model_1", "model_2", "model_3")
#' test_df$Target = c(0.24, 0.28, 0.25, 0.23, 0.28)
#' test_df$model_1 = c(0.25, 0.35, 0.35, 0.23, 0.21)
#' test_df$model_2 = c(0.27, 0.27, 0.33, 0.35, 0.29)
#' test_df$model_3 = c(0.29, 0.37, 0.22, 0.37, 0.37)
#' getBestInstanceModel(test_df)
getBestInstanceModel = function(df){
  best_model = c();
  for(row in 1:length(df$Target)){
    min_error = 1e10
    for(i in 2:ncol(df)){
      error = abs(df[row,]$Target - df[row,i])
      if(error < min_error){
        min_error = error
        col = i
      };
    };
    best_model = rbind(best_model, colnames(df)[col])
  }
  return(best_model)
}

#' getOrderedListOfBestModelsByTrend
#'
#' Get list with the best models to each trend
#'
#' @param df A data frame whose first column contains "Target" values,
#' a 'Model' column which indicates the best model to each instance,
#' and a 'Class' column which indicate the trend classification
#' to each instance.
#'
#' @param tbl_len_perc float (default = 1). a value that indicates the
#' percentage of data to be used to count the best models by trend class.
#'
#' @return A list that contains three ranks with the best model to
#' each class of trend.
#' @export
#'
#' @examples
#' test_df = data.frame(matrix(ncol=4, nrow=10))
#' colnames(test_df) = c("Target", "model_1", "model_2", "model_3")
#' test_df$Target = c(0.24, 0.28, 0.25, 0.23, 0.28, 0.24, 0.28, 0.25, 0.23, 0.28)
#' test_df$model_1 = c(0.25, 0.35, 0.35, 0.23, 0.21, 0.25, 0.35, 0.35, 0.23, 0.21)
#' test_df$model_2 = c(0.27, 0.27, 0.33, 0.35, 0.29, 0.27, 0.27, 0.33, 0.35, 0.29)
#' test_df$model_3 = c(0.29, 0.37, 0.22, 0.37, 0.37, 0.29, 0.37, 0.22, 0.37, 0.37)
#' test_df$Model = best_models; lag = 5
#' sliding_windows_df = getSlidingWindows_wTrendAnalysis(test_df$Target
#'                                                    , w = lag, running_mean = 1
#'                                                    , nStepAhead = 1, alpha = 0.5)
#' test_df$Class[(lag+1):length(test_df$Target)] = sliding_windows_df$Class
#' test_df = na.omit(test_df)
#' getOrderedListOfBestModelsByTrend(test_df, tbl_len_perc = 1)
getOrderedListOfBestModelsByTrend = function(df, tbl_len_perc = 1){
  none_df = df %>% filter(Class == "None")
  nrows_ = round(nrow(none_df)*tbl_len_perc)
  none_df = none_df[((nrow(none_df)-nrows_):nrow(none_df)),]
  none_order = none_df %>%
    group_by(Model) %>%
    summarise(no_rows = length(Model)) %>%
    arrange(desc(no_rows))

  positive_df = df %>% filter(Class == "Positive")
  nrows_ = round(nrow(positive_df)*tbl_len_perc)
  positive_df = positive_df[((nrow(positive_df)-nrows_):nrow(positive_df)),]
  positive_order = positive_df %>%
    group_by(Model) %>%
    summarise(no_rows = length(Model)) %>%
    arrange(desc(no_rows))

  negative_df = df %>% filter(Class == "Negative")
  nrows_ = round(nrow(negative_df)*tbl_len_perc)
  negative_df = negative_df[((nrow(negative_df)-nrows_):nrow(negative_df)),]
  negative_order = negative_df %>%
    group_by(Model) %>%
    summarise(no_rows = length(Model)) %>%
    arrange(desc(no_rows))

  rtr = list()
  rtr$none_order = none_order
  rtr$positive_order = positive_order
  rtr$negative_order = negative_order
  return(rtr)
}

#' ensemblingModels
#'
#' Combines the k predictions based on the specified method (mean, or median)
#'
#' @param ensemble_method Character (default = "median"). A ensemble method
#' ("median", "mean").
#' @param forecasts A data frame with values to be combined (by rows).
#'
#' @return Combined values.
#' @export
#' @import matrixStats
#'
#' @examples
#' test_df = data.frame(matrix(ncol=3, nrow=5))
#' colnames(test_df) = c("model_1", "model_2", "model_3")
#' test_df$model_1 = c(0.25, 0.35, 0.35, 0.23, 0.21)
#' test_df$model_2 = c(0.27, 0.27, 0.33, 0.35, 0.29)
#' test_df$model_3 = c(0.29, 0.37, 0.22, 0.37, 0.37)
#' ensemblingModels(ensemble_method = "median", forecasts = test_df)
ensemblingModels = function(ensemble_method = "median", forecasts){
  ensemble = switch(ensemble_method
                    , "median" = rowMedians(as.matrix(forecasts))
                    , "mean" = rowMeans(as.matrix(forecasts)))
  return(ensemble)
}

#' DESTC model forecast
#'
#' Generates forecasts based on DESTC model
#'
#' @param train_df Data frame. A pool of single models to be used as training set.
#' @param test_df Data frame. A pool of single models to be used as test set.
#' @param w Int. The sliding windows lag size.
#' @param alpha Float. The significance level (alpha) of MK trend test.
#' @param running_mean Int (default = 1). If running_mean>1, a running mean function is applied.
#' @param ensemble_method Character. A ensemble method ("median", "mean").
#' @param none_models_number Int. The number of models to be combined to "none trend" instances.
#' @param pos_models_number Int. The number of models to be combined to "positive trend" instances.
#' @param neg_models_number Int. The number of models to be combined to "negative trend" instances.
#'
#' @return A numeric vector of DESTC forecasts.
#' @export
#'
#' @examples
#' train_pool_df = read.csv(paste0("data/", country, "/pool_complete_valid.csv"))
#' test_pool_df =  read.csv(paste0("data/", country, "/pool_complete_test.csv"))
#' test = getDESTC(train_df = train_pool_df, test_df = test_pool_df
#'                 , w = 10, alpha = 0.1
#'                 , ensemble_method = "median", running_mean = 1
#'                 , none_models_number = params$none_number
#'                 , pos_models_number = params$pos_number
#'                 , neg_models_number = params$neg_number)
getDESTC = function(train_df, test_df, w, alpha, running_mean=1
                    , ensemble_method, none_models_number
                    , pos_models_number, neg_models_number){

  train_df$Model = as.vector(getBestInstanceModel(train_df))
  trend_train_df = getSlidingWindows_wTrendAnalysis(train_df$Target, w = w
                                                    , running_mean = running_mean
                                                    , nStepAhead = 1, alpha = alpha)

  train_df$Class[(w+1):length(train_df$Target)] = trend_train_df$Class
  train_df = na.omit(train_df)

  model_order = getOrderedListOfBestModelsByTrend(train_df)
  none_order = model_order$none_order;
  positive_order = model_order$positive_order
  negative_order = model_order$negative_order

  trend_test_df = getSlidingWindows_wTrendAnalysis(test_df$Target, w = w
                                                   , running_mean = running_mean
                                                   , nStepAhead = 1, alpha = alpha)
  test_df$Class[(w+1):length(test_df$Target)] = trend_test_df$Class
  test_df = na.omit(test_df)

  DESTC_test = c();
  for(row in 1:length(test_df$Target)){
    if(test_df$Class[row] == "None"){
      DESTC_test[row] = ensemblingModels(ensemble_method = ensemble_method
                                         , test_df[row, none_order$Model[1:min(nrow(none_order), none_models_number)]])
    }else{
      if(test_df$Class[row] == "Positive"){
        DESTC_test[row] = ensemblingModels(ensemble_method = ensemble_method
                                           , test_df[row, positive_order$Model[1:min(nrow(positive_order), pos_models_number)]])
      }else{
        DESTC_test[row] = ensemblingModels(ensemble_method = ensemble_method
                                           , test_df[row, negative_order$Model[1:min(nrow(negative_order), neg_models_number)]])
      }
    }
  }

  DESTC = c(rep(NA, w), DESTC_test)
  return(DESTC)
}

#' calculateMetrics
#'
#' Calculates a set of metrics (RMSE, MAE, and Theil)
#'
#' @param results_df A data frame whose first column contains "Target" values
#' and other columns are models predictions.
#'
#' @return A data frame with RMSE, MAE and Theil metrics
#' @export
#'
#' @examples
#' test_df = data.frame(matrix(ncol=3, nrow=5))
#' colnames(test_df) = c("Target", "Model_1", "Model_2")
#' test_df$Target = c(2.7, 2.5, 3.1, 5.6, 5.7)
#' test_df$Model_1 = c(1.7, 3.5, 2.1, 2.6, 5.1)
#' test_df$Model_2 = c(3.7, 5.1, 2.4, 2.2, 4.2)
#' calculateMetrics(test_df)
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

#' getMAE
#'
#' Mean absolute error
#'
#' @param target Array of targets (observed values).
#' @param forecast Array of forecasts (forecasts values).
#'
#' @return Mean absolute error
#' @export
#'
#' @examples
#' target = c(2.7, 2.5, 3.1, 5.6, 5.7)
#' forecast = c(1.7, 3.5, 2.1, 2.6, 5.1)
#' getMAE(target,forecast)
getMAE = function(target, forecast){ # target = a; forecast = b
  values = na.omit(data.frame(target = target, forecast = forecast))
  MAE = sum(abs(values$target - values$forecast))/length(values$target)
  return(MAE)
}

#' getMSE
#'
#' Mean squared error
#'
#' @param target Array of targets (observed values).
#' @param forecast Array of forecasts (forecasts values).
#'
#' @return Mean squared error
#' @export
#'
#' @examples
#' target = c(2.7, 2.5, 3.1, 5.6, 5.7)
#' forecast = c(1.7, 3.5, 2.1, 2.6, 5.1)
#' getMSE(target,forecast)
getMSE = function(target, forecast){
  SE = getSE(target,forecast)
  MSE=mean(SE, na.rm=TRUE)
  MSE
}

#' getTheil
#'
#' Theil's U2 Statistic
#'
#' @param target Array of targets (observed values).
#' @param forecast Array of forecasts (forecasts values).
#'
#' @return Theil's U2 Statistic
#' @export
#'
#' @examples
#' target = c(2.7, 2.5, 3.1, 5.6, 5.7)
#' forecast = c(1.7, 3.5, 2.1, 2.6, 5.1)
#' getTheil(target,forecast)
getTheil = function(target, forecast){
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

#' getWPOCID
#'
#' WRONG Prediction on Change of Direction (WPOCID)
#'
#' @param target Array of targets (observed values).
#' @param forecast Array of forecasts (forecasts values).
#'
#' @return WPOCID metric
#' @export
#'
#' @examples
#' target = c(2.7, 2.5, 3.1, 5.6, 5.7)
#' forecast = c(1.7, 3.5, 2.1, 2.6, 5.1)
#' getWPOCID(target,forecast)
getWPOCID = function(target, forecast){
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

#' getRMSE
#'
#' Root Mean Squared Error (RMSE)
#'
#' @param target Array of targets (observed values).
#' @param forecast Array of forecasts (forecasts values).
#'
#' @return RMSE
#' @export
#'
#' @examples
#' target = c(2.7, 2.5, 3.1, 5.6, 5.7)
#' forecast = c(1.7, 3.5, 2.1, 2.6, 5.1)
#' getRMSE(target,forecast)
getRMSE = function(target, forecast){
  MSE = getMSE(target,forecast)
  RMSE = sqrt(MSE)
  return(RMSE)
}

#' getRegressionMetrics
#'
#' Regression metrics
#'
#' @param target Array of targets (observed values).
#' @param forecast Array of forecasts (forecasts values).
#'
#' @return Regression metrics
#' @export
#'
#' @examples
#' target = c(2.7, 2.5, 3.1, 5.6, 5.7)
#' forecast = c(1.7, 3.5, 2.1, 2.6, 5.1)
#' getRegressionMetrics(target,forecast)
getRegressionMetrics = function(target, forecast){
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

#' getSE
#'
#' Get squared error
#'
#' @param target Array of targets (observed values).
#' @param forecast Array of forecasts (forecasts values).
#'
#' @return A vector with squared errors
#' @export
#'
#' @examples
#' target = c(2.7, 2.5, 3.1, 5.6, 5.7)
#' forecast = c(1.7, 3.5, 2.1, 2.6, 5.1)
#' getSE(target, forecast)
getSE = function(target, forecast){
  SE=(target-forecast)^2
  return(SE)
}


#' createPlot
#'
#' Generates a graphic with Target and the pool models
#'
#' @param df  A data frame whose first column contains "Target" values
#' and other columns are models predictions.
#' @param legend_place Character (default = 'topright'). Legend place.
#' @param leg_names Character. Legend names.
#'
#' @return A plot
#' @export
#'
#' @examples
#' test_df = data.frame(matrix(ncol=20, nrow=100))
#' colnames(test_df) = c("Target")
#' test_df$Target = rexp(100, rate = 0.2)
#' test_df$model_1 = rexp(100, rate = 0.1)
#' for(model in 2:20){
#'   test_df[,model] = rexp(100)
#'  }
#'   createPlot(test_df)
createPlot = function(df, legend_place = "topright"
                      , leg_names = c('Target', 'Pool')){
  x_label = "Test set index"
  y_label = "Observations"
  plot(df$Target, type="l", xlab=x_label, ylab=y_label)
  for(model in 2:ncol(df)){#model=12
    lines(df[model], col='gray')
  }
  lines(df['Target'], lwd=2, lty=3)
  legend("topright", leg_names,
         col = c(1, 'gray', 'Red'),
         lwd=2, lty=c(3, 1, 1), inset = 0.01,
        box.col = NA)
}

run = function(countries){
  for(country in countries){
    valid_pool_df = read.csv(paste0("data/", country, "/pool_complete_valid.csv"))
    test_pool_df =  read.csv(paste0("data/", country, "/pool_complete_test.csv"))
    params = read.csv(paste0("data/", country, "/destc_params.csv"), row.names = "X")

    results_df = data.frame(test_pool_df)
    proc_time_begin = proc.time()[3]
    results_df$DESTC = getDESTC(train_df = valid_pool_df
                                , test_df = test_pool_df
                                , w = params$w
                                , alpha = params$alpha
                                , ensemble_method = params$ensemble_method
                                , none_models_number = params$none_number
                                , pos_models_number = params$pos_number
                                , neg_models_number = params$neg_number
                                , running_mean = 1)
    proc_time_end = proc.time()[3] - proc_time_begin

    results_df = na.omit(results_df)
    metrics_test = calculateMetrics(results_df)
    if(!dir.exists(paste0("results/", country))){
      dir.create(paste0("results/", country))
    }

    write.csv(results_df, paste0("results/", country, "/", country, "_forecasring_results.csv"))
    write.csv(metrics_test, paste0("results/", country, "/", country, "_metrics.csv"))
    write.csv(proc_time_end, paste0("results/", country, "/", country, "_proc_time.txt")
              , row.names = F)
  }
}
