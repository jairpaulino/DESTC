# Identifies the best model for each of the instances
getBestInstanceModel = function(df){
  best_model = c();
  for(row in 1:length(df$Target)){
    min_error = 1e10
    for(i in 2:ncol(df)){
      #print(i)
      #cat(df[row,]$Target, " -- ", df[row,i], '\n')
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

# Get list with the best models to each trend
getOrderedListOfBestModelsByTrend = function(df, tbl_len_perc = 1){
  #df = train_df
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

# Combines the k predictions based on the specified method (mean, or median)
ensemblingModels = function(ensemble_method = "median", predict){
  ensemble = switch(ensemble_method
                    , "median" = rowMedians(as.matrix(predict))
                    , "mean" = rowMeans(as.matrix(predict)))
  return(ensemble)
}

# DESTC model
getDESTC = function(train_df, test_df, w, alpha, ensemble_method
                    , none_models_number, pos_models_number, neg_models_number
                    , running_mean){
  # training  
  # Identifies the best model for each instances (regarding training pool)
  train_df$Model = as.vector(getBestInstanceModel(train_df))
  # Creates a sliding windows df with trend analysis to training target
  trend_train_df = getSlidingWindows_wTrendAnalysis(train_df$Target, w = w
                                                    , running_mean = running_mean
                                                    , nStepAhead = 1, alpha = alpha)
  # Insert trend classification ($Class) into train pool 
  train_df$Class[(w+1):length(train_df$Target)] = trend_train_df$Class
  train_df = na.omit(train_df)
  # Create three lists with the best models (a list by trend class)
  model_order = getOrderedListOfBestModelsByTrend(train_df)
  none_order = model_order$none_order;
  positive_order = model_order$positive_order
  negative_order = model_order$negative_order
  
  # Test
  # Creates a sliding windows df with trend analysis to test target 
  trend_test_df = getSlidingWindows_wTrendAnalysis(test_df$Target, w = w
                                                   , running_mean = running_mean
                                                   , nStepAhead = 1, alpha = alpha)
  # Insert trend classification ($Class) into test pool
  test_df$Class[(w+1):length(test_df$Target)] = trend_test_df$Class
  test_df = na.omit(test_df)
  
  # Chooses the best model for each instance (test pool) 
  # based on its trend and previously created ranks
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
    #print(proc_time_end)
    
    results_df = na.omit(results_df)
    metrics_test = calculateMetrics(results_df)#; View(metrics_test)
    
    if(!dir.exists(paste0("results/", country))){
      dir.create(paste0("results/", country))
    }
    
    write.csv(results_df, paste0("results/", country, "/", country, "_forecasring_results.csv"))
    write.csv(metrics_test, paste0("results/", country, "/", country, "_metrics.csv"))
    write.csv(proc_time_end, paste0("results/", country, "/", country, "_proc_time.txt"))    
  }
}
