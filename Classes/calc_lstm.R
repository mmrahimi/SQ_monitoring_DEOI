lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}


#Normalize the data
scale_data = function(data, feature_range = c(0, 1)) {
  x = data
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train) ,scaler= c(min =min(x), max = max(x))) )
  
}

## revert the predicted values to the original scale.
invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

calc_lstm <- function(count, sentiment){
  
  epochs <- 15
  k = 3
  
  sentiments.ts <- as.ts(abs(sentiment))
  counts.ts <- as.ts(count)
  
  #Transform data to stationary
  diffed_sen = diff(sentiments.ts, differences = 1)
  supervised_sen = lag_transform(diffed_sen, 1)
  Scaled_sen = scale_data(supervised_sen, c(-1, 1))
  y_supervised_sen = Scaled_sen$scaled_train[, 2]
  x_supervised_sen = Scaled_sen$scaled_train[, 1]
  
  # Reshape the input to 3-dim
  timesteps_sen = 1
  dimensions_sen = 1
  samples_sen = length(x_supervised_sen)
  dim(x_supervised_sen) <- c(round(samples_sen/timesteps_sen), timesteps_sen, dimensions_sen)
  
  # specify required arguments
  X_shape2_sen = dim(x_supervised_sen)[2]
  X_shape3_sen = dim(x_supervised_sen)[3]
  batch_size_sen = 1                # must be a common factor of both the train and test samples
  units_sen = 50                     # can adjust this, in model tuninig phase
  
  #=========================================================================================
  
  model_sen <- keras_model_sequential()
  model_sen%>%
    layer_lstm(units_sen, batch_input_shape = c(batch_size_sen, X_shape2_sen, X_shape3_sen), stateful= TRUE)%>%
    layer_dense(units=1)
  
  model_sen %>% compile(
    loss = 'mean_squared_error',
    #optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),
    optimizer = optimizer_adam(),  
    metrics = c('accuracy')
  )
  
  summary(model_sen)
  
  model_sen %>% fit(x_supervised_sen, y_supervised_sen, epochs=epochs, batch_size=batch_size_sen, verbose=1, shuffle=FALSE)
  
  L_sen = length(x_supervised_sen)
  scaler_sen = Scaled_sen$scaler
  predictions_sen = numeric(L_sen)
  
  for(i in 1:L_sen){
    X_sen = x_supervised_sen[i]
    dim(X_sen) = c(1,1,1)
    yhat_sen = model_sen %>% predict(X_sen, batch_size=batch_size_sen)
    # invert scaling
    yhat_sen = invert_scaling(yhat_sen, scaler_sen,  c(-1, 1))
    # invert differencing
    yhat_sen  = yhat_sen + sentiments.ts[i]
    # store
    predictions_sen[i] <- yhat_sen
  }
  
  
  ts.plot(sentiments.ts, ts(predictions_sen), gpars = list(col = c("black", "red")))
  ET_sen <- sentiments.ts - ts(predictions_sen)
  ET_ST_sen <- stdev(ET_sen)
  ET_Mean_sen <- mean(ET_sen)
  Outliers_sent <- which(ET_sen>ET_Mean_sen+ k*ET_ST_sen)
  points(Outliers_sent,sentiments.ts[Outliers_sent],cex=.5,col="blue")
  
  # Count
  
  #Transform data to stationary
  diffed = diff(counts.ts, differences = 1)
  supervised = lag_transform(diffed, 1)
  Scaled = scale_data(supervised, c(-1, 1))
  y_supervised = Scaled$scaled_train[, 2]
  x_supervised = Scaled$scaled_train[, 1]
  
  # Reshape the input to 3-dim
  timesteps = 1
  dimensions = 1
  samples = length(x_supervised)
  dim(x_supervised) <- c(round(samples/timesteps), timesteps, dimensions)
  
  # specify required arguments
  X_shape2 = dim(x_supervised)[2]
  X_shape3 = dim(x_supervised)[3]
  batch_size = 1                # must be a common factor of both the train and test samples
  units = 50                     # can adjust this, in model tuninig phase
  
  #=========================================================================================
  
  model <- keras_model_sequential()
  model%>%
    layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
    layer_dense(units=1)
  
  model %>% compile(
    loss = 'mean_squared_error',
    #optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),
    optimizer = optimizer_adam(),  
    metrics = c('accuracy')
  )
  
  summary(model)
  
  model %>% fit(x_supervised, y_supervised, epochs=epochs, batch_size=batch_size, verbose=1, shuffle=FALSE)
  
  L = length(x_supervised)
  scaler = Scaled$scaler
  predictions = numeric(L)
  
  for(i in 1:L){
    X = x_supervised[i]
    dim(X) = c(1,1,1)
    yhat = model %>% predict(X, batch_size=batch_size)
    # invert scaling
    yhat = invert_scaling(yhat, scaler,  c(-1, 1))
    # invert differencing
    yhat  = yhat + counts.ts[i]
    # store
    predictions[i] <- yhat
  }
  
  
  ts.plot(counts.ts, ts(predictions), gpars = list(col = c("black", "red")))
  ET <- counts.ts - ts(predictions)
  ET_ST <- stdev(ET)
  ET_Mean <- mean(ET)
  Outliers_count <- which(ET>ET_Mean+ k*ET_ST)
  points(Outliers_count,counts.ts[Outliers_count],cex=.5,col="blue")
  
  
  res_lstm_union = union(Outliers_count, Outliers_sent)
  res_lstm_intersect = intersect(Outliers_count, Outliers_sent)
  
  return(list(A= Outliers_count, B= Outliers_sent, union = res_lstm_union, intersect=res_lstm_intersect))
}
