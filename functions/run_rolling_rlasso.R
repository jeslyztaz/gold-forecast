runrlasso <- function(df, indice, pl=FALSE) {
  
  train <- df[-nrow(df),]
  test  <- df[nrow(df),]
  X_train <- train[,-indice]
  y_train <- train[,indice]
  X_test <- test[,-indice]
  
  model <- rlasso(
    x = X_train,
    y = y_train,
    post = pl
  )
  
  pred <- predict(model, newdata = X_test)
  
  return(list("model" = model,"pred" = pred))
}

rlasso.rolling.window <- function(Y, test_start,indice=2, pl=FALSE){
  nprev = sum(Y$date >= test_start, na.rm = TRUE)
  save.coef=matrix(NA, nprev, ncol(Y)) #blank matrix for coefficients at each iteration
  save.pred=matrix(NA, nprev, 1) #blank for forecasts
  
  for(i in nprev:1){ #NB: backwards FOR loop
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window
    lasso=runrlasso(Y.window,indice,pl) #call the modified function to fit rlasso and predict
    save.coef[(1+nprev-i),]=lasso$model$coef #save estimated coefficients
    save.pred[(1+nprev-i),]=lasso$pred #save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors))
}