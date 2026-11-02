set.seed(759)

MSE <- function(pred, truth){ #start and end body of the function by { } - same as a loop 
  return(mean((truth - pred)^2)) #end function with a return(output) statement. Here we can go straight to return because the object of interest is a simple function of inputs
}

runrf <- function(df, test_start, indice=2) {
  train = df[df$date < test_start,]
  test = df[df$date >= test_start,]
  
  X_cols <- setdiff(names(train), c("date", "close_usd_per_troy_oz"))
  p <- length(X_cols)
  
  X_train = train[,X_cols]
  y_train = train[,indice]
  X_val = val[,X_cols]
  y_val = val[,indice]
  X_test = test[,X_cols]
  y_test = test[,indice]
  
  model <- randomForest(x=X_train, y=y_train)   
  pred <- predict(model, newdata=X_test)
  return(list("model"=model,"pred"=pred))
}

run_bagging <- function(df, test_start, indice=2) {
  train = df[df$date < test_start,]
  test = df[df$date >= test_start,]
  
  X_cols <- setdiff(names(train), c("date", "close_usd_per_troy_oz"))
  p <- length(X_cols)
  
  X_train = train[,X_cols]
  y_train = train[,indice]
  X_test = test[,X_cols]
  y_test = test[,indice]
  
  ntreev = c(10,500,5000)
  nset = length(ntreev)
  
  fmat = matrix(0,nrow(train),nset)
  
  for(i in 1:nset) {
    rffit = randomForest(x=X_train, y=y_train, ntree=ntreev[i], maxnodes=10)
    fmat[,i] = predict(rffit)
    cat("Bagging: ntree =", ntreev[i], "\n")
  }
  
  #We plot the OOB error using the largest ntree case to see how it evolves:
  #Plotting the RF object will yield the OOB error plot.
  
  windows()
  par(mfrow=c(1,1)) #1x1 plot
  plot(rffit) #plot the last fitted (largest) OOB error
  
  #Now plot the predicted values:
  oo = order(y_train) #ordering for nonlinear plot
  
  rffit2 = randomForest(x=X_train, y=y_train, ntree=5000, mtry=max(1, floor(p / 3)))
  
  windows()
  plot(rffit2) #plot the OOB error
  
  rffit3 = randomForest(x=X_train, y=y_train, ntree=5000, mtry=p)
  windows()
  plot(rffit3)
  
  #Do the plot of RF vs. bagging:
  xx=seq(1,5000,1)
  windows()
  plot(xx,rffit2$mse, type="l",col="blue", ylab="OOB MSE", xlab="B", main="Bagging vs. Random forest")
  lines(xx,rffit3$mse, type="l", col="red")
  legend("topright", c("Random Forest, m=7","Bagging"), lty=c(1,1) ,col=c("blue","red"))
  
  #Let's see how bagging does in OOS prediction
  ybag = predict(rffit3, newdata = X_test)
  return(ybag)
  
  #rmse.bag=sqrt(MSE(test$close_usd_per_troy_oz,ybag)) #compute test RMSE for the tree
}