workingdir = "/Users/elainelai/OneDrive/Wharton/Class/STAT701-DataMining/FinalProject"

RandomForest <- function(wd){
  # Required packages
  library(tree)              # regression/classification trees
  library(randomForest)      # to see how a package is evolved to be better: rfNews()
  
  # Import Data
  moviedata <- read.csv("post-cleaning-data.csv")

  # Clean data for decision tree -> can't use factor with more than 32 categories
  moviedata$X <- NULL
  moviedata$director_name <- NULL
  moviedata$genres <- NULL
  moviedata$genres.sub <- NULL
  moviedata$movie_title <- NULL
  moviedata$language <- NULL
  moviedata$title_year <- NULL
  moviedata$actor_2_name <- NULL
  moviedata$actor_1_name <- NULL
  moviedata$actor_3_name <- NULL
  moviedata$country <- NULL
  
  # user Log of gross due to left tail
  moviedata$loggross <- log(moviedata$gross)
  moviedata$gross <- NULL
  
  # separate into test and training data
  smp_size <- floor(0.75 * nrow(moviedata))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(moviedata)), size = smp_size)
  train <- moviedata[train_ind, ]
  test <- moviedata[-train_ind, ]

  ### A Single Tree based on all budget only
  fit1.single <- tree(loggross~budget, train)
  
  png("fit1.png", width = 800, height = 400)
  plot(fit1.single)
  text(fit1.single, pretty=1)
  par(mfrow=c(1,1))
  dev.off()
  
  fit1.single.result <- summary(fit1.single)
  rss1.single = fit1.single.result$dev
  
  ### A Single Tree based on all variables
  fit2.single <- tree(loggross~., train)
  
  png("fit2.png", width = 800, height = 400)
  plot(fit0.single)
  text(fit0.single, pretty=1)
  par(mfrow=c(1,1))
  dev.off()
  
  fit0.single.result <- summary(fit0.single)
  rss0.single = fit0.single.result$dev
  
  # A Random forest based on all variables
  # Set mtry = 5 because of the rule of thumb: mtry = n/3
  fit3.rf <- randomForest(loggross~., train, mtry=5, ntree=100)
  yhat <- predict(fit.rf, train)   # predicted values we use 
  
  png("fit2.png", width = 800, height = 400)
  plot(train$loggross, yhat, pch=16,  # add a 45 degree line:looks very good!
       main="Y vs. Predicted Y", col="blue")
  abline(0, 1, lwd=5, col="red")
  dev.off()
  
  rss.rf = sum((train$loggross-yhat)^2)
  plot(fit.rf, col="red", pch=16, type="p", main="Random Forest Error")
  
  # Plot says we need 500 trees
  #Run above loop a few time, it is not very unstable. 
  #The recommended mtry for reg trees are mtry=p/3=19/3 about 6 or 7. Are you convinced with p/3?
  
  fit2.rf <- randomForest(loggross~., train, mtry=5, ntree=500, importance=TRUE)
  yhat2 <- predict(fit2.rf, train)   # predicted values we use 
  
  plot(train$loggross, yhat2, pch=16,  # add a 45 degree line:looks very good!
       main="Y vs. Predicted Y", col="blue")
  abline(0, 1, lwd=5, col="red")
  
  rss2.rf = sum((train$loggross-yhat2)^2)

  # RSS went down, cool
  
  #### Evaluate vs Test Data
  
  # four fits fit0.single, fit1.single, fit.rf, fit2.rf
  
  fit0.single.test <- predict(fit0.single, test)
  plot(test$loggross, fit0.single.test, pch=16,  # add a 45 degree line:looks very good!
       main="Y vs. Predicted Y", col="blue")
  abline(0, 1, lwd=5, col="red")
  rss2.rf = sum((test$loggross-fit0.single.test)^2)
  
  fit1.single.test <- predict(fit1.single, test)
  plot(test$loggross, fit1.single.test, pch=16,  # add a 45 degree line:looks very good!
       main="Y vs. Predicted Y", col="blue")
  abline(0, 1, lwd=5, col="red")
  rss5.rf = sum((test$loggross-fit1.single.test)^2)
  
  
  fit.rf.test <- predict(fit.rf, test)
  plot(test$loggross, fit.rf.test, pch=16,  # add a 45 degree line:looks very good!
       main="Y vs. Predicted Y", col="blue")
  abline(0, 1, lwd=5, col="red")
  rss2.rf = sum((test$loggross-fit.rf.test)^2)
  
  
  fit2.rf.test <- predict(fit2.rf, test)
  plot(test$loggross, fit2.rf.test, pch=16,  # add a 45 degree line:looks very good!
       main="Y vs. Predicted Y", col="blue")
  abline(0, 1, lwd=5, col="red")
  rss7.rf = sum((test$loggross-fit2.rf.test)^2)
  
  imp <- importance(fit2.rf)
  write.csv(imp, "importance.csv")
  
  Install R, get the randomForest package, and run this code: 
    library(randomForest) set.seed(4543) # Random Forest relative importance of variables as predictors 
  rffit <- randomForest(resp ~ v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9, data=data, ntree=2000, keep.forest=FALSE, importance=TRUE) 
  importance(rffit) # relative importance of predictors (highest <-> most important) varImpPlot(rffit) 
  # plot results data is your dataset; resp is is your response variable; the vi are your predictor variables. The top two are the most important.
  
}