library('dplyr')
library('grid')
library('ggplot2')
library('Rmisc')
library('glmnet')
library('caret')
##Regularization

spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speedDatingSimple.csv")

df= spd_df 
  g = 0
  rating = "attr_o"

##arbitrary function!
arb_rating = function(df, g, rating){
  df_gender = dplyr::filter(df,gender==g)
  ma = match(rating,colnames(df))
  print (ma)
  activities_df = dplyr::select(df_gender,sports:yoga,ma)
  #find lambda range
  lambda_range = (cv.glmnet(scale(select(activities_df,sports:yoga)),
                           activities_df$attr_o, alpha = 1))$lambda
  
  #do the caret
  param_grid = expand.grid(.alpha = 1:10 * 0.1,
                           .lambda=10^seq(-4, -1, length.out=10))
  
  control = trainControl(method = 'repeatedcv', number = 10, repeats=3, verboseIter = TRUE)
  
  print("welp")
  
  ma2 = match(rating,colnames(activities_df))
  predictors = (scale(select(activities_df,sports:yoga)))
  rated = activities_df[[ma2]]
                
 
  caret_fit = train(x=predictors,
                    y = rated, 
                    method="glmnet",
                    tuneGrid=param_grid,
                    trControl=control)
  
  print('here')
  
  min_ind = caret_fit$results$lambda[which.min(caret_fit$results$RMSE)]
  best_lambda = caret_fit$results$lambda[min_ind]
  best_alpha = caret_fit$results$lambda[min_ind]
  lowest_RMSE = caret_fit$results$RMSE[min_ind]
  
  return(c(best_lambda,best_alpha,lowest_RMSE))}


# caret_fit = train(x=(scale(select(activities_1,sports:yoga))),
#                   y = activities_1$attr_o, 
#                   method="glmnet",
#                   tuneGrid=param_grid,
#                   trControl=control)

#spd_df is the most general df