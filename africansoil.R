library('readr')
library('dplyr')
library('ggplot2')
library('caret')

df = read_csv('C:/Users/User/Documents/GitHub/Signal-Data-Science/training.csv')

soil = dplyr::select(df,starts_with('m'))
targets = dplyr::select(df,Ca:Sand)

caret_fit_vec = vector(mode="list",5)
min_ind_vec = vector(mode="numeric",5)
best_lambda_vec = vector(mode="numeric",5)
best_alpha_vec = vector(mode="numeric",5)
lowest_RMSE_vec = vector(mode="numeric",5)  

for (i in 1:5){

  
# lambda_range = seq(0,0.4,0.005)
param_grid = expand.grid(.alpha = c(1,0.1,0.05,0.001,0),
                         .lambda=10^seq(-6, -1, length.out=15))

control = trainControl(method = 'repeatedcv', number = 10, repeats=3, verboseIter = TRUE)
# rated = activities_df[[ma2]]
caret_fit = train(x=soil,
                  y = targets[[i]], 
                  method="glmnet",
                  tuneGrid=param_grid,
                  trControl=control)

min_ind = which.min(caret_fit$results$RMSE)
best_lambda = caret_fit$results$lambda[min_ind]
best_alpha = caret_fit$results$alpha[min_ind]
lowest_RMSE = caret_fit$results$RMSE[min_ind]

caret_fit_vec[[i]] = caret_fit
min_ind_vec[i] = min_ind
best_lambda_vec[i] = best_lambda
best_alpha_vec[i] = best_alpha
lowest_RMSE_vec[i] = lowest_RMSE  

}


# back_up = caret_fit_vec
