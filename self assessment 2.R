##Start 10:08

##load libraries
library(dplyr)
library(ggplot2)
library(glmnet)
library(psych)
library(corrplot)

##Set the consistent alpha and lambda values
alpha_list = 0.1*(0:10)
# lambda_list = sapply(seq(1,-3,length.out=50),
#                      function(w){10^w})
##simple replacement
lambda_list = 10^(seq(1,-3,length.out=50))

##Load msq dataset
df_msq = msq

##Split into target and features
features_df = select(msq,active:scornful)
targets_df = select(msq,Extraversion, Neuroticism)

##Replace NAs with mean of column
replace_na = function(df){
  for (i in seq_len(ncol(df))){
    col_mean = mean(df[[i]],na.rm=TRUE)
    na_index = is.na(df[i])
    df[na_index,i] = col_mean
  }
  return(df)
}

targets_df = replace_na(targets_df)
features_df = replace_na(features_df)


# for (i in seq_len(ncol(features_df))){
#   col_mean = mean(features_df[[i]],na.rm=TRUE)
#   na_index = is.na(features_df[i])
#   features_df[na_index,i] = col_mean
# }

##Generate fold assignment (6 instead of 4 made)
N_fold = 10
train_features = vector(mode="list",N_fold)
test_features = vector(mode="list",N_fold)
train_Extraversion = vector(mode="list",N_fold)
test_Extraversion = vector(mode="list",N_fold)
train_Neuroticism = vector(mode="list",N_fold)
test_Neuroticism = vector(mode="list",N_fold)

##Folding the data
set.seed(1)
fold_ind = sample(nrow(features_df)) %% N_fold + 1   ##placed here in edit; should not be rerun every time
for (i in 1:N_fold){
  # fold_ind = sample(nrow(features_df)) %% N_fold + 1
  train_features[[i]] = scale(features_df[fold_ind != i,])
  test_features[[i]] = scale(features_df[fold_ind == i,], 
                             center= attr(train_features[[i]], "scaled:center"), 
                             scale=attr(train_features[[i]], "scaled:scale"))
  train_Extraversion[[i]]  = targets_df$Extraversion[fold_ind != i]
  test_Extraversion[[i]]  = targets_df$Extraversion[fold_ind == i]
  train_Neuroticism[[i]]  = targets_df$Neuroticism[fold_ind != i]
  test_Neuroticism[[i]]  = targets_df$Neuroticism[fold_ind == i]
}

##root mean square error function
rmse=function(x,y){
  return(sqrt(mean(((x-y)^2))))
}

##answer data frame
answer_df = data.frame()


#Storing Data (list of list, retrieve by alpha first and then by n )
Extraversion_fit_a = vector(mode="list", length(alpha_list))
Neuroticism_fit_a = vector(mode="list", length(alpha_list))

## Iterating over each alpha

for (i in seq_len(length(alpha_list))){
  a = alpha_list[[i]]
  print (a)
  Extraversion_fit_n = vector(mode="list",N_fold)
  Neuroticism_fit_n = vector(mode="list",N_fold)
  ##Iterating over each fold
  for (n in seq_len(N_fold)){
    # train_features_scaled = scale(train_features[[n]])
    Extraversion_fit_n[[n]]=
      glmnet(train_features[[n]],train_Extraversion[[n]],alpha=a,lambda=lambda_list)
    Neuroticism_fit_n[[n]]=
      glmnet(train_features[[n]],train_Extraversion[[n]],alpha=a,lambda=lambda_list)
    
  }
  Extraversion_fit_a[[i]]=Extraversion_fit_n
  Neuroticism_fit_a[[i]]=Neuroticism_fit_n
}

## (my understanding of the assessment breaks down here, as there are 110 models,
## N_fold for each value of alpha, so iterating over lambda and then each fold
## would not be helpful; will also iterate over alpha)
## Debugging the human: did not read indent
for (i in seq_len(length(lambda_list))){
  lambda = lambda_list[[i]]
  print(lambda)
  for (m in seq_len(length(alpha_list))){
    a = alpha_list[[m]]
    for (n in 1:N_fold){
      current_Extraversion_mod = Extraversion_fit_a[[m]][[n]]
      current_Neuroticism_mod = Neuroticism_fit_a[[m]][[n]]
      pred_Extraversion = predict(current_Extraversion_mod,
                                  test_features[[n]],
                                  s=lambda)
      pred_Neuroticism = predict(current_Neuroticism_mod,
                                  test_features[[n]],
                                  s=lambda)
      rmse_Extraversion = rmse(pred_Extraversion, test_Extraversion[[n]])
      rmse_Neuroticism= rmse(pred_Neuroticism, test_Neuroticism[[n]])
      answer_row = c(a, lambda, rmse_Extraversion, rmse_Neuroticism, n)
      answer_df = rbind(answer_df,answer_row)
    }
  }
}
names(answer_df) = c("alpha","lambda","rmse_Extraversion","rmse_Neuroticism","n")
## We have 5500 models (lambda*alpha*N_fold)
## arg_min is which.min
# backup = answer_df

##average out the rmses from each combination of alpha and lambda

by_a_l = dplyr::group_by(answer_df,alpha,lambda)
concise_answer = summarise(
  by_a_l,
  rmse_Ex = mean(rmse_Extraversion),
  rmse_Ne = mean(rmse_Neuroticism)
)

min_Ex = which.min(concise_answer$rmse_Ex)
min_Ne = which.min(concise_answer$rmse_Ne)
best_Ex = concise_answer[min_Ex,]
best_Ne = concise_answer[min_Ne,]
best_Ex_mod =glmnet(scale(features_df),
                   targets_df$Extraversion,
                   alpha=best_Ex$alpha,
                   lambda=best_Ex$lambda)
best_Ne_mod = glmnet(scale(features_df),
                     targets_df$Neuroticism,
                     alpha=best_Ne$alpha,
                     lambda=best_Ne$lambda)

coef_Ex = coef(best_Ex_mod, s=best_Ex$lambda)
coef_Ne = coef(best_Ne_mod, s=best_Ne$lambda)
coef_df = data.frame(as.numeric(coef_Ex),as.numeric(coef_Ne))
#Setting names
rownames(coef_df)=rownames(coef_Ex)
colnames(coef_df)=c("Extraversion","Neuroticism")
coef_df = coef_df[-1,]
##Quantile comparision
q1 = quantile(abs(coef_df[[1]]))
q2 = quantile(abs(coef_df[[2]]))
#filter rows
# filtered_df = dplyr::filter(
#   coef_df,
#   (Extraversion<q1[4] & Neuroticism<q2[4])
# )

##with for loop this time (row names broken in dplyr)
##note that this doesn't work because q2[4]=0, modified to be >q2[4] for Neuroticism
filtered_df = data.frame()
for (i in seq_len(nrow(coef_df))){
  current_row = coef_df[i,]
  Ex = current_row$Extraversion
  Ne = current_row$Neuroticism
  Ex_boolean = (abs(Ex))>=q1[4]
  Ne_boolean = (abs(Ne))>q2[4]
  # print(paste(Ex,Ne))
  # print(paste(Ex_boolean,Ne_boolean))
  if (Ex_boolean|Ne_boolean)
  {
    filtered_df = rbind(filtered_df, current_row)
  }
}

##returned to corrplot at 14:15, done at 14:16 (still too many to see whole graph)
##which.min picked a lambda arbitrarily from all possible lambdas of equal RMSE
corrplot(as.matrix(filtered_df), is.corr=FALSE)
