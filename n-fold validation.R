library("ggplot2")

##df, double -> list of (df)
##Given a data frame, returns double*nrow(df) rows of the data frame, and the remainder
split_data = function(df, test_size=0.5){
  train_size = floor((nrow(df))*test_size)
  train_ind <<- sample(seq_len(nrow(df)),train_size)
  train_df <<- df[train_ind,]
  test_df <<- df[-train_ind,]
  return(list(train_df,test_df))
}

##df, double -> list of list of dataframes
##given a dataframe and a number, returns list of n training and test sets 

n_split_data = function(df, n){
  #return list of dfs
  test_df_li = vector(mode="list",n)
  train_df_li = vector(mode="list",n)
  train_ind = vector(mode="logical",n)  #debugging purpose
  #shuffle data
  df_temp = df[sample(nrow(df)),]
  for (i in seq_len(n)){
    test_ind = seq_len(nrow(df))%%n==(i-1)
    test_var = seq_len(nrow(df))[test_ind]  #debugging purpose
    test_df = df_temp[test_ind,]
    train_df = df_temp[!test_ind,]
    test_df_li[[i]]=test_df
    train_df_li[[i]]=train_df
    
  }
  return (list(test_df_li,train_df_li))
}

##n-fold cross validation wrapper
##df is the original dataframe with all but one column as predictors
##cname is the column that is to be predicted
##n is the n in n-fold cross validation

n_fold_val = function(df, cname, n){
    df_split = n_split_data(df,n)
    train_dfs = df_split[[2]]
    test_dfs = df_split[[1]]
    train_r2_vec = vector(mode="numeric",n)
    test_r2_vec = vector(mode="numeric",n)
    for (i in seq_len(n)){
      train_df = train_dfs[[i]]
      test_df = test_dfs[[i]]
      f = formula(paste(cname,"~."))
      lm_mod = lm(f,data=train_df)
      predicted = predict(lm_mod, test_df)
      test_r_2 = (cor(predicted,test_df[cname]))^2
      test_r2_vec [i] = test_r_2
      train_r2_vec[i] = summary(lm_mod)$adj.r.squared
    }
    return (list(test_r2s = test_r2_vec, train_r2s = train_r2_vec))
}

##Load Dating set
spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speedDatingSimple.csv")
gender_0_df = dplyr::filter(spd_df, gender=="0")
gender_1_df = dplyr::filter(spd_df, gender=="1")
activities_0 = select(gender_0_df,sports:yoga,attr_o)

## get a vector of r^2 values
n_fold_val(activities_0,"attr_o",10)
## note that train/split is the same as getting the first vector from n-fold with 2

## run 100 times
rep = 100
test_r2_10 = vector(mode="list", rep)
test_r2_2 = vector(mode="list", rep)
test_r2_split = vector(mode="list", rep)
train_r2_10 = vector(mode="list", rep)
train_r2_2 = vector(mode="list", rep)
train_r2_split = vector(mode="list", rep)



for (i in seq_len(rep)){
  test_r2_10[[i]] = n_fold_val(activities_0,"attr_o",10)[[1]]
  test_r2_2[[i]] = n_fold_val(activities_0,"attr_o",2)[[1]]
  test_r2_split[[i]] = n_fold_val(activities_0,"attr_o",2)[[1]][1]
  
  train_r2_10[[i]] = n_fold_val(activities_0,"attr_o",10)[[2]]
  train_r2_2[[i]] = n_fold_val(activities_0,"attr_o",2)[[2]]
  train_r2_split[[i]] = n_fold_val(activities_0,"attr_o",2)[[2]][1]
}

test_r2_10_vec = unlist(test_r2_10)
test_r2_2_vec = unlist(test_r2_2)
test_r2_split_vec = unlist(test_r2_split)

train_r2_10_vec = unlist(train_r2_10)
train_r2_2_vec = unlist(train_r2_2)
train_r2_split_vec = unlist(train_r2_split)

std_err_10 = (sum((test_r2_10_vec)^2)/(length(test_r2_10_vec)))^(0.5)
std_err_2 = (sum((test_r2_2_vec)^2)/(length(test_r2_2_vec)))^(0.5)
std_err_split = (sum((test_r2_split_vec)^2)/(length(test_r2_split_vec)))^(0.5)

##Plotting
(ggplot()
  +geom_point(aes(x=train_r2_10_vec,y=test_r2_10_vec),color="purple", alpha=0.7)
  # +geom_point(aes(x=train_r2_2_vec,y=test_r2_2_vec),color="blue", alpha=0.3)
  +geom_point(aes(x=train_r2_2_vec,y=test_r2_2_vec),color="yellow",alpha=0.7)
  +xlim(c(0,0.3)))

mean(test_r2_10_vec)
mean(test_r2_2_vec)
mean(test_r2_split_vec)
