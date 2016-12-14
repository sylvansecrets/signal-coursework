##Stepwise Regression

##Load dating data
spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speedDatingSimple.csv")
gender_0_df = dplyr::filter(spd_df, gender=="0")
gender_1_df = dplyr::filter(spd_df, gender=="1")
activities_0 = select(gender_0_df,sports:yoga,attr_o)

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


lm_test = lm(attr_o~., data=activities_0)

##n-fold cross validation wrapper
##df is the original dataframe with all but one column as predictors
##cname is the column that is to be predicted
##n is the n in n-fold cross validation
##returns train r^2, test r^2, and p-values for each tested variable
n_fold_val = function(df, cname, n){
  df_split = n_split_data(df,n)
  train_dfs = df_split[[2]]
  test_dfs = df_split[[1]]
  train_r2_vec = vector(mode="numeric",n)
  test_r2_vec = vector(mode="numeric",n)
  p_val = vector(mode="numeric",ncol(df))
  for (i in seq_len(n)){
    train_df = train_dfs[[i]]
    test_df = test_dfs[[i]]
    f = formula(paste(cname,"~."))
    lm_mod = lm(f,data=train_df)
    predicted = predict(lm_mod, test_df)
    test_r_2 = (cor(predicted,test_df[cname]))^2
    test_r2_vec [i] = test_r_2
    train_r2_vec[i] = summary(lm_mod)$adj.r.squared
    p = summary(lm_mod)$coefficients[,4]
    p_val = p_val+p
  }
  p_val=p_val/n
  return (list(test_r2s = test_r2_vec, train_r2s = train_r2_vec, one_tailed_p = p_val))
}

n_fold_val(activities_0,"attr_o",10)

n_step = function(df,cname,n){
  k = ncol(df)-1
  avg_train_r2_vec = vector(mode="numeric",k)
  avg_test_r2_vec = vector(mode="numeric",k)
  removed_vec = vector(mode="character",k)
  df_mod=df
  for (i in seq_len(k)){
    n_fold_result = n_fold_val(df_mod,cname,n)
    test_r2s = n_fold_result[[1]]
    train_r2s = n_fold_result[[2]]
    p_vals = n_fold_result[[3]]
    elim = names(which.max(p_vals))
    df_mod = df_mod[,!colnames(df_mod)==elim]
    avg_train_r2_vec[i]=mean(train_r2s)
    avg_test_r2_vec[i]=mean(test_r2s)
    removed_vec[i]=elim
  }
  return(
    data.frame(avg_train_r2 = avg_train_r2_vec,
               avg_test_r2 = avg_test_r2_vec,
               removed = removed_vec))
  
  
  
}
step_data = n_step(activities_0,"attr_o",10)
##It appears that the r^2 increases as you remove some variables, but decreases when nearly all are removed

ggplot(data=step_data)+geom_point(aes(x=1:17,y=avg_test_r2))
