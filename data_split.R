split_data = function(df, test_size=0.5){
  train_size = floor((nrow(df))*test_size)
  train_ind <<- sample(seq_len(nrow(df)),train_size)
  train_df <<- df[train_ind,]
  test_df <<- df[-train_ind,]
  return(list(train_df,test_df))
}