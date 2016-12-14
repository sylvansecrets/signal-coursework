library("foreign")
library("dplyr")
library("caret")
elections_df = read.dta("C:/Users/User/Documents/GitHub/Signal-Data-Science/elections.dta")
elections_clean_df = dplyr::select(elections_df, year,age:religion, vote, presvote)
elect_years = 1948+(0:20)*4
elections_filtered = elections_clean_df[elections_clean_df$year %in% elect_years,]

##attempted succinctness
lapply(elections_filtered[sapply(elections_filtered,is.factor)],levels)
levels(elections_filtered$gender) = c("na","male","female")
levels(elections_filtered$race)[7] = "na"
levels(elections_filtered$educ) = c("none or na","0-8 years", "8-12 years",
                                    "incomplete college", "complete college")
levels(elections_filtered$urban)[1]= c("na")
levels(elections_filtered$urban)[4] = c("rural")
levels(elections_filtered$region) = c("na", "northeast", "north central", "south",
                                      "west")

levels(elections_filtered$income)[1] = c("na")
levels(elections_filtered$occup1)[1] = c("na")
levels(elections_filtered$occup1)[7] = c("homemakers")

levels(elections_filtered$presvote)=c("na", "democrat", "republican", "third party")

##they have already been converted to NAs...

##filling NAs numeric
##Replace NAs with mean of column
replace_na = function(df){
  for (i in seq_len(ncol(df))){
    if (is.numeric(df[[i]])){
    col_mean = mean(df[[i]],na.rm=TRUE)
    na_index = is.na(df[i])
    df[na_index,i] = col_mean
    s = sum(is.na(df[i]))#debug
    }
   else if (is.factor(df[[i]])){
    clean_sample = df[[i]][is.na(df[[i]])==FALSE]
    df[[i]]= sapply(df[[i]],
                    function(w){
                      if (is.na(w)){
                        return (sample(clean_sample,1))
                        } else {return(w)}
                      })
    
   }
  }
  return(df)
}

replace_0s = function(df){
  for (i in seq_len(ncol(df))){
    if (is.numeric(df[[i]])){
      col_mean = mean(df[[i]],na.rm=TRUE)
      index_0 = (df[[i]]==0)
      df[index_0,i] = col_mean
    }
    else if (is.factor(df[[i]])){
      clean_sample = df[[i]][is.na(df[[i]])==FALSE]
      df[[i]]= sapply(df[[i]],
                      function(w){
                        if (w==0){
                          return (sample(clean_sample,1))
                        } else {return(w)}
                      })
      
    }
  }
  return(df)
}

elections_filtered = replace_na(elections_filtered)
elections_filtered = replace_0s(elections_filtered)

##Trying out model.matrix
elect_matrix = model.matrix(~.,data=elections_filtered)

##It works!
# df_test = data.frame(
#   c(1,4,5,7),
#   c("test","quiz","exam","test")
# )
##It also automatically avoids numerics
# elections_filtered = elections_clean_df[elections_clean_df$year %in% elect_years,]
voted_df = dplyr::filter(elections_filtered, vote=="2. yes, voted" & year==1992 
                         & (presvote=="democrat" | presvote=="republican") )
voted_features = subset(voted_df, select=-c(vote,presvote))
voted_target = voted_df$presvote
voted_target = factor(voted_target)
voted_features_matrix = model.matrix(~., data=voted_features)
no_constant_matrix = voted_features_matrix[,apply(voted_features_matrix,2,sd)!=0]

# voted_target_matrix = model.matrix(~., data=voted_target)

lambda_list = sapply(seq(1,-6,length.out=50),
                     function(w){10^w})
param_grid = expand.grid(.alpha = 1:10 * 0.1,
                         .lambda = lambda_list)

control = trainControl(method = 'repeatedcv', number = 5, repeats=1, verboseIter = TRUE,
                       summaryFunction=twoClassSummary,classProbs=TRUE)

caret_fit = train(x=scale(no_constant_matrix),
                  y=voted_target,
                  method="glmnet",
                  tuneGrid=param_grid,
                  metric = "ROC",
                  trControl=control)


best_lambda = caret_fit$bestTune[2]
best_alpha = caret_fit$bestTune[1]
# fitl1 = glmnet(scale(select(activities_1,sports:yoga)),activities_1$attr_o,alpha = 1)
best_model = glmnet(scale(no_constant_matrix), voted_target,family="binomial", alpha=best_alpha, lambda=best_lambda)

## welp, by the year time

