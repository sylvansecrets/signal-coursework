library('dplyr')
library('grid')
library('ggplot2')
library('Rmisc')
library('glmnet')
library('caret')
##Regularization

set.seed(1); j=50; a=0.25
x=rnorm(j)
error = sqrt(1-a^2)*rnorm(j)
y = a*x + error

summary(lm(y~x-1))

cost =function(x,y,aEst,lambda,p){
  return(
    (mean((y-aEst*x)^2))^0.5+lambda*sum((abs(aEst))^p)
  )
}

reg_df = 
  expand.grid(
    lambda=sapply(1:10,function(x){
      2^(x-9)
    }),
    a= sapply(1:401,function(x){
      -0.1+0.001*(x-1)
    })
  )

## add two columns
reg_df[3] = costL1=0
reg_df[4] = costL2=0

##for loop to fill these columns
for (i in seq_len(nrow(reg_df))){
  reg_df[i,3]=cost(x,y,reg_df[i,2],reg_df[i,1],1)
}
for (i in seq_len(nrow(reg_df))){
  reg_df[i,4]=cost(x,y,reg_df[i,2],reg_df[i,1],2)
}


##making plots

graph_list=
  lapply(
    unique(reg_df[1]),
    function(w){
      # df_temp = dplyr::filter(reg_df, lambda==w)
      l = (ggplot(data=dplyr::filter(reg_df,reg_df[1]==w), aes(x=a)) + 
        geom_point(aes(y=V3, colour="purple", alpha=0.7)) +
        geom_point(aes(y=V4, colour="green", alpha=0.7))
  )
      return(l)
    }
  )

# args.list = c(graph_list, nrow=5,ncol = 2)
# do.call(multiplot, args.list)

multiplot(plotlist=graph_list,rows = 5,cols=2)
# multiplot(graph_list[[1]])
graph_li = vector(mode="list",10)
#seq_len(unique(reg_df[1]))){
# for (i in 1:10){
#   graph_li[[i]] = 
#     (ggplot(data=dplyr::filter(reg_df,reg_df[1]==unique(reg_df[i,1])), aes(x=a)) + 
#        geom_line(aes(y=V3, colour="purple", alpha=0.7)) +
#        geom_line(aes(y=V4, colour="green", alpha=0.7))
#     )
# }

# graph_li
multiplot(plotlist = graph_li,cols =2)



#glmnet start
spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speedDatingSimple.csv")
gender_0_df = dplyr::filter(spd_df, gender=="0")
gender_1_df = dplyr::filter(spd_df, gender=="1")
activities_0 = select(gender_0_df,sports:yoga,attr_o)
activities_1 = select(gender_1_df,sports:yoga,attr_o)

##step function
f_1 = lm(attr_o~., data=activities_1)
step_result = step(f_1, direction='backward')
set.seed(11)
fitl1 = glmnet(scale(select(activities_1,sports:yoga)),activities_1$attr_o,alpha = 1)
fitl2 = glmnet(scale(select(activities_1,sports:yoga)),activities_1$attr_o,alpha = 0)
fitl1$beta[,length(fitl1$beta)]


predicted = predict(fitl1,scale(select(activities_1, sports:yoga)))

RMSE_li = NULL
lambda_li = NULL
##iterate over all of fit
min_RMSE = function(pred_val, tru_val, fit=fitl1){
  RMSEs = apply(
    pred_val,2,
    FUN=function(w){
      (mean((w-tru_val)^2))^0.5
    }
  )
  min_RMSE_ind = which.min(RMSEs)
  lambda_li <<- fit$lambda
  RMSE_li <<- RMSEs
  min_lambda = lambda_li[min_RMSE_ind]
  least_RMSE = min(unlist(RMSEs))

  return(c(least_RMSE,min_lambda))
}
#lambda = 1
predicted = predict(fitl1,scale(select(activities_1, sports:yoga)))
min_RMSE(predicted,activities_1$attr_o,fitl1)
ggplot()+geom_point(aes(x=lambda_li,y=RMSE_li))

#lambda = 2
predicted = predict(fitl2,scale(select(activities_1, sports:yoga)))
min_RMSE(predicted,activities_1$attr_o,fitl2)
ggplot()+geom_point(aes(x=lambda_li,y=RMSE_li))
  
#compare against backwards stepwise
min_RMSE(predicted,activities_1$attr_o,fitl1)
min_RMSE(predicted,activities_1$attr_o,fitl2)
sqrt(mean(step_result$residuals^2))

#compare coefficients for l1 and l2
fit_l1$beta[,length(fit_l1$a0)]
fit_l2$beta[,length(fit_l2$a0)]

#run for females
fitl1_0 = glmnet(scale(select(activities_0,sports:yoga)),activities_0$attr_o,alpha = 1)
fitl2_0 = glmnet(scale(select(activities_0,sports:yoga)),activities_0$attr_o,alpha = 0)
fitl1_0$beta[,length(fitl1_0$a0)]
fitl2_0$beta[,length(fitl2_0$a0)]

###use cv.glmnet

activities_0
activities_1

attrmalel1 = cv.glmnet(scale(select(activities_1,sports:yoga)),activities_1$attr_o, alpha = 1)
attrmalel2 = cv.glmnet(scale(select(activities_1,sports:yoga)),activities_1$attr_o, alpha = 0)

#try to plot lambda vs rmse
predicted = predict(attrmalel1,scale(select(activities_1, sports:yoga)))
min_RMSE(predicted,activities_1$attr_o,attrmalel1)
ggplot()+geom_point(aes(x=lambda_li,y=RMSE_li))

predicted = predict(attrmalel2,scale(select(activities_1, sports:yoga)))
min_RMSE(predicted,activities_1$attr_o,attrmalel2)
ggplot()+geom_point(aes(x=lambda_li,y=RMSE_li))

##N-fold cross validation
#dave's code from wk1day2
nfold = function(df,numfolds){
  predictions = numeric(nrow(df))
  predictions2 = predictions
  predictions3 = predictions
  
  folds = sample(nrow(df)) %% numfolds + 1
  for (i in unique(folds)){
    train = df[folds != i,]
    test = df[folds == i,]
    
    
    #train on everything but the fold (stepwise)
    # m1 = lm(attr_o ~ select(df,sports:yoga),train)
    m1 = lm(attr_o~., data=train)
    model = step(m1, direction='backward')
    foldpred = predict(model, test)
    predictions[folds == i] = foldpred
    
    #train on everything but the fold (L1)
    m2 = cv.glmnet(scale(select(train,sports:yoga)), train$attr_o,alpha=0)
    foldpred2 = predict(m2,scale(select(test,sports:yoga)),s=m2$lambda.min)
    predictions2[folds ==i] = foldpred2
    
    #    cv.glmnet(scale(select(activities_1,sports:yoga)),activities_1$attr_o, alpha = 0)
    #predicted = predict(attrmalel1,scale(select(activities_1, sports:yoga)))
    m3 = cv.glmnet(scale(select(train,sports:yoga)), train$attr_o,alpha=1)
    foldpred3 = predict(m3,scale(select(test,sports:yoga)),s=m3$lambda.min)
    predictions3[folds ==i] = foldpred3
  }
  # R1 = cor(predictions, df$attr_o)
  R1 = sqrt(mean((predictions-df$attr_o)^2))
  names(R1) = "stepwise"
  R2 = sqrt(mean((predictions2-df$attr_o)^2))
  names(R2) = "L1 regularization"
  R3 = sqrt(mean((predictions3-df$attr_o)^2))
  names(R3) = "L2 regularization"
  return(c(R1,R2,R3))
}

result = nfold(activities_1,10)
girls = nfold(activities_0,10)

###caret implementation
# use cv.glmnet to get useful lambda value range

activities_0
activities_1

attrmalel1 = cv.glmnet(scale(select(activities_1,sports:yoga)),activities_1$attr_o, alpha = 1)
attrmalel2 = cv.glmnet(scale(select(activities_1,sports:yoga)),activities_1$attr_o, alpha = 0)

l1search = attrmalel1$lambda
l2search = attrmalel2$lambda

param_grid = expand.grid(.alpha = 1:10 * 0.1,
                         .lambda = l1search)

control = trainControl(method = 'repeatedcv', number = 10, repeats=3, verboseIter = TRUE)

caret_fit = train(x=(scale(select(activities_1,sports:yoga))),
                     y = activities_1$attr_o, 
                     method="glmnet",
                     tuneGrid=param_grid,
                     trControl=control)

thelambda = caret_fit$results$lambda[which.min(caret_fit$results$RMSE)]
thealpha = caret_fit$results$alpha[which.min(caret_fit$results$RMSE)]
#malecaret = caret()

##arbitrary function!
arb_rating = function(df, g, rating){
  df_gender = dplyr::filter(df,gender==g)
  activities_df = select(df_gender,sports:yoga,rating)
  #find lambda range
  lambda_range = cv.glmnet(scale(select(activities_df,sports:yoga)),
                           activities_df$attr_o, alpha = 1)
  #do the caret
  param_grid = expand.grid(.alpha = 1:10 * 0.1,
                           .lambda = l1search)
  
  control = trainControl(method = 'repeatedcv', number = 10, repeats=3, verboseIter = TRUE)
  
  caret_fit = train(x=(scale(select(activities_df,sports:yoga))),
                    y = activities_df$rating, 
                    method="glmnet",
                    tuneGrid=param_grid,
                    trControl=control)
  min_ind = caret_fit$results$lambda[which.min(caret_fit$results$RMSE)]
  best_lambda = caret_fit$results$lambda[min_ind]
  best_alpha = caret_fit$results$lambda[min_ind]
  lowest_RMSE = caret_fit$results$RMSE[min_ind]

  return(c(best_lambda,best_alpha,lowest_RMSE))
}

#spd_df is the most general df