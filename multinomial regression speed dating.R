library("dplyr")
library("ggplot2")
library("pROC")
library("glmnet")
library("corrplot")


spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speeddating-aggregated.csv")

top = 4
tbl = sort(table(spd_df$career_c),decreasing=TRUE)[1:top]
top_career = names(tbl)

##The top careers are respectively Academic/Research, Business, Lawyer, Creative Arts/Entertainment

##Filter out all the unnecessary parts
spd_career= dplyr::filter(spd_df, career_c%in%(as.numeric(top_career)))
predictors= dplyr::select(spd_career, -X,-gender,-dec_o,-race_o,-race,-career_c)
scaled_predictors = as.matrix(scale(predictors))

## Run glmnet
career_log = glmnet(scaled_predictors, spd_career$career_c, family="multinomial", lambda=0)

## Create a matrix of log odds coefs
career_matrix = matrix(0, ncol=length(coef(career_log)[[1]]), nrow=length(coef(career_log)))
for (i in 1:nrow(career_matrix)){
  career_matrix[i,]=as.numeric(coef(career_log)[[i]])
}
career_matrix = career_matrix[,-1]
rownames(career_matrix)=names(coef(career_log))
colnames(career_matrix)=names(predictors)

## Corrplot the log odds for each career
corrplot(career_matrix, is.corr=FALSE, type="upper")

##Make predictions on the entire datase
p = predict(career_log, scaled_predictors, s=0)
pca = prcomp(p[,,1])

##corrplot the PCA
corrplot(as.matrix(pca$rotation), is.corr=FALSE)

## PC1 - NOT money/social
## PC2 - Arts-based, humanities-based
## PC3 - ???


## Changes a particular row of a matrix of log odds into probabilities
probabilities = function(preds, rownum){
  result = vector(mode="numeric", ncol(preds))
  for (i in 1:ncol(preds)){
    e = exp(preds[,i])
    s = e/sum(e)
    result[i]=s[rownum]
  }
  return (result)
}
