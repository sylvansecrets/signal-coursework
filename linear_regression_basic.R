install.packages("foreign")
library("foreign")
library("ggplot2")
child.iq.df = read.dta("C:/Users/User/Documents/GitHub/Signal-Data-Science/child.iq/child.iq.dta")
kidiq.df = read.dta("C:/Users/User/Documents/GitHub/Signal-Data-Science/child.iq/kidiq.dta")

## Linear fit of the variables independently
summary(lm(kidiq.df$kid_score~kidiq.df$mom_hs+kidiq.df$mom_iq))

## Graphing kid score and mom iq
ggplot(kidiq.df, aes(x= mom_iq, y = kid_score)) + geom_smooth(method="lm", color="blue") + geom_point(alpha = 0.4) + geom_smooth(color="green")

## Graphing hs
ggplot(kidiq.df, aes(x= mom_hs, y = kid_score)) + geom_point(alpha = 0.1)

##Exercise 3.4
##Working with child.iq.df
summary(lm(child.iq.df$ppvt~child.iq.df$momage))
ggplot(child.iq.df, aes(x = momage, y = ppvt)) + geom_smooth(method = "lm") + geom_point(alpha = 0.5)
## Intercept - 67 (mother giving birth at age 0 have children with score 67)
## Recommendation: Give birth as late as possible, assuming that the relationship is truly linear, and that this is possible
age = child.iq.df$momage
score = child.iq.df$ppvt
edu = child.iq.df$educ_cat
inter = age*score
summary(lm(score~age+edu+inter))
summary(lm(score~inter))
## Recommendation: Get more education, return on age is bad
summary(lm(score~age+edu+inter)) #This doesn't work
ggplot(child.iq.df, aes(x = educ_cat, y = ppvt)) + geom_smooth(method = "lm") + geom_jitter(alpha = 0.5, height = 0, width = 0.2)
ggplot(child.iq.df, aes(x = educ_cat, y = ppvt)) + geom_smooth(method = "lm") + geom_jitter(alpha = 0.5,)

## c. high school
mom_hs = as.numeric(child.iq.df$educ_cat>1)
hs_df = cbind(child.iq.df, "hs" = mom_hs)
summary(lm(age~mom_hs))
summary(lm(score~age+edu+mom_hs))

## prediction
## random split
train_size = floor((nrow(child.iq.df))*0.5)
# set.seed(33)
train_ind = sample(seq_len(nrow(child.iq.df)),train_size)
train_df = child.iq.df[train_ind,]
test_df = child.iq.df[-train_ind,]

r_dif = function(train,test){
  model = lm(ppvt~momage+educ_cat, train)
  summary(model)
  predicted = predict(model, test)
  c2 = cor(predicted, test$ppvt)^2
  r2 = summary(model)$adj.r.squared
  return (c2-r2)
}

r_dif(train_df,test_df)



c_v_r = function(n){
  counter = 0
  total = 0
  for (i in 1:n){
    counter = counter+1
    train_ind = sample(seq_len(nrow(child.iq.df)),train_size)
    train_df <<- child.iq.df[train_ind,]
    test_df <<- child.iq.df[-train_ind,]
    total = total + r_dif(train_df,test_df)
  }
  return (total/counter)
}

##graph prediction vs actual
set.seed(33)
c_v_r(1)
predicted_df = data.frame(cbind(test_df$momage,predicted))
(ggplot()+
  geom_jitter(data = test_df, aes(x = momage, y = ppvt), color = "purple", alpha = 0.3, height = 0, width = 0.5)+
  geom_jitter(data = predicted_df, aes(x=V1,y=predicted), color = "yellow", alpha = 0.5, height = 0, width = 0.5)+
  geom_smooth(data = test_df, aes(x = momage, y = ppvt), color = "purple", method = "lm")+
  geom_smooth(data = predicted_df, aes(x=V1,y=predicted), color = "yellow", method = "lm"))

## graph many

(ggplot()
  +geom_point(data=test_df, aes(x=momage,y=ppvt), color = "blue", alpha = 0.3)
  + geom_point(data=train_df, aes(x=momage,y=ppvt), color = "green", alpha = 0.3))


##beauty
beauty_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/beauty/ProfEvaltnsBeautyPublic.csv")
beauty_model = lm(courseevaluation~btystdave, beauty_df)
controls = c("tenured","minority","age", "female")
tenured_model = lm(courseevaluation~btystdave+tenured+btystdave*tenured, beauty_df)
tenured_naive = lm(courseevaluation~btystdave+tenured, beauty_df)
summary(tenured_model)
summary(tenured_naive)
##0.05 for beauty, -0.017 for tenured, 0.15 for both
##for those who are not tenured, beauty matters at a lesser rate
##

minority_model = lm(courseevaluation~btystdave+minority+btystdave*minority, beauty_df)
summary(minority_model)
##0.16 for beauty, -0.14 for minority, -0.25 for both
##for minorities, beauty is a negative contributer(0.25-0.16)
age_model = lm(courseevaluation~btystdave+age+btystdave*age, beauty_df)
age_naive = lm(courseevaluation~btystdave+age, beauty_df)
summary(age_naive)
female_model = lm(courseevaluation~btystdave+female+btystdave*female, beauty_df)
summary(female_model)

##graphing females
(ggplot() + 
  geom_jitter(data = beauty_df[beauty_df$female==0,], aes(x = btystdave , y = courseevaluation), color = "green", alpha = 0.7, width = 0.5, height = 0) + 
  geom_jitter(data = beauty_df[beauty_df$female==1,], aes(x = btystdave , y = courseevaluation), color = "purple", alpha = 0.3, width = 0.5, height = 0) + 
  geom_smooth(data = beauty_df[beauty_df$female==0,], method = "lm", aes(x = btystdave, y=courseevaluation), color = "green", alpha = 0.2) +
  geom_smooth(data = beauty_df[beauty_df$female==1,], method = "lm", aes(x = btystdave, y=courseevaluation), color = "purple", alpha = 0.2)
)


##graphing tenured
(ggplot() + 
  geom_jitter(data = beauty_df[beauty_df$tenured==0,], aes(x = btystdave , y = courseevaluation), color = "green", alpha = 0.7, width = 0.5, height = 0) + 
  geom_jitter(data = beauty_df[beauty_df$tenured==1,], aes(x = btystdave , y = courseevaluation), color = "purple", alpha = 0.3, width = 0.5, height = 0) + 
  geom_smooth(data = beauty_df[beauty_df$tenured==0,], method = "lm", aes(x = btystdave, y=courseevaluation), color = "green", alpha = 0.2) +
  geom_smooth(data = beauty_df[beauty_df$tenured==1,], method = "lm", aes(x = btystdave, y=courseevaluation), color = "purple", alpha = 0.2)
)

##graphing minorities
(ggplot() + 
  geom_jitter(data = beauty_df[beauty_df$minority==0,], aes(x = btystdave , y = courseevaluation), color = "green", alpha = 0.7, width = 0.5, height = 0) + 
  geom_jitter(data = beauty_df[beauty_df$minority==1,], aes(x = btystdave , y = courseevaluation), color = "purple", alpha = 0.3, width = 0.5, height = 0) + 
  geom_smooth(data = beauty_df[beauty_df$minority==0,], method = "lm", aes(x = btystdave, y=courseevaluation), color = "green", alpha = 0.2) +
  geom_smooth(data = beauty_df[beauty_df$minority==1,], method = "lm", aes(x = btystdave, y=courseevaluation), color = "purple", alpha = 0.2)
)

##

