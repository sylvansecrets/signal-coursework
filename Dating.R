## What is the largest power, n, required to fit the data through every point?
## Answer: where N is the sample size, n=N; as each additional power allows for an additional 'bend' in the line (and gives the same degree of freedom)

library("dplyr")
## Trying set.seed()
set.seed(11)
runif(5)

spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speedDatingSimple.csv")
gender_0_df = dplyr::filter(spd_df, gender=="0")
gender_1_df = dplyr::filter(spd_df, gender=="1")

##Linear Regression of Attractiveness against Activities
activities_0 = select(gender_0_df,sports:yoga,attr_o)
lm_attr_0 = lm(attr_o ~., data=activities_0)
summary(lm_attr_0)

## random split
train_size = floor((nrow(activities_0))*0.5)
train_ind_0 = vector(mode="numeric",length=train_size)
train_df_0 = head(activities_0,train_size)
test_df_0 = tail(activities_0, nrow(activities_0)-train_size)
split_data = function(){
  train_ind_0 <<- sample(seq_len(nrow(activities_0)),train_size)
  train_df_0 <<- activities_0[train_ind_0,]
  test_df_0 <<- activities_0[-train_ind_0,]
}



##test_statistics
N_repeats = 1000
test_r2_vec = vector(mode="numeric",N_repeats)
train_r2_vec = vector(mode="numeric",N_repeats)

for (i in seq_len(N_repeats)){
  split_data()
  lm_train_0 = lm(attr_o~.,data=train_df_0)
  predicted = predict(lm_train_0,test_df_0)
  test_r_2 = (cor(predicted,test_df_0$attr_o))^2
  test_r2_vec[i]=test_r_2
  train_r2_vec[i]=summary(lm_train_0)$adj.r.squared
}

ggplot()+geom_point(aes(x=train_r2_vec,y=test_r2_vec))+ylim(c(0,0.2))

std_err = (sum((test_r2_vec)^2)/(N_repeats))^(0.5)

##arbitrary
