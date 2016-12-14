library('dplyr')
library('ggplot2')
library("e1071")
library("MASS")
library("kernlab")

# Simulate Data

class_pred = function(fa_a, fa_p){
  tt = 0
  tf = 0
  ft = 0
  ff = 0
  lv1 = levels(fa_a)[1]
  lv2 = levels(fa_a)[2]
  for (i in 1:length(fa_a)){
    if (fa_a[i]==lv1 && fa_p[i]==lv1){
      tt = tt+1
    } else if (fa_a[i]==lv1 && fa_p[i]==lv2){
      tf = tf+1
    } else if (fa_a[i]==lv2 && fa_p[i]==lv1){
      ft = ft+1
    } else if (fa_a[i]==lv2 && fa_p[i]==lv2){
      ff = ff+1
    }
  }
  desc_str = paste(lv1,"is considered as True and", lv2, "is considered as False")
  col_n = c("True/True", "True/False", "False/True", "False/False")
  row_n = c("Raw Counts", "Proportion")
  desc_vec = c(tt,tf,ft,ff)
  t = tt+tf
  f = ft+ff
  prop_vec = c(tt/t, tf/t, ft/f, ff/f)
  desc_df = rbind.data.frame(desc_vec, prop_vec)
  rownames(desc_df) = row_n
  colnames(desc_df) = col_n
  return (list(desc_str, desc_df))
}

lin_pair = function(m,b,label){
  obj = l_make(m,b,label)
  x = obj[1]
  y = obj[2]
  while (y<0||y>1){
    obj = l_make(m,b,label)
    x = obj[1]
    y = obj[2]
  }
  return (c(x,y))
}

l_make = function(m,b,label){
  x = runif(1,0,1)
  y_true = m*x+b
  if (label==1){
    if (y_true<1){
      y = runif(1, y_true,1)
    } else {y=-1}
  } else {
    if (y_true>0){
      y = runif(1, 0, y_true)
    } else {y=-1}
  }
  return(c(x,y))
}

lin_pair_gen = function(m,b,label,n){
  lin_df = as.data.frame(matrix(0, nrow=n, ncol=2))
  for (i in 1:n){
    l=lin_pair(m,b,label)
    lin_df[i,1]=l[[1]]
    lin_df[i,2]=l[[2]]
  }
  return (lin_df)
}

# Simulate quadratic data

quad_pair = function(a,b,c,label){
  obj = q_make(a,b,c,label)
  x = obj[1]
  y = obj[2]
  while (y<0||y>1){
    obj = q_make(a,b,c,label)
    x = obj[1]
    y = obj[2]
  }
  return (c(x,y))
}

q_make = function(a,b,c,label){
  x = runif(1,0,1)
  y_true = a*(x-b)^2+c
  if (label==1){
    if (y_true<1){
      y = runif(1, y_true,1)
    } else {y=-1}
  } else {
    if (y_true>0){
      y = runif(1, 0, y_true)
    } else {y=-1}
  }
  return(c(x,y))
}

quad_pair_gen = function(a,b,c,label,n){
  quad_df = as.data.frame(matrix(0, nrow=n, ncol=2))
  for (i in 1:n){
    l=quad_pair(a,b,c,label)
    quad_df[i,1]=l[[1]]
    quad_df[i,2]=l[[2]]
  }
  return (quad_df)
}

# 
# Multivariate distribution (not sure if this works)
mvnorm_pair = function(mu,cov){
  m = mvrnorm(1, mu=mu, Sigma=cov)
  c1 = m[[1]]>1
  c2 = m[[2]]>1
  c3 = m[[1]]<0
  c4 = m[[2]]<0
  while (c1||c2||c3||c4){
    m = mvrnorm(1, mu=mu, Sigma=cov)
    c1 = m[[1]]>1
    c2 = m[[2]]>1
    c3 = m[[1]]<0
    c4 = m[[2]]<0
  }
  return(m)
}

m_pair_gen = function(mu,cov,n){
  m_df = as.data.frame(matrix(0, nrow=n, ncol=2))
  for (i in 1:n){
    l=mvnorm_pair(mu,cov)
    m_df[i,1]=l[[1]]
    m_df[i,2]=l[[2]]
  }
  return (m_df)
}

# SVM on linear
lin_1 = lin_pair_gen(1.5,0.2,1,1000)
lin_1$labels = 1
colnames(lin_1) = c("x","y")
lin_2 = lin_pair_gen(1.5,0.05,-1,1000)
lin_2$labels = -1
colnames(lin_2) = c("x","y")

lin_df = rbind(lin_1, lin_2)
colnames(lin_df) = c("x","y","label")
lin_df$label = factor(lin_df$label)
ggplot(data=lin_df, aes(x=x, y=y))+geom_point()


lin_svm = svm(label~., data=lin_df, kernel="linear")
plot(lin_svm, lin_df)
lin_svm1 = svm(label~., data=lin_df, kernel="linear", cost=1)
plot(lin_svm1, lin_df)
lin_svm2 = svm(label~., data=lin_df, kernel="linear", cost=5)
plot(lin_svm2, lin_df)

# SVM on qudratic


quad_df_1 = quad_pair_gen(3,0.5,0.55,1,20)
quad_df_1$label = 1
colnames(quad_df_1) = c("x","y","label")
quad_df_2 = quad_pair_gen(3,0.5,0.4,-1,20)
quad_df_2$label = -1
colnames(quad_df_2) = c("x","y","label")
quad_df = rbind(quad_df_1, quad_df_2)
colnames(quad_df) = c("x","y","label")
quad_df$label = factor(quad_df$label)
ggplot(data=quad_df, aes(x=x,y=y))+geom_point()

quad_svm = svm(label~., data=quad_df, kernel="linear", cost=0.1)
plot(quad_svm, quad_df)
quad_svm1 = svm(label~., data=quad_df, kernel="linear", cost=1)
plot(quad_svm1, quad_df)
quad_svm2 = svm(label~., data=quad_df, kernel="linear", cost=5)
plot(quad_svm2, quad_df)
#does extremely poorly, shifts slightly with C

# SVM on multivariate
cov_1 = matrix(c(0.2,0.1,0.1,0.2), ncol=2)
m_1 = c(0.2,0.8)
m_df_1 = m_pair_gen(m_1, cov_1,200)
m_df_1$label = 1
colnames(m_df_1) = c("x","y","label")
ggplot(data=m_df_1, aes(x=x,y=y))+geom_point()

m_2 = c(0.05, 0.2)
m_df_2 = m_pair_gen(m_1, cov_1,200)
m_df_2$label = -1
colnames(m_df_2) = c("x","y","label")
ggplot(data=m_df_2, aes(x=x,y=y))+geom_point()

m_df = rbind(m_df_1, m_df_2)
colnames(m_df)=c("x","y","label")
m_df$label = factor(m_df$label)
ggplot(data=m_df, aes(x=x,y=y))+geom_point()
#definitely inseparable
m_svm = svm(label~., data=m_df, kernel="linear", cost=0.1)
plot(m_svm, m_df)
m_svm1 = svm(label~., data=m_df, kernel="linear", cost=1)
plot(m_svm1, m_df)
m_svm2 = svm(label~., data=m_df, kernel="linear", cost=5)
plot(m_svm2, m_df)
#cannot separate at all

# Radial SVM
quad_rsvm = svm(label~., data=quad_df, kernel="radial", cost=0.1, sigma=0.01)
plot(quad_rsvm, quad_df)
quad_rsvm1 = svm(label~., data=quad_df, kernel="radial", cost=1)
plot(quad_rsvm1, quad_df)
quad_rsvm2 = svm(label~., data=quad_df, kernel="radial", cost=5, sigma=0.1)
plot(quad_rsvm2, quad_df)

lin_rsvm = svm(label~., data=lin_df, kernel="radial")
plot(lin_rsvm, lin_df)
lin_rsvm1 = svm(label~., data=lin_df, kernel="radial", cost=1)
plot(lin_rsvm1, lin_df)
lin_rsvm2 = svm(label~., data=lin_df, kernel="radial", cost=5)
plot(lin_rsvm2, lin_df)


# On dating
spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speedDating-aggregated.csv")
activities_df = dplyr::select(spd_df, sports:yoga, gender)
activities_df$gender = factor(activities_df$gender)
tune_range = list(sigma=2^(seq(from=-4,to=4,length.out=10)),
                  cost=2^(seq(from=-4,to=4,length.out=10)))
tune_control = tune.control(sampling="fix")
act_t_svm = tune(svm, gender~., data=activities_df, ranges=tune_range, tunecontrol=tune_control, kernel="radial")
tuned_svm = predict(act_t_svm$best.model, dplyr::select(activities_df, -gender))
class_pred(activities_df$gender, tuned_svm)
# 0.72 True/True, 0.77 False/False (radial svm)

act_t_lsvm = tune(svm, gender~., data=activities_df, ranges=tune_range, tunecontrol=tune_control, kernel="linear")
tuned_lsvm = predict(act_t_lsvm$best.model, dplyr::select(activities_df, -gender))
class_pred(activities_df$gender, tuned_lsvm)
# 0.75 True/True, 0.76 False/False