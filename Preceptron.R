library('dplyr')
library('ggplot2')

# Simulate Data

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

lin_1 = lin_pair_gen(1.5,0.2,1,1000)
lin_1$labels = 1
colnames(lin_1) = c("x","y")
lin_2 = lin_pair_gen(1.5,0.05,-1,1000)
lin_2$labels = -1
colnames(lin_2) = c("x","y")
lin_df = rbind(lin_1, lin_2)
colnames(lin_df) = c("x","y")
ggplot(data=lin_df, aes(x=x, y=y))+geom_point()
lin_df$arb = 1

# Dot product of two numeric vectors
dot = function(x,y){
  dot_sum = 0
  for (i in 1:length(x)){
    dot_sum = dot_sum+x[i]*y[i]
  }
  return(dot_sum)
}

# xs as matrix of data
# y as vector of labels
# w as a learning rate
# niter as a number of iterations
perceptron = function(xs,y,w,rate,niter=nrow(xs)){
  r_ind = sample(1:nrow(xs), niter, replace=FALSE)
  weight = w
  for (i in r_ind){
    x = xs[i,]
    d = dot(x,weight)
    c = sign(d)
    if (y[i]==1 && c==-1){
      weight = weight+rate*x
    } else if (y[i]==-1 && c==1){
      weight = weight-rate*x
    }
  }
  return (weight)
}

# errr... here we go?
lin_pruned = select(lin_df, x,y,arb)
lin_class = lin_df[[3]]
w0 = rep(0.1, ncol(lin_pruned))
w1 = perceptron(lin_pruned, lin_class, w0, 1)
w2 = perceptron(lin_pruned, lin_class, w1, 1)
w3 = perceptron(lin_pruned, lin_class, w2, 1)
w4 = perceptron(lin_pruned, lin_class, w3, 1)
ab = function(w){
  slope = -w$x/w$y
  intercept = -w$arb/w$y
  return(c(slope=slope, intercept=intercept))
}
ggplot(data=lin_df, aes(x=x, y=y))+geom_point()+geom_abline(slope=ab(w1)[1], intercept=ab(w1)[2], color="Red")+geom_abline(slope=ab(w2)[1], intercept=ab(w2)[2], color="Orange")

ggplot()+geom_abline(slope=ab(w1)[1], intercept=ab(w1)[2])

perceptron_conv = function(xs,y,w,rate){
  weight = w*0
  alt_weight = w
  counter = 0
  while (!identical(alt_weight,weight)){
    weight=alt_weight
    alt_weight=perceptron(xs,y,weight,rate)
    counter=counter+1
  }
  return (list(counter, weight))
}

perceptron_conv(lin_pruned, lin_class, w0, 1)
