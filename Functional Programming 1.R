df_test = data.frame(c(1:5),c(letters[1:5]))

unlist(lapply(mtcars,class))

normalizer = function(df)
data.frame(lapply(df, 
       function(x){
         x-mean(x)/sd(x)}))



num_check = function(df){
lapply(df, function(x)
  is.numeric(x))}

stan_num = function(df){
  normalizer(df[as.logical(num_check(df))]
  )
}


my_func = function(x,f){
  l = length(x)
  answer = vector(mode="list", l)
  for (i in 1:l){
    answer[[i]]=f(x[[i]])
  }
  return(answer)
}
# 
# my_lapply = function(L,f)
# {
#   answer = list(rep(NULL,length(L)))
#   
#   unlisted = unlist(L)
#   
#   for(i in c(1:length(L)))
#   {
#     answer[[i]] = f(unlisted[[i]])
#   }
#   
#   return(answer)
#   
# }


library("tictoc")
t1 = tic()
NROW = 5000
NCOL = 500
x = vector(mode='list', NROW)

for (i in seq(NROW)){
  x[[i]] = runif(NCOL)
}
t2 = toc()

t2 = tic()


col_namer = function(df){
  l=ncol(df)
  for (i in seq(l)){
    colnames(df)[i]=paste(colnames(df)[i],i,seq="_")
    
  }
  return (colnames(df))
}
  
  
prev_sub = function(df){
  l = length(df)
  new_df = df
  for (i in 2:l){
    new_df[[i]]=new_df[[i]]-df[[i-1]]
  }
  return (new_df)
}

##Old Code
##Returns a vector of numbers divisible by k
## dataframe, integer -> vector(integers)
divDf = function(df,k){
  n_vec = c()
  for (i in 1:ncol(df)){
    for (j in 1:nrow(df)){
      now = df[i,j]
      print(now)
      if (now%%k == 0){
        n_vec = c(now,n_vec)
      }
    }
    
  }
  return (n_vec)
}

##New Code

##Supplemental exercises
sum(sapply(
  10:100, function(x){
    (x^3)+4*(i^2)
  }
))

sum(sapply(
  1:25, function(x){
    (2^x)/x+(3^x)/(x^2)
  }
))

m = matrix(1:9, nrow=3)
apply(m,c(1,2),mean)
apply(m, MARGIN=c(1,2), FUN=is.na)


# ##OUTER is broken
# ##min i,j
# min_matrix = function(n,m){
#   return(outer(1:n,1:m,FUN=min))
# }
# 
# 



