a = matrix(1:120,nrow=10)
a
as.numeric(a)

flip_matrix = function(ma){
  num_vec = as.numeric(ma)
  matrix(num_vec, nrow=(ncol(ma)))
}
flip_matrix(a)


df = data.frame(matrix(1:100,nrow=10))
# df[5, 5] = NA
# df[6, 6] = NA

na_df = is.na(df)
View(na_df)
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

divDf(df,2)
View(t(df))

df_sym = rbind(c(0,0,1),c(0,1,0),c(1,0,0))

min_matrix = function(n,m){
  start = matrix(0,nrow=n,ncol=m)
  for (i in 1:n){
    for (j in 1:m){
      start[i,j]=min(i,j)
    }
  }
  return (start)
}

matrix_sym = function(ma){
  return(all(ma==t(ma)))
}

matrix_sym(df_sym)
matrix_sym(df)

trace = function(ma){
  return(sum(diag(ma)))
}
## conserved in addition (if addition is possible)
## conserved when scaled

mystery = function(x){
  matrix(c(cos(x), -sin(x), sin(x), cos(x)), nrow=2)
}

mystery(2)

## looks like a... rotation matrix?

mat2 = function(li){
  new = matrix(0,nrow=2,ncol=2)
  for (c in 1:4){
    n = unlist(li)[c]
    i = floor((c+1)/2)
    t = c%%2
    if (t==0){j=2} else {j=t}
    print ("-n")
    print (n)
    print ("-i")
    print (i)
    print ("-j")
    print (j)
    new[i,j] = n
  }

  return (new)
}

test_mat1 = matrix(1:10,ncol=2)
test_mat2 = matrix(1:10,ncol=5)
true_multi = test_mat1%*%test_mat2


matrix_multi = function(mat1,mat2){
  r1 = nrow(mat1)
  r2 = nrow(mat2)
  c1 = ncol(mat1)
  c2 = ncol(mat2)
  if (c1!=r2){return ("CAN'T DO THIS")}
  product_mat = matrix(0,nrow=r1,ncol=c2)
  for (i in 1:r1){
    for (j in 1:c2){
      running_sum = 0
      print ("i") 
      print(i)
      print ("j") 
      print(j)
      for (k in 1:r2){
        print ("k")
        print (k)
        A = mat1[i,k]
        B = mat2[k,j]
        running_sum = running_sum+A*B
      }
      print (running_sum)
      product_mat[i,j] = running_sum
    }
  }
  return (product_mat)
}


rot_test = list(c(1,1),c(1,4),c(6,1))
rot_mat = function(li,c){
  df = data.frame()
  for (i in li){
    j = t(mystery(c*pi)%*%i)
    print (j)
    df = rbind(df,j)
  }
  colnames(df) = c("hori","verti")
  ggplot(df,aes(x=hori,y=verti))+geom_point()
  return (df)
}
aw = rot_mat(rot_test,0)
ggplot(aw,aes(x=hori,y=verti))+geom_point()

set.seed(2)
m1 = matrix(sample(seq_len(16),16),nrow=4)
m2 = matrix(sample(seq_len(16),16),nrow=4)
m3 = matrix(sample(seq_len(16),16),nrow=4)

trace(m1%*%m2)
trace(m2%*%m1)
trace(m1%*%m2%*%m3)
trace(m1%*%m3%*%m2)
trace(m2%*%m1%*%m3)
trace(m2%*%m3%*%m1)
trace(m3%*%m1%*%m2)
trace(m3%*%m2%*%m1)