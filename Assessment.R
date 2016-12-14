##Self Assessment
##Started at 10:06
library("ggplot2")
##R and Probability

##Creating 10^6 instances of x
N = 1000000

set.seed(22)
X = runif(N)

##Creating the corresponding y
Y = vector(mode="numeric", N)

Y = lapply(X,
           function(w)
             runif(1,max=w))
Y = as.numeric(Y)

##Plotting
df1 = data.frame(X,Y)
colnames(df1) = c("X_vec", "Y_vec")
qplot(X_vec,Y_vec,data=df1)

# ggplot(df1, aes(x=X_vec, y=Y_vec))+geom_point()

##Binning
Y_bin = vector(mode="numeric",N)
Y_bin = round(Y,digits=2)

##
test_df = data.frame(1:20,c(1,5,1,5,1,5,1:14))
##
df2 = data.frame(X,Y_bin)
unique_Y_bin = sort(unique(Y_bin))
unique_N = length(unique_Y_bin)

mean_x = vector(mode="numeric", unique_N)

for (i in seq(unique_N)){
  mean_x[i] = 
    mean(unlist(
      df2[Y_bin==unique_Y_bin[i],][1]
    ))
}

bin_mean_df = data.frame(mean_x,unique_Y_bin)
colnames(bin_mean_df) = c("Mean_X","Y")

##Plotting
qplot(Mean_X,Y, data=bin_mean_df)
## This kind of makes sense, as the Mean_X and Y should be more or less linearly related (the bigger Mean_X, the larger the Y that results from the runif(X) call)

##Deterministic model
eqt_Y = function(y){
  return(
    (y-1)/(log(y))
  )
}

num_gen = 1000
Y_vector = runif(num_gen)
expected_Y = vector(mode="numeric",num_gen)

expected_Y = unlist(lapply(Y_vector,eqt_Y))
eqt_df = data.frame(Y_vector, expected_Y)
qplot(expected_Y, Y_vector)

##This looks similar to my expected results

##New dataframe
combined_df = bin_mean_df
combined_df$theory = unlist(lapply(combined_df$Y,eqt_Y))

##Plot new data frame
ggplot()+geom_point(aes(x=Mean_X,y=Y),data=combined_df)+geom_smooth(aes(x=theory,y=Y),data=combined_df)
#looks nicely matched


##############################################################################
##Part 2 : Data Analysis
install.packages("psych")
library("psych")
df=psych::msq

##number of NA in each column
d = dim(df)[2]
NA_df = vector(mode="numeric",d)
for (i in seq(d)){
  NA_df[i]=sum(is.na(df[i]))
}
names(NA_df)=colnames(df)
##order(df_sort,decreasing=TRUE)
##broken, returned to at 12:14
# order(df_sort[1,], decreasing=TRUE)
##temporarily give up
##misunderstood problem, restarted at 12:36
NA_df = NA_df/nrow(df)
NA_df[order(NA_df, decreasing=TRUE)]
##corrected at 12:45



##New Data Frame
df_sub = df[which(colnames(df)=="active"):which(colnames(df)=="scornful")]
df_sub = cbind(df_sub,Extraversion=df$Extraversion, Neuroticism=df$Neuroticism)
names(df_sub)

# for (i in ncol(df_sub)){
#   col_mean = mean(df_sub[[i]], na.rm=TRUE)
#   col = df_sub[i]
#   df_sub[is.na(df_sub[i]),i] = col_mean
# }
##also broken
##returned to at 12:20, left at 12:22
##returned to at 12:48
## test with any(is.na(df_sub[4]))
df_test = data.frame(c(21:25,NA,27:40),c(1:3,NA,5:8,NA,10:20))
Replace_NA_mean = function(df){
  for (i in seq(ncol(df))){
    col_mean = mean(df[[i]], na.rm=TRUE)
    df[is.na(df[i]),i]=col_mean
  }
  return (df)
}
Replace_NA_mean(df_sub)
##fixed at 13:03, problem in forgetting seq()

##Histograms
ggplot(df_sub, aes(x=Extraversion))+geom_histogram()
ggplot(df_sub, aes(x=Neuroticism))+geom_histogram()


##Density Plots
ggplot(df_sub, aes(x=Extraversion))+geom_density()
ggplot(df_sub, aes(x=Neuroticism))+geom_density()

##Scatter with fit
ggplot(df_sub, aes(x=Extraversion,y=Neuroticism))+geom_point()+geom_smooth(method="auto")

##Linear Regression
ex_nu = lm(Extraversion~Neuroticism, data=df_sub)
nu_ex = lm(Neuroticism~Extraversion, data=df_sub)
coef(ex_nu)[2]
coef(nu_ex)[2]
##It seems that Neuroticism and Extraversion are predictors of each other, and is weakly negative (with adjusted R squared at ~0.02)

##Linear Regression against other featuers
reg_vec_nu = vector(mode="numeric", ncol(df_sub)-2)

l = ncol(df_sub)-2

for (i in seq(l)){
  reg_vec_nu[i]=
  coef(
    lm(
      Neuroticism~df_sub[[i]],
      data=df_sub
    )
  )[2]
  names(reg_vec_nu)[i]=colnames(df_sub)[i]
}
##12:00 passed while working on above. Finished at 12:04

reg_vec_ex = vector(mode="numeric", ncol(df_sub)-2)

for (i in seq(l)){
  reg_vec_ex[i]=
    coef(
      lm(
        Extraversion~df_sub[[i]],
        data=df_sub
      )
    )[2]
  names(reg_vec_ex)[i]=colnames(df_sub)[i]
}

sort(abs(reg_vec_nu), decreasing=TRUE)[1:10]
sort(abs(reg_vec_ex), decreasing=TRUE)[1:10]
##also broken, why are values the same? time= 12:12
##Fixed, time=12:14

