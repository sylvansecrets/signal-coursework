library(ggplot2)

##Try to plot periodic data
x = runif(100000,0,2*pi)
y = (cos(x))^2+rnorm(n=100000, sd=0.2)+0.7*x+1.3*x^2+sin(x)+5*(cos(x))^4
##Try everything
df = data.frame(x,y)
ggplot(data=df, aes(x=x,y=y))+geom_point()+geom_smooth()
x_2 = x^2
x_3 = x^3
x_sin = sin(x)
# x_sin_2 = (sin(x))^2
x_sin_3 = (sin(x))^3
x_cos = cos(x)
x_cos_2 = (cos(x))^2
x_cos_3 = (cos(x))^3
x_cos_4 = (cos(x))^4
df_test = data.frame(x,y,x_2,x_3,x_sin,x_sin_3, x_cos,x_cos_2,x_cos_3, x_cos_4)
model = lm(y~., data=df_test)
p_cutoff = 0.001
coeffs = coef(summary(model))
ind = coeffs[,4]<p_cutoff
#significant predictors, as filtered by cutoff
sig_pred = names(ind)[ind==TRUE]


#rerun mod
df_final = data.frame(y)
for (i in seq_len(length(sig_pred))){
loc = match(sig_pred[i], names(df_test))
df_final = cbind(df_final, df_test[[loc]])
}
colnames(df_final)=c("y", sig_pred)
final_model = lm(y~., data=df_final)

summary(final_model)
# summary(model)

ggplot(aes(x=x, y=y), data=df)+geom_smooth()
