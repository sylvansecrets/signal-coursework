# Mixture Models
library("ggplot2")
library("datasets")
library("mixtools")
library("mclust")

# Convenience function
print_clusters = function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein_df[labels == i, c( "RedMeat",
                                     "Fish", "Fr.Veg")])
  }
}

# Convenience function
hclust_plot = function(d, method,k){
  scaled = scale(d)
  distance = dist(scaled)
  uncut = hclust(distance, method=method)
  cut = cutree(uncut, k=k)
  return(
    clusplot(scaled, cut, color=TRUE, shade=TRUE, labels=2, lines=0)
  )
}

# Load dataset
df_faithful = faithful
scale_faithful = as.data.frame(scale(faithful))

# plot
ggplot(scale_faithful)+geom_histogram(aes(x=waiting), alpha=0.5, fill="black")+geom_histogram(aes(x=eruptions), alpha=0.5, fill="red")



# MULTIVARIATE MODELS
# plot log-likelihood and guassian density estimates vs. histogram
nme = normalmixEM(df_faithful$waiting)
plot(nme, density=TRUE)

# run a few times to look at variation
nme = normalmixEM(df_faithful$waiting)
summary(nme)
# comp 1    comp 2
# lambda  0.639113  0.360887
# mu     80.091102 54.614907
# sigma   5.867708  5.871255
# loglik at estimate:  -1034 

# Try 2: numbers fairly close but swapped... number of iterations changed

# run a few times with k=3 to look at variation
rme = normalmixEM(df_faithful$waiting, k = 3)
plot(nme, density=TRUE)
summary(nme)
# jumped from ~30 iterations to 287, didn't converge 2nd time, 767 on third
# gave no third component
# otherwise the samish when it does converge

# semi-parametric models
# looks about right with n = 2, b = 3
n=20
b=0.5
semi_model = spEMsymloc(df_faithful$waiting, mu0=n, bw = b)
plot(semi_model, sub = paste("b =", b, "n =", n))

# Add outliers and compare plots
alt_wait = c(0,200, -5, df_faithful$waiting)
alt_nme = normalmixEM(alt_wait, k=2)
plot(alt_nme, which=2)
alt_semi = spEMsymloc(alt_wait, mu0=2, bw=3)
plot(alt_semi)

# Scatter eruptions and waiting times
ggplot(data=df_faithful)+geom_point(aes(x= waiting, y=eruptions))
# There looks to be two clusteres

# 
multi_lust = Mclust(scale_faithful)
plot(multi_lust)

# Try this on proteins
setwd("C:/Users/User/Documents/GitHub/Signal-Data-Science")
protein_df = read.delim("~/GitHub/Signal-Data-Science/protein.txt")

multi_protein = Mclust(scale(protein_df))
plot(multi_protein)
# it's got four components!

pc = prcomp(scale(protein_df))
qplot(1:9, pc$sdev)
multi_pc = Mclust(scale(pc$x[,1:2]))
plot(multi_pc)
#PCA and then multi leads to readable results

# Non-parameteric
np_faith = npEM(scale(df_faithful), mu0=2, bw=.3)
np_protein = npEM(scale(protein_df), mu0=2, bw=.3)
np_pc_protein = npEM(scale(pc$x[,1:2]), mu0=2, bw=0.5)


plot(np_faith)
plot(np_protein)
plot(np_pc_protein)

silenced = cbind(pc$x[,1:2],np_protein$posteriors)
silenced = as.data.frame(silenced)
colnames(silenced)[3:4] = c("G1", "G2")

library("tidyr")
test = gather(silenced, "Group", "GroupValue", G1)
test = gather(test, "PC", "PCvalue", PC1:PC2)
ggplot(test, aes(x=GroupValue, y=PCvalue, color=Group, size=PC, shape=PC)) + geom_jitter(width=.3, height=.3)
# ?????????