##Galton Family Data Analysis
install.packages("HistData")
install.packages("dplyr")
library("HistData")
library("dplyr")
library("ggplot2")
df = GaltonFamilies

##Gender from Factor
gendernum = c()
for (i in df$gender){
  if (i == "Male"){
    gendernum = c(gendernum, 1)
  } else {
    gendernum = c(gendernum,0)
  }
}
  
df$GenderNum = gendernum

names(df)

##Aggregate data
#arrange(df,family)

##Plotting

mother_vec = c()
father_vec = c()
mid_vec = c()

df = group_by(df,family)
summarize(df,child_vec = mean(childHeight))
child_vec