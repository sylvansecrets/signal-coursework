library(ggplot2)
library(dplyr)
library(pROC)

spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speeddating-aggregated.csv")
act_gender_df = select(spd_df, sports:yoga,gender)
career_gender_df = select(spd_df, career_c,gender)

actual = spd_df$gender

##Predict gender in terms of activities
act_mod = glm(gender~.,family="binomial",data=act_gender_df)
coef(act_mod)
act_fit = fitted(act_mod)
act_r = roc(act_fit,actual)
plot(act_r)

##Predict gender in terms of career
career_mod = glm(gender~.,family="binomial",data=career_gender_df)
career_fit = fitted(career_mod)
career_r = roc(career_fit,actual)
plot(career_r)


##Predict gender based on academia and business/finance
binary_career_df = dplyr::filter(career_gender_df, career_c==7 | career_c==4)
busi_aca_actual = (dplyr::filter(spd_df, career_c==7|career_c==4))$gender
binary_career_df = binary_career_df[!is.na(rowSums(binary_career_df)),]
busi_aca_mod = glm(gender~., family="binomial", data=binary_career_df)
busi_aca_fit = fitted(busi_aca_mod)
busi_aca_r = roc(busi_aca_fit, busi_aca_actual)
plot(busi_aca_r)


##Predict gender based on Asian or Caucasian
race_df = dplyr::select(spd_df, race,gender)
binary_race_df = dplyr::filter(race_df, race==2|race==4)
cau_asian_actual = (dplyr::filter(spd_df, race==2|race==4))$gender
cau_asian_mod = glm(gender~.,family='binomial',data=binary_race_df)
cau_asian_fit = fitted(cau_asian_mod)
cau_asian_r = roc(cau_asian_fit,cau_asian_actual)
plot(cau_asian_r)
