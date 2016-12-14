##Load Dating set
spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speedDatingSimple.csv")
gender_0_df = dplyr::filter(spd_df, gender=="0")
gender_1_df = dplyr::filter(spd_df, gender=="1")
activities_0 = select(gender_0_df,sports:yoga,attr_o)
ratings_0 = select(gender_0_df,attr_o:amb_o)
ratings_1 = select(gender_1_df,attr_o:amb_o)

##step function
f_0 = lm(attr_o~., data=ratings_0)
step_female_rating = step(f_0)
female_rate_df = data.frame(summary(step_female_rating)$coef[,1])


f_1 = lm(attr_o~., data=ratings_1)
step_male_rating = step(f_1)
male_rate_df = data.frame(summary(step_male_rating)$coef[,1])
##fun rating predicts attractiveness rating in both genders
##amibtion rating only predicts attactiveness in females