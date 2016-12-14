library("dplyr")
library("klaR")

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

spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speedDating-aggregated.csv")
activities_df = dplyr::select(spd_df, sports:yoga, gender)

activities_0 = dplyr::filter(activities_df, gender==0)
activities_1 = dplyr::filter(activities_df, gender==1)

means_0 = lapply(activities_0, mean)
means_1 = lapply(activities_1, mean)

cov_0 = cov(dplyr::select(activities_0, -gender))
cov_1 = cov(dplyr::select(activities_1, -gender))

act_lda = lda(gender~., data=activities_df)
act_qda = qda(gender~., data=activities_df)

p_act_lda = predict(act_lda, select(activities_df, sports:yoga))
p_act_qda = predict(act_qda, select(activities_df, sports:yoga))

class_pred(factor(activities_df$gender), p_act_lda$class)
# True/True is at 0.74 and False/False at 0.76
class_pred(factor(activities_df$gender), p_act_qda$class)
# True/True is at 0.79 and False/False at 0.75


parts = c(0.1, 0.15, 0.2, 0.25, 0.5, 0.75, 1)
df_anal = as.data.frame(matrix(0, nrow=length(parts), ncol=8))
colnames(df_anal) = c("ldaTT", "ldaFF", "qdaTT", "qdaFF", "all_ldaTT", "all_ldaFF", "all_qdaTT", "all_qdaFF")
rownames(df_anal) = parts
for (i in 1:length(parts)){
  part_df = dplyr::sample_frac(activities_df, parts[i])
  ld = lda(gender~., data=part_df)
  qd = qda(gender~., data=part_df)
  p_ld = predict(ld, select(part_df, sports:yoga))
  p_qd = predict(qd, select(part_df, sports:yoga))
  pa_ld = predict(ld, select(activities_df, sports:yoga))
  pa_qd = predict(qd, select(activities_df, sports:yoga))
  c_ld = class_pred(factor(part_df$gender), p_ld$class)
  c_qd = class_pred(factor(part_df$gender), p_qd$class)
  ca_ld = class_pred(factor(activities_df$gender), pa_ld$class)
  ca_qd = class_pred(factor(activities_df$gender), pa_qd$class)
  df_anal[i,1]=c_ld[[2]][2,1]
  df_anal[i,2]=c_ld[[2]][2,4]
  df_anal[i,3]=c_qd[[2]][2,1]
  df_anal[i,4]=c_qd[[2]][2,4]
  df_anal[i,5]=ca_ld[[2]][2,1]
  df_anal[i,6]=ca_ld[[2]][2,4]
  df_anal[i,7]=ca_qd[[2]][2,1]
  df_anal[i,8]=ca_qd[[2]][2,4]
}

df_anal$parts = parts
(ggplot(data=df_anal, aes(x=parts))+
  geom_line(aes(y=ldaTT), color="Green")+geom_line(aes(y=ldaFF), color="Green")+
  geom_line(aes(y=qdaTT), color="Purple")+geom_line(aes(y=qdaFF), color="Purple")+
  geom_line(aes(y=all_ldaTT), color="Orange")+geom_line(aes(y=all_ldaFF), color="Orange")+
  geom_line(aes(y=all_qdaTT), color="Blue")+geom_line(aes(y=all_qdaFF), color="Blue")
)
