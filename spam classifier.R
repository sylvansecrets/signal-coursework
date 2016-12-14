library('dplyr')
library('ngram')


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


#Naive Bayes Spam Classifier

#Read all files in the training folder and test folder
train_folder = "C:/Users/User/Documents/GitHub/Signal-Data-Science/CSDMC2010_SPAM/TRAINING"
train_emails = list.files(path=train_folder, pattern="eml", full.names=TRUE)
test_folder = "C:/Users/User/Documents/GitHub/Signal-Data-Science/CSDMC2010_SPAM/TESTING"
test_emails = list.files(path=train_folder, pattern="eml", full.names=TRUE)

email_li = lapply(train_emails, scan, what=character(), quote=NULL, sep="")
email_li = lapply(email_li, unique)

test_li = lapply(test_emails, scan, what=character(), quote=NULL, sep="")

#Load the indicators
train_ind = read.table("C:/Users/User/Documents/GitHub/Signal-Data-Science/CSDMC2010_SPAM/SPAMTrain.label", header=FALSE, sep=" ")

#Separate into spam and non-spam
n_ham = sum(train_ind[,1])
n_spam = nrow(train_ind)-n_ham
ham_ind = which(train_ind[,1]==1)
spam_ind = which(train_ind[,1]==0)

#Table each
ham_table = as.data.frame(table(unlist(email_li[ham_ind]))/n_ham)
spam_table = as.data.frame(table(unlist(email_li[spam_ind]))/n_spam)

#Throw out non-matches
joined_sham = inner_join(ham_table, spam_table, by="Var1")
colnames(joined_sham) = c("word", "ham_freq", "spam_freq")

#Get probability of the matches
joined_sham$p_ham = joined_sham$ham_freq/(joined_sham$ham_freq+joined_sham$spam_freq)
joined_sham$p_spam = 1-joined_sham$p_ham


#all-in-one
word_prob = function(ws, p_df){
  p_li = vector(mode="numeric", length=length(ws))
  for (i in 1:length(ws)){
    p_li[i]=retrieve_p(ws[i],p_df)
  }
  return(p_li)
}

retrieve_p = function(w, p_df){
  m = match(w, p_df$word)
  if (is.na(m)){
    return(0.5)
  } else {
    return(p_df$p_spam[m])
  }
}

#Detect spam
detect_spam = function(ws=test_str, p_df=joined_sham){
  # filtered_ws = word_filter(unique(ws), p_df$word)
  # p_vec = spam_prob(filtered_ws, p_df)
  p_vec = word_prob(unique(ws), p_df)
  s = sum(log(1-p_vec)-log(p_vec))
  end= 1/(1+exp(s))
  return (1-end)
}

test_str = unlist(strsplit("Hey Richard! I haven't seen you for three months. How are you doing these days?", " "))

#Try this on the entire set of test emails
test_predict = vector(mode="logical", length=length(email_li))
for (i in 1:length(email_li)){
  test_predict[i]=detect_spam(email_li[[i]])
  print (i)
}

###########################
#Failure due to high run time. Could not be fixed through profiling.
#Largest call time required was from matching
#Try next time: using environments

overall_df = cbind(train_ind, test_predict)
overall_df$half = round(test_predict,0)
class_pred(as.factor(overall_df[,1]), as.factor(overall_df$half))
# TRUE/TRUE 0.98, FALSE/FALSE 0.95