##Spam

##Libraries
library(readr)
library(caret)
library(tm)
library(glmnet)
library(quanteda)
library(SnowballC)
library(dplyr)
library(stringr)

##
spam_df = read_csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/spam-emails.csv")
spam_ind = read_csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/spam-emails-key.txt")

##
##Pre-processing
docs = VCorpus(VectorSource(spam_df$Message))
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, tolower)
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, stemDocument)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, PlainTextDocument)

##As Matrix
dtm = DocumentTermMatrix(docs)

##Filtering for <10 word frequency
freq_words = findFreqTerms(x=dtm, lowfreq=10, highfreq=Inf)
dtm2 = dtm[,freq_words]

##Inner Join
names(spam_ind)=names(spam_df[1])
indicator = vector(mode="numeric", nrow(spam_ind))
filenames = vector(mode="character", nrow(spam_ind))
for (i in seq_len(nrow(spam_ind))){
  st = str_split(spam_ind[i,1], " ")[[1]]
  indicator[i]=st[1]
  filenames[i]=st[2]
}
joiner = data.frame(cbind(indicator,filenames))
names(joiner) = c("indicator", names(spam_df[1]))
joined = dplyr::inner_join(spam_df[1],joiner)


lambda_list = sapply(seq(1,-6,length.out=50),
                     function(w){10^w})
param_grid = expand.grid(.alpha = 1,
                         .lambda = lambda_list)

control = trainControl(method = 'repeatedcv', number = 10, repeats=1, verboseIter = TRUE,
                       summaryFunction=twoClassSummary,classProbs=TRUE)

ind = factor(joined$indicator)

caret_fit = train(x=dtm2,
                  y=ind,
                  method="glmnet",
                  tuneGrid=param_grid,
                  metric = "ROC",
                  trControl=control)


