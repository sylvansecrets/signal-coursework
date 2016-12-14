## Diaspora Survey
library("ggplot2")
library("readr")
library("dplyr")
library("psych")
library("corrplot")
library("dummies")
## Load data
survey_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/2016_lw_survey_public_release_3.csv")

## Clean the data

# "Yes" to research consent
# non-NA age
# non-bank  Depression value

survey_df = dplyr::filter(survey_df, ResearchConsent=="Yes" & Depression!="" & !(is.na(Age)))

# Drop Calibration, CharityDonations, and Peak columns
survey_df = survey_df[,-c(
                      grep("^Calibration", colnames(survey_df)),
                      grep("^CharityDonations", colnames(survey_df)),
                      grep("^Peak", colnames(survey_df)))]


# Filter out written-in answers (many levels), and single levels
# Determine unique answers, and then look at whether they can be coerced into integers

delete_ind = numeric(0)
for (i in 1:ncol(survey_df)){
  current = survey_df[[i]]
  if (class(current)=="factor"&& length(unique(current))>15){
    delete_ind = c(delete_ind, i)
  }
}

survey_df = survey_df[,-delete_ind]

# Fixing factors

factor_NA_char = function(fac){
  new_f = fac
  new_f[fac=="N/A" | fac==""] = NA
  return(addNA(factor(new_f)))
}

for (i in 1:ncol(survey_df)){
  if (class(survey_df[[i]])=="factor"){
    survey_df[[i]]=factor_NA_char(survey_df[[i]])
  }
}

# Deleting numeric outliers
# Alternatively, deleting numeric results more than 5 standard deviations out

del_outlier = function(vec){
  q = quantile(vec, na.rm=TRUE)
  iqr = q[4]-q[2]
  cleaned = vec
  low = vec<(q[2]-2*iqr)
  high =vec<(q[2]-2*iqr)
  cleaned[low] = NA
  cleaned[high] = NA
  return(cleaned)
}

for (i in 1:ncol(survey_df)){
  if (class(survey_df[[i]])%in%(c("numeric","integer"))){
    survey_df[[i]]=del_outlier(survey_df[[i]])
  }
}

# log transform Income, IncomeCharityPortion and XriskCharity, and Singularity Year
survey_df$Income = log(survey_df$Income+1)
survey_df$IncomeCharityPortion = log(survey_df$IncomeCharityPortion+1)
survey_df$XriskCharity = log(survey_df$XriskCharity+1)
survey_df$SingularityYear = log(survey_df$SingularityYear-2015)

# Yes/No to 1/0
bin_factor = function(fac){
  new_fac = fac
  if (class(fac)=="factor" && length(unique(fac))==3){
    new_fac[new_fac=="No"] = 0
    new_fac[new_fac=="Yes"] = 1
    return (as.numeric(as.character(new_fac)))
  } else {
    return (fac)
  }

}

for (i in 1:ncol(survey_df)){
  survey_df[[i]]=bin_factor(survey_df[[i]])
}

# Changing levels
convert_level = function(fac){
  l = length(unique(fac))
  if (sum(is.na(fac))==0){l=l+1}
  new_fac = as.character(fac)
  if (l==5){
    new_fac[new_fac=="No"]=0
    new_fac[grepl("^No", new_fac)& nchar(new_fac)>2] = 1
    new_fac[new_fac=="Yes"]=3
    new_fac[grep("^Yes", new_fac)& nchar(new_fac)>3] = 2
  } else if (l==4){
    new_fac[new_fac=="No"]=0
    new_fac[nchar(new_fac)>3]=1
    new_fac[new_fac=="Yes"]=2
  }
  return(as.numeric(new_fac))
}

cn = colnames(survey_df)
replace_list = c(match("RokoKnowledge", cn):match("SubstanceUseWonder", cn), grep("Successor", cn), grep("BlogsRead", cn), 
                 grep("Stories", cn), grep("SQ001", cn), grep("Genetic", cn))

for (i in replace_list){
  survey_df[[i]]=convert_level(survey_df[[i]])
}

##/End cleaning data

## Factor Analysis

# Create correlation matrix
num_df = survey_df[sapply(survey_df, class)%in%c("numeric", "integer")]
num_df = select(num_df, -Age, -Income, -IncomeCharityPortion, -XriskCharity)
na_lim=0.98
na_del = function(w, lim=na_lim){
  ratio = sum(is.na(w))/length(w)
  return (ratio<na_lim)
}
num_df = num_df[sapply(num_df, na_del)]
cor_mat = cor(num_df, use="pairwise.complete.obs")

#replace NAs with 0s
replace_matrix_NA = function(ma){
  replacement = ma
  for (i in 1:nrow(ma)){
    ind = is.na(ma[i,])
    replacement[i,ind]=0
  }
  return(replacement)
}

cor_mat = replace_matrix_NA(cor_mat)

VSS.scree(cor_mat)
# Try 15 factors

fac_num = fa(cor_mat, nfactors=8, covar=TRUE, rotate="oblimin")
corrplot(fac_num$loadings, is.corr=FALSE)

# Look at the factor names
factor_names = c("Immortality/MIRI", "Religious", "SAT/IQ", "ACT/IQ", "Aliens", "ADHD", "Substance", "Personality Disorder")

# Let's look at the ability of these factors to predict XRiskType
risk_types = dummy(survey_df$XRiskType)
risk_prominence = sort((colSums(risk_types))/(nrow(risk_types)-sum(is.na(survey_df$XRiskType))), decreasing=TRUE)
# The candidates that exceeded 10% response rates are bioengineered pandemics, nuclear war, unfriendly AI, and environmental collapse
# Interesting to know that LW members believe that our extinction is more likely to be by our own hands
risk_targets = as.data.frame(risk_types[,(colSums(risk_types))/(nrow(risk_types)-sum(is.na(survey_df$XRiskType)))>0.1])
risk_targets$XRiskTypeNA = NULL

# Use logistic regression to predict each Xrisk using factor scores

#Impute NAs
imputed_df = num_df
for (n in 1:ncol(num_df)){
  imputed_df[[n]][is.na(imputed_df[[n]])] = mean(imputed_df[[n]], na.rm=TRUE)
}

# Create factor scores
loadings = fac_num$loadings
colnames(loadings) = factor_names
fac_scores = as.matrix(imputed_df)%*%loadings
scaled_scores = scale(fac_scores)
