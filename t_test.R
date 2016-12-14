library(readr)
library("dplyr")

rating_data = read.delim("C:/Users/User/Documents/GitHub/ml-100k/u.data", sep="\t", header=FALSE)
rating_data = rating_data[,-4]
colnames(rating_data)=c("user_id", "item_id", "rating")

user_data = read.delim("C:/Users/User/Documents/GitHub/ml-100k/u.user", sep="|", header=FALSE)
colnames(user_data) = c("user_id", "age", "gender", "occupation", "zipcode")

joined = inner_join(user_data, rating_data)

men_rate = dplyr::filter(joined, gender=="M")
women_rate = dplyr::filter(joined, gender=="F")

best_ps = c()
best_ids = c()
p_threshold = 0.05
for (i in unique(joined$item_id)){
  mens = dplyr::filter(men_rate, item_id == i)
  mens = mens$rating
  womens = dplyr::filter(women_rate, item_id==i)
  womens = womens$rating
  print (i)
  if (length(womens)<=1 || length(mens)<=1){
    print(paste("err....", i, "is broken"))
    next()
  }
  if (sd(womens)<0.01 && sd(mens)<=0.01){
    print(paste("err....", i, "is broken2"))
    next()
  }
  t = t.test(mens, womens, alternative="less")
  p = t$p.value
  if (p<p_threshold){
    best_ps = c(p, best_ps)
    best_ids = c(i, best_ids)
    print (paste("Best ID has changed to ", i, " with p value", p))
  }
}


movie_data = read.delim("C:/Users/User/Documents/GitHub/ml-100k/u.item", sep="|", header=FALSE, stringsAsFactors = FALSE)
names(movie_data)[1:2] = c("movie_id", "movie_title")


results = cbind.data.frame(best_titles = movie_data$movie_title[movie_data$movie_id %in% best_ids], best_ps)
results = results[order(best_ps),]

# These are the results
results



#---------------------------------------------------------
library("lattice")
singer_df = singer
head(singer_df)

unique(singer_df$voice.part)
singer_df$voice.part = as.character(singer_df$voice.part)

replace_vec = c("Soprano", "Tenor", "Alto", "Bass")
for (i in replace_vec){
  singer_df$voice.part[grepl(i, singer_df$voice.part)] = i
}

singer_df = group_by(singer_df, voice.part)
heights = mutate(singer_df, mean(height))

pairwise.t.test(singer_df$height, singer_df$voice.part, pool.sd=FALSE)
# differences are significant :)