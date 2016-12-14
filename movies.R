library("sqldf")
# See here if confused: http://www.r-bloggers.com/r-and-sqlite-part-1/

db = dbConnect(SQLite(), dbname="Movies.sqlite")

rating_data = read.delim("C:/Users/User/Documents/GitHub/ml-100k/u.data", sep="\t", header=FALSE)
user_data = read.delim("C:/Users/User/Documents/GitHub/ml-100k/u.user", sep="|", header=FALSE)
movie_data = read.delim("C:/Users/User/Documents/GitHub/ml-100k/u.item", sep="|", header=FALSE, stringsAsFactors = FALSE)

colnames(rating_data)=c("user_id", "item_id", "rating", "timestamp")
colnames(user_data) = c("user_id", "age", "gender", "occupation", "zipcode")
colnames(movie_data) = c("movie_id", "movie_title", "release_date", "video_release_date", "IMDB_url", "unknown", "Action", "Adventure",
                         "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                          "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",
                          "Thriller", "War", "Western")


# write into database
dbWriteTable(db, name="Rating", value=rating_data, row.names = FALSE)
dbWriteTable(db, name="User", value=user_data, row.names = FALSE)
dbWriteTable(db, name="Movie", value=movie_data, row.names = FALSE)

# test getting
testy = sqldf("SELECT * FROM Rating", dbname = "Movies.sqlite")

# remove the database
dbRemoveTable(db, "Rating")
dbRemoveTable(db, "User")
dbRemoveTable(db, "Movie")

# Don't forget to disconnect!
dbDisconnect(db)