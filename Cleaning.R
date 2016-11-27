## Final Project
## Data Cleaning

workingdir = "/Users/elainelai/OneDrive/Wharton/Class/STAT701-DataMining/FinalProject"
library('ggplot2')

datacleaning <- function(wd){
  setwd(wd)
  moviedata <- read.csv("movie_metadata.csv", na.strings=c("", "NA"))
  # dim = 5043, 28
  
  # delete rows that use NA for potentially important columns
  moviedata <- moviedata[!(is.na(moviedata$director_name)), ]
  moviedata <- moviedata[!(is.na(moviedata$title_year)), ]
  moviedata <- moviedata[!(is.na(moviedata$actor_1_name)), ]
  moviedata <- moviedata[!(is.na(moviedata$actor_1_facebook_likes)), ]
  moviedata <- moviedata[!(is.na(moviedata$actor_2_name)), ]
  moviedata <- moviedata[!(is.na(moviedata$actor_2_facebook_likes)), ]
  moviedata <- moviedata[!(is.na(moviedata$actor_3_name)), ]
  moviedata <- moviedata[!(is.na(moviedata$actor_3_facebook_likes)), ]
  moviedata <- moviedata[!(is.na(moviedata$budget)), ]
  moviedata <- moviedata[!(is.na(moviedata$content_rating)), ]
  
  # unlikely predictor but useful - changed NA to the mean value or color because it's probabably color
  moviedata$duration[is.na(moviedata$duration)] <- round(mean(moviedata$duration, na.rm = TRUE))
  moviedata$facenumber_in_poster[is.na(moviedata$facenumber_in_poster)] <- round(mean(moviedata$facenumber_in_poster, na.rm = TRUE))
  
  # These movies are in color and English so applied those attributes
  moviedata$color[is.na(moviedata$color)] <- "Color"
  moviedata$language[is.na(moviedata$language)] <- "English"
  moviedata$num_critic_for_reviews[is.na(moviedata$num_critic_for_reviews)] <- 0.0
  
  # unlikely predictor is deleted because there are too many NAs
  moviedata$aspect_ratio <- NULL
  
  moviedata$genres.sub <- substr(moviedata$genres, 0, 13)
  moviedata$genres.sub <- as.factor(moviedata$genres.sub)
  
  post.cleaning.data <- moviedata
  post.cleaning.data$plot_keywords <- NULL
  post.cleaning.data$movie_imdb_link <- NULL
  
  # Write 2 files, one for regular analysis and one for text mining if desired
  write.csv(post.cleaning.data, "post-cleaning-data.csv")
  write.csv(moviedata, "text-mining-data.csv")
  
  return("data is clean")
}



ggplot(moviedata, aes(genres.sub)) + geom_bar()
## Next create a histogram with genres.sub data
datacleaning(workingdir)
