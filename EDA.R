## EDA

workingdir = "/Users/elainelai/OneDrive/Wharton/Class/STAT701-DataMining/FinalProject"
library('ggplot2')

EDA <- function(wd){
  setwd(wd)
  # Import cleaned dataset
  moviedata <- read.csv("post-cleaning-data.csv")

  
  
  # Take the dataset "chol" to be plotted, pass the "AGE" 
  # column from the "chol" dataset as values on the x-axis 
  # and compute a histogram of this
  png('GrossHistogram.png', width = 800, height = 600)
  ggplot(data=moviedata, aes(moviedata$gross)) + 
    geom_histogram(breaks=seq(0, 80000000, by = 10000000),
                   color="red4",
                   fill="navy") +
    labs(title="Histogram for Gross Revenue") +
    labs(x="Gross Revenue", y="Count")
  dev.off()
  
  png("CastFBLikesvIMDBScore.png")
  ggplot(moviedata, aes(x=moviedata$imdb_score, y=moviedata$cast_total_facebook_likes)) +
    geom_point(shape=1, color="navy") +    # Use hollow circles
    geom_smooth(method=lm, color="red4") +
    ylim(c(0,150000)) +
    xlim(c(0,10))
  dev.off()

  png("GrossvIMDBScore.png")
  ggplot(moviedata, aes(x=moviedata$imdb_score, y=moviedata$gross)) +
    geom_point(shape=1, color="navy") +    # Use hollow circles
    geom_smooth(method=lm, color="red4") 
    #ylim(c(0,150000)) 
  dev.off()
  
  return("EDA Complete")

}


EDA(workingdir)
