#
#----------------------------------#
# Project: MovieLens               #
# Name   : Sunil Kumar Pasupuletti #
# Date   : 15/Dec/21               #
#----------------------------------#
#

# Code from the edx assignment to prepare the movielens dataset

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("recosystem")


library(tidyverse)
library(caret)
library(data.table)
library(knitr)
library(recosystem)

set.seed(1, sample.kind="Rounding")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

####

#Exploring the movielens dataset

#fields in the dataset
str(movielens)

#sample data from the dataset
paste("Sample observations from the movielens dataset:")
  movielens %>% head 

#no. of unique Movies in the dataset
length(unique(movielens$movieId))

#no. of unique users in the dataset
length(unique(movielens$userId))

#no. of reviews by user (Top 10)
top10_u <- movielens %>% group_by(userId) %>% summarize(reviews=n()) %>% arrange(desc(reviews)) %>% top_n(10)
top10_u %>% mutate(userId=reorder(userId,desc(reviews))) %>% ggplot(aes(userId,reviews)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#no. of reviews by movie (Top 10)
top10_m <- movielens %>% group_by(title) %>% summarize(reviews=n()) %>% top_n(10)
top10_m %>% mutate(title=reorder(title,desc(reviews)))%>% ggplot(aes(title,reviews)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#best rated movies(with >10k reviews) (Top 10)
top10_m_r <- movielens %>% group_by(title) %>% filter(n()>10000) %>% summarize(reviews=n(),Rating=mean(rating)) %>% top_n(10)
top10_m_r %>% arrange(desc(Rating))
#top10_m_r %>% mutate(title=reorder(title,desc(rat)))%>% ggplot(aes(title,rat)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

####


# Validation set will be 10% of MovieLens data
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed) #@ rm is to clear objects from memory


#########@

#Split edx into train and test set to apply the models/methods

# create test and train sets on edx
#set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)  
train_set <- edx[-test_index,]
temp <- edx[test_index,]
# nrow(test_set)  #1800012
# nrow(train_set) #7200043

# ensuring movieID and userID are in test as well 
test_set <- temp %>%  #@ this basically ensures same moveID and userID in test and train
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

###

#method#1 (Making a random prediction)
#set.seed(1, sample.kind = "Rounding")
predictions <- sample(c(1,2,3,4,5),nrow(test_set),replace=TRUE)    
mthd1<-RMSE(test_set$rating, predictions) #1.17783
rmse_results <- tibble(method = "Just a hunch", RMSE = mthd1)
rmse_results %>% kable(caption="Results")
#

#method#2 (prediction based on the average rating)
mu_hat <- mean(train_set$rating)
mthd2 <- RMSE(test_set$rating, mu_hat)  #@ RMSE(true ratings, predicted ratings)
rmse_results <- bind_rows(rmse_results,tibble(method = "All about Average", RMSE = mthd2))
rmse_results %>% kable(caption="Results")

#
#method#3 (prediction based on the movie effect)
mu <- mean(train_set$rating) 

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("white"),fill=I("dark blue"))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
#predicted_ratings
mthd3 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,  
                          tibble(method="The Movie Effect", RMSE = mthd3))
rmse_results %>% kable(caption="Results")

#
#method#4 (user bias)

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "white",fill="dark blue")

#user bias
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#prediction
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

mthd4 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="The Movie + User Effect",  
                                     RMSE = mthd4 ))
rmse_results %>% kable(caption="Results") #@ big improvement on the RMSE



#method#5 (genres effect)

train_set %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_g)) + 
  geom_histogram(bins = 30, color = "white",fill="dark blue")

# top 10 genres by average rating
train_set %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating)) %>%
  arrange(desc(b_g)) %>% top_n(10) 

# worst 10 genres by average rating
train_set %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating)) %>%
  arrange((b_g)) %>% top_n(-10) 

genre_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

mthd5 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="The Movie + User + Genre Effect",  
                                     RMSE = mthd5 ))
rmse_results %>% kable(caption="Results") #not much of an improvement

##

#method6 - Regularization

#Tuning using the penalty factor,lambda and applying regularization on the movie+user+genre effects

lambdas <- seq(0, 10, 0.25)
rmse <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_g <- train_set %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g,by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmse)  

#lambda <- lambdas[which.min(rmse)]
  
mthd6<-min(rmse)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="The Movie + User + Genre Effect (Regularized)",  
                                     RMSE = mthd6))
rmse_results %>% kable(caption="Results")


###



#method7  - matrix factorization/recosystems 
# Converting to reco input formats 
test_reco  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
train_reco <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

# reco object creation
r <- Reco()
attributes(r)
r$train(train_reco)

# prediction
y_hat_reco <-  r$predict(test_reco, out_memory())
y_hat_reco %>% head
test_set$rating %>% head

# rmse results
mthd6 <- RMSE(test_set$rating, y_hat_reco)
rmse_results <- bind_rows(rmse_results, 
                    tibble(method = "Matrix Factorization/Recosystem", 
                           RMSE = mthd6 ))

rmse_results %>% kable(caption="Results")


####Final validation
## Predicting on the Validation set


# edx and validation sets converted to reco inputs 
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
valid_reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))
# create model and train 
r2 <-  Reco()
r2$train(edx_reco) 

# predictions on the validation set
predicted_ratings <-  r2$predict(valid_reco, out_memory())

Finalmthd <- RMSE(predicted_ratings,validation$rating)

#rmse results
rmse_results <- bind_rows(rmse_results,  
                          tibble(method="Final Recommendation on Validation using recosystem", RMSE = Finalmthd))
rmse_results %>% kable(caption="Results")


#@ according to the final prediction, best and worst movies

#10 best movies
tibble(title=validation$title,rating=predicted_ratings) %>% arrange(-rating) %>% group_by(title) %>% select(title) %>% head(10) %>% kable(caption = "Best 10 Movies")


#10 worst movies
tibble(title=validation$title,rating=predicted_ratings) %>% arrange(rating) %>% group_by(title) %>% select(title) %>% head(10)%>% kable(caption = "Worst 10 Movies")


####End-of-Code