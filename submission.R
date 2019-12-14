################################################################
# CODE GENERATING MOVIE RATING MODEL
#
# This script requires that the script loading data provided
# with the assignment has been run, and edx and validation data
# frames are in the global environment
################################################################
library(tidyverse)
library(caret)
################################################################
# Goal of the model is to meet certain targets with RMSE defined as follows:
################################################################

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
################################################################
# Model will focus on predicting movie ratings based on movie
# variance, user variance, and user-genre variance
# Therefore we will remove timestamp and title columns
# for the sake of optimizing memory use.
################################################################

edx <- edx %>% select(movieId, userId, genres, rating)
validation <- validation %>%select(movieId, userId, genres, rating)

################################################################
# BASELINE MODEL:
#
# All movies rated at mean
################################################################

mu_hat <- mean(edx$rating)
predicted_ratings <- replicate(nrow(validation), mu_hat)
rmse_1 <- RMSE(validation$rating, predicted_ratings)
cat("Baseline RMSE (on mean value only) = ", rmse_1, "\n")
rm(predicted_ratings)

# incorparate baseline model into data set - replace rating column with residual
edx <- edx %>% mutate(residual = rating - mu_hat) %>%
  select(movieId, userId, genres, residual)

################################################################
# MOVIE EFFECT MODEL:
#
# Incorporate variance in movies as b_i
################################################################

#Calculate average residual for each movie as b_i

movie_effect <- edx %>% group_by(movieId) %>%
  summarize(b_i = mean(residual))

#recalculate RMSE

predicted_ratings <- validation %>%
  left_join(movie_effect, by = 'movieId') %>%
  mutate(prediction = mu_hat + b_i) %>%
  pull(prediction)
rmse_2 <- RMSE(validation$rating, predicted_ratings)
cat("RMSE with basic movie effect = ", rmse_2, "\n")
rm(predicted_ratings)

#Regularize the movie effect to mitigate extreme values with parameter lambda_m

#First create train (90%) and test (10%) sets to cross-validate the lambda parameter
# with reset seed for reproducibility

set.seed(1956, sample.kind = "Rounding")
test_index <- createDataPartition(edx$residual, p = 0.1, list = FALSE)
test_set <- edx[test_index,]
train_set <- edx[-test_index,]

#Free up memory
rm(test_index, edx)

#Test possible lambdas from 0 to 5 (range determined from preliminary exploratory work)
test_range <- seq(0, 5, 0.5)

# Named function is used rather than in-line, for better memory management

test_movie_lambda <- function(lambda) {
  #calculate movie effect for selected lambda value
  movie_effect <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(residual)/(n() + lambda))
  #estimate RMSE from test_set
  test_set %>% inner_join(movie_effect, by = 'movieId') %>%
    summarise(rmse = sqrt(mean((b_i - residual)^2)))%>%
    pull(rmse)
}

lambda_m_set <- sapply(test_range, test_movie_lambda)

#Plot the results

ggplot() +
  geom_point(aes(x = test_range, y = lambda_m_set)) +
  geom_smooth(aes(x = test_range, y = lambda_m_set)) +
  xlab("lambda_m") +
  ylab("Estimated RMSE")

# Optimum value of lambda_m chosen as 2 on the basis of this plot

lambda_m <- 2

# Generate data frame "movie_effect" for use in the model

movie_effect <- rbind(train_set, test_set) %>%
  group_by(movieId) %>%
  summarize(b_i = sum(residual)/(n() + lambda_m))

# recalculate rmse

predicted_ratings <- validation %>%
  inner_join(movie_effect, by = 'movieId') %>%
  mutate(prediction = mu_hat + b_i) %>%
  pull(prediction)
rmse_3 <- RMSE(validation$rating, predicted_ratings)
cat("RMSE with regularised movie effect = ", rmse_3, "\n")
rm(predicted_ratings)

# incorparate movie effect into data set - recalculate residual column

train_set <- train_set %>%
  inner_join(movie_effect, by = 'movieId') %>%
  mutate(residual = residual - b_i)
test_set <- test_set %>%
  inner_join(movie_effect, by = 'movieId') %>%
  mutate(residual = residual - b_i)

################################################################
# USER EFFECT MODEL:
#
# Incorporate variance in users as b_u
################################################################

#Calculate average residual for each user as b_u

user_effect<- rbind(test_set, train_set) %>%
  group_by(userId) %>%
  summarize(b_u = mean(residual))

#recalculate RMSE

predicted_ratings <- validation %>%
  left_join(movie_effect,by='movieId') %>%
  left_join(user_effect,by='userId') %>%
  mutate(prediction=mu_hat + b_i + b_u) %>%
  pull(prediction)
rmse_4 <- RMSE(validation$rating, predicted_ratings)
cat("RMSE with basic user effect = ", rmse_4, "\n")
rm(predicted_ratings)

#Regularize the user effect to mitigate extreme values with parameter lambda_u
#
#We will use the same train and test sets that were used to regularize the movie effect

#Test possible lambdas from 0 to 10 (range determined from preliminary exploratory work)
test_range <- seq(0, 10, 1) 

test_user_lambda <- function(lambda) {
  #calculate user effect for selected lambda value
  user_effect <- train_set %>%
    group_by(userId) %>%
    summarize(b_u = sum(residual)/(n() + lambda))
  #estimate RMSE from test_set
  test_set %>% inner_join(user_effect, by = 'userId') %>%
    summarise(rmse=sqrt(mean((b_u - residual)^2)))%>%
    pull(rmse)
}

lambda_u_set <- sapply(test_range, test_user_lambda)

#Plot the results

ggplot() +
  geom_point(aes(x = test_range, y = lambda_u_set)) +
  geom_smooth(aes(x = test_range, y = lambda_u_set)) +
  xlab("lambda_u") +
  ylab("Estimated RMSE")

# Optimum value of lambda_u chosen as 5 on the basis of this plot

lambda_u <- 5

# Generate data frame "user_effect" for use in the model

user_effect <- rbind(train_set, test_set) %>%
  group_by(userId) %>%
  summarize(b_u = sum(residual)/(n() + lambda_u))

# recalculate RMSE

predicted_ratings <- validation %>%
  inner_join(movie_effect, by = 'movieId') %>%
  inner_join(user_effect, by = 'userId') %>%
  mutate(prediction = mu_hat + b_i + b_u) %>%
  pull(prediction)
rmse_5 <- RMSE(validation$rating, predicted_ratings)
cat("RMSE with regularised movie and user effect =", rmse_5)
rm(predicted_ratings)

# incorparate user effect into data set - recalculate residual column

train_set <- train_set %>%
  inner_join(user_effect, by = 'userId') %>%
  mutate(residual = residual - b_u)
test_set <- test_set %>%
  inner_join(user_effect, by = 'userId') %>%
  mutate(residual = residual - b_u)

################################################################
# GENRE EFFECT MODEL:
#
# Incorporate variance in users-genres as b_g
################################################################

#Calculate average residual for each user/genre combination

genre_effect<- rbind(train_set, test_set) %>%
  group_by(userId, genres) %>%
  summarize(b_g = mean(residual))

#recalculate RMSE

predicted_ratings <- validation %>%
  left_join(movie_effect,by='movieId') %>%
  left_join(user_effect,by='userId') %>%
  left_join(genre_effect, by = c('userId', 'genres')) %>%
  replace_na(list(b_g = 0)) %>% #in case specific user-genres combination is unique to validation set
  mutate(prediction = mu_hat + b_i + b_u + b_g) %>%
  pull(prediction)
rmse_6 <- RMSE(validation$rating, predicted_ratings)
cat("RMSE with basic genre effect = ", rmse_6, "\n")
rm(predicted_ratings, genre_effect)

#Regularize the genre effect to mitigate extreme values with parameter lambda_g
#
#We will use the same train and test sets that were used to regularize the movie and user effects

#Test possible lambdas from 6 to 12 (range determined from preliminary exploratory work)

test_range <- seq(6, 12, 1) 

# THE FOLLOWING ITERATION WILL TAKE A FEW MINUTES TO RUN.
# IF IT CAUSES A CRASH, ONE OPTION AT THIS POINT IS TO 
# SAVE AN IMAGE OF THE GLOBAL ENVIRONMENTAND START A FRESH 
# SESSION FROM THIS POINT WITH RELOADED IMAGE.

test_genre_lambda <- function(lambda) {
  #calculate genre effect for selected lambda value
  genre_effect <- train_set %>%
    group_by(userId, genres) %>%
    summarize(b_g = sum(residual)/(n() + lambda))
  #estimate RMSE from test_set
  r<-test_set %>% left_join(genre_effect, by = c('userId', 'genres')) %>%
    replace_na(list(b_g = 0)) %>%
    summarise(rmse = sqrt(mean((b_g - residual)^2)))%>%
    pull(rmse)
  rm(genre_effect)
  r  
}

lambda_g_set <- sapply(test_range, test_genre_lambda)

#Plot the results

ggplot() +
  geom_point(aes(x = test_range, y = lambda_g_set)) +
  geom_smooth(aes(x = test_range, y = lambda_g_set)) +
  xlab("lambda_g") +
  ylab("Estimated RMSE")

# Optimum value of lambda_u chosen as 9 on the basis of this plot

lambda_g <- 9

# Generate data frame "genre_effect" for use in the model

genre_effect <- rbind(train_set, test_set) %>%
  group_by(userId, genres) %>%
  summarize(b_g = sum(residual)/(n() + lambda_g))

# recalculate RMSE

predicted_ratings <- validation %>%
  left_join(movie_effect, by = 'movieId') %>%
  left_join(user_effect, by = 'userId') %>%
  left_join(genre_effect, by = c("userId", "genres")) %>%
  replace_na(list(b_g = 0)) %>%
  mutate(prediction = mu_hat + b_i + b_u + b_g) %>%
  pull(prediction)
rmse_7 <- RMSE(validation$rating, predicted_ratings)
cat("RMSE with regularised movie, user and genre effects =", rmse_7, "\n")


