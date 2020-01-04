############################################
# Create edx set, validation set 
###########################################
# CODE AS PROVIDED WITH THE ASSIGNMENT
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)
################################################################
# ORIGINAL CODE FROM THIS POINT
################################################################
# Goal of the model is to meet certain targets with RMSE defined as follows:
################################################################

RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


################################################################
# DATA EXPLORATION:
#
# Review distibution per movie and per user
################################################################

n_users <- length(unique(edx$userId))
n_movies <- length(unique(edx$movieId))
n_genres <- length(unique(edx$genres))
sparsity <- round(100*(1 - length(edx$rating)/(n_users * n_movies)),1)
#
#Analyse distribution of ratings by user:
c_user_ratings <- edx %>% group_by(userId) %>% summarize(c = n()) %>% arrange(c)
max_user_ratings <- tail(c_user_ratings$c,1)
min_user_ratings <- head(c_user_ratings$c,1)
#Frequency of ratings per user to get modal value
fr_user_ratings <- c_user_ratings %>% group_by(c) %>% summarize(cf = n()) 
mode_user_ratings <- fr_user_ratings[which.max(fr_user_ratings$cf),"c"]
#Plot results
uf_plot <- fr_user_ratings %>% filter(c < 200) %>%
  ggplot() + geom_line(aes(x=c, y=cf)) + xlab("Number of ratings per user") +
  ylab("Frequency")
uf_plot
#
#Analyse distribution of ratings by movie
c_movie_ratings <- edx %>% group_by(movieId) %>% summarize(c = n()) %>% arrange(c)
max_movie_ratings <- tail(c_movie_ratings$c,1)
min_movie_ratings <- head(c_movie_ratings$c,1)
most_rated <- edx %>% filter(movieId == tail(c_movie_ratings$movieId,1)) %>%
  .$title %>% head(1)
least_rated <- edx %>% filter(movieId == head(c_movie_ratings$movieId,1)) %>%
  .$title %>% head(1)
#Frequency of ratings per movie to get modal value
fr_movie_ratings <- c_movie_ratings %>% group_by(c) %>% summarize(cf = n()) 
mode_movie_ratings <- fr_movie_ratings[which.max(fr_movie_ratings$cf),"c"] 
#Plot results
mf_plot <- fr_movie_ratings %>% filter(c < 200) %>%
  ggplot() + geom_line(aes(x=c, y=cf)) + xlab("Number of ratings per movie") +
  ylab("Frequency")
mf_plot

################################################################
# BASELINE MODEL:
#
# All movies rated at mean
################################################################

mu_hat <- mean(edx$rating)

################################################################
# MOVIE EFFECT MODEL:
#
# Incorporate bias in movies as b_i
# MODEL: y = mu_hat + b_i
################################################################

#Calculate average residual for each movie as b_i, with lambda parameter to regularise

#First create train (90%) and test (10%) sets to cross-validate the lambda parameter
# with reset seed for reproducibility

set.seed(1956, sample.kind = "Rounding")
test_index <- createDataPartition(edx$rating, p = 0.1, list = FALSE)
test_set <- edx[test_index,]
train_set <- edx[-test_index,]

#Free up memory
rm(test_index, edx)

#Test possible lambdas from 0 to 5 (range determined from preliminary exploratory work)
test_range_m <- seq(0, 5, 0.5)

# Named function is used rather than in-line, for better memory management

test_movie_lambda <- function(lambda) {
  #calculate movie bias for selected lambda value
  cat("lambda = ", lambda, "...\n") #Progress monitor
  movie_effect <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n() + lambda))
  #estimate RMSE from test_set
  test_set %>% inner_join(movie_effect, by = 'movieId') %>%
    summarise(rmse = sqrt(mean((rating - mu_hat - b_i)^2)))%>%
    pull(rmse)
}

#Apply to range of lambdas

lambda_m_set <- sapply(test_range_m, test_movie_lambda)
est_rmse_m_0 <- round(lambda_m_set[1],5) #Starting point rmse estimate
est_rmse_m_best <- round(min(lambda_m_set),5) #Optimised rmse estimate

#Plot the results

m_plot <- ggplot() +
  geom_point(aes(x = test_range_m, y = lambda_m_set)) +
  geom_smooth(aes(x = test_range_m, y = lambda_m_set)) +
  xlab("lambda_m") +
  ylab("Estimated RMSE")
m_plot

# Optimum value of lambda_m chosen as 2 on the basis of this plot

lambda_m <- test_range_m[which.min(lambda_m_set)]

# Generate data frame "movie_effect" from entire edx dataset for use in the model

movie_effect <- rbind(train_set, test_set) %>% #Reconstitute edx from train/test parts
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat)/(n() + lambda_m))

# Add b_i column into data sets
train_set <- train_set %>% inner_join(movie_effect, by = 'movieId')
test_set <- test_set %>% inner_join(movie_effect, by = 'movieId')

################################################################
# USER EFFECT MODEL:
#
# Incorporate bias in users as b_u with regulatisation
# MODEL: y = mu_hat + b_i + b_u
################################################################

#Regularize the user effect to mitigate extreme values with parameter lambda_u
#
#We will use the same train and test sets that were used to regularize the movie effect

#Test possible lambdas from 0 to 10 (range determined from preliminary exploratory work)
test_range_u <- seq(0, 10, 1) 

test_user_lambda <- function(lambda) {
  #calculate user bias for selected lambda value
  cat("lambda = ", lambda, "...\n") #Progress monitor
  user_effect <- train_set %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu_hat - b_i)/(n() + lambda))
  #estimate RMSE from test_set
  test_set %>% inner_join(user_effect, by = 'userId') %>%
    summarise(rmse=sqrt(mean((rating - mu_hat - b_i - b_u)^2)))%>%
    pull(rmse)
}

lambda_u_set <- sapply(test_range_u, test_user_lambda)
est_rmse_u_0 <- round(lambda_u_set[1],5) #Starting point rmse estimate
est_rmse_u_best <- round(min(lambda_u_set),5) #Optimised rmse estimate

#Plot the results

u_plot <- ggplot() +
  geom_point(aes(x = test_range_u, y = lambda_u_set)) +
  geom_smooth(aes(x = test_range_u, y = lambda_u_set)) +
  xlab("lambda_u") +
  ylab("Estimated RMSE")
u_plot

# Optimum value of lambda_u chosen as 5 on the basis of this plot

lambda_u <- test_range_u[which.min(lambda_u_set)]


# Generate data frame "user_effect" for use in the model

user_effect <- rbind(train_set, test_set) %>% #Reconstitute edx from train/test parts
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu_hat - b_i)/(n() + lambda_u))

# Add b_u column into data sets
train_set <- train_set %>% inner_join(user_effect, by = 'userId')
test_set <- test_set %>% inner_join(user_effect, by = 'userId')


################################################################
# GENRE EFFECT MODEL:
#
# Incorporate bias in users-genres as b_g with regularisation
# MODEL: y = mu_hat + b_i + b_u + b_g
################################################################

#Regularize the genre effect to mitigate extreme values with parameter lambda_g
#We will use the same train and test sets that were used to regularize the movie and user effects

#Test possible lambdas from 6 to 12 (range determined from preliminary exploratory work) 0 added in
# as baseline measure

test_range_g <- c(0, seq(6, 12, 1)) #lambda=0 added to determine baseline

# THE FOLLOWING ITERATION WILL TAKE A FEW MINUTES TO RUN.

test_genre_lambda <- function(lambda) {
  #calculate genre effect for selected lambda value
  cat("lambda = ", lambda, "...\n") #Progress monitor
  genre_effect <- train_set %>%
    group_by(userId, genres) %>% 
    summarize(b_g = sum(rating - mu_hat - b_i - b_u)/(n() + lambda))
  #estimate RMSE from test_set. Missing values of b_g replaced by zero
  r<-test_set %>% left_join(genre_effect, by = c('userId', 'genres')) %>%
    replace_na(list(b_g = 0)) %>% #Set b_g to zero if no information
    summarise(rmse = sqrt(mean((rating - mu_hat - b_i - b_u - b_g)^2)))%>%
    pull(rmse)
  rm(genre_effect) #Memory management
  r  
}
#PLEASE BE PATIENT WHILE THIS CODE BLOCK RUNS...
lambda_g_set <- sapply(test_range_g, test_genre_lambda)

est_rmse_g_0 <- round(lambda_g_set[1],5) #Starting point rmse estimate
est_rmse_g_best <- round(min(lambda_g_set),5) #Optimised rmse estimate

#Remove baseline entry before doing plot
lambda_g_set <- lambda_g_set[-1]
test_range_g <- test_range_g[-1]

#Plot the results

g_plot <- ggplot() +
  geom_point(aes(x = test_range_g, y = lambda_g_set)) +
  geom_smooth(aes(x = test_range_g, y = lambda_g_set)) +
  xlab("lambda_g") +
  ylab("Estimated RMSE")
g_plot

# Optimum value of lambda_u chosen as 9 on the basis of this plot

lambda_g <- test_range_g[which.min(lambda_g_set)]

# Generate data frame "genre_effect" for use in the model. This is a sparse
# matrix in long-form.

genre_effect <- rbind(train_set, test_set) %>% #Reconstitute edx from train/test parts
  group_by(userId, genres) %>%
  summarize(b_g = sum(rating - mu_hat - b_i - b_u)/(n() + lambda_g))

# b_g column is not added to train/test sets since no further use will be made
# of these datasets

#############################################################
# RESULTS - using validation set
#############################################################
#Baseline - prediciton = mu_hat
predicted_ratings <- replicate(nrow(validation), mu_hat)
rmse_1 <- RMSE(predicted_ratings, validation$rating)
cat("Baseline RMSE (on mean value only) = ", rmse_1, "\n")
#
#Include movie effect
predicted_ratings <- validation %>%
  inner_join(movie_effect, by = 'movieId') %>%
  mutate(prediction = mu_hat + b_i) %>%
  pull(prediction)
rmse_2 <- RMSE(predicted_ratings, validation$rating)
cat("RMSE with regularised movie effect = ", rmse_2, "\n")

#Include user effect
predicted_ratings <- validation %>%
  inner_join(movie_effect, by = 'movieId') %>%
  inner_join(user_effect, by = 'userId') %>%
  mutate(prediction = mu_hat + b_i + b_u) %>%
  pull(prediction)
rmse_3 <- RMSE(predicted_ratings, validation$rating)
cat("RMSE with regularised movie and user effect =", rmse_3)

#Include genre effect
predicted_ratings <- validation %>%
  inner_join(movie_effect, by = 'movieId') %>%
  inner_join(user_effect, by = 'userId') %>%
  left_join(genre_effect, by = c("userId", "genres")) %>%
  replace_na(list(b_g = 0)) %>%
  mutate(prediction = mu_hat + b_i + b_u + b_g) %>%
  pull(prediction)
rmse_4 <- RMSE(predicted_ratings, validation$rating)
cat("RMSE with regularised movie, user and genre effects =", rmse_4, "\n")
