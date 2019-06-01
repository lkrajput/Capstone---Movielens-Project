##
## title: "CAPSTONE-PH125.9x"
## author: "Laxmansingh Rajput"
## date: "5/31/2019"
##

##
##   Libraries used
##
library(tidyverse)
library(kableExtra)
library(knitr)
library(ggplot2)

## OBJECTIVES

## Creating a movie recommendation system using the MovieLens dataset. 
## Train a machine learning algorithm using the inputs in one subset to predict movie ratings in the validation set.
## The movie rating predictions will be compared to the true ratings in the validation set using RMSE 
##    and the report outputs the RMSE.

## This project is being completed in R version 3.6.0 (_Planting of a Tree_)

###################################
# Read edx set and validation set #
###################################

set.seed(1, 
         sample.kind = "Rounding") # if using R 3.6.0
##
##  Retrieve the training and the validation data
##
edx <- readRDS("Data/edx.rds")
validation <- readRDS("Data/validation.rds")
##
##  Get the frequency for all ratings
##
tab_rating <- data.frame(table(edx$rating))
names(tab_rating) <- c("Rating", 
                       "Frequency")

##
##  Top 5 rated movies
##

top_5rated_movies <- edx %>% 
                        group_by(title) %>% 
                        tally() %>% 
                        arrange(., desc(n)) %>% 
                        head(.,5)

names(top_5rated_movies) <- c("Title", 
                              "Frequency")

##
##  Rating and frequency
##
ratings_in_desc <- edx %>% 
                      group_by(rating) %>% 
                      tally() %>% 
                      arrange(., desc(n))

names(ratings_in_desc) <- c("Ratings", 
                            "Frequency")

DESCRIPTION <- c("Observations", 
                 "Variables", 
                 "Unique User IDs", 
                 "Unique Movie IDs", 
                 "Unique Ratings", 
                 "Mean Rating")

VALUES <- rbind(formatC(as.numeric(dim(edx)[1]),format="d"),
                    formatC(as.numeric(dim(edx)[2]),format="d"),
                    formatC(as.numeric(n_distinct(edx$userId)),format="d"),
                    formatC(as.numeric(n_distinct(edx$movieId)),format="d"),
                    formatC(as.numeric(n_distinct(edx$rating)),format="d"),
                    formatC(as.numeric(mean(edx$rating)),format="f",digits=6))
##
##  Create table with the dataset information
##
dataset_information <- data.frame(DESCRIPTION, 
                                  VALUES)

#### FUNCTIONS TO BE USED
##
##  Root mean square error function
##
RMSE <- function(true_ratings, predicted_ratings){
        sqrt(mean((true_ratings - predicted_ratings)^2))
}

##
##  Predicted rating based on the movie and user biases
##

predict_rating <- function(reg_param){
  ##
  ##  movie bias based on movieID (group_by) 
  ##
  movie_bias <- edx %>% 
    group_by(movieId) %>%
    summarize(movie_bias = sum(rating - mu)/(n()+reg_param))  # movie bias 
  ##
  ##  user bias based on userID (group_by) 
  ##     movie bias also being taken into consideration
  ##
  user_bias <- edx %>% 
    left_join(movie_bias, 
              by="movieId") %>%
    group_by(userId) %>%
    summarize(user_bias = sum(rating - movie_bias - mu)/(n()+reg_param)) # User bias
  ##
  ##  return the prediction from the function
  ##
  return(validation %>% 
             left_join(movie_bias, 
                       by = "movieId") %>%
             left_join(user_bias, 
                       by = "userId") %>%
             mutate(pred = mu + movie_bias + user_bias) %>%     # predictions for the movies
             pull(pred))                                        # extract only the prediction 
  
}
#### EXPLORE THE DATASET

glimpse(edx)
head(edx, 6)

## 1. The list of the variables.

kable(t(variable.names(edx)), 
      caption = "Variable Names") %>%  
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, latex_options = "hold_position")
## 2. Other information regarding the dataset - Observations, variables, unique values, and mean rating

options(scipen=999)

kable(dataset_information, align = c("l", "r"),
      caption = "Variable Information") %>%  
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, 
                latex_options = "hold_position")

ggplot(tab_rating, aes(Rating, Frequency)) + 
  geom_bar(stat = "identity", fill = "blue")  +
  ggtitle("Rating Information")

##  3. Rating information 

kable(t(tab_rating), align=rep('c', 7),
      caption = "Rating Information") %>%  
  kable_styling(bootstrap_options = c("striped", 
                                      "hover"), 
                full_width = F, 
                latex_options = "hold_position") %>%
  footnote(general = "   Overall Summary of the ratings. ",
           number = c("     Rating ranges from 0.5 to 5 in 0.5 steps "))

kable(ratings_in_desc, 
    caption = "Rating in reverse order") %>%  
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, latex_options = "hold_position")

## 4. Five most rated movies

kable(top_5rated_movies, 
      caption = "Top 5 rated movies") %>%  
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, 
                latex_options = "hold_position")

## 5. edx dataset

kable(select(head(edx, 20),1,2,3,5,6),
      caption = "edx Dataset (Snapshot)") %>%  
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, 
                latex_options = c("scale_down","hold_position"))

## 6. validation dataset

kable(select(head(validation, 20),1,2,3,5,6),
      caption = "validation Dataset (Snapshot)") %>%  
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, 
                latex_options = c("scale_down","hold_position"))
##
##   CROSS-VALIDATION TO PICK A REGULARIZATION PARAMETER (LAMBDA)
##             MOVIE + USER EFFECT
##    Use sapply function to return a vector and then retrieve the 
##      parameter that gives the minimum RMSE
##         1. Calculate the regularized bias for each movies
##         2. Calculate the regularized bias for each user
##         3. Predict ratings based on both regularized movies & user biases
##         4. Compare against the validation (test) set and compute the RMSE

##
##   use cross-validation to pick a λ:
##
##   using the value between 0 and 15 in 0.25 increments
##
lambdas <- seq(0, 15, 0.25)

mu <- mean(edx$rating)   # compute the mean to be used in the function
##
##  Find the RMSE for the regularized value and prediction
##
rmses <- sapply(lambdas, function(reg_param){

  predicted_ratings <- predict_rating(reg_param)          # both of the functions could be combined,
  return(RMSE(predicted_ratings, validation$rating))      # but I kept it separate for readability

})
##
##  Plot the regularization parameter against the RMSE
##
qplot(lambdas, 
      rmses)  
##
##  RESULTS SECTION
##     Best parameter
##     Regularization parameters and RMSE

paste("Best regularization parameter: ", 
      lambdas[which.min(rmses)])

lambdas_rmses <- data.frame(lambdas, rmses)
names(lambdas_rmses) <- c("LAMBDAS", "RMSE")

library(gridExtra)
library(grid)

x <- lambdas_rmses[1:31,]
y <- lambdas_rmses[32:61,]
##
##   Using grid arrange so that we can display all the information
##
grid.arrange(tableGrob(x), 
             tableGrob(y), 
             ncol = 2,
             top = textGrob("Regularization parameters and RMSE",
                            x = 0, # starts far left
                            y = 0.5, # experiment with vertical placement
                            just = "left", # left-aligned,
                            gp = gpar(fontsize = 18) # bigger font
             ))

##  CONCLUSION SECTION
##    Best RMSE
##    Some predicted ratings 
##
##   Select the best regularization parameter to get the prediction
##       and compute RMSE based on the prediction against the validation dataset
##
reg_param <- lambdas[which.min(rmses)]  
predicted_ratings <- predict_rating(reg_param)
    
paste("Best RMSE: ", RMSE(predicted_ratings, validation$rating))

## PREDICTED RATING (SNAPSHOT)

kable(select((cbind(validation, predicted_ratings) %>% head(20)), 1,2,3,5,7),
      caption = "PREDICTED RATINGS (Snapshot)") %>%  
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, latex_options = "hold_position")
##
##  Information About The Current R Session
##

sessionInfo()
