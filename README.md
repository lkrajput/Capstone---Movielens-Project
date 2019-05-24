# Capstone---Movielens-Project
Capstone for the Data Science Certificate

This project is being completed in R version 3.6.0 (_Planting of a Tree_)

The submission for the MovieLens project will be three files: 
    1. A report in the form of an Rmd file, 
    2. A report in the form of a PDF document knit from your Rmd file, and 
    3. An Rmd file that generates the predicted movie ratings and calculates RMSE. 

The movie rating predictions will be compared to the true ratings in the validation set using RMSE and the report outputs the RMSE.

# Functions to be used:   
- RMSE <- function(true_ratings, predicted_ratings) {
                sqrt(mean((true_ratings - predicted_ratings)^2))}

    1. Dataset
       a. MovieLens dataset (downloaded as R Version is 3.6.0)
    2. Goals of the project
       a. Predict the ratings for the validation dataset
       b. Compute the RMSE
    3. Key Steps
       a. Compute the average of the movie ratings
       b. Calculate the reqularized parameter (lambda) using cross-validation
       c. Get the lambda that gives the least RMSE
       d. Use the lambda to predict the ratings
       e. Compute the RMSE on the predict ratings vs the validation test
       
# Understand the data
   1. Observations
   2. Variables (no and names)
   3. Unique users
   4. Unique movies
   5. Ratings and frequency
   
#   CROSS-VALIDATION TO PICK A regularization paramater (LAMBDA) - MOVIE + USER EFFECT
    Use sapply function to return a vector and then retrieve the 
      parameter that gives the minimum RMSE
         1. Calculate the regularized bias for each movies
         2. Calculate the regularized bias for each user
         3. Predict ratings based on both regularized movies & user biases
         4. Compare against the validation (test) set and compute the RMSE
         
Calculate RMSE for the best values of the regularization parameter

Complete the SessionInfo

# Data to be downloaded from the folder into existing directories
