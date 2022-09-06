# The purpose of this script is to demonstrate (as a proof of concept) that you can use a random forest model to create a synthetic data set of vaccine rates by the known underlying demographics of a given populaton.

library(tidyverse)
library(viridis)
library(googlesheets4)
library(randomForest)
library(lubridate)

# This function will calculate the age of someone
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = lubridate::interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

# This function will return true or false according to a given frequency
true_false_prob <- function(proba) {sample(c(TRUE, FALSE), 1, prob = c(proba, 1 - proba))}

# It's my mdy birthday!
set.seed(10151987)


# Load in data ------------------------------------------------------------

# Name, age, team, and position data are from a 1990s Japanese baseball game I use for fake data sometimes
vaccine_records <- read_sheet("1-fQPeU7YSjBQtnXEiLGbskeCrazKfHxOsnT727nHHy8") %>%
  # People are classified as either pitchers or not.
  transmute(pitcher = str_detect(Position,"SP") | (Position == "RP"),
            age = age(DOB, age.day = ymd("2022-1-1")),
            firstname = toupper(First_Name),
            lastname = toupper(Last_Name),
            team_name = toupper(Team)) %>%
  rowwise() %>%
  # We assign each person a MMR and HAV vaccination status based on position and age
  mutate(
    MMR = case_when(
    (pitcher == TRUE) & (age < 50) ~ true_false_prob(proba  = 0.5),
    (pitcher == FALSE) & (age < 50) ~ true_false_prob(proba = 0.9),
    (pitcher == TRUE) & (age >= 50) ~ true_false_prob(proba = 0.3),
    (pitcher == FALSE) & (age >= 50) ~ true_false_prob(proba = 0.6)
  ) %>%
    as.integer(),
  
  HAV = case_when(
    (pitcher == TRUE) & (age < 50) ~ true_false_prob(proba  = 0.2),
    (pitcher == FALSE) & (age < 50) ~ true_false_prob(proba = 0.8),
    (pitcher == TRUE) & (age >= 50) ~ true_false_prob(proba = 0.1),
    (pitcher == FALSE) & (age >= 50) ~ true_false_prob(proba = 0.7)
  ) %>%
    as.integer()
  
  ) %>%
  ungroup()

# We create some fake data about individual teams (which is the same technique you'd use to do data for census tracts)
team_makeup <- data.frame(
  team_name = vaccine_records$team_name %>% unique(),
  meanage = rnorm(28, mean = 50,sd=5),
  sdage = rnorm(28,mean = 5,sd=1),
  total_pop = rnorm(28,mean=50,sd=5) %>% round(0),
  pitchers = rnorm(28,mean=25,sd=5) %>% round(0),
  stringsAsFactors = FALSE
  
) 


# Create a random forest model -----------------

rf_model_mmr <- randomForest::randomForest(
                                           MMR ~ .,
                                           data = vaccine_records %>%
                                             select(age,pitcher,MMR),
                                           type = "regression")

rf_model_HAV <- randomForest::randomForest( 
                                           HAV ~ .,
                                           data = vaccine_records%>%
                                             select(age,pitcher,HAV),
                                           type = "regression")


# Generate synthetic records -----------------

# Loop through each team and create synthetic people with the distribution from "team_makeup"
synthetic_records <- data.frame (stringsAsFactors = FALSE) 

synthetic_players <- for(i in 1:nrow(team_makeup)){
  team_data = team_makeup[i,]
  
  synth_team <- data.frame(team_name = team_data$team_name,
                           age = rnorm(team_data$total_pop,
                                       team_data$meanage,
                                       team_data$sdage) %>%
                             floor(),
                           stringsAsFactors = FALSE
  ) %>%
    mutate(pitcher = 
             (row_number() <= team_data$pitchers),
           MMR = NA,
           HAV = NA
           
    )
  
  synthetic_records<- bind_rows(synth_team,synthetic_records)
  
}




# Predict vaccine status of synthetic records -----------------------------------------------

# Get the predictions for MMR vaccination
mmr_predictions <- predict(object = rf_model_mmr,
                                   newdata = synthetic_records)


# Get the predictions for HAV vaccination
hav_predictions <- predict(object = rf_model_HAV,
                           newdata = synthetic_records)

synthetic_records_preditions <- synthetic_records %>%
  mutate(mmr_predictions = mmr_predictions,
         hav_predictions = hav_predictions) %>%
  rowwise() %>%
  mutate(MMR = true_false_prob(mmr_predictions),
         HAV = true_false_prob(hav_predictions)
  ) 


