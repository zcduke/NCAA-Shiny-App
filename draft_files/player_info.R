### Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ncaahoopR)

### Function to convert game data for a given game_id
convert_game_data <- function(game_id) {
  # Check if game_id is NULL
  if (is.null(game_id)) {
    stop("game_id cannot be NULL!")
  }
  
  # Fetch game data
  game_data <- get_boxscore(game_id)
  
  # Combine the nested list into a single data frame 
  combined_data <- bind_rows(game_data, .id = "team") %>%
    filter(player != "TEAM")  # Remove rows for TEAM statistics
  
  # Add new columns for combined statistics
  combined_data <- combined_data %>% 
    mutate(
      FG = paste(FGM, FGA, sep = "-"), # Combine Field Goals Made (FGM) and Attempted (FGA)
      `3PT` = paste(`3PTM`, `3PTA`, sep = "-"),
      FT = paste(FTM, FTA, sep = "-")
    )
  
  ### Separate data for home and away teams
  home_data <- combined_data[combined_data$home == TRUE, ]  
  away_data <- combined_data[combined_data$home == FALSE, ] 
  
  ### Remove unnecessary columns and rearrange the sequence of columns
  home_data <- home_data %>%
    select(player, FG, `3PT`, FT, everything(), -player_id, -TO, -opponent, -home, -FGM, -FGA, -`3PTM`, -`3PTA`, -FTM, -FTA)
  
  away_data <- away_data %>%
    select(player, FG, `3PT`, FT, everything(), -player_id, -TO, -opponent, -home, -FGM, -FGA, -`3PTM`, -`3PTA`, -FTM, -FTA)
  
  ### Divide data into starters and bench players, and remove extra columns
  home_starters <- home_data %>% 
    filter(starter == TRUE) %>% 
    select(-team, -starter)  
  home_bench <- home_data %>% 
    filter(starter == FALSE) %>% 
    select(-team, -starter)   
  away_starters <- away_data %>% 
    filter(starter == TRUE) %>% 
    select(-team, -starter) 
  away_bench <- away_data %>% 
    filter(starter == FALSE) %>% 
    select(-team, -starter)  # Remove 'team' and 'starter' columns
  
  ### Return a list containing data frames for starters and bench players
  return(list(
    home_starters = home_starters,
    home_bench = home_bench,
    away_starters = away_starters,
    away_bench = away_bench
  ))
}
