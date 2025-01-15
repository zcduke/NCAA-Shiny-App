## This function is designed to analyze NCAA basketball game play-by-play data. It computes detailed statistics about a team's shooting, scoring, and behavioral aspects.
### Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ncaahoopR)

### Define the function get_compe_stats()
get_compe_stats <- function(game_id) {
  # Fetch play-by-play data for the specified game
  game_data <- get_pbp_game(game_id, extra_parse = TRUE)
  
  # Compute shooting type statistics
  summary_stats <- game_data %>%
    mutate(
      # Classify shots into different types
      shot_type = case_when(
        !is.na(three_pt) & three_pt == TRUE ~ "3 Pointers",
        !is.na(free_throw) & free_throw == TRUE ~ "Free Throws",
        !is.na(three_pt) & !three_pt ~ "Field Goals",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(action_team, shot_type) %>% # Group by team and shot type
    summarise(
      attempted = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = shot_type, # Convert shot types into columns
      values_from = attempted,
      values_fill = 0 # Fill missing values with 0
    ) %>%
    mutate(
      FieldGoals = `Field Goals` + `3 Pointers`
    ) %>%
    # Rename columns for better readability
    rename(
      FreeThrows = `Free Throws`,
      ThreePointers = `3 Pointers`
    )
  
  #test
  #print(head(summary_stats))
  
  ### Compute score differences for each play
  score_stats <- game_data %>%
    mutate(
      home_score_diff = home_score - lag(home_score, default = first(home_score)),
      away_score_diff = away_score - lag(away_score, default = first(away_score))
    ) %>%
    rowwise() %>%
    mutate(
      score_diff = ifelse(home_score_diff > 0, home_score_diff, away_score_diff) # Determine which team scored
    ) %>%
    ungroup() %>%
    filter(score_diff > 0) %>% # Keep only plays with scoring
    group_by(action_team, score_diff) %>% # Group by team and score difference
    summarise(
      count = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = score_diff, # Convert score differences into columns
      values_from = count,
      values_fill = 0
    ) %>%
    mutate(
      FieldGoals = `2` + `3` # The value of total field goals made equals to 2-point plus 3-point
    )
  
  ### Combine shooting statistics and calculate percentages
  final_stats <- score_stats %>%
    left_join(summary_stats, by = "action_team") %>% # Merge score stats with shooting stats
    # Compute the percentage
    mutate(
      FreeThrowPercentage = round(`1` / FreeThrows * 100, 1),
      FieldGoalPercentage = round(`FieldGoals.x` / `FieldGoals.y` * 100, 1),
      ThreePointerPercentage = round(`3` / ThreePointers * 100, 1)
    ) %>%
    select(
      action_team,
      `1`,
      FreeThrows,
      FreeThrowPercentage,
      `FieldGoals.x`,
      `FieldGoals.y`,
      FieldGoalPercentage,
      `3`,
      ThreePointers,
      ThreePointerPercentage
    ) %>%
    rename(
      FreeThrows_Made = `1`,
      FreeThrows_Total = FreeThrows,
      FreeThrow_Rate = FreeThrowPercentage,
      FieldGoals_Made = `FieldGoals.x`,
      FieldGoals_Total = `FieldGoals.y`,
      FieldGoal_Rate = FieldGoalPercentage,
      ThreePointers_Made = `3`,
      ThreePointers_Total = ThreePointers,
      ThreePointer_Rate = ThreePointerPercentage
    ) %>%
    mutate(
      FreeThrows_Combined = paste(FreeThrows_Made, "/", FreeThrows_Total, sep = ""), # Combine attempts and made as a string
      FieldGoals_Combined = paste(FieldGoals_Made, "/", FieldGoals_Total, sep = ""),
      ThreePointers_Combined = paste(ThreePointers_Made, "/", ThreePointers_Total, sep = "")
    )
  
  ### Extract behavioral statistics
  behavior_stats <- game_data %>%
    mutate(
      rebounds = str_detect(description, regex("rebound", ignore_case = TRUE)), # Identify rebound events
      offensive_rebounds = str_detect(description, regex("offensive rebound", ignore_case = TRUE)), # Offensive rebounds
      defensive_rebounds = str_detect(description, regex("defensive rebound", ignore_case = TRUE)), # Defensive rebounds
      assists = str_detect(description, regex("assist", ignore_case = TRUE)), # Assists
      steals = str_detect(description, regex("steal", ignore_case = TRUE)), # Steals
      blocks = str_detect(description, regex("block", ignore_case = TRUE)), # Blocks
      turnovers = str_detect(description, regex("turnover", ignore_case = TRUE)), # Turnovers
      fouls = str_detect(description, regex("foul", ignore_case = TRUE)) # Fouls
    ) %>%
    group_by(action_team) %>%
    summarise(
      Rebounds = sum(rebounds, na.rm = TRUE),
      Offensive_Rebounds = sum(offensive_rebounds, na.rm = TRUE),
      Defensive_Rebounds = sum(defensive_rebounds, na.rm = TRUE),
      Assists = sum(assists, na.rm = TRUE),
      Steals = sum(steals, na.rm = TRUE),
      Blocks = sum(blocks, na.rm = TRUE),
      Turnovers = sum(turnovers, na.rm = TRUE),
      Fouls = sum(fouls, na.rm = TRUE),
      .groups = "drop"
    )
  
  ### Combine shooting and behavioral statistics
  compe_stats <- behavior_stats %>%
    left_join(final_stats, by = "action_team") %>% # Merge behavioral stats with final stats
    filter(!is.na(action_team)) # Filter out rows without team data
  
  ### Return the combined statistics
  return(compe_stats)
}
