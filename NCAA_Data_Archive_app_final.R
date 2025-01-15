library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ncaahoopR)
library(shinyjs)
library(echarts4r)
library(htmlwidgets)
library(rsconnect)
library(bsicons)
library(DT)
library(purrr)
library(tidyr)
library(ncaahoopR)
library(shinycssloaders)
library(stringr)
library(lubridate)
library(glue)
################################################################################
# Basic information dealing
################################################################################
# Build season list
available_seasons <- sapply(2015:2023, function(x) {
  paste0(x, "-", substr(x + 1, 3, 4))
})

sref_name <- ncaahoopR::dict %>%
  reframe(
    espn_name = ESPN,
    sref_name = sref_name
  )
# Get team information
teams_data <- ncaahoopR::ncaa_colors %>%
  filter(!is.na(espn_name), !is.na(logo_url), !is.na(conference), !is.na(ncaa_name)) %>%
  distinct(espn_name, ncaa_name, logo_url, conference, primary_color) %>%
  left_join(sref_name, by = "espn_name")

teams_no_logo <- c('Bellarmine', 'Florida A&M', 'Gardner-Webb', 'George Mason', 'G Washington', 'South Alabama', 'USF', 
                   'St Bonaventure', 'Stetson', 'Le Moyne', 'Miss Valley St')
teams_logo <- c('https://upload.wikimedia.org/wikipedia/en/9/92/Bellarmine_knights_logo20.png',
                'https://upload.wikimedia.org/wikipedia/commons/7/75/Florida_A%26M_Rattlers_wordmark.svg',
                'https://upload.wikimedia.org/wikipedia/commons/e/e1/Gardner-Webb_Athletic_Wordmark_-_full%2722.jpg',
                'https://upload.wikimedia.org/wikipedia/commons/9/99/Georgemason_athletics_logo.png',
                'https://upload.wikimedia.org/wikipedia/commons/1/12/George_Washington_Athletics_logo.svg',
                'https://upload.wikimedia.org/wikipedia/commons/d/d2/South_Alabama_Jaguars_wordmark.svg',
                'https://upload.wikimedia.org/wikipedia/commons/1/13/Official_USF_Bulls_Athletic_Logo.png',
                'https://upload.wikimedia.org/wikipedia/commons/a/a5/2024_STB_Logo_BonniesMTN-Wordmark_Light.png',
                'https://upload.wikimedia.org/wikipedia/en/6/61/Stetson_Hatters_current_logo.svg',
                'https://upload.wikimedia.org/wikipedia/en/c/cc/Le_Moyne_Dolphins_logo.svg',
                'https://upload.wikimedia.org/wikipedia/commons/1/10/MVSU_VS_simple_monogram.svg')

for (i in seq_along(teams_no_logo)){
  teams_data$logo_url[teams_data$espn_name == teams_no_logo[i]] <- 
    teams_logo[i]
}
teams_data$logo_url[teams_data$espn_name == 'Florida A&M'] <- 'https://upload.wikimedia.org/wikipedia/commons/7/75/Florida_A%26M_Rattlers_wordmark.svg'
# 将新的 logo_url 全部替换到 ncaa_colors
ncaa_colors <- ncaa_colors %>%
  mutate(logo_url = ifelse(espn_name %in% teams_no_logo,
                           teams_logo[match(espn_name, teams_no_logo)],
                           logo_url))


################################################################################
## Function used to design to analyze NCAA basketball game play-by-play data. 
## It computes detailed statistics about a team's shooting, scoring, and behavioral aspects.
################################################################################
### Define the function get_compe_stats()
get_compe_stats <- function(game_id) {
  # Fetch play-by-play data for the specified game
  game_data <- get_pbp_game(game_id, extra_parse = TRUE)
  #library(stringr)
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

################################################################################
### Function to render a Win Probability Chart using ggplot2
################################################################################
#' Win Probability Chart
#'
#' Renders Win Probability Charts in ggplot
#'
#' @param game_id ESPN game_id for which to render chart
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart
#' @param include_spread Logical, whether to include pre-game spread in Win Probability
#' calculations. Default = True.
#' @param show_labels Logical whether Game Exictement Index and Minimum
#' Win Probability metrics should be displayed on the plot. Default = TRUE.
#' @export
#'
wp_chart <- function(game_id, home_col = NULL, 
                     away_col = NULL, 
                     home_team = NULL,
                     away_team = NULL,include_spread = T, show_labels = T) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }
  
  ### Fetch Play-by-Play Data
  data <-
    get_pbp_game(game_id, extra_parse = F) %>%
    dplyr::filter(!wrong_time) # Filter out rows with incorrect timestamps
  
  if(is.null(data)) {
    warning("PBP Data Not Available for Win Probability Chart")
    return(NULL) # Return NULL if no data is available
  }
  
  ### Derive Team Names (if not provided)
  if(is.null(home_team)) {
    home_team <- data$home[1]
  }
  if(is.null(away_team)) {
    away_team <- data$away[1]
  }
  
  
  ### Derive Team Colors
  if(is.null(home_col)) {
    home_col <- ncaa_colors$primary_color[ gsub('State', 'St', ncaa_colors$espn_name) == gsub('State', 'St', dict$ESPN[dict$NCAA == home_team | 
                                                                                                                         dict$ESPN == home_team |
                                                                                                                         dict$ESPN_PBP == home_team  | 
                                                                                                                         dict$ESPN_PBP == gsub('State', 'St', home_team) |
                                                                                                                         dict$ESPN_PBP == gsub('St', 'State', home_team)][1]) ]
  }
  if(is.null(away_col)) {
    away_col <- ncaa_colors$primary_color[ gsub('State', 'St', ncaa_colors$espn_name) == gsub('State', 'St', dict$ESPN[dict$NCAA == away_team |
                                                                                                                         dict$ESPN == away_team |
                                                                                                                         dict$ESPN_PBP == away_team | 
                                                                                                                         dict$ESPN_PBP == gsub('State', 'St', away_team) | 
                                                                                                                         dict$ESPN_PBP == gsub('St', 'State', away_team)][1]) ] 
  }
  
  home_url <- ncaa_colors$logo_url[ gsub('State', 'St', ncaa_colors$espn_name) == gsub('State', 'St', dict$ESPN[dict$NCAA == home_team | 
                                                                                                                  dict$ESPN == home_team |
                                                                                                                  dict$ESPN_PBP == home_team  | 
                                                                                                                  dict$ESPN_PBP == gsub('State', 'St', home_team) |
                                                                                                                  dict$ESPN_PBP == gsub('St', 'State', home_team)][1]) ]
  away_url <- ncaa_colors$logo_url[ gsub('State', 'St', ncaa_colors$espn_name) == gsub('State', 'St', dict$ESPN[dict$NCAA == away_team |
                                                                                                                  dict$ESPN == away_team |
                                                                                                                  dict$ESPN_PBP == away_team | 
                                                                                                                  dict$ESPN_PBP == gsub('State', 'St', away_team) | 
                                                                                                                  dict$ESPN_PBP == gsub('St', 'State', away_team)][1]) ] 
  
  
  
  ### Generate Time Lines for Overtime
  plot_lines <- 1200 # Start with regulation time
  msec <- max(data$secs_remaining_absolute) # Maximum game seconds
  sec <- msec - 2400
  ot_counter <- 0
  while(sec > 0) {
    sec <- sec - 300
    plot_lines <- c(plot_lines, 2400 + ot_counter * 300) # Add OT lines
    ot_counter <- ot_counter + 1
  }
  
  ### Convert Date for Subtitle
  date <- format(as.Date(data$date[1]), "%B %d, %Y")
  
  ### Use Naive Win Probability if Spread is Excluded
  if(!include_spread) {
    data$win_prob <- data$naive_win_prob
  }
  
  ### Get into Appropriate Format
  x <- rbind(
    dplyr::select(data, secs_remaining_absolute, win_prob,home_score, away_score) %>%
      dplyr::mutate(team = "home"), # Add team label
    dplyr::select(data, secs_remaining_absolute, win_prob,home_score, away_score) %>%
      dplyr::mutate("win_prob" = 1 - win_prob, # Flip probability for away team
                    team = "away")
  ) %>%
    dplyr::mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute) # Calculate elapsed time
  
  ### Game Excitement Index
  data$wp_delta <- 0
  for(i in 2:nrow(data)) {
    data$wp_delta[i] <- abs(data$win_prob[i] - data$win_prob[i-1]) # Calculate probability changes
  }
  gei <- sum(data$wp_delta, na.rm = T) # Sum changes for GEI
  gei <- paste("Game Excitement Index:", round(gei, 2))
  
  ### Minimum Win Probability
  if(data$score_diff[nrow(data)] > 0) { # Check if home team wins
    min_prob <- min(data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", home_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  } else { # Away team wins
    min_prob <- min(1 - data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", away_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  }
  
  home_score <- data$home_score[nrow(data)]
  away_score <- data$away_score[nrow(data)]
  st <- paste0(home_team, ": ", home_score, "  ", away_team, ": ", away_score, "\n", date)
  
  ### Generate ggplot Chart
  p <- ggplot2::ggplot(x, ggplot2::aes(x = secs_elapsed/60, y = win_prob, group = team, col = team)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "",
                  y = "Win Probability",
                  col = "",
                  title = paste0(ifelse(include_spread, "", "Naive "), "Win Probability Chart for ", home_team,
                                 " vs. ", away_team),
                  subtitle = st,
                  caption = "Luke Benz (@recspecs730) Data Accessed via ncaahoopR") +
    ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5),
                   plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
                   axis.title = element_text(size = 14),
                   plot.caption = element_text(size = 8, hjust = 0),
                   legend.position = "bottom",) +
    ggplot2::scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    ggplot2::scale_y_continuous(labels = function(x) {paste(100 * x, "%")}) +
    ggplot2::scale_color_manual(values = c(away_col, home_col),
                                labels = c(away_team, home_team))
  
  #if(show_labels) {
  #  p <- p +
  #    ggplot2::annotate("text", x = 5, y = 0.05, label = gei) +
  #    ggplot2::annotate("text", x = 5, y = 0.025, label = min_prob)
  #}
  
  ### Add Interactivity
  #p <- p +
  #ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5, size = 0.8, inherit.aes = FALSE)+  # Hover line placeholder
  #ggplot2::annotate("text", x = 5, y = 0.05, label = paste0("Game Excitement Index: ", round(sum(abs(diff(x$win_prob)), na.rm = TRUE), 2))) +
  #ggplot2::annotate("text", x = 5, y = 0.025, label = paste0("Minimum Win Probability for ", home_team, ": ", round(min(x$win_prob) * 100, 1), "%"))
  
  ### Return Chart and Additional Information
  return(list(
    plot = p,
    data = x,
    home_team = home_team,
    away_team = away_team,
    home_score = home_score,
    away_score = away_score,
    home_url = home_url,  # Add home logo URL
    away_url = away_url,   # Add away logo URL
    home_col = home_col,
    away_col = away_col
  ))
  p
}



################################################################################
### Function to convert game data for a given game_id
################################################################################
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




################################################################################
# Prediction Part
################################################################################
library(shinydashboard)
library(readr)
library(xgboost)  # Load XGBoost model

# Read historical data containing scores
# Assume unc_duke_games.csv contains: game_id, home, away, home_score, away_score
historic_data <- read.csv("unc_duke_games.csv") %>%
  mutate(
    home = ifelse(home == "North Carolina", "UNC", home),
    away = ifelse(away == "North Carolina", "UNC", away),
    winner = ifelse(home_score > away_score, home, away),
    margin = home_score - away_score
  )

# Sort by game_id to ensure a chronological order
historic_data <- historic_data %>% arrange(game_id)

# Add a game index and create home-court features
historic_data <- historic_data %>%
  mutate(
    game_index = row_number(),
    is_unc_home = ifelse(home == "UNC", 1, 0),
    is_duke_home = ifelse(home == "Duke", 1, 0)
  )

# Create training data: use game_index, is_unc_home, is_duke_home as features to predict margin
train_data <- historic_data %>% select(margin, game_index, is_unc_home, is_duke_home)

# Split data into training and validation sets
set.seed(1234)
train_idx <- sample(nrow(train_data), floor(0.8 * nrow(train_data)))
training <- train_data[train_idx, ]
validation <- train_data[-train_idx, ]

dtrain <- xgb.DMatrix(data = as.matrix(training %>% select(-margin)),
                      label = training$margin)
dvalid <- xgb.DMatrix(data = as.matrix(validation %>% select(-margin)),
                      label = validation$margin)

# Set XGBoost parameters
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 3
)

# Train the XGBoost model
watchlist <- list(train = dtrain, eval = dvalid)
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100,
                       watchlist = watchlist, print_every_n = 10, early_stopping_rounds = 10)

# Display feature importance
importance <- xgb.importance(model = xgb_model)
#print(importance)


########################################
# Fetch 2023-24 season schedule and predict
########################################

schedule_2023_24_unc <- tryCatch(get_schedule("UNC", season = "2023-24"), error = function(e) NULL)

if (!is.null(schedule_2023_24_unc) && nrow(schedule_2023_24_unc) > 0) {
  # Determine home and away teams based on the location field
  schedule_2023_24_unc <- schedule_2023_24_unc %>%
    mutate(
      home = ifelse(location == "H", "UNC", opponent),
      away = ifelse(location == "H", opponent, "UNC")
    )
  
  # Filter for UNC vs Duke games
  match_2023_24 <- schedule_2023_24_unc %>%
    filter(opponent == "Duke")
  
  n_future <- nrow(match_2023_24)
  if (n_future > 0) {
    # Assign game_index for future games
    last_index <- max(historic_data$game_index)
    future_indices <- (last_index + 1):(last_index + n_future)
    match_2023_24 <- match_2023_24 %>%
      mutate(
        game_index = future_indices,
        is_unc_home = ifelse(home == "UNC", 1, 0),
        is_duke_home = ifelse(home == "Duke", 1, 0)
      )
    
    # Create prediction dataset and make predictions
    dpred <- xgb.DMatrix(data = as.matrix(match_2023_24 %>% select(game_index, is_unc_home, is_duke_home)))
    match_2023_24$pred_margin <- predict(xgb_model, newdata = dpred)
    
    # Round up the predicted margin to the nearest integer
    match_2023_24$pred_margin <- ceiling(match_2023_24$pred_margin)
    
    match_2023_24 <- match_2023_24 %>%
      mutate(
        pred_winner = ifelse(pred_margin > 0, home, away),
        pred_margin_abs = abs(pred_margin)
      )
  } else {
    match_2023_24 <- data.frame(date = NA, home = NA, away = NA, pred_winner = NA, pred_margin_abs = NA)
  }
} else {
  match_2023_24 <- data.frame(date = NA, home = NA, away = NA, pred_winner = NA, pred_margin_abs = NA)
}

##########################################################################
# Schedule
##########################################################################

# Get the current date
current_date <- as.Date(format(Sys.time(), "%Y-%m-%d"))
# Fetch today's schedule and Duke's game details
current_date <- as.Date(format(Sys.time(), "%Y-%m-%d"))
todays_schedule <- get_master_schedule(current_date)
# Retrieve Duke's game schedule
duke_games <- get_schedule(team = "Duke", season = "2024-25") %>%
  as_tibble()  # Ensure the data is in tibble format

# Find the next Duke game
next_duke_game <- duke_games %>%
  filter(date > current_date) %>%
  arrange(date)

if (nrow(next_duke_game) > 0) {
  next_duke_game <- next_duke_game[1, ]  # Get the first row without slice()
} else {
  next_duke_game <- NULL  # Handle case where no future games are found
}

# Find the last Duke game
last_duke_game <- duke_games %>%
  filter(date <= current_date) %>%
  arrange(desc(date))

if (nrow(last_duke_game) > 0) {
  last_duke_game <- last_duke_game[1, ]  # Get the first row without slice()
} else {
  last_duke_game <- NULL  # Handle case where no past games are found
}


# Determine if Duke won their most recent game
did_duke_win <- if (!is.null(last_duke_game)) {
  # Check if it was a home or away game and compare scores
  if (last_duke_game$location == "H" && last_duke_game$team_score > last_duke_game$opp_score) {
    "Yes"
  } else if (last_duke_game$location == "A" && last_duke_game$team_score > last_duke_game$opp_score) {
    "Yes"
  } else {
    "No"
  }
} else {
  "No recent game found"  
}

# Logo fetching function
get_team_logo <- function(team_name, teams_data) {
  logo_url <- teams_data$logo_url[teams_data$espn_name == team_name]
  return(logo_url)
}

# Adjust NCAA colors data to include logos
ncaa_colors <- ncaahoopR::ncaa_colors %>%
  mutate(logo_url = ifelse(espn_name %in% teams_no_logo,
                           teams_logo[match(espn_name, teams_no_logo)],
                           logo_url))

# Fetch Duke's team logo
duke_logo <- get_team_logo("Duke", teams_data)



################################################################################
# UI Part
################################################################################
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # self-define CSS to make a picture gallery
    tags$style(HTML("
      @keyframes cycleBackgrounds {
        0% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F12%2F9%2F2024_Duke_MBB_Game_v_ARMY__11-08-145_16x9.jpg&height=1100&type=webp'); }
        10% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2021%2F9%2F29%2FCoach_K_practice.jpg&height=1100&type=webp'); }
        20% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F12%2F9%2F2024_Duke_MBB_Louisville_Game_12-08-158.jpg&height=1100&type=webp'); }
        30% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F12%2F7%2F2024_Duke_MBB_Auburn_Game_12-04-051_16x9.jpg&height=1100&type=webp'); }
        40% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2019%2F9%2F12%2FACC_Champions.jpg&height=1100&type=webp'); }
        50% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F12%2F10%2F2024_Duke_MBB_Incarnate_Word_Game_12-10-39.jpg&height=1100&type=webp'); }
        60% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F12%2F5%2FDuke_MBB_24-25_Auburn_0018.jpg&height=1100&type=webp'); }
        70% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F11%2F29%2F041A0495.jpeg&height=1100&type=webp'); }
        80% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F11%2F3%2F0M4A0188_16x9web.jpg&height=1100&type=webp'); }
        90% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F7%2F15%2F2023-2024_Duke_MBB_Vermont_Game_03_22-73_16x9.jpg&height=1100&type=webp'); }
        100% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F2%2F16%2F2023-2024_MBB_Game_Wake_Forest_02_12-80.jpg&height=1100&type=webp'); }
      } 
      body, html {
        margin: 0;
        padding: 0;
        /* overflow: hidden; */
      }
      .welcome-page {
        height: 100vh;
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        animation: cycleBackgrounds 30s infinite; /* cycle backgroud picture */
      }
      .welcome-content {
        color: white;
        text-shadow: 2px 2px 4px #000000;
        text-align: center;
        padding-top: 30vh;
      }
      .button-container {
        margin-top: 30px;
      }
      .navbar {
        background: linear-gradient(to right, #0082da,#009cde); /* gradually change backgroud color */
        border-bottom: none; 
      }
      .navbar-brand {
        color: white !important; 
        font-size: 1.5em; 
        font-weight: bold; 
      }
      .navbar-nav > li > a {
        color: white !important;  
        text-decoration: none; 
      }
      .navbar-nav > li > a:hover {
        color: #012169 !important; 
        border-bottom: none !important; 
      }
       .navbar-toggler {
        color: #FFFFFF !important; 
        border-color: #FFFFFF !important;
       }
      
      .scroll-container {
        display: flex;
        overflow-x: auto;
        gap: 20px;
        padding: 10px;
        position: relative;
        scroll-behavior: smooth;
      }

      .scroll-container::-webkit-scrollbar {
        display: none;
      }

      .game-card {
        display: flex;
        align-items: center;
        justify-content: center;
        width: 300px;
        height: 200px;
        border: 1px solid #ccc;
        padding: 15px;
        box-sizing: border-box;
        text-align: center;
        background-color: #f9f9f9;
        border-radius: 10px;
      }

      .team-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        flex: 1;
      }

      .team-logo {
        width: 80px;
        height: 80px;
        margin-bottom: 10px;
      }

      .team-name {
        font-size: 18px;
        color: #000000;
        font-weight: bold;
      }

      .vs-container {
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 0 15px;
        font-size: 20px;
        font-weight: bold;
        color: #000000;
      }

    ")),
      tags$script(HTML(
        "
        $(document).on('click', '#scroll_left_btn', function() {
          const container = $('.scroll-container');
          container.animate({ scrollLeft: '-=150px' }, 200);
        });
  
        $(document).on('click', '#scroll_right_btn', function() {
          const container = $('.scroll-container');
          container.animate({ scrollLeft: '+=150px' }, 200);
        });
        "
      ))
  ),
  ###################################################################################
  # Click button to expand
  ###################################################################################
  # HTML Element Creation
  tags$head(
    tags$style(HTML("
      .block_wrap {
        width: 100%;
        margin: 20px auto;
        border: 1px solid #e3e3e3;
        border-radius: 10px;
        transition: max-height 0.5s ease, opacity 0.5s ease;
        overflow: hidden;
      }
      .block_title {
        padding: 10px;
        background: #f7f7f7;
        cursor: pointer;
        font-size: 16px;
        font-weight: bold;
        border-bottom: 1px solid #ddd;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .block_content {
        padding: 10px;
        background: white;
        opacity: 0;
        max-height: 0;
      }
      .block_wrap.active .block_content {
        opacity: 1;
        max-height: 2000px; /* Controls the maximum height after expansion */
      }
    "))
  ),
  tags$head(
    tags$style(HTML("
    table {
      width: 100%;
      table-layout: fixed; 
      border-collapse: collapse;
      margin: 20px auto; 
    }

    th {
      text-align: left; 
      font-weight: bold; 
      padding: 12px 8px; 
      font-size: 14px;
      border-bottom: 2px solid #000; 
    }

    td {
      text-align: left; 
      padding: 10px 8px; 
      font-size: 13px; 
      border-bottom: 1px solid #ddd; 
    }

    td:first-child, th:first-child {
      text-align: center; 
      font-weight: bold; 
    }

    tr:nth-child(even) {
      background-color: #f9f9f9; 
    }

    tr:hover {
      background-color: #f1f1f1; 
    }

    table caption {
      caption-side: top; 
      font-size: 16px; 
      font-weight: bold; 
      margin-bottom: 8px; 
    }
  "))
  ),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=UnifrakturCook:wght@700&display=swap")
  ),
  page_navbar(
    title = "NCAA Data Archive",
    id = "navId", 
    #------------------------------------------------------------------------------------------
    #####################################################################################
    # Welcome Page
    #####################################################################################
    nav_panel(
      title = "Home", 
      value = "welcome",
      div(
        class = "welcome-page",
        div(
          class = "welcome-content",
          tags$style(HTML("
            @import url('https://fonts.googleapis.com/css2?family=Anton&display=swap');
          
            /* title */
            .custom-title {
              font-family: 'Anton', sans-serif;
              font-size: 100px; 
              color: #FFFFFF; 
              text-align: center; 
              margin-top: 30px; 
              margin-bottom: 20px; 
              letter-spacing: 2px; 
              text-transform: uppercase; 
            }
          
            /* description */
            .custom-description {
              font-family: 'Anton', sans-serif;
              font-size: 40px; 
              color: #FFFFFF; 
              text-align: center; 
              margin-top: 10px; 
              margin-bottom: 30px; 
            }
          ")),
          div(class = "custom-title", "Welcome to NCAA Data Archive"),
          div(class = "custom-description", "The world of basketball game data!")
        )
      )
    ),
    #------------------------------------------------------------------------------------------
    #####################################################################################
    # Game Data Page
    #####################################################################################
    nav_panel(
      # title for the panel
      title = "Competition Analysis",
      # sidebar layout for competition searching
      sidebarLayout(
        # sidebar
        sidebarPanel(
          textInput("game_id", "Enter Game ID (default: UNC hosting Duke):", value = "401168364"), # The competition between Duke and UNC
          actionButton("update", "Update Chart"),
          div(
            style = "border: 1px solid #ddd; padding: 10px; margin-top: 10px; background-color: #f9f9f9;",
            h4("Search Guide 🏀"),
            p("1. Searching like a true basketball fanatic is an art. Fine-tune your queries and unleash your inner scout!"),
            p("2. Can't wait to uncover the drama of your favorite matchup? Neither can we!"),
            p("3. Not sure where to start?"),
            div(
              style = "margin-top: 10px;",
              actionButton("go_to_team_data", "Click Here to Pick Your Team!", class = "btn-primary")
            )
          )  
        ),
        
        # mainbar
        mainPanel(
          ###################################################################################
          # The frame graph of displaying A VS B
          ###################################################################################
          # Header with team logos, scores, home and away info and win probabilities
          div(
            style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0;",
            #######################
            # Add "Away" label in the left, which is traditional
            #######################
            div(
              style = "display: flex; flex-direction: column; align-items: center; text-align: center;",
              div(innerHTML="", id= 'away_name',style = "font-size: 30px; font-family: 'UnifrakturCook';font-weight: bold; color: #555; margin-bottom: 5px;"),
              img(src = "", id = "away_logo", height = "100px", style = "margin-bottom: 5px;"),
              div("Away", id= 'away',style = "font-size: 16px; font-weight: bold; color: #555; margin-bottom: 5px;"),
              # Score
              div(
                style = "display: flex; align-items: center;",
                div("Score:", id = "away_score_label", style = "font-size: 20px; font-weight: bold; margin-right: 5px;"),
                div(id = "away_score", "0", style = "font-size: 20px; font-weight: bold;")
              ),
              # Win Prob.
              div(
                style = "display: flex; align-items: center;",
                div("Win Prob.:", id = "away_prob_label", style = "font-size: 12px; font-weight: bold; margin-right: 5px;"),
                div(id = "away_prob", "0%", style = "font-size: 14px; font-weight: bold;")
              )
            ),
            div("VS", style = "font-size: 60px; font-weight: bold;"),
            #######################
            # Add "Home" label
            #######################
            div(
              style = "display: flex; flex-direction: column; align-items: center; text-align: center;",
              div(innerHTML="", id= 'home_name',style = "font-size: 30px;font-family: 'UnifrakturCook'; font-weight: bold; color: #555; margin-bottom: 5px;"),
              img(src = "", id = "home_logo", height = "100px", style = "margin-bottom: 5px;"),
              div("Home", id = 'home',style = "font-size: 16px; font-weight: bold; color: #555; margin-bottom: 5px;"),
              div(
                style = "display: flex; align-items: center;",
                div("Score:", id = "home_score_label", style = "font-size: 20px; font-weight: bold; margin-right: 5px;"),
                div(id = "home_score", "0", style = "font-size: 20px; font-weight: bold;")
              ),
              div(
                style = "display: flex; align-items: center;",
                div("Win Prob.:", id = "home_prob_label", style = "font-size: 12px; font-weight: bold; margin-right: 5px;"),
                div(id = "home_prob", "0%", style = "font-size: 14px; font-weight: bold;")
              )
            )
          ),
          
          ###################################################################################
          # Win Probability Chart Block
          ###################################################################################
          # give click button here
          div(
            class = "block_wrap",
            id = "chart_block",
            div(class = "block_title", "Click to Show/Hide Win Probability Chart"),
            div(
              class = "block_content",
              style = "flex: 1; text-align: center;",
              h4("Win Probability Chart"),
              echarts4rOutput("win_prob_chart", height = "400px"), # Update to echarts4rOutput
              div(
                id = "game_phase",
                style = "text-align: center; font-weight: bold; margin: 10px 0; font-size: 20px;",
                "Remaining Time: Final"
              )
            )
          ),
          ###################################################################################
          # competition stats
          ###################################################################################
          div(
            class = "block_wrap",
            id = "stats_block",
            div(class = "block_title", "Click to Show/Hide Competition Stats"),
            #############################
            # Content for competition
            #############################
            div(
              class = "block_content",
              # win Prob.
              div(
                style = "flex: 1; text-align: center;",
                tags$h4("Team Stats Comparison"),
                echarts4rOutput("team_stats_chart", height = "400px")
              ),
              
              # dividing line between bar chart and pie chart
              div(
                style = "border-top: 1px solid #ccc; margin: 20px 0;"
              ),
              
              # Thre pie chart in a line
              div(
                style = "display: flex; justify-content: space-around; text-align: center;",
                
                # Free Throws pie chart
                div(
                  style = "flex: 1;",
                  h4("Free Throws Distribution"),
                  echarts4rOutput("free_throws_chart", height = "300px")
                ),
                
                # Field Goals pie chart
                div(
                  style = "flex: 1;",
                  h4("Field Goals Distribution"),
                  echarts4rOutput("field_goals_chart", height = "300px")
                ),
                
                # Three Pointers pie chart
                div(
                  style = "flex: 1;",
                  h4("Three Pointers Distribution"),
                  echarts4rOutput("three_pointers_chart", height = "300px")
                )
              )
            )
          ),
          
          
          ###################################################################################
          # Player Stats Block (Moved below Team Stats)
          ###################################################################################
          div(
            class = "block_wrap",
            id = "players_block",
            div(class = "block_title", "Click to Show/Hide Player Stats"),
            div(
              class = "block_content",
              # Leading Players Section (refer NBA)
              div(
                id = "leading_players",
                style = "margin-top: 20px; text-align: center;",
                tags$h3("Leading Players - Flower Chart"),
                echarts4rOutput("leading_players_chart", height = "600px")
              ),
              # Player Stats Display Section
              div(
                id = "player_stats",
                style = "margin-top: 40px; text-align: center;",
                tags$h3("Player Stats"),
                
                # Home Team Players
                div(
                  style = "margin-top: 20px; text-align: center;",
                  # Logo in the middle
                  img(src = "", id = "home_logo_third", height = "30px", style = "margin-bottom: 10px;"),
                  # Away Team Heading in the middle
                  h4("Home Team"),
                  div(
                    style = "text-align: left; margin-top: 20px;",
                    h4("Starters"),
                    tableOutput("home_starters_table"),  # Home Starters Table
                  ),
                  div(
                    style = "text-align: left; margin-top: 20px;",
                    h4("Bench"),
                    tableOutput("home_bench_table")      # Home Bench Table
                  )
                  
                ),
                
                # Away Team Players
                div(
                  style = "margin-top: 20px; text-align: center;",
                  # Logo in the middle
                  img(src = "", id = "away_logo_third", height = "30px", style = "margin-bottom: 10px;"),
                  # Away Team Heading in the middle
                  h4("Away Team"),
                  # Content sections: Starters and Bench
                  div(
                    style = "text-align: left; margin-top: 20px;",
                    h4("Starters"),
                    tableOutput("away_starters_table")  # Away Starters Table
                  ),
                  div(
                    style = "text-align: left; margin-top: 20px;",
                    h4("Bench"),
                    tableOutput("away_bench_table")  # Away Bench Table
                  )
                )
              )
            )
          ),
          ###################################################################################
          # JavaScript for Click Button Functionality
          ###################################################################################
          tags$script(HTML("
                document.querySelectorAll('.block_title').forEach(function(title) {
                  title.addEventListener('click', function() {
                    const parent = this.parentNode;
                    parent.classList.toggle('active');
                  });
                });
              ")
          )
          
          
        )
      )        
      
    ), 
    #------------------------------------------------------------------------------------------
    #####################################################################################
    # Team Data Page
    #####################################################################################
    # Dropdown menu under "Team Data"
    #####################################################################################
    # Teams page
    #####################################################################################
    nav_panel(
      title = "Team & Player", value = "teams",
      h2("NCAA Teams"),
      h4("Click team name or logo for detail"),
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = "conference_filter",
            label = "Filter by Conference:",
            choices = c("ALL", sort(unique(teams_data$conference))),
            selected = "ALL"
          )
        ),
        column(
          width = 6,
          selectInput(
            inputId = "team_filter",
            label = "Filter by Team:",
            choices = c("ALL", unique(teams_data$espn_name)),
            selected = "ALL"
          )
        )
      ),
      fluidRow(
        uiOutput("filtered_teams_ui")
      )
    ),
    #####################################################################################
    # Details page
    #####################################################################################
    nav_panel(
      title = "Statistics", value = "details",
      conditionalPanel(
        condition = "output.teamSelected == false",
        h3("select a team from Teams page")
      ),
      conditionalPanel(
        condition = "output.teamSelected == true",
        page_sidebar(
          sidebar = sidebar(
            width = 250,
            bg = "#f8f9fa",
            padding = 20,
            class = "p-3",
            h3(textOutput("selected_team_name"), class = "mb-4"),
            div(
              class = "text-center mb-4",
              uiOutput("team_logo_ui")
            ),
            div(
              class = "mt-4",
              selectInput(
                "selected_year", 
                "Choose a season:", 
                choices = available_seasons, 
                selected = "2023-24",
                width = "100%"
              )
            )
          ),
          navset_card_tab(
            nav_panel(
              title = "Team & Player Data",
              # Tabular with nested tables
              navset_tab(
                nav_panel(
                  "Team Schedule Data",
                  card(
                    card_header("Team Schedule Data"),
                    dataTableOutput('team_games_table')
                  )
                ),
                nav_panel(
                  "Player Data",
                  card(
                    card_header("Player Data Summary"),
                    withSpinner(dataTableOutput('other_table'), type = 1, caption = "Loading... good things take time!")
                  )
                )
              )
            ),
            nav_panel(
              title = "Assist Network",
              card(
                height = "calc(100vh - 160px)",
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      inputId = 'assignmentForm', 
                      label = 'Select a graph',
                      choices = c(
                        "--select--" = "",
                        "Assist Network" = "assist_net",
                        "Circle Assist Network" = "circle_assist_net"
                      ),
                      selected = ""
                    )
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.assignmentForm == 'circle_assist_net'",
                      selectInput(
                        inputId = 'selected_player',
                        label = 'Select a player',
                        choices = c("--select--" = ""),
                        selected = ""
                      )
                    )
                  )
                ),
                withSpinner(
                  plotOutput("assist_network_plot", height = "calc(100vh - 260px)", width = "100%"),
                  type = 6, 
                  caption = "I don't want to lie—it takes around half a minute, but we're making it worth the wait!" 
                )
              )
            )
          )
        )
      )
    )
    ,
    
    #------------------------------------------------------------------------------------------
    
    #####################################################################################
    #####################################################################################
    # Schedule Page
    #####################################################################################
    nav_panel(
      title = "Schedule", # The title of the panel in the navigation bar
      fluidPage(
        fluidRow(
          h2("Next Duke Match", style = "color: #00539b; font-size: 28px;"),
          column(4, 
                 div(
                   img(src = duke_logo, height = "120px"),  # Duke logo URL
                   h4(glue("Opponent: {next_duke_game$opponent}"), style = "color: #000000;margin-top: 10px; font-size: 20px;"),
                   h4(glue("Date: {next_duke_game$date}"), style = "color: #000000;font-size: 20px;"),
                   style = "text-align: center; margin-bottom: 20px;"
                 )
          ),
          column(4,
                 div(
                   div(
                     textOutput("countdown"),
                     class = "countdown-timer",
                     style = "font-size: 50px; font-weight: bold; color: #00539b; text-align: center; margin-top: 20px;"
                   ),
                   style = "text-align: center; margin-bottom: 20px;"
                 )
          ),
          column(4,
                 h2("Did Duke Win the Last Match?", style = "color: #00539b;text-align: center; font-size: 28px;"),
                 div(
                   textOutput("did_duke_win"),
                   style = "font-size: 28px; text-align: center; font-family: 'UnifrakturCook'; color: #00539b;" # Duke Blue Color
                 ),
                 style = "text-align: center; margin-bottom: 20px;"
          )
        ),
        fluidRow(
          h2("Today's Games", style = "color: #00539b;"),
          div(class = "scroll-container",
              lapply(1:nrow(todays_schedule), function(i) {
                game <- todays_schedule[i, ]
                home_logo <- get_team_logo(game$home, teams_data)
                away_logo <- get_team_logo(game$away, teams_data)
                div(class = "game-card",
                    div(class = "team-container",
                        img(src = home_logo, class = "team-logo"),
                        h4(game$home, class = "team-name")
                    ),
                    div(class = "vs-container",
                        h4("vs", style = "margin: 0; color: #000000;font-weight: bold;")
                    ),
                    div(class = "team-container",
                        img(src = away_logo, class = "team-logo"),
                        h4(game$away, class = "team-name")
                    )
                )
              })
          )
        )
      )
    ),
    
    #------------------------------------------------------------------------------------------
    ###########################################################################
    # Prediction
    ###########################################################################
    # Navigation bar items
    navbarMenu("Prediction",  # Main navigation item with dropdown for sub-items
               tabPanel("Method We Use",
                        h2("Introduction to XGBoost"),
                        p("XGBoost, short for \\(\\textbf{eXtreme Gradient Boosting}\\), is like the Michael Jordan of machine learning models: powerful, reliable, and built for greatness. Designed for tasks like regression and classification, it has become a go-to algorithm for its speed, accuracy, and ability to handle large, complex datasets."),
                        p("Some key reasons why XGBoost is a fan-favorite include:"),
                        tags$ul(
                          tags$li("\\(\\textbf{Gradient Boosting}\\): Builds an ensemble of decision trees by iteratively optimizing a loss function using gradient descent."),
                          tags$li("\\(\\textbf{Regularization}\\): Incorporates L1 (Lasso) and L2 (Ridge) penalties to prevent overfitting."),
                          tags$li("\\(\\textbf{Parallelization}\\): Runs computations in parallel to speed up training."),
                          tags$li("\\(\\textbf{Handling Missing Values}\\): Automatically adjusts for missing data."),
                          tags$li("\\(\\textbf{Tree Pruning}\\): Uses depth limits and child weights to control complexity."),
                          tags$li("\\(\\textbf{Customizable Objectives}\\): Allows defining custom loss functions for flexibility.")
                        ),
                        p("With these advantages, XGBoost has excelled in forecasting, anomaly detection, and sports analytics."),
                        
                        h2("How XGBoost is Applied in Our Web?"),
                        p("In our web, we predict the \\(\\textbf{point margin}\\) between UNC and Duke basketball teams using XGBoost. Data from the \\(\\textit{ncaahoopR}\\) package includes scores, game locations, and team identifiers."),
                        tags$div(
                          withMathJax("
             The margin \\(m\\) is calculated as: \\[m = \\textit{home score} - \\textit{away score}\\]
           "),
                          p("Positive \\(m\\): UNC wins. Negative \\(m\\): Duke wins.")
                        )
               )
               ,
               tabPanel("Data & Predicting",  # Sub-item 2: Historical data
                        h2("UNC vs Duke Historical Data"),
                        div(
                          style = "overflow-y: auto; max-height: 400px; margin-bottom: 20px;",  
                          dataTableOutput("history_table")  
                        ),
                        h3("Historical Margin Visualization (UNC Perspective)"),
                        div(
                          style = "overflow: hidden; margin-top: 20px;",  
                          plotOutput("history_plot", height = "400px")  
                        )
               ),
               tabPanel("Prediction Results",  # Sub-item 3: Predictions
                        h2("2023-24 Season Prediction Results"),
                        dataTableOutput("pred_table")
               )
    ),
    #------------------------------------------------------------------------------------------
    #####################################################################################
    # Write Up Page
    #####################################################################################
    nav_panel(
      title = "Write Up", 
      value = "writeup",
      h1("Write Up", style = "color: #000000;"),
      p("The goal of our final project was to design a shiny web app to aggregate and show history NCAA men’s basketball game data, serving as a preparation of our future prediction task. In this write up section, we will give some background of the NCAA games, explain the functionality of the application and discuss how we implemented the task."),
      p("The National Collegiate Athletic Association (NCAA) Men's Basketball program is one of the most celebrated and dynamic components of college sports in the United States. It features hundreds of teams from colleges and universities competing across various divisions, culminating in a high-stakes national tournament known as March Madness. This tournament captures the nation's attention with its single-elimination format, thrilling games, and underdog stories. NCAA Men's Basketball is more than just a game—it is a cultural institution that brings communities together, fosters young talent, and showcases the unrelenting spirit of competition."),
      p("As students of Duke University, we take immense pride in the efforts and performances of our men's basketball team, a program renowned for its rich history and unwavering spirit of excellence. To celebrate and analyze the team's achievements, we have chosen to create a Shiny app that showcases relevant data in an interactive and visually engaging way. This app will allow users to explore key statistics, game trends, individual player and overall team performances, providing deeper insights into the factors that contribute to our team's success. By leveraging this innovative tool, we aim to highlight the dedication and talent that define Duke basketball."),
      p("Our final Shiny app features five main components designed to provide a comprehensive and engaging user experience: Game Data, Team-player Data, Schedule, Prediction, and Write-Up. In the Data section, we developed interactive interfaces that allow users to click and instantly view summary statistics for a single match or a team’s overall performance. In the Prediction section, we implemented the XGBoost algorithm and tried to focus specifically on games between Duke and UNC to predict both the winner and the difference in total final points. Schedule section provides real time information of current NCAA games and trend of Duke's team. This write-up provides a concise overview of our work. The detailed implementation of each component is discussed below."),
      p("In order to provide more real time information of the current season, we also added a schedule scetion featuring Duke's next game time and last game result, including other NCAA games held today."),
      # textAreaInput(
      #   inputId = "writeup_text",
      #   label = "Enter your write-up here:",
      #   width = "100%",
      #   height = "200px",
      #   placeholder = "Start typing your content..."
      # ),
      # actionButton("back_to_welcome_from_writeup", "Go Back to Welcome Page", class = "btn-light")
    )
  )
)




###################################################################################
# Server Part
###################################################################################
server <- function(input, output, session) {
  #----------------------------------------------------------------------------------------------------------------------------------------
  #This part is for competition information
  #########################################################################################
  # Attribute
  # 1.input here is the game_id which can be usen to search a competition by get_pbp_game(game_ids, extra_parse)
  #########################################################################################
  # Reactive value here
  # 1.chart_data is using to store reactive value of win probability and relate information.
  # 2.stats_data is a reactive expression using to get competition stats in second part 
  # like PTS,AST from the get_compe_stats function.
  # 3.game_data is using for getting the players information like PTS personally.
  #########################################################################################
  
  
  # Reactive value to store chart data
  #chart_data <- reactive({
  #  req(input$game_id)
  #  get_compe_stats(input$game_id)
  #})
  chart_data <- reactiveVal(NULL)
  
  # Reactive expression to fetch competition stats based on game_id
  stats_data <- reactive({
    req(input$game_id)
    get_compe_stats(input$game_id)
  })
  
  # Reactive expression to fetch game data (players' stats)
  game_data <- reactive({
    req(input$game_id)
    convert_game_data(input$game_id)
  })
  
  ###################################################################################
  # 0. Observe event for Update Button and Basic information
  ###################################################################################
  observeEvent(input$update, {
    # Fetch and store chart data of the final win prob and score something to update for the title information
    chart <- wp_chart(game_id = input$game_id)
    
    chart$home_url <- ifelse(
      is.na(chart$home_url) | chart$home_url == "",
      teams_logo[match(chart$home_team, teams_no_logo)],
      chart$home_url
    )
    chart$away_url <- ifelse(
      is.na(chart$away_url) | chart$away_url == "",
      teams_logo[match(chart$away_team, teams_no_logo)],
      chart$away_url
    )
    # Update chart_data when get to now the competition which is being searched
    chart_data(chart)
    # debug code to see the chart 
    # print(names(chart))
    
    
    # Update team logos for Title
    shinyjs::runjs(sprintf("document.getElementById('home_logo').src = '%s';",  chart$home_url))
    shinyjs::runjs(sprintf("document.getElementById('away_logo').src = '%s';",  chart$away_url))
    shinyjs::runjs(sprintf("document.getElementById('home_logo_third').src = '%s';", chart$home_url))
    shinyjs::runjs(sprintf("document.getElementById('away_logo_third').src = '%s';", chart$away_url))
    # Update score and probability and home&away colors
    shinyjs::runjs(sprintf("document.getElementById('home_score').style.color = '%s';", chart$home_col))
    shinyjs::runjs(sprintf("document.getElementById('away_score').style.color = '%s';", chart$away_col))
    shinyjs::runjs(sprintf("document.getElementById('home_prob').style.color = '%s';", chart$home_col))
    shinyjs::runjs(sprintf("document.getElementById('away_prob').style.color = '%s';", chart$away_col))
    shinyjs::runjs(sprintf("document.getElementById('home').style.color = '%s';", chart$home_col))
    shinyjs::runjs(sprintf("document.getElementById('away').style.color = '%s';", chart$away_col))
    shinyjs::runjs(sprintf("document.getElementById('home_name').style.color = '%s';", chart$home_col))
    shinyjs::runjs(sprintf("document.getElementById('away_name').style.color = '%s';", chart$away_col))
    
    # Update team name
    shinyjs::runjs(sprintf("document.getElementById('away_name').innerHTML = '%s';", chart$away_team))
    shinyjs::runjs(sprintf("document.getElementById('home_name').innerHTML = '%s';", chart$home_team))
    
    # Update scores
    shinyjs::html("home_score", chart$home_score)
    shinyjs::html("away_score", chart$away_score)
    
    # Update win probabilities
    # in the chart$data the last win_prob is the final prob for away team
    last_win_prob <- chart$data[nrow(chart$data), ]$win_prob
    
    shinyjs::html("home_prob", paste0(round((1 - last_win_prob) * 100), "%"))
    shinyjs::html("away_prob", paste0(round(last_win_prob * 100), "%"))
    
    # Update game phase (first half, second half, over, final)
    shinyjs::html("game_phase", "Remaining Time: Final")
  })
  
  ###################################################################################
  # We Use Echarts For all the plot since it's Smooth and Easy to interact with
  ###################################################################################
  # 1. First Part win Prob
  ###################################################################################  
  
  output$win_prob_chart <- renderEcharts4r({
    req(chart_data())
    chart <- chart_data()
    # print(chart) # debug code 
    # get home and away team data
    home_data <- chart$data %>% filter(team == "home")
    away_data <- chart$data %>% filter(team == "away")
    # print(str(chart$data))
    # check if they have Home data of prob
    if (nrow(home_data) == 0) {
      stop("Home team data is empty!")
    }
    
    combined_data <- bind_rows(
      home_data %>% mutate(team = "Home Team"),
      away_data %>% mutate(team = "Away Team")
    ) %>%
      mutate(team = factor(team, levels = c("Home Team", "Away Team")))  # keep win prob in order
    
    # get the time of whole competition, some of them has over time competition.
    total_time <- max(chart$data$secs_remaining_absolute, na.rm = TRUE)  
    e_chart <- combined_data %>%
      group_by(team) %>%
      e_charts(secs_elapsed, debug = TRUE) %>%
      e_line(win_prob, smooth = TRUE, showSymbol = FALSE) %>%
      e_tooltip(trigger = "axis", formatter = JS("function() { return ''; }")) %>%
      e_x_axis(
        name = "",
        min = 0,
        max = total_time,
        axisLabel = list(show = FALSE)  
      ) %>%
      e_y_axis(name = "Win Probability", formatter = "{value}") %>%
      e_grid(left = "15%") %>%
      e_color(c(chart$home_col, chart$away_col)) %>%
      
      onRender("
        function(el, x) {
          var chart = echarts.getInstanceByDom(el);
          chart.setOption({
            xAxis: {
              data: null
            },
            graphic: [
              {
                type: 'text',
                left: chart.convertToPixel({ xAxisIndex: 0 }, 1100),
                top: el.offsetHeight - 50,
                style: { text: 'Half Time', fill: '#000', font: 'bold 12px sans-serif' }
              },
              {
                type: 'text',
                left: chart.convertToPixel({ xAxisIndex: 0 }, 2300),
                top: el.offsetHeight - 50,
                style: { text: 'End', fill: '#000', font: 'bold 12px sans-serif' }
              }
            ]
          });
        }
      ")
    # Update the position of my mouse to record basic information and update time by time.
    e_chart <- e_chart %>% onRender("
      function(el, x) {
        var myChart = echarts.getInstanceByDom(el);
        if (myChart) {
          // Update tooltip when move the mouse
          el.addEventListener('mousemove', function(event) {
            var pointInPixel = [event.offsetX, event.offsetY];
            var pointInGrid = myChart.convertFromPixel('grid', pointInPixel);
            
            if (pointInGrid) {
              var xCoord = pointInGrid[0]; // X axis
              var yCoord = pointInGrid[1]; // Y axis
              Shiny.setInputValue('hover_event', {
                x: xCoord,
                y: yCoord,
              }, {priority: 'event'});
            }
          });
    
          // clean if the mouse leave the frame
          el.addEventListener('mouseout', function() {
            Shiny.setInputValue('hover_event', null, {priority: 'event'});
          });
        }
      }
    ")
    e_chart
  })
  
  # Update basic information(Score, Win prob, etc.) as mouse move in the graph.
  observeEvent(input$hover_event, {
    req(chart_data())
    hover <- input$hover_event
    chart <- chart_data()
    
    data <- chart_data()$data  # which contains the secs_elapsed
    # get the closest point in x direction
    closest_point <- data[which.min(abs(data$secs_elapsed - hover$x)), ]
    #print(hover$x)
    if (nrow(closest_point) == 0) {
      return()
    }
    
    closest_point <- closest_point[1, ]  # If there are many points, choose the first one
    
    # Update score and win prob
    shinyjs::html("home_score", closest_point$home_score)
    shinyjs::html("away_score", closest_point$away_score)
    shinyjs::html("home_prob", paste0(round((closest_point$win_prob) * 100), "%"))
    shinyjs::html("away_prob", paste0(round((1-closest_point$win_prob) * 100), "%"))
    
    # calculate the phase and remian time
    total_time <- max(data$secs_remaining_absolute, na.rm = TRUE)
    remaining_time <- closest_point$secs_remaining_absolute
    phase <- "Final"  # default
    phase_remaining_time <- remaining_time
    
    # Each phase is 20mintues = 1200seconds in NCAA
    if (remaining_time > (total_time - 1200)) {
      phase <- "First Half"
      phase_remaining_time <- remaining_time - (total_time - 1200)
    } else if (remaining_time > (total_time - 2400)) {
      phase <- "Second Half"
      phase_remaining_time <- remaining_time - (total_time - 2400)
    } else {
      phase <- "Overtime"
    }
    
    # transfer seconds to m/s
    minutes <- floor(phase_remaining_time / 60)
    seconds <- phase_remaining_time %% 60
    
    shinyjs::html(
      "game_phase", 
      sprintf("Remaining Time: %02d:%02d - %s", minutes, seconds, phase)
    )
    if (is.null(hover) || is.null(hover$x) ||hover$x < 0 || hover$x > total_time ||hover$y <0  ||hover$y>1) {
      # if mouse leave the chart, the score and win prob change to the final result.
      last_win_prob <- chart$data[nrow(chart$data), ]$win_prob
      shinyjs::html("home_score", chart$home_score)
      shinyjs::html("away_score", chart$away_score)
      shinyjs::html("home_prob", paste0(round((1 - last_win_prob) * 100), "%"))
      shinyjs::html("away_prob", paste0(round(last_win_prob * 100), "%"))
      shinyjs::html("game_phase", "Remaining Time: Final")
    }
  })
  
  ###################################################################################
  # 2.1 Second Part Bar Plot
  ###################################################################################
  output$team_stats_chart <- renderEcharts4r({
    req(stats_data(), chart_data())  
    stats <- stats_data()
    chart <- chart_data()  
    # print(str(stats))
    # print(sprintf("Home Team Color: %s, Away Team Color: %s", chart$home_col, chart$away_col)) 
    # check print the color
    
    # Prepare the data(select the feature for bar plot)
    team_stats <- data.frame(
      Stat = c("REB", "AST", "STL", "BLK", "TO", "FG%", "3P%", "FT%"),
      # minus here is for horizontal bar plot, however we modify the display of data.
      Home = -c(stats$Rebounds[1], stats$Assists[1], stats$Steals[1],
                stats$Blocks[1], stats$Turnovers[1], stats$FieldGoal_Rate[1],
                stats$ThreePointer_Rate[1], stats$FreeThrow_Rate[1]), 
      
      Away = c(stats$Rebounds[2], stats$Assists[2], stats$Steals[2],
               stats$Blocks[2], stats$Turnovers[2], stats$FieldGoal_Rate[2],
               stats$ThreePointer_Rate[2], stats$FreeThrow_Rate[2])
    )
    head(team_stats)
    # Horizontal bar plot 
    team_stats %>%
      e_charts(Stat) %>%
      e_bar(Home, name = "Home Team", stack = "grp") %>%
      e_bar(Away, name = "Away Team", stack = "grp") %>%
      e_x_axis(inverse = TRUE) %>%
      e_y_axis(axisLabel = list(interval = 0, rotate = 45), show = FALSE) %>%
      e_flip_coords() %>%
      e_tooltip(trigger = "axis", formatter = JS("
      function(params) {
        var tooltip = '';
        params.forEach(function(param) {
          var value = param.value[0];
          if (isNaN(value)) {
            console.error('Invalid value detected:', param);  
          }
          if (!isNaN(value)) {
            value = Math.abs(value);
          } else {
            value = 0;  
          }
          tooltip += param.seriesName + ': ' + value.toFixed(2) + '<br>';  
        });
        return tooltip;
      }
    ")) %>%
      e_legend(right = 10) %>%
      e_color(c(chart$home_col, chart$away_col)) %>%
      e_grid(left = "15%") %>%
      e_labels(
        position = "outside", 
        formatter = JS("
            function(value) {
              var val = value.value[0];
              if (!isNaN(val)) {  
                return Math.abs(val).toFixed(1);  
              }
              return '0';  
            }
          "),
        fontSize = 12
      )
  })
  
  ###################################################################################
  # 2.2 Second part pie chart
  ###################################################################################
  # free_throws_chart
  output$free_throws_chart <- renderEcharts4r({
    req(stats_data(), chart_data())
    stats <- stats_data()
    chart <- chart_data()
    
    data <- data.frame(
      Team = c("Home Team", "Away Team"),
      Score = c(stats$FreeThrows_Made[1], stats$FreeThrows_Made[2])
    )
    
    data %>%
      e_charts(Team) %>%
      e_pie(
        Score,
        radius = c("40%", "70%"),
        roseType = "radius",         
        label = list(
          show = TRUE,
          position = "inside",
          formatter = "{b}: {c}"
        ),
        itemStyle = list(
          borderRadius = 10,         
          borderWidth = 2,           
          borderColor = "#ffffff"    
        )
      ) %>%
      e_color(c(chart$home_col, chart$away_col)) %>%
      e_tooltip(formatter = JS("
      function(params) {
        return params.name + ': ' + params.value + ' (' + params.percent.toFixed(2) + '%)';
      }
    ")) %>%
      e_legend(show = TRUE)
  })
  # field_goals_chart
  output$field_goals_chart <- renderEcharts4r({
    req(stats_data(), chart_data())
    stats <- stats_data()
    chart <- chart_data()
    
    data <- data.frame(
      Team = c("Home Team", "Away Team"),
      Score = c(stats$FieldGoals_Made[1], stats$FieldGoals_Made[2])
    )
    
    data %>%
      e_charts(Team) %>%
      e_pie(
        Score,
        radius = c("40%", "70%"),
        roseType = "radius",         
        label = list(
          show = TRUE,
          position = "inside",
          formatter = "{b}: {c} ({d}%)"
        ),
        itemStyle = list(
          borderRadius = 10,         
          borderWidth = 2,           
          borderColor = "#ffffff"    
        )
      ) %>%
      e_color(c(chart$home_col, chart$away_col)) %>%
      e_tooltip(formatter = JS("
      function(params) {
        return params.name + ': ' + params.value + ' (' + params.percent.toFixed(2) + '%)';
      }
    ")) %>%
      e_legend(show = TRUE)  
  })
  
  # three_pointers_chart
  output$three_pointers_chart <- renderEcharts4r({
    req(stats_data(), chart_data())
    stats <- stats_data()
    chart <- chart_data()
    
    data <- data.frame(
      Team = c("Home Team", "Away Team"),
      Score = c(stats$ThreePointers_Made[1], stats$ThreePointers_Made[2])
    )
    
    data %>%
      e_charts(Team) %>%
      e_pie(
        Score,
        radius = c("40%", "70%"),
        roseType = "radius",         
        label = list(
          show = TRUE,
          position = "inside",
          formatter = "{b}: {c} ({d}%)"
        ),
        itemStyle = list(
          borderRadius = 10,         
          borderWidth = 2,           
          borderColor = "#ffffff"    
        )
      ) %>%
      e_color(c(chart$home_col, chart$away_col)) %>%
      e_tooltip(formatter = JS("
      function(params) {
        return params.name + ': ' + params.value + ' (' + params.percent.toFixed(2) + '%)';
      }
    ")) %>%
      e_legend(show = TRUE)  
  })
  
  
  
  ###################################################################################
  # 3.1 Third part leading player rose(pie) chart
  ###################################################################################
  leading_players_data <- reactive({
    game <- game_data()
    req(game)
    
    # Refer NBA traditional for data choosing, we choose the first-place in both team in each category 
    # as a leading player
    categories <- c("PTS", "REB", "AST", "BLK")
    
    home_players <- bind_rows(game$home_starters, game$home_bench) %>%
      select(player, all_of(categories)) %>%
      mutate(team = "Home")
    
    away_players <- bind_rows(game$away_starters, game$away_bench) %>%
      select(player, all_of(categories)) %>%
      mutate(team = "Away")
    
    combined <- bind_rows(home_players, away_players) %>%
      pivot_longer(cols = all_of(categories), names_to = "category", values_to = "value")
    
    # When scores are the same, schosen by whose PTS higher
    combined <- combined %>%
      group_by(category, team) %>%
      arrange(desc(value), desc("PTS")) %>%  
      slice_head(n = 1) %>%  # first place
      ungroup()
    
    return(combined)
  })
  
  
  output$leading_players_chart <- renderEcharts4r({
    req(leading_players_data(), chart_data())  
    chart <- chart_data()
    home_color <- chart$home_col  
    away_color <- chart$away_col  
    # get stats of first place players
    stats <- leading_players_data()
    player_data <- stats %>%
      arrange(category, team) %>%  
      mutate(
        id = paste(player, row_number(), sep = "_"), # using for distinguish a same person got many first prize
        label = paste0(player, " (", value, ")"),  
        color = ifelse(team == "Home", home_color, away_color)  
      )
    
    # print(player_data)
    player_data %>%
      e_charts(id) %>%
      e_pie(
        value,
        radius = c("15%", "50%"),  
        roseType = "area",         # fixed angle
        label = list(
          show = TRUE,
          position = "outside",    
          formatter = JS("
            function(params) {
              // dynamically remove the part after _
              var nameWithoutSuffix = params.name.split('_')[0];
              return nameWithoutSuffix + ': ' + params.value;
            }
          ")
        )
      ) %>%
      e_color(player_data$color) %>%
      e_tooltip(formatter = JS("
        function(params) {
          var nameWithoutSuffix = params.name.split('_')[0];
          return nameWithoutSuffix + ': ' + params.value + ' (' + params.percent.toFixed(2) + '%)';
        }
      ")) %>%
      e_legend(show = FALSE)%>% onRender("
        function(el, x) {
          var chart = echarts.getInstanceByDom(el);
          chart.setOption({
            graphic: [
              {
                type: 'text',
                left: '51%',
                top: '44%',
                rotation: 3*Math.PI / 4,
                style: {
                  text: 'AST',
                  fill: '#000',
                  font: 'bold 14px Arial',
                  textAlign: 'center',
                  textVerticalAlign: 'middle'
                }
              },
              {
                type: 'text',
                left: '51%',
                top: '51%',
                rotation: Math.PI / 4,
                style: {
                  text: 'BLK',
                  fill: '#000',
                  font: 'bold 14px Arial',
                  textAlign: 'center',
                  textVerticalAlign: 'middle'
                }
              },
              {
                type: 'text',
                left: '46%',
                top: '44%',
                rotation: 5*Math.PI / 4,
                style: {
                  text: 'REB',
                  fill: '#000',
                  font: 'bold 14px Arial',
                  textAlign: 'center',
                  textVerticalAlign: 'middle'
                }
              },
              {
                type: 'text',
                left: '46%',
                top: '51%',
                rotation: 7*Math.PI / 4,
                style: {
                  text: 'PTS',  // corrext to 'PTS' rather than 'AST'
                  fill: '#000',
                  font: 'bold 14px Arial',
                  textAlign: 'center',
                  textVerticalAlign: 'middle'
                }
              }
            ]
          });
        }
      ")
  })
  
  
  
  
  ###################################################################################
  # 3.2 Third Players Stats
  ###################################################################################
  # home_starters
  output$home_starters_table <- renderTable({
    req(game_data())
    game_data()$home_starters
  })
  
  # home_bench
  output$home_bench_table <- renderTable({
    req(game_data())
    game_data()$home_bench
  })
  
  #  away_starters
  output$away_starters_table <- renderTable({
    req(game_data())
    game_data()$away_starters
  })
  
  # away_bench
  output$away_bench_table <- renderTable({
    req(game_data())
    game_data()$away_bench
  })
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------
  #This part is for team information
  selected_team <- reactiveVal(NULL)
  # From competition page to Team data page
  observeEvent(input$go_to_team_data, {
    updateNavbarPage(session, inputId = "navId", selected = "teams")
  })
  
  # Hide Details tab on load
  session$onFlushed(function() {
    shinyjs::runjs("$('#navId li a[data-value=\"details\"]').hide();")
  }, once = TRUE)
  
  # Clear team selection when switching to teams page
  observeEvent(input$navId, {
    if (input$navId == "teams") {
      selected_team(NULL)
      shinyjs::runjs("$('#navId li a[data-value=\"details\"]').hide();")
    }
  })
  
  # Filter teams based on user selection
  filtered_teams <- reactive({
    data <- teams_data
    if (input$conference_filter != "ALL") {
      data <- data %>% filter(conference == input$conference_filter)
    }
    if (input$team_filter != "ALL") {
      data <- data %>% filter(espn_name == input$team_filter)
    }
    data
  })
  
  # Update team filter choices dynamically
  observe({
    conference_selected <- input$conference_filter
    teams_in_conference <- if (conference_selected == "ALL") {
      unique(teams_data$espn_name)
    } else {
      unique(teams_data %>% filter(conference == conference_selected) %>% pull(espn_name))
    }
    updateSelectInput(
      session, 
      "team_filter", 
      choices = c("ALL", teams_in_conference), 
      selected = "ALL"
    )
  })
  
  # Render filtered teams
  output$filtered_teams_ui <- renderUI({
    data <- filtered_teams()
    fluidRow(
      lapply(1:nrow(data), function(i) {
        team_name <- data$espn_name[i]
        logo_url <- data$logo_url[i]
        column(
          width = 2,
          style = "text-align:center; margin-bottom:20px;",
          actionLink(
            inputId = paste0("select_team_", i),
            label = tagList(
              tags$img(src = logo_url, width = "100px", height = "100px"),
              tags$div(team_name, style = "margin-top:5px;")
            ),
            style = "text-decoration:none; color:inherit;"
          )
        )
      })
    )
  })
  
  # Handle team selection dynamically
  observeEvent(filtered_teams(), {
    data <- filtered_teams()
    lapply(1:nrow(data), function(i) {
      local({
        my_i <- i
        link_id <- paste0("select_team_", my_i)
        team_name_i <- data$espn_name[my_i]
        
        observeEvent(input[[link_id]], {
          selected_team(team_name_i)
          shinyjs::runjs("$('#navId li a[data-value=\"details\"]').show();")
          updateNavbarPage(session, "navId", selected = "details")
        }, ignoreInit = TRUE)
      })
    })
  })
  
  output$teamSelected <- reactive({
    !is.null(selected_team())
  })
  outputOptions(output, "teamSelected", suspendWhenHidden = FALSE)
  
  output$selected_team_name <- renderText({
    req(selected_team())
    selected_team()
  })
  ##############################################################################
  # Display team logo
  ##############################################################################
  output$team_logo_ui <- renderUI({
    req(selected_team())
    team_info <- filtered_teams() %>% filter(espn_name == selected_team())
    if (nrow(team_info) > 0) {
      tags$img(src = team_info$logo_url[1], width = "100px", height = "100px")
    } else {
      tags$p("No logo available")
    }
  })
  
  # Get roster data
  roster_data <- reactive({
    req(selected_team(), input$selected_year)
    tryCatch({
      get_roster(team = selected_team(), season = input$selected_year)
    }, error = function(e) {
      data.frame()
    })
  })
  
  
  # Update player selection when roster data changes
  observe({
    players <- roster_data()$name
    if (length(players) > 0) {
      updateSelectInput(session, "selected_player",
                        choices = c("--select--" = "", players),
                        selected = ""
      )
    }
  })
  
  # Get team data
  team_data <- reactive({
    req(selected_team(), input$selected_year)
    team_name <- selected_team()
    year_str <- input$selected_year
    
    schedule_data <- tryCatch({
      get_schedule(team = team_name, season = year_str)
    }, error = function(e) {
      data.frame()
    })
    if("team_score" %in% names(schedule_data) && "opp_score" %in% names(schedule_data)) {
      schedule_data <- schedule_data %>% mutate(score_diff = team_score - opp_score)
    }
    schedule_data
  })
  
  all_games_stats <- reactive({
    req(selected_team())
    name_info <- teams_data %>%
      filter(espn_name == selected_team()) %>%
      select(ncaa_name, espn_name, sref_name)
    
    team_name_candidates <- c(name_info$ncaa_name, name_info$espn_name, name_info$sref_name)
    team_name_candidates <- team_name_candidates[!is.na(team_name_candidates)]
    
    td <- team_data() 
    req("game_id" %in% names(td))
    
    td$game_id %>%
      map_df(function(g_id) {
        box <- get_boxscore(g_id)
        matched_box1 <- any(sapply(team_name_candidates, function(cand) {
          any(grepl(cand, box[[1]]$team))
        }))
        matched_box2 <- any(sapply(team_name_candidates, function(cand) {
          any(grepl(cand, box[[2]]$team))
        }))
        
        if (matched_box1) {
          team_box <- box[[1]]
        } else if (matched_box2) {
          team_box <- box[[2]]
        } else {
          team_box <- data.frame()
        }
        team_box
      })
  })
  
  Player_data <- reactive({
    ags <- all_games_stats()
    req("player" %in% names(ags))
    ags %>%
      group_by(player) %>%
      summarize(
        across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 2), .names = "{.col}"),
        .groups = "drop"
      )
  })
  
  # Table display
  output$team_games_table <- renderDT({
    df <- team_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(message="No Data"), options = list(pageLength = 5)))
    }
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  ##############################################################################
  # other_table display another_data
  ##############################################################################
  output$other_table <- renderDT({
    df <- Player_data()
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Get team color
  get_team_color <- reactive({
    req(selected_team())
    team_color <- teams_data %>%
      filter(espn_name == selected_team()) %>%
      pull(primary_color)
    
    if (length(team_color) == 0 || is.na(team_color)) {
      return("royalblue4")  # fallback color
    }
    return(team_color)
  })
  ##############################################################################
  # Assist Network Plot with team-specific color
  ##############################################################################
  output$assist_network_plot <- renderPlot({
    req(selected_team(), input$selected_year, input$assignmentForm != "")
    # Additional requirement for circle assist network
    if (input$assignmentForm == "circle_assist_net") {
      req(input$selected_player)
      # Validate that we have roster data
      validate(
        need(nrow(roster_data()) > 0, "Unable to load roster data for the selected team and season"),
        need(input$selected_player != "", "Please select a player to view the circle assist network")
      )
    }
    
    team_color <- get_team_color()
    
    tryCatch({
      if (input$assignmentForm == "assist_net") {
        assist_net(
          team = selected_team(),
          node_col = team_color,
          season = input$selected_year
        )
      } else if (input$assignmentForm == "circle_assist_net") {
        req(input$selected_player)
        circle_assist_net(
          team = selected_team(),
          season = input$selected_year,
          highlight_player = input$selected_player,
          highlight_color = team_color
        )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Unable to generate assist network for this selection", cex = 1.2)
    })
  })
  
  
  ##########################################################################################
  # Prediction Part
  ##########################################################################################
  output$history_table <- renderDataTable({
    historic_data %>%
      select(game_id, home, away, home_score, away_score, winner, margin)
    output$history_table <- renderDataTable({
      historic_data %>%
        select(game_id, home, away, home_score, away_score, winner, margin)
    })
    
    output$history_plot <- renderPlot({
      plot_data <- historic_data %>%
        mutate(UNC_margin = ifelse(home == "UNC", home_score - away_score,
                                   ifelse(away == "UNC", away_score - home_score, NA)))
      ggplot(plot_data, aes(x = factor(game_id), y = UNC_margin)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "UNC vs Duke Historical Margin (UNC Perspective)",
             x = "Game ID",
             y = "Margin (UNC Score - Duke Score)")
    })
    
    
    output$pred_table <- renderDataTable({
      match_2023_24 %>%
        select(date, home, away, pred_winner, pred_margin_abs) %>%
        rename(`Game Date` = date,
               `Home Team` = home,
               `Away Team` = away,
               `Predicted Winner` = pred_winner,
               `Predicted Margin` = pred_margin_abs)
    })
  })
  
  #####################################################################################
  # Schedule
  #####################################################################################
  output$next_duke_game <- renderText({
    next_duke_game
  })
  # Reactive timer to trigger updates every second
  timer <- reactiveTimer(1000)
  output$countdown <- renderText({
    timer()  # Trigger updates every second
    time_left <- as.numeric(difftime(next_duke_game$date, Sys.time(), units = "secs"))  # Convert to numeric
    
    if (time_left <= 0) {
      return("Match has started!")
    }
    
    days <- floor(time_left / (60 * 60 * 24))
    hours <- floor((time_left %% (60 * 60 * 24)) / (60 * 60))
    minutes <- floor((time_left %% (60 * 60)) / 60)
    seconds <- floor(time_left %% 60)
    
    paste0(days, "d ", hours, "h ", minutes, "m ", seconds, "s")
  })
  
  # Did Duke Win?
  output$did_duke_win <- renderText({
    did_duke_win
  })
}
# Run the Shiny App
shinyApp(ui = ui, server = server)
