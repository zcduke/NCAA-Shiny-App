library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(ncaahoopR)
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
print(importance)

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

########################################
# Build Shiny application
########################################

ui <- dashboardPage(
  dashboardHeader(title = "UNC vs Duke Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction Method", tabName = "intro", icon = icon("info-circle")),
      menuItem("Historical Data", tabName = "history", icon = icon("chart-line")),
      menuItem("2023-24 Predictions", tabName = "predictions", icon = icon("bullseye"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h2("Introduction to XGBoost"),
              p("
XGBoost (eXtreme Gradient Boosting) is an efficient and scalable implementation of the gradient boosting algorithm, designed for both regression and classification tasks. Developed by Tianqi Chen, XGBoost has gained popularity due to its high performance, flexibility, and ability to handle large datasets with high-dimensional features.

Key features of XGBoost include:

Gradient Boosting: It uses gradient descent to optimize an ensemble of decision trees by iteratively minimizing the loss function.
Regularization: L1 (Lasso) and L2 (Ridge) regularization are incorporated to prevent overfitting and improve generalization.
Parallelization: XGBoost supports parallel computation, making it significantly faster than traditional gradient boosting.
Handling Missing Values: The algorithm automatically handles missing data without requiring imputation.
Tree Pruning: It uses a `max depth` and `min child weight` approach to control tree complexity, reducing the chance of overfitting.
Customizable Objective Functions: XGBoost allows users to define custom loss functions, making it versatile for various tasks.
Due to these advantages, XGBoost is widely used in data science competitions and real-world applications for tasks like forecasting, classification, and anomaly detection."),
              h2("How XGBoost is Applied in This Program"),
              p("In our program, XGBoost is used to predict the point margin between the University of North Carolina (UNC) and Duke basketball teams based on historical game data. The data, gathered from the ncaahoopR package, includes scores, game locations, and team identifiers. We create features such as game_index (the chronological order of games), is_unc_home (indicating if UNC is the home team), and is_duke_home (indicating if Duke is the home team), with the target variable being margin, which represents the score difference (home score minus away score).

The dataset is split into a training set (80%) and a validation set (20%) to evaluate model performance. An XGBoost model is then trained with parameters set for regression tasks, including a learning rate (eta) of 0.1, a maximum tree depth of 3, and early stopping to prevent overfitting. After training, feature importance is examined to understand which factors contribute most to predictions.

For the 2023-24 season, we retrieve UNC’s upcoming schedule and identify games against Duke. The trained model is used to predict the point margin for these games, rounding the predictions up to the nearest integer using ceiling() for clarity. Based on these margins, the predicted winner is determined (positive margin for the home team, negative for the away team). The results are presented in a Shiny dashboard, allowing users to explore historical data, visualize past performance, and view predictions for the 2023-24 season, including the forecasted winner and score margin.")
      ),

      tabItem(tabName = "history",
              h2("UNC vs Duke Historical Data"),
              dataTableOutput("history_table"),
              h3("Historical Margin Visualization (UNC Perspective)"),
              plotOutput("history_plot")
      ),

      tabItem(tabName = "predictions",
              h2("2023-24 Season Prediction Results"),
              dataTableOutput("pred_table")
      )
    )
  )
)

server <- function(input, output, session) {

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
}

shinyApp(ui, server)
