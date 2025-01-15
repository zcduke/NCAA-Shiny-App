### Function to render a Win Probability Chart using ggplot2
library("ggplot2")
library("dplyr")
library("ncaahoopR")
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