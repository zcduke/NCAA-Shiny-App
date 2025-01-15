library(shiny)
library(dplyr)
library(ggplot2)
library(ncaahoopR)
library(shinyjs)
library(echarts4r)
library(htmlwidgets)
library(rsconnect)
source("win_prob_info.R")
source("match_info.R")
source("player_info.R")

# UI Part
ui <- fluidPage(
  useShinyjs(),
  
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
  # title for the panel
  titlePanel("Competition Analysis"),
  # sidebar layout for competition searching
  sidebarLayout(
    # sidebar
    sidebarPanel(
      textInput("game_id", "Enter Game ID:", value = "401168364"), # The competition between Duke and UNC
      actionButton("update", "Update Chart")
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
      "))
    )
  )
)





###################################################################################
# Server Part
###################################################################################
server <- function(input, output, session) {
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
    # Update chart_data when get to now the competition which is being searched
    chart_data(chart)
    # debug code to see the chart 
    # print(names(chart))
    
    # Update team logos for Title
    shinyjs::runjs(sprintf("document.getElementById('home_logo').src = '%s';", chart$home_url))
    shinyjs::runjs(sprintf("document.getElementById('away_logo').src = '%s';", chart$away_url))
    
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
    print(str(stats))
    #print(sprintf("Home Team Color: %s, Away Team Color: %s", chart$home_col, chart$away_col)) 
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
  # 3. Third part leading player rose(pie) chart
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
      slice(1) %>%  # first place
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
                  text: 'PTS',  // 修正为 'PTS' 而非重复的 'AST'
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
  # Third Players Stats
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
  
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
