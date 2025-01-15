library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ncaahoopR)
library(shinyjs)
library(DT)
library(purrr)
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
#teams_data$logo_url[teams_data$espn_name == 'Florida A&M'] <- 'https://upload.wikimedia.org/wikipedia/commons/7/75/Florida_A%26M_Rattlers_wordmark.svg'


ui <- page_navbar(
  title = "NCAA Data Archive",
  id = "navId", 
  bg = NULL,
  
  useShinyjs(),
  
  tags$style(HTML("
    .navbar {
      background: linear-gradient(to right, #8ed7fe, #019cde); /* 渐变背景色 */
      border-bottom: 2px solid #900C3F; /* 可选：添加底部边框 */
    }
    .navbar-brand {
      color: white !important; /* 标题文字颜色 */
      font-size: 1.5em; /* 调整标题字体大小 */
      font-weight: bold; /* 标题加粗 */
    }
    .navbar-nav > li > a {
      color: white !important; /* 导航链接文字颜色 */
    }
    .navbar-nav > li > a:hover {
      color: #012169 !important; /* 鼠标悬停时链接的颜色 */
    }
  ")),
  
  # Teams page
  nav_panel(
    title = "Teams", value = "teams",
    h2("NCAA Teams"),
    p("Click team name or logo for detail"),
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
  
  # Details page
  nav_panel(
    title = "Details", value = "details",
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
            title = "Tabular",
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
                  dataTableOutput('other_table')
                )
              )
            )
          ),
          nav_panel(
            title = "Chart",
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
              plotOutput("assist_network_plot", height = "calc(100vh - 260px)", width = "100%")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_team <- reactiveVal(NULL)
  
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
  
  # Display team logo
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
  
  # other_table显示another_data
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
  
  # Assist Network Plot with team-specific color
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
}

shinyApp(ui = ui, server = server)
