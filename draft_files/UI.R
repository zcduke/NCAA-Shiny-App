library(shiny)
library(bslib)

# Define UI
ui <- fluidPage(
  tags$head(
    # 自定义 CSS：实现背景图片多张循环切换并隐藏滚动条
    tags$style(HTML("
      @keyframes cycleBackgrounds {
        0% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F12%2F9%2F2024_Duke_MBB_Game_v_ARMY__11-08-145_16x9.jpg&height=1100&type=webp'); }
        33% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2021%2F9%2F29%2FCoach_K_practice.jpg&height=1100&type=webp'); }
        66% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F12%2F9%2F2024_Duke_MBB_Louisville_Game_12-08-158.jpg&height=1100&type=webp'); }
        100% { background-image: url('https://images.sidearmdev.com/resize?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fgoduke.com%2Fimages%2F2024%2F12%2F7%2F2024_Duke_MBB_Auburn_Game_12-04-051_16x9.jpg&height=1100&type=webp'); }
      } 
      body, html {
        margin: 0;
        padding: 0;
        overflow: hidden; /* 隐藏滚动条 */
      }
      .welcome-page {
        height: 100vh;
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        animation: cycleBackgrounds 40s infinite; /* 背景循环切换 */
      }
      .welcome-content {
        color: white;
        text-shadow: 2px 2px 4px #000000;
        text-align: center;
        padding-top: 30vh;
      }
      .welcome-content h1 {
        font-size: 48px; /* 主标题字体大小 */
      }
      .welcome-content p {
        font-size: 20px; /* 段落文字字体大小 */
      }
      .button-container {
        margin-top: 20px;
      }
      .navbar {
        background: linear-gradient(to right, #019cde, #007bc2); /* 渐变背景色 */
        border-bottom: none; /* 确保无底部边框 */
      }
      .navbar-brand {
        color: white !important; /* 标题文字颜色 */
        font-size: 1.5em; /* 调整标题字体大小 */
        font-weight: bold; /* 标题加粗 */
      }
      .navbar-nav > li > a {
        color: white !important; /* 导航链接文字颜色 */
        text-decoration: none; /* 去掉下划线 */
      }
      .navbar-nav > li > a:hover {
        color: #012169 !important; /* 鼠标悬停时链接的颜色 */
        border-bottom: none !important; /* 鼠标悬停时去除底部边框 */
      }
    "))
  ),
  
  page_navbar(
    title = "NCAA Data Archive",
    id = "navId", 
    
    # Welcome Page
    nav_panel(
      title = "Welcome", 
      value = "welcome",
      div(
        class = "welcome-page",
        div(
          class = "welcome-content",
          h1("Welcome to NCAA Data Archive"),
          p("The world of basketball game data"),
          div(
            class = "button-container",
            actionButton("go_to_game", "Go to Game Data", class = "btn-primary"),
            actionButton("go_to_team", "Go to Team Data", class = "btn-primary"),
            actionButton("go_to_writeup", "Go to Write Up", class = "btn-primary")
          )
        )
      )
    ),
    
    # Game Data Page
    nav_panel(
      title = "Game Data", 
      value = "game_data",
      h1("Game Data Section", style = "color: #007bc2;"),
      p("This page contains detailed game data."),
      actionButton("back_to_welcome_from_game", "Go Back to Welcome Page", class = "btn-light")
    ),
    
    # Team Data Page
    nav_panel(
      title = "Team Data", 
      value = "team_data",
      h1("Team Data Section", style = "color: #007bc2;"),
      p("This page contains detailed team data."),
      actionButton("back_to_welcome_from_team", "Go Back to Welcome Page", class = "btn-light")
    ),
    
    # Write Up Page
    nav_panel(
      title = "Write Up", 
      value = "writeup",
      h1("Write Up Section", style = "color: #007bc2;"),
      p("This is where you can find the write-up and detailed information."),
      textAreaInput(
        inputId = "writeup_text",
        label = "Enter your write-up here:",
        width = "100%",
        height = "200px",
        placeholder = "Start typing your content..."
      ),
      actionButton("back_to_welcome_from_writeup", "Go Back to Welcome Page", class = "btn-light")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Navigate to Game Data page
  observeEvent(input$go_to_game, {
    updateNavbarPage(session, "navId", selected = "game_data")
  })
  
  # Navigate to Team Data page
  observeEvent(input$go_to_team, {
    updateNavbarPage(session, "navId", selected = "team_data")
  })
  
  # Navigate to Write Up page
  observeEvent(input$go_to_writeup, {
    updateNavbarPage(session, "navId", selected = "writeup")
  })
  
  # Navigate back to Welcome page
  observeEvent(input$back_to_welcome_from_game, {
    updateNavbarPage(session, "navId", selected = "welcome")
  })
  observeEvent(input$back_to_welcome_from_team, {
    updateNavbarPage(session, "navId", selected = "welcome")
  })
  observeEvent(input$back_to_welcome_from_writeup, {
    updateNavbarPage(session, "navId", selected = "welcome")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
