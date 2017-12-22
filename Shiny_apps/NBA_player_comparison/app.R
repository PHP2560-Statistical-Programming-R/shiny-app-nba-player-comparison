# check for requiered packages and get them if needed
names <- c(
  "shiny",
  "dplyr", 
  "shinydashboard", 
  "stringr", 
  "httr", 
  "rvest"
)

for(name in names) {
  if (!(name %in% installed.packages()))
    install.packages(name, repos="http://cran.us.r-project.org")
  library(name, character.only=TRUE)
}

# get the seasons
get_seasons <- function(first = 1949) {
  date <- Sys.Date()
  last <- as.numeric(str_sub(date, 1, 4))
  start <- first:(last-1)
  end <- (first+1):last
  n <- length(start)
  start_end <- replicate(n, NA)
  for(i in 1:n) {
    start_end[i] <- paste0(as.character(start[i]), "-", as.character(end[i]))
  }
  start_end
}

# get the totals table for given season
get_totals <- function(season) {
  end_year <- str_sub(season, -4, -1)
  doc <- GET(paste0("https://www.basketball-reference.com/leagues/NBA_", end_year, "_totals.html"))
  cont <- content(doc, "text") %>% 
    gsub(pattern = "<!--\n", "", ., fixed = TRUE) %>% 
    read_html %>% 
    html_nodes(".table_outer_container table") %>% 
    html_table()
  
  df <- cont[[1]] %>%
    filter(Player != "Player")
  df$Player <- df$Player %>% str_replace_all("[:punct:]", "")
  df
}


# get the per game table for given season
get_per_game <- function(season) {
  end_year <- str_sub(season, -4, -1)
  doc <- GET(paste0("https://www.basketball-reference.com/leagues/NBA_", end_year, "_per_game.html"))
  cont <- content(doc, "text") %>% 
    gsub(pattern = "<!--\n", "", ., fixed = TRUE) %>% 
    read_html %>% 
    html_nodes(".table_outer_container table") %>% 
    html_table()
  
  df <- cont[[1]] %>%
    filter(Player != "Player")
  
  df$Player <- df$Player %>% str_replace_all("[:punct:]", "")
  df
}


# get list of players given table
get_player_list <- function(df) {
  player_df <- df %>%
    select(Player) %>%
    unique() 
  player_df$Player <- sort(player_df$Player)
  player_df
}


# get individual player table given player
get_player_table <- function(player, df) {
  player_table_df <- df %>%
    filter(Player == player)
  player_table_df
}


# get player age
get_player_age <- function(player_table_df) {
  player_table_df["Age"] %>% unique()
}


get_player_position <- function(player_table_df) {
  player_table_df["Pos"] %>% unique()
}


# get_player
get_player_team <- function(player_table_df) {
  if(length(player_table_df$Tm > 1)) {
    return(player_table_df["Tm"] %>% filter(Tm != "TOT"))
  } else {
    return(player_table_df["Tm"] %>% unique())
  }
}


get_player_stats <- function(player_table_df) {
  player_table_df[1, -c(1, 2, 3, 4, 5)]
}


get_pic_link <- function(player) {
  split <- str_split(tolower(player), " ")
  name <- unlist(split)
  two <- str_sub(name[1], 1, 2)
  five <- str_sub(name[2], 1, 5)
  first <- "01"
  second <- "02"
  third <- "03"
  # placeholder
  "https://d2p3bygnnzw9w3.cloudfront.net/req/999999999/images/klecko/placeholder.jpg"
  convention <- paste0(five, two, first)
  link <- paste0("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/", convention, ".jpg")
  link
}


ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Player Comparison",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Ozan",
                                                   message = "Welcome! Let's find out the best player!"
                                                 )
                                    ),
                                    dropdownMenu(type = "notifications",
                                                 notificationItem(
                                                   text = "Please read instructions to start",
                                                   icon = icon("info-circle"),
                                                   status = "success"
                                                 )
                                    )
                    ),
                    dashboardSidebar(
                      width = 180,
                      sidebarMenu(
                        menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
                        menuItem("Compare", tabName = "compare", icon = icon("bar-chart"), badgeLabel = "app", badgeColor = "purple"),
                        menuItem("References", tabName = "sources", icon = icon("code")),
                        menuItem("Contact", tabName = "contact", icon = icon("envelope-o"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "instructions",
                                h4("1. Choose a season"),
                                h4("2. Select the first player"),
                                h4("3. Select the second player"),
                                h4("4. Click Compare"),
                                h4('5. Check out "Per Game" and "Totals" tabs'),
                                h4("6. Every time you choose new player(s) click compare to update the information"),
                                h4("7. Enjoy!"),
                                br(),
                                h4("NOTES:"),
                                h4("> Expansion and explanation of the abbreviations:",
                                   a("Glossary", href="https://www.basketball-reference.com/about/glossary.html")
                                ),
                                h4("> Due to", a("Basketball Reference", href="https://www.basketball-reference.com/"), "'s ID conventions, some pictures may correspond to previous players with similar IDs. Work is being done to improve the accuracy of the code in the future. Despite the possible picture mismatches, all stats are accurate.")
                                ),
                        # Second tab content
                        tabItem(tabName = "compare",
                                fluidRow(
                                  box(
                                    title = "Player 1", status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE, width = 4, height = 330,
                                    htmlOutput("player1imgOutput"),
                                    HTML('<center><div id="info1" class="shiny-html-output"></div></center>')
                                  ),
                                  
                                  box(
                                    title = "Controls", status = "danger", solidHeader = TRUE,
                                    width = 4, height = 330,
                                    selectInput(inputId = "seasonInput", "Choose a season:", choices = get_seasons(), selected = "2016-2017"),
                                    uiOutput("player1Output"),
                                    uiOutput("player2Output"),
                                    uiOutput("compareOutput")
                                  ),
                                  
                                  box(
                                    title = "Player 2", status = "info", solidHeader = TRUE,
                                    collapsible = TRUE, width = 4, height = 330,
                                    htmlOutput("player2imgOutput"),
                                    HTML('<center><div id="info2" class="shiny-html-output"></div></center>')
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title = "Stats",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabsetInput", height = "220px", width = 12,
                                    
                                    tabPanel("Per Game",
                                             div(style = 'overflow-x: scroll', tableOutput('result3Output')),
                                             div(style = 'overflow-x: scroll', tableOutput('result4Output'))
                                    ),
                                    
                                    tabPanel("Totals",
                                             div(style = 'overflow-x: scroll', tableOutput("result1Output")),
                                             div(style = 'overflow-x: scroll', tableOutput("result2Output"))
                                             )
                                    
                                    
                                  )
                                  
                                )
                                
                        ),
                        
                        
                        tabItem(tabName = "sources",
                                h4("All the data is scraped from",
                                   a("Basketball Reference", href="https://www.basketball-reference.com/")

                                )
                        ),
                        
                        tabItem(tabName = "contact",
                                h4("Please feel free to share your comments and/or report any issues."),
                                h4("Ozan Adıgüzel", 
                                   br(),
                                   a("Email", href="mailto:ozanadiguzel147@gmail.com")
                                   )
                                )
                      )
                      
                      
                    )
)


server <- function(input, output, session) {
  
  totals <- reactive( {
    get_totals(input$seasonInput)
  })
  
  per_game <- reactive( {
    get_per_game(input$seasonInput)
  })
  
  main_totals_1 <- eventReactive(input$compareInput, {
    get_player_table(input$player1Input, totals())
  })
  
  main_per_game_1 <- eventReactive(input$compareInput, {
    get_player_table(input$player1Input, per_game())
  })
  
  main_totals_2 <- eventReactive(input$compareInput, {
    get_player_table(input$player2Input, totals())
  })
  
  main_per_game_2 <- eventReactive(input$compareInput, {
    get_player_table(input$player2Input, per_game())
  })
  
  img1 <- eventReactive(input$compareInput, {
    get_pic_link(input$player1Input)
  })
  
  img2 <- eventReactive(input$compareInput, {
    get_pic_link(input$player2Input)
    
  })
  
  info1 <- eventReactive(input$compareInput, {
    age1 <- get_player_age(main_totals_1())
    position1 <- get_player_position(main_totals_1())
    team1 <- get_player_team(main_totals_1())
    cbind(team1, age1, position1, row.names = NULL)
  })
  
  info2 <- eventReactive(input$compareInput, {
    age2 <- get_player_age(main_totals_2())
    position2 <- get_player_position(main_totals_2())
    team2 <- get_player_team(main_totals_2())
    cbind(team2, age2, position2, row.names = NULL)
  })
  
  output$player1Output <- renderUI({
    selectInput(inputId = "player1Input", "Select player 1:", choices = get_player_list(totals()))
  })
  
  output$player2Output <- renderUI({
    selectInput(inputId = "player2Input", "Select player 2:", choices = get_player_list(totals()))
  })
  
  output$compareOutput <- renderUI({
    HTML('<center><button id="compareInput" type="button" class="btn btn-default action-button">Compare</button></center>')
  })
  
  
  output$player1imgOutput <- renderText({
    c('<center>', '<img height="180" width="120" src="', img1(), '">', '</center>')
  })
  
  output$player2imgOutput <- renderText({
    c('<center>', '<img height="180" width="120" src="', img2(), '">', '</center>')
  })
  
  output$info1 <- renderTable({
    info1()
  })
  
  output$info2 <- renderTable({
    info2()
  })
  
  output$result1Output <- renderTable({
    get_player_stats(main_totals_1())
  })
  
  output$result2Output <- renderTable({
    get_player_stats(main_totals_2())
  })
  
  output$result3Output <- renderTable({
    get_player_stats(main_per_game_1())
  })
  
  output$result4Output <- renderTable({
    get_player_stats(main_per_game_2())
  })
  
}

shinyApp(ui = ui, server = server)
