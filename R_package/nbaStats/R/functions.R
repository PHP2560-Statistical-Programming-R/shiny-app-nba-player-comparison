
# please note that the package has: 0 errors | 0 warnings | 0 notes in R CMD check results
# this is a great step to get it accepted to CRAN

# creating global variables to avoid "notes" from R CMD check results
if(getRversion() >= "2.15.1") {
  
  utils::globalVariables(c(".", 
                           "Player", 
                           "W", 
                           "L", 
                           "win", 
                           "lose", 
                           "game_played", 
                           "division_performance",
                           "win_rate",
                           "Div",
                           "Home",
                           "L",
                           "Road",
                           "division",
                           "team",
                           "conference",
                           "home_performance",
                           "road_performance"
  ))
  
}

#' @title Scrape regular season rankings
#'
#' @description Function to scrape regular season rankings from Wikipedia
#' 
#' @author Ozan Adiguzel
#' 
#' @references \url{https://www.wikipedia.org}
#' 
#' @param start_year start year of the season (numeric value from 1976 to 2017)
#' 
#' @return a data frame of regular season ranking
#' 
#' @keywords nba ranking
#' 
#' @export
#' 
#' @examples
#' season_ranking(2017)
#' 
#' @importFrom dplyr %>% mutate if_else
#' @importFrom rebus one_or_more WRD %R% START or


season_ranking <- function(start_year) {
  
  # friendly reminder to choose valid arguments
  if(!(start_year %in% 1976:2017)) {
    return(print("Please enter a valid season start year from 1976 to 2017"))
  }
  
  # Wikipedia is not consistent for different seasons the order of tables differ
  # this sub-function finds first table we need whereever it is
  get_first_table_no <- function(season = NA) {
    # built the link
    link <- paste0("https://en.wikipedia.org/wiki/", season, "_NBA_season")
    # create the url
    url <- read_html(link)
    # get table nodes
    nodes <- html_nodes(url, "table")
    # find the first division table, which corresponds to "Atlantic" division
    for(table_no in 1:length(nodes)) {
      ranking <- html_table(nodes[[table_no]], fill = T)
      if(str_extract(names(ranking)[1], pattern = one_or_more(WRD)) == "Atlantic") break
    }
    # return the first table
    return(table_no)
    
  }
  
  # this sub-function scrapes a table and formats it nicely
  get_table <- function(season = NA, table_no = NA) {
    
    # built the link
    link <- paste0("https://en.wikipedia.org/wiki/", season, "_NBA_season")
    # create the url
    url <- read_html(link)
    # get table nodes
    nodes <- html_nodes(url, "table")
    # get the table
    ranking <- html_table(nodes[[table_no]], fill = T)
    # clean the names
    ranking[ , 1] <- str_replace(ranking[ , 1], pattern = START %R% WRD %R% or(fixed(" - "), "-"), "")
    # further cleaning and arranging for a user friendly table
    ranking <- ranking %>%
      mutate(
        win = as.numeric(W), 
        lose = as.numeric(L),
        game_played = win + lose,
        win_rate = win / game_played,
        home_performance = Home,
        road_performance = Road,
        division_performance = Div,
        season = season,
        division = str_extract(names(ranking)[1], pattern = one_or_more(WRD)), 
        conference = if_else(division %in% c("Atlantic", "Central", "Southeast"), "East", "West")
      )
    
    # rename the first column
    names(ranking)[1] <- "team"
    # rearrange the columns
    ranking <- ranking %>% 
      select(
        season, 
        team, 
        conference, 
        division, 
        game_played, 
        win, 
        lose, 
        win_rate, 
        home_performance, 
        road_performance,
        division_performance
      ) %>%
      arrange(desc(win_rate))

  }
  
  # create the string that we will use to built the link from the argument
  end <- start_year + 1
  start_char <- as.character(start_year)
  end_char <- str_sub(as.character(end), start = -2)
  season <- paste0(start_char, "-", end_char)
  # find the first table using the first sub-function above
  first_table_no <- get_first_table_no(season)
  
  # scrape all tables starting from the first one we found based on given year 
  if(start_year > 2004) {
    atlantic <- get_table(season, table_no = first_table_no)
    central <- get_table(season, table_no = first_table_no + 1)
    southeast <- get_table(season, table_no = first_table_no + 2)
    northwest <- get_table(season, table_no = first_table_no + 3)
    pacific <- get_table(season, table_no = first_table_no + 4)
    southwest <- get_table(season, table_no = first_table_no + 5)
    # merging all division tables
    overall_ranking <- bind_rows(atlantic, central, southeast, northwest, pacific, southwest) %>%
      arrange(desc(win_rate))
    return(overall_ranking)
    # NBA divisions are different before this year, code is slightly different as well
  } else if(start_year < 2004) {
    atlantic <- get_table(season, table_no = first_table_no)
    central <- get_table(season, table_no = first_table_no + 1)
    midthwest <- get_table(season, table_no = first_table_no + 2)
    pacific <- get_table(season, table_no = first_table_no + 3)
    # merge division tables
    overall_ranking <- bind_rows(atlantic, central, midthwest, pacific) %>%
      arrange(desc(win_rate))
  } else {
    # there is no information on this season in Wikipedia
    paste("2004 is not a valid argument due to a problem in the corresponding Wikipedia page")
  }
  
}


############################################################################


#' @title Scrape regular season stats totals for all players
#'
#' @description Function to scrape regular season stats totals for all players from Basketball Reference
#' 
#' @author Ozan Adiguzel
#' 
#' @references \url{https://www.basketball-reference.com}
#' 
#' @note Glossary: \url{https://www.basketball-reference.com/about/glossary.html}
#' 
#' @param start_year start year of the season (numeric value from 1949 to 2017)
#' 
#' @return a data frame of season stats totals
#' 
#' @keywords nba stats totals
#' 
#' @export
#' 
#' @examples
#' season_totals(2017)
#' 
#' @importFrom httr GET content
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom dplyr filter select arrange desc bind_rows
#' @importFrom stringr str_replace_all str_split str_sub str_extract str_replace fixed

season_totals <- function(start_year) {
  
  # friendly reminder to choose valid arguments
  if(!(start_year %in% 1949:2017)) {
    return(print("Please enter a valid season start year from 1949 to 2017"))
  }
  
  # create string based on the argument to built the link
  end_year <- as.character(start_year + 1)
  # extract the content of the linked page
  doc <- GET(paste0("https://www.basketball-reference.com/leagues/NBA_", end_year, "_totals.html"))
  cont <- content(doc, "text") %>%
    # Basketball Reference stores some tables in a weird way
    # reading them from text solves the problem
    gsub(pattern = "<!--\n", "", ., fixed = TRUE) %>% 
    read_html %>% 
    html_nodes(".table_outer_container table") %>% 
    html_table()
  
  # extract the target table
  df <- cont[[1]] %>%
    # clean the player names
    filter(Player != "Player")
  # clean the punctuation
  df$Player <- df$Player %>% str_replace_all("[:punct:]", "")
  df
  
}


############################################################################


#' @title Scrape regular season stats per game for all players
#'
#' @description Function to scrape regular season stats per game for all players from Basketball Reference
#' 
#' @author Ozan Adiguzel
#' 
#' @references \url{https://www.basketball-reference.com}
#' 
#' @note Glossary: \url{https://www.basketball-reference.com/about/glossary.html}
#' 
#' @param start_year start year of the season (numeric value from 1949 to 2017)
#' 
#' @return a data frame of season stats per game
#' 
#' @keywords nba stats pergame
#' 
#' @export
#' 
#' @examples
#' season_pergame(2017)

season_pergame <- function(start_year) {
  
  # friendly reminder to choose valid arguments
  if(!(start_year %in% 1949:2017)) {
    return(print("Please enter a valid season start year from 1949 to 2017"))
  }
  
  # create string based on the argument to built the link
  end_year <- as.character(start_year + 1)
  # extract the content of the linked page
  doc <- GET(paste0("https://www.basketball-reference.com/leagues/NBA_", end_year, "_per_game.html"))
  cont <- content(doc, "text") %>% 
    # Basketball Reference stores some tables in a weird way
    # reading them from text solves the problem
    gsub(pattern = "<!--\n", "", ., fixed = TRUE) %>% 
    read_html %>% 
    html_nodes(".table_outer_container table") %>% 
    html_table()
  
  # extract the target table
  df <- cont[[1]] %>%
    # clean the player names
    filter(Player != "Player")
  # clean the punctuation
  df$Player <- df$Player %>% str_replace_all("[:punct:]", "")
  df
  
}


############################################################################


#' @title Get list of players by season
#'
#' @description Function to get list of players 
#' 
#' @author Ozan Adiguzel
#' 
#' @references \url{https://www.basketball-reference.com}
#' 
#' @param start_year start year of the season (numeric value from 1949 to 2017)
#' 
#' @return a list of players
#' 
#' @keywords nba stats player
#' 
#' @export
#' 
#' @examples
#' season_players(2017)
 
season_players <- function(start_year) {
  
  # friendly reminder to choose valid arguments
  if(!(start_year %in% 1949:2017)) {
    return(print("Please enter a valid season start year from 1949 to 2017"))
  }
  
  # create string based on the argument to built the link
  end_year <- as.character(start_year + 1)
  # extract the content of the linked page
  doc <- GET(paste0("https://www.basketball-reference.com/leagues/NBA_", end_year, "_totals.html"))
  cont <- content(doc, "text") %>% 
    # Basketball Reference stores some tables in a weird way
    # reading them from text solves the problem
    gsub(pattern = "<!--\n", "", ., fixed = TRUE) %>% 
    read_html %>% 
    html_nodes(".table_outer_container table") %>% 
    html_table()
  
  # extract the target table
  df <- cont[[1]] %>%
    # clean the player names
    filter(Player != "Player")
  # clean the punctuation
  df$Player <- df$Player %>% str_replace_all("[:punct:]", "")
  
  # select unique players
  player_df <- df %>%
    select(Player) %>%
    unique()
  # sort and put the names in alphabetic order
  player_df$Player <- sort(player_df$Player)
  player_df$Player
  
}


############################################################################


#' @title Scrape regular season stats totals for given player
#'
#' @description Function to scrape regular season stats totals for given player from Basketball Reference
#' 
#' @author Ozan Adiguzel
#' 
#' @references \url{https://www.basketball-reference.com}
#' 
#' @note Glossary: \url{https://www.basketball-reference.com/about/glossary.html}
#' 
#' @param player player name (character string)
#' @param start_year start year of the season (numeric value from 1949 to 2017)
#' 
#' @return a data frame of season stats totals
#' 
#' @keywords nba stats totals
#' 
#' @seealso \code{\link{season_players}}
#' 
#' @export
#' 
#' @examples
#' player_totals("Stephen Curry", 2017)

player_totals <- function(player, start_year) {
  
  # friendly reminder to choose valid arguments
  if(!(start_year %in% 1949:2017)) {
    return(print("Please enter a valid season start year from 1949 to 2017"))
  }
  
  # create string based on the argument to built the link
  end_year <- as.character(start_year + 1)
  # extract the content of the linked page
  doc <- GET(paste0("https://www.basketball-reference.com/leagues/NBA_", end_year, "_totals.html"))
  cont <- content(doc, "text") %>% 
    # Basketball Reference stores some tables in a weird way
    # reading them from text solves the problem
    gsub(pattern = "<!--\n", "", ., fixed = TRUE) %>% 
    read_html %>% 
    html_nodes(".table_outer_container table") %>% 
    html_table()
  
  # extract the target table
  df <- cont[[1]] %>%
    # clean the player names
    filter(Player != "Player")
  # clean the punctuation
  df$Player <- df$Player %>% str_replace_all("[:punct:]", "")
  
  # filter based on given argument and remove the unnecessary column
  player_table_df <- df %>%
    filter(Player == player)
  player_table_df[ , -1]
  
}


############################################################################


#' @title Scrape regular season stats per game for given player
#'
#' @description Function to scrape regular season stats per game for given player from Basketball Reference
#' 
#' @author Ozan Adiguzel
#' 
#' @references \url{https://www.basketball-reference.com}
#' 
#' @note Glossary: \url{https://www.basketball-reference.com/about/glossary.html}
#' 
#' @param player player name (character string)
#' @param start_year start year of the season (numeric value from 1949 to 2017)
#' 
#' @return a data frame of season stats per game
#' 
#' @keywords nba stats pergame
#' 
#' @seealso \code{\link{season_players}}
#' 
#' @export
#' 
#' @examples
#' player_pergame("Stephen Curry", 2017)

player_pergame <- function(player, start_year) {
  
  # friendly reminder to choose valid arguments
  if(!(start_year %in% 1949:2017)) {
    return(print("Please enter a valid season start year from 1949 to 2017"))
  }
  
  # create string based on the argument to built the link
  end_year <- as.character(start_year + 1)
  # extract the content of the linked page
  doc <- GET(paste0("https://www.basketball-reference.com/leagues/NBA_", end_year, "_per_game.html"))
  cont <- content(doc, "text") %>% 
    # Basketball Reference stores some tables in a weird way
    # reading them from text solves the problem
    gsub(pattern = "<!--\n", "", ., fixed = TRUE) %>% 
    read_html %>% 
    html_nodes(".table_outer_container table") %>% 
    html_table()
  
  # extract the target table
  df <- cont[[1]] %>%
    # clean the player names
    filter(Player != "Player")
  # clean the punctuation
  df$Player <- df$Player %>% str_replace_all("[:punct:]", "")
  
  # filter based on given argument and remove the unnecessary column
  player_table_df <- df %>%
    filter(Player == player)
  player_table_df[ , -1]
  
}


############################################################################


#' @title Get the link of given player's picture
#'
#' @description Function to built a link to given player's picture from Basketball Reference
#' 
#' @author Ozan Adiguzel
#' 
#' @references \url{https://www.basketball-reference.com}
#' 
#' @param player player name (character string)
#' 
#' @return a character string which is the link of player's picture
#' 
#' @keywords nba stats pergame
#' 
#' @seealso \code{\link{season_players}}
#' 
#' @export
#' 
#' @examples
#' player_piclink("Stephen Curry")

player_piclink <- function(player) {
  
  # split first and last names, make them all lower case
  split <- str_split(tolower(player), " ")
  # unlist splitted names
  name <- unlist(split)
  # firts two letters of first name
  two <- str_sub(name[1], 1, 2)
  # first five letters of last name
  five <- str_sub(name[2], 1, 5)
  # combine these letters to create the convention for link
  convention <- paste0(five, two, "01")
  # built the link
  link <- paste0("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/", convention, ".jpg")
  link
  
}

