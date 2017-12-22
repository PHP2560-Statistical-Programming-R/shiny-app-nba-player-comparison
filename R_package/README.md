<h1 align="center">R Package: nbaStats</h1>

<h3 align="center">Author: Ozan Adiguzel</h3>

<h4 align="center"><i>A package to facilitate data scraping in basketball analytics field</i></h4>

## Introduction

There is a huge amount of data on player stats. Basketball reference and Wikipedia are doing a good job by storing this information and making it publicly available. However, it is still difficult to extract specifics using these mediums.

This package facilitates access to player stats and is useful for all professional including players and coaches, but also basketball fans in general. 

## Source Websites 

:information_source: [Basketball Reference](https://www.basketball-reference.com/)  
:information_source: [Wikipedia](https://en.wikipedia.org/wiki/Main_Page)  

## Functions 

:basketball: **season_ranking**: Scrape regular season rankings   
```
# get end of regular season team ranking for 2016-2017 season
season_ranking(2016)
```
:basketball: **season_totals**: Scrape regular season stats totals for all players  
```
# get all player stats totals for 2016-2017 season
season_totals(2016)
```
:basketball: **season_pergame**: Scrape regular season stats per game for all players 
```
# get all player stats per game for 2016-2017 season
season_pergame(2016)
```
:basketball: **season_players**: Get list of players by season  
```
# get a list of players for 2016-2017 season
season_players(2016)
```
:basketball: **player_totals**: Scrape regular season stats totals for given player  
```
# get Stephen Curry's stats totals for 2016-2017 season
player_totals("Stephen Curry", 2016)

# you can get the player names using above-mentioned season_players function
```
:basketball: **player_pergame**: Scrape regular season stats per game for given player  
```
# get Stephen Curry's stats per game for 2016-2017 season
player_pergame("Stephen Curry", 2016)

# you can get the player names using above-mentioned season_players function
```
:basketball: **player_piclink**: Get the link of given player's picture  
```
# get the link to Stephen Curry's image
player_piclink("Stephen Curry")

# you can get the player names using above-mentioned season_players function
# player's image can be rendered or saved using this link
```

## Further Development

This project leaves much room for improvement. There are plenty of player stats available online, which would allow this package to grow exponentially.

As this is my first R package, I would appreciate any feedback that would improve it and my future work. 

