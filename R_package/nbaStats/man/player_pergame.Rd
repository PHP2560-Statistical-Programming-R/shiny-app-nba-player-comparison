% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{player_pergame}
\alias{player_pergame}
\title{Scrape regular season stats per game for given player}
\usage{
player_pergame(player, start_year)
}
\arguments{
\item{player}{player name (character string)}

\item{start_year}{start year of the season (numeric value from 1949 to 2017)}
}
\value{
a data frame of season stats per game
}
\description{
Function to scrape regular season stats per game for given player from Basketball Reference
}
\note{
Glossary: \url{https://www.basketball-reference.com/about/glossary.html}
}
\examples{
player_pergame("Stephen Curry", 2017)
}
\references{
\url{https://www.basketball-reference.com}
}
\seealso{
\code{\link{season_players}}
}
\author{
Ozan Adiguzel
}
\keyword{nba}
\keyword{pergame}
\keyword{stats}
