# check for requiered packages and get them if needed
names <- c(
  "shiny",
  "dplyr", 
  "shinydashboard", 
  "stringr"
)

for(name in names) {
  if (!(name %in% installed.packages()))
    install.packages(name, repos="http://cran.us.r-project.org")
  library(name, character.only=TRUE)
}

all_matches <- read.csv("allMatches.csv")

df <- all_matches
df$defender <- replicate(nrow(all_matches), NA)
for(i in 1:nrow(all_matches)) {
  df$defender[i] <- paste(df$defender_firstname[i], df$defender_lastname[i], sep = " ")
}
 
df <- df %>% dplyr::select(defender,
                    shot_clock, 
                    shot_outcome,
                    defender_height,
                    player_distance, 
                    shot_dist, 
                    def_angle, 
                    def_reyting, 
                    distance,
                    rowNum
                    )
colnames(df) <- c("Defender Name",
                  "Shot Clock",
                  "Shot Result",
                  "Defender Height",
                  "Distance to Steph",
                  "Shot Distance",
                  "Angle",
                  "Defender Rating",
                  "Prop Score",
                  "rowNum"
                  )
all_matches$rowNum <- as.character(all_matches$rowNum)

get_table_left <- function(id, df) {
  left <- paste0(id, "L") 
  l <- df %>% filter(rowNum == left) %>% dplyr::select(-rowNum)
}

get_table_right <- function(id, df) {
  right <- paste0(id, "R") 
  l <- df %>% filter(rowNum == right) %>% dplyr::select(-rowNum)
}

get_pic_left <- function(id, df) {
  left <- paste0(id, "L")
  name <- df %>% filter(rowNum == left) %>% dplyr::select(defender_firstname)
  lastname <- df %>% filter(rowNum == left) %>% dplyr::select(defender_lastname)
  name <- tolower(name[1, 1])
  name <- name %>% str_replace_all(fixed("."), "")
  name <- name %>% str_replace_all(fixed("'"), "")
  lastname <- tolower(lastname[1, 1])
  lastname <- lastname %>% str_replace_all(fixed("."), "")
  lastname <- lastname %>% str_replace_all(fixed("'"), "")
  two <- str_sub(name, 1, 2)
  five <- str_sub(lastname, 1, 5)
  first <- "01"
  second <- "02"
  third <- "03"
  convention <- paste0(five, two, first)
  if(convention == "davisan01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/davisan02.jpg")
  } else if(convention == "johnsst01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/johnsst04.jpg")
  } else if(convention == "walkeke01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/walkeke02.jpg")
  } else if(convention == "millean01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/millean02.jpg")
  } else if(convention == "willilo01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/willilo02.jpg")
  } else if(convention == "johnsjo01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/johnsjo02.jpg")
  } else {
    link <- paste0("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/", convention, ".jpg")
    return(link)
  }

}

get_pic_right <- function(id, dataframe) {
  right <- paste0(id, "R")
  name <- dataframe %>% filter(rowNum == right) %>% dplyr::select(defender_firstname)
  lastname <- dataframe %>% filter(rowNum == right) %>% dplyr::select(defender_lastname)
  name <- tolower(name[1, 1])
  name <- name %>% str_replace_all(fixed("."), "")
  name <- name %>% str_replace_all(fixed("'"), "")
  lastname <- tolower(lastname[1, 1])
  lastname <- lastname %>% str_replace_all(fixed("."), "")
  lastname <- lastname %>% str_replace_all(fixed("'"), "")
  two <- str_sub(name, 1, 2)
  five <- str_sub(lastname, 1, 5)
  first <- "01"
  second <- "02"
  third <- "03"
  convention <- paste0(five, two, first)
  if(convention == "davisan01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/davisan02.jpg")
  } else if(convention == "johnsst01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/johnsst04.jpg")
  } else if(convention == "walkeke01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/walkeke02.jpg")
  } else if(convention == "millean01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/millean02.jpg")
  } else if(convention == "willilo01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/willilo02.jpg")
  } else if(convention == "johnsjo01") {
    return("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/johnsjo02.jpg")
  } else {
    link <- paste0("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/", convention, ".jpg")
    return(link)
  }
}


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "SportsVU Final Paper (Web)",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Ozan, Steven, Nico & Varun",
                                                   message = "Welcome !!!"
                                                 )
                                    )
                    ),
                    dashboardSidebar(
                      width = 200,
                      sidebarMenu(
                        menuItem("Abstract", tabName = "abstract", icon = icon("file-text-o")),
                        menuItem("Contributors", tabName = "contributors", icon = icon("users")),
                        menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
                        menuItem("Methodology", tabName = "methodology", icon = icon("rocket")),
                        menuItem("Results", tabName = "results", icon = icon("soccer-ball-o")),
                        menuItem("Discussion", tabName = "discussion", icon = icon("comments-o")),
                        menuItem("Curry Shots", tabName = "interactive", icon = icon("bar-chart"), badgeLabel = "cool", badgeColor = "green"),
                        menuItem("Conclusion", tabName = "conclusion", icon = icon("gavel")),
                        menuItem("References", tabName = "sources", icon = icon("code")),
                        menuItem("Github Code", tabName = "code", icon = icon("chain")),
                        menuItem("Contact", tabName = "contact", icon = icon("envelope-o"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "contributors",
                                
                                HTML("<h1>Ozan Adiguzel '19</h1>
                                     <h1>Steven Liao '18</h1>
                                     <h1>Nico Mandel '20</h1>
                                     <h1>Varun Senthil Nathan '20")
                        ),
                        tabItem(tabName = "introduction",
                                HTML('<h1><span style="font-weight: 400;">Introduction</span></h1>
<p><span style="font-weight: 400;">At the heart of our project are the questions: how does one quantify defensive proficiency and how can one better defend certain shooters? The ability to analyze defensive performance in the NBA is rapidly changing. Defensive stats 10 years ago mainly focused on blocks and steals, which do not truly capture the defensive expertise of an individual. A block typically occurs when the shooter believes that they will be able to get a shot off given the defensive landscape at the time, but their judgement was wrong. Ultimately, the best &lsquo;rim defenders&rsquo; may have less blocked shots simply because offensive players avoid shooting when near them. Additionally, while a steal may be a result of strong defensive play, it may also be indicative of a defender&rsquo;s tendency to gamble, or an offensive player&rsquo;s lapse in judgment. </span></p>
                                     <img src ="nba_old.png">
                                     <h1 style="text-align: center;"></h1>
                                     <p><span style="font-weight: 400;">A few years ago, the NBA introduced player tracking data in the form of SportVU cameras. SportVU technology consists of six cameras which collect optical tracking data 25 times per second. Since the 2013-2014 NBA season, SportVU technology has been available in every NBA arena. Optical tracking data offers new, useful ways to evaluate defensive play - consider the following heatmap:</span></p>
                                     <img src ="nba_new.png">
                                     <p><span style="font-weight: 400;">With access to Sport VU data from the 2015-2016 NBA season we plan to investigate variables, within a defender or coach&rsquo;s control, which might most influence the probability a shot goes in. The variables we are analyzing include defender distance from shooter, defender angle to shooter, which side of the shooter the defender is coming from, and defender height. </span></p>
                                     <h2><span style="font-weight: 400;">Previous Literature</span></h2>
                                     <p><span style="font-weight: 400;">Studying a shooter&rsquo;s &lsquo;distance from the defender&rsquo; is not a novel ideal. According to an article by numberFire (reference number 1), data seems to suggest a drop off in shooting percentage when the defender is roughly 4 feet away. In addition, others have </span><span style="font-weight: 400;">developed a metric called Effective Shot Quality, which measured shot quality using a machine learning model taking into account shot distance, shot angle, defender distance, defender angle, player speed, and player velocity angle (reference number two).
                                     <p><span style="font-weight: 400;">Our project builds on past projects by focusing on individual players (as opposed to doing analysis on the aggregate), with particular attention paid towards actionable steps defensive players and coaches can take in defending these players. For example, what steps can an opposing player or coach take to better defend Curry&rsquo;s 3?</span></p>')
                        ),
                        tabItem(tabName = "methodology",
                                HTML("<h1>Methodology</h1>
<p>We divide our methodology into three sections: <strong>Data Collection </strong>, <strong>Data Cleaning</strong>, and <strong>Data Analysis.</strong></p>
                                     <h2>Data Collection</h2>
                                     <h4>SportVU Data</h4>
                                     <p>SportVU was founded in Israel in 2005. It has since been adopted in the NBA and in soccer. 25 times per second, the Sport VU cameras track:</p>
                                     <ul>
                                     <li>First name, last name of the players and their player ID</li>
                                     <li>X, Y coordinates of all ten players on court</li>
                                     <li>X, Y, Z coordinates of the ball</li>
                                     <li>Shot clock and game clock</li>
                                     </ul>
                                     <p>For reference, one game of data takes around 3 minutes to load into an R data frame (converted from JSON), and typically will consist of over 2 million rows.</p>
                                     <p>The NBA no longer makes SportVU data publically available, but we found a public Github repository of ~600 games (half the NBA season) from the 2015-2016 NBA season.</p>
                                     <p><a href='https://github.com/rajshah4/BasketballData/tree/master/2016.NBA.Raw.SportVU.Game.Logs'>Rajiv Shah Github Repository</a></p>
                                     <h4>NBA Play By Play Data</h4>
                                     <p>NBA&rsquo;s official website provides play-by-play information for each game, including a description of each possession. We used Play-By-Play data to find the time of each three-pointer in a game, which we could then match SportVU data to.</p>
                                     <p>Here is an example game:</p>
                                     <p><a href='http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&amp;EndRange=55800&amp;GameID=0021500492&amp;RangeType=2&amp;StartPeriod=1&amp;StartRange=0'>http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&amp;EndRange=55800&amp;GameID=0021500492&amp;RangeType=2&amp;StartPeriod=1&amp;StartRange=0</a></p>
                                     <p>We used Python&rsquo;s Selenium library to scrape play-by-play information corresponding to each of the ~600 games in the SportVU Github repository.</p>
                                     <h4>NBA Heights</h4>
                                     <p>We downloaded a dataset of player heights from Kaggle (reference number four)&nbsp;. We would integrate defender height in our later analysis.</p>
                                     <h4>NBA Player Defensive Ratings</h4>
                                     <p>Using R, we scraped NBA player defensive ratings (a rough measure of a player&rsquo;s defensive skill) from https://www.basketball-reference.com/, which would also be integrated in our later analysis.</p>
                                     <h2>Data Cleaning</h2>
                                     <p>The primary objective of data collection was to obtain the tracking information at the time of release for 3 point shots (i.e. defender position). SportVu data does not provide any play-by-play information, so we could only guess a three-point shot from the ball&rsquo;s position (i.e. travels far, while high in the air).</p>
                                     <p>To improve our identification of 3 point shots, we matched &lsquo;potential&rsquo; 3 point attempts with actual 3 point attempts as noted in the play by play data.</p>
                                     <p>To find &lsquo;potential 3 point attempts&rsquo; in the SportVU data, we looked at stretches in the data where the ball travelled continuously for at least 19 feet while simultaneously at least 7 feet above the ground. For reference, the basket is 10 feet high, and the distance of a three point attempt is at least 22 feet (22 feet at the corners, 25 feet elsewhere)</p>
                                     <p>These filters leave us with ball movement data for what are likely either three point shots, long two pointers, or long passes.</p>
                                     <p>We look for shots traveling 19 feet (instead of the 22 feet three point distance) to account for edge cases (better safe than sorry). For instance, some shots are woefully off, while sometimes camera timing or unusual shooting motions can affect how far the ball travels while 7+ feet above the ground.</p>
                                     <p>Our criteria, however, fails to identify blocked shots (the ball does not travel very far if blocked). This is one limitation of our study, as blocked shots are good defensive plays which we are not capturing.</p>
                                     <p>For each official 3 point attempt in the Play By Play data, we then searched for &lsquo;potential 3-point attempts&rsquo; in the SportVu data occurring within five seconds of the official Play By Play data. This gave us ball movement data corresponding to each three-point attempt.</p>
                                     <p>Finally, we wanted to isolate the exact moment of release for each three-point attempt (we could then gather shooter and defender positions at that exact time). We defined the moment of release as the first time at which the ball was above 7 feet. &nbsp;</p>
                                     <p>While this simplification may not perfectly capture the moment of release for each player, for our purposes it was reasonably sufficient. For the most part, every player releases the ball somewhere between 6 feet and 10 feet in the air. The time it takes for a player to lift the ball 6 feet in the air to shoot, to when the ball reaches 10 feet, is about 0.2 seconds. Since SportVU data is collected once every 0.04 seconds, this gives us 5-6 possible data points to assign as the &lsquo;release of the ball.&rsquo;</p>
                                     <p>If we&rsquo;re off, we&rsquo;re off by no more than 0.2 seconds (and likely much, much less). We&rsquo;re primarily concerned with the defender&rsquo;s position at the time of release, and the defender&rsquo;s position should not change drastically in a 0.2 second period.</p>
                                     <p>Finally, we extracted the defender and shooters&rsquo; positions at the calculated time of release for each three-pointer. Using this information, we wrote R code applying standard distance and angle formulas to get:</p>
                                     <ul>
                                     <li>Defender&rsquo;s distance from shooter</li>
                                     <li>Defender&rsquo;s angle to shooter</li>
                                     <li>Defender&rsquo;s approach side corresponding to shooter&rsquo;s</li>
                                     </ul>
                                     <p>Defender height (matching by player name) and defender defensive rating data (matched by name) from Kaggle and NBA-Reference.com, respectively, were then appended to the dataset.</p>
                                     <p>Below is a snapshot of our final dataset for all of Steph Curry&rsquo;s 3 point attempts in games we have data for (roughly ~360 attempts in 42 games):</p>
                                     <img src = 'final_data.png'>
                                     <h2>Data Analysis</h2>
                                     <p>Our goal in analysis was to better understand which variables, specifically those within a defender&rsquo;s control, were most influential or predictive in the result of a shot. Ideally, our project could have applicable implications for defenders or coaches looking to better defend a player.</p>
                                     <p>A key underlying assumption for our project was that different variables affect different shooters. Thus, we intended to run an individual analysis for each offensive player; due to time constraints, we completed the analysis only for Curry, but a similar methodology would follow for other offensive players (Harden, Durant, Thompson, etc.).</p>
                                     <h3>Logistic Regression</h3>
                                     <p>To begin our analysis, we run a logistic regression predicting whether a shot would go in for a given offensive player. Our input variables are:</p>
                                     <ul>
                                     <li><strong>shot_clock: </strong>time left on the shot clock (0 to 24 seconds)</li>
                                     </ul><ul>
                                      <li><strong>quarter</strong>: quarter the play occured in (1-4, 5+ for overtime)</li>
                                     </ul>
                                     <ul>
                                     <li><strong><strong>quarter_clock: </strong>time left in the quarter</strong></li>
                                     </ul>
                                     <ul>
                                     <li><strong>shooter_X: </strong>shooter&rsquo;s X coordinate</li>
                                     </ul>
                                     <ul>
                                     <li><strong>shooter_Y: </strong>shooter&rsquo;s Y coordinate</li>
                                     </ul>
                                     <ul>
                                     <li><strong>distance: </strong>distance between closest defender and shooter</li>
                                     </ul>
                                     <ul>
                                     <li><strong>defender_height: </strong>height of the closest defender</li>
                                     </ul>
                                     <ul>
                                     <li><strong>def_reyting: </strong>defensive rating of the closest defender (2015-2016 season stats)</li>
                                     </ul>
                                     <ul>
                                     <li><strong>shot_dist: </strong>distance of the shot from the basket</li>
                                     </ul>
                                     <ul>
                                     <li><strong>def_angle: </strong>The angle between the shooter, defender, and basket (draw a line connecting the shooter to the basket, and another line connecting the defender to the shooter - so the shooter is the vertex of the angle. That is the angle we used).</li>
                                     </ul>
                                     <ul>
                                     <li><strong>LeftOrRight: </strong>whether the defender was to the left or right of the shooter (from the shooter&rsquo;s point of view)</li>
                                     </ul>
                                     <ul>
                                     <li><strong>def_angle:LeftOrRight: </strong>Interaction variable for defender angle and whether the defender was on the left or right - in case the effect of the angle change was stronger depending on whether the defender is on the left or right of the shooter.</li>
                                     </ul>
                                     <p>A logistic regression was preferred over a linear regression, as our output variable is binary (made or miss shot). A logistic regression runs a linear regression on the logit (logarithm of odds - i.e. log(p/1-p)) of the output probability (probability of make or miss); one can then derive the output probability from the logit, which by the construction of the logit, results in a value between 0 and 1.</p>
                                     <p>To avoid over-fitting our model, we used an AIC backwards stepwise process to filter for the most significant covariates.</p>
                                     <p>One consideration in building the model was whether or not to include the defender&rsquo;s angle as one covariate (i.e. -180 to 180) or as two: the absolute value of the angle (i.e. 0 to 180), and a separate covariate for whether the defender was on the left or right of the shooter.</p>
                                     <p>We opted to use the two separate variables. This was primarily to account for the possibility that the more severe the angle, the worse the shot. In other words, someone might shoot best with a defender standing directly between the shooter and basket (angle of 0), and worse as the defender took a more &lsquo;tilted&rsquo; angle (or vice versa), regardless of direction (left or right). We also included an interaction variable between the side and angle, in case a change in angle was more predictive on the shot outcome depending on whether the defender was to the left or right. The interaction variable was however filtered out in the AIC backwards stepwise process.</p>
                                     <p>Using two separate variables however posed a problem in interpreting the logistic regression. Consider the case where Steph Curry shoots considerably worse when a defender is on his right. Then the coefficient of 'side' (right or left) would be pretty significant in our regression. But then suppose if we used our model to predict Steph Curry's shot percentage for a defender angle of 1 degree on the left and 1 degree on the right. This is practically the same; however the left or right coefficient would change our model&rsquo;s prediction by quite a bit.</p>
                                     <p>The results of our regression will be highlighted in the next section, but one interesting finding was that the side the defender played on was rather predictive (p-value of 0.03110824), with Curry shooting on average worse when a defender was on his right (dominant hand). A logistic regression serves as a predictive model, but says very little about causation. The next step of the analysis was to apply causal inference methods to isolate the effect the side of the defender might have on Curry&rsquo;s shot percentage.</p>
                                     <h3>Causal Inference</h3>
                                     <p>Our goal here was to determine the causal effect of the defender&rsquo;s position on shot outcome. The gist was to match shots where the defender was on the right of Curry to shots where the defender was on the left, with all other covariates as similar as possible. The hope was to isolate or simulate what might happen if everything else was the same, except the defender simply switched sides.</p>
                                     <p>We matched left and right defender positioning on the following covariates:</p>
                                     <ul>
                                     <li><strong>shot_clock: </strong>amount of time left on the shot clock for the given possession</li>
                                     </ul>
                                     <ul>
                                     <li><strong>shooter_X: </strong>Curry&rsquo;s X coordinate at the time of release</li>
                                     </ul>
                                     <ul>
                                     <li><strong>shooter_Y: </strong>Curry&rsquo;s Y coordinate at the time of release</li>
                                     </ul>
                                     <ul>
                                     <li><strong>player_distance: </strong>distance between closest defender and Curry at time of release</li>
                                     </ul>
                                     <ul>
                                     <li><strong>defender_height: </strong>height of closest defender at time of release</li>
                                     </ul>
                                     <ul>
                                     <li><strong>shot_dist: </strong>distance between Curry and the basket at moment of release</li>
                                     </ul>
                                     <ul>
                                     <li><strong>def_angle: </strong>the angle between the shooter, defender, and basket (draw a line connecting the shooter to the basket, and another line connecting the defender to the shooter - so the shooter is the vertex of the angle. That is the angle we used).</li>
                                     </ul>
                                     <ul>
                                     <li><strong>def_reyting: </strong>defensive rating of the closest defender at the time of release</li>
                                     </ul>
                                     <p>To conduct our matching process, we applied propensity score matching. Propensity score matching (a new concept for us) builds a logistic regression model to predict whether the defender will be on the left or the right of Curry with the covariates being the variables we want to control for. The idea is, if for two shots, the likelihood of the defender being on the left or right of Curry is very close, then the conditions surrounding both shots should be similar.</p>
                                     <p>We used matching-without-replacement and nearest-neighbor as our matching algorithm, with a caliper of 0.15. The caliper corresponds to a maximum difference in propensity scores a valid match can have; the lower the caliper, the closer a match needs to be to be considered valid. Thus, the caliper value decision is a tradeoff between quality of match, and sample size. We arrived at 0.15 after testing several values; with this caliper, for the original 360 3 point attempts we had for Curry (176 with defender on his left, 184 with defender on his right), we obtained 146 matched pairs (292 total shots matched).</p>
                                     <p>Finally, after conducting matching, we performed a McNemar test to determine if there was a significant difference in shot make percentage amongst the matched pairs. The null hypothesis for the McNemar test, on a high level, is that it is equally likely for a make to become a miss as it is for a miss to become a make, if we flipped a defender from the left of Curry to the right, keeping all other variables the same. If flipping the defender from left to right resulted in far more misses to makes than makes to misses (or vice versa), then we would reject the null and evidence would support a causal effect, in our case.</p>")
                        ),
                        tabItem(tabName = "results",
                                HTML('<h1><span style="font-weight: 400;">Results</span></h1>
<h2><span style="font-weight: 400;">Logistic Regression</span></h2>
                                     <p><span style="font-weight: 400;">After running the AIC stepwise process, our logistic regression had three covariates: the distance between the Curry and the closest defender at the time of release (</span><strong>distance</strong><span style="font-weight: 400;">), the distance Curry was from the basket at the top of release (</span><strong>shot_dist</strong><span style="font-weight: 400;">), and whether the defender was on Curry&rsquo;s right or left, from Currys&rsquo; perspective (</span><strong>LeftOrRightRight</strong><span style="font-weight: 400;">). </span></p>
                                     <p><img src="logistic_results.png" alt="" /></p>
                                     <p><span style="font-weight: 400;">Our model predicts the odds of a shot going in to increase by a factor of 1.1 for every additional foot the closest defender is away from Curry (all else being equal), which makes sense, as the further the defender, the more open Curry is. Our model predicts the odds of the shot going in to decrease (multiplied by 0.919) for every additional foot Curry is away from the basket (all else being equal), which also makes sense, as generally further shots are more difficult to make.</span></p>
                                     <p><span style="font-weight: 400;">Finally, if the defender &lsquo;switches&rsquo; from Curry&rsquo;s left to Curry&rsquo;s right (Curry&rsquo;s shooting side), all else being equal, our model predicts the odds of the shot to drop by quite a bit (multiplied by 0.627).</span></p>
                                     <p><span style="font-weight: 400;">The p-values of all three coefficients are relatively low, indicating a low likelihood that the true coefficient is 0.</span></p>
                                     <p><span style="font-weight: 400;">Here are some of the predicted outputs for the model. Consider that Curry&rsquo;s shooting percentage overall in our dataset was about 43% from 3 point land.</span></p>
                                     <p><img src="logistic_examples.png" alt="" /></p>
                                     <p><span style="font-weight: 400;">Most of these seem reasonable - for a regular three point attempt (row 2), our model predicts Curry&rsquo;s shooting percentage to be on par with his average. For a wide open three (row 1), Curry is predicted to make it more often than not. For a contested three (row 3), the model predicts poor results for Curry. &nbsp;Curiously, when the defender is on Curry&rsquo;s right as opposed to his left for a regular three (rows 4 and 2), the model predicts Curry&rsquo;s likelihood of making the shot to drop by quite a bit (43.7% to 32.7%).</span></p>
                                     <p><span style="font-weight: 400;">We conduct two tests to evaluate the fit of our logistic regression. The first is a Hosmer-Lemeshow Goodness of fit test, which divides our dataset into buckets, and compares how many shots our model expected to go in versus how many actually went in for each bucket.</span></p>
                                     <p><img src="hosmer.png" alt="" /></p>
                                     <p><span style="font-weight: 400;">A low p-value would have indicated that the difference between expected and observed makes in each bucket was high, and thus the fit was poor. The above results are from dividing the data into ten buckets; similar results were received when dividing the data into eight or twelve buckets (all with p-value over 0.7). Thus, the Hosmer-Lemeshow test does not seem to indicate bad calibration (our model seems to have relatively accurate prediction percentages) - this falls in line with our predicted value tables above.</span></p>
                                     <p><span style="font-weight: 400;">Another way we can test the fit of our logistic regression is by calculating its AUC (Area Under [ROC] Curve). The AUC tests how good our model is at discriminating between made or missed shots. </span></p>
                                     <p><img src="roc.png" alt="" /></p>
                                     <p><span style="font-weight: 400;">An ROC value of 0.6066 is quite poor, and indicates that our model is not very good at distinguishing between made or missed shots. More precisely, the logistic regression outputs a probability &lsquo;p&rsquo; that is supposed to represent the likelihood of a shot going on. We can arbitrarily assign a threshold t, where we say the shot will go in if p &gt;= t, and the shot will miss if p &lt; t. </span></p>
                                     <p><span style="font-weight: 400;">The ROC curve takes various values of t to predict makes or misses. For each value of t, it then plots the true positive rate (percentage of actual makes we correctly predict) versus the false positive rate (percentage of misses we accidentally classify as makes). Ideally, we would want a high true positive rate and a low false positive rate most of the time; if this were to be the case, we would have an ROC curve with a very high AUC.</span></p>
                                     <h2><span style="font-weight: 400;">Causal Inference</span></h2>
                                     <p><span style="font-weight: 400;">As mentioned in the methodology section, for our propensity score matching, we used matching-without-replacement and nearest-neighbor as our matching algorithm, with a caliper of 0.15. Our matches (where the treatment is shots from "left") were as follows:</span></p>
                                     <p><img src="pschart.png" alt="" /></p>
                                     <p><span style="font-weight: 400;">For the most part, the propensity scores appear to be matched fairly closely. </span></p>
                                     <p><span style="font-weight: 400;">We also tested our matching by running a paired t-test on each of the &nbsp;eight variables we were attempting to control for (all of which were continuous). The hope was that none of the t-tests would be significant, which would indicate that we matched very poorly on one of the variables. </span></p>
                                     <p><span style="font-weight: 400;">All p-values for the t-tests were at least 0.5388 - with an average p-value of 0.774785. At the very least, this meant that none of the variables we wanted to control for had significantly different averages between matched pairs. </span></p>
                                     <p><span style="font-weight: 400;">Of our 146 matched pairs, we observed the following:</span></p>
                                     <p><img src="table.png" alt="" /></p>
                                     <p><span style="font-weight: 400;">For 39 of the pairs, both of the shots in the pair (i.e one when defender was on Curry&rsquo;s left, one when defender was on Curry&rsquo;s right) missed. For 32 of the pairs, the shot with the defender on the left missed, while the matched shot with the defender on the right went in. For 51 of the pairs, the shot with the defender on the right missed, while the shot with the defender on the left went in. Finally, for 24 of the pairs, the shot attempts for both shots in the matched pair were successful.</span></p>
                                     <p><span style="font-weight: 400;">Running a McNemar test gives:</span></p>
                                     <img src="mcnemar.png">
                                     <p><span style="font-weight: 400;">The low p-value tells us to reject the null hypothesis, and suggests that flipping a defender from the left of Curry to the right, all other variables being the same, has a significant impact on change in shot probability.</span></p>
                                     <p><span style="font-weight: 400;">If you are viewing this paper in Shiny App mode, click on the &ldquo;Curry Shots&rdquo; tab on the left sidebar to view each of the matched pairs (for similarity).</span></p>')
                        ),
                        tabItem(tabName = "discussion",
                                HTML('<h1><span style="font-weight: 400;">Discussion</span></h1>
<p><span style="font-weight: 400;">Our logistic regression had logical coefficients for our three covariates, and performed decently on the Hosmer Lemeshow test, giving reasonable probabilities in our specific test cases (i.e. 57% for an open corner three, 43.7% for an average normal distance three). However, the discrimination of our test was poor; our model was not able to discriminate very well between made and missed shots.</span></p>
                                     <p><span style="font-weight: 400;">Furthermore, using a threshold of p = 0.5, we were able to correctly predict the shot outcome (make or miss) for 211 of Steph&rsquo;s 360 shots, or 58.6%. Predicting every shot to be a miss (baseline comparison) would have correctly predicted 205 or the 360 shots, or 56.9%. So our model performed better than baseline, but just barely.</span></p>
                                     <p><span style="font-weight: 400;">While this indicates poor predictive power on the part of our model, we must also acknowledge how inherent randomness in the NBA three point outcome makes a model that can effectively discriminate extremely unlikely. There is never going to be a 3 pointer with a 90% likelihood of going on, no matter how open a player is. In the 2015-2016 season, no team made over 45% of their </span><em><span style="font-weight: 400;">wide open</span></em><span style="font-weight: 400;"> three pointers</span><span style="font-weight: 400;">6</span><span style="font-weight: 400;">. It is highly unlikely, then, that any model can with high certainty predict that a shot is going in, even though over 30% of 3 pointers typically go in.</span></p>
                                     <p><span style="font-weight: 400;">While our logistic model did not discriminate very well, it did lead us to explore more in depth the impact of defender side on shot outcome. In both our logistic regression and our causation analysis, defensive positioning on Curry&rsquo;s right (dominant) side seemed to predict a decreased shooting percentage. In both cases (coefficient in Logistic regression, McNemar Test), the p-value was less than 0.05, indicating it is quite unlikely that shot outcome and defender position are independent.</span></p>
                                     <p><span style="font-weight: 400;">This is an interesting finding, but there were several limitations to our study which caution reading too much into this finding. First, our model failed to account for shot selection. While good defense should make the shot tougher for Curry, oftentimes good defense may even cause Curry to avoid shooting the shot in the first place. These instances were not captured in our project. In addition, our project did not include blocked shots because we could figure out a systematic way to capture the moment of release for blocked shots in the SportVU data.</span></p>
                                     <p><span style="font-weight: 400;">Another weakness in our causal inference was we did not capture movement information prior to the shot. Was the player dribbling to his right? Was he dribbling to his left? Was it a catch-and-shoot? Was the player coming off a screen? The situation leading up the shot can have a significant impact on a player&rsquo;s comfort when shooting, and may influence the location of the nearest defender (right or left). Matching shots off the situation would have been a further improvement on our analysis.</span></p>
                                     <p><span style="font-weight: 400;">Finally, our project suffered from limited sample size, as we were limited to half a season of SportVU data. We only had 360 total shots for Steph Curry, one of the most prolific 3 point shooters in the league. With several seasons of data, our matching could have been improved, and there would be several more players with a significant number of shots who we could run the analysis on.</span></p>')
                        ),
                        # Second tab content
                        tabItem(tabName = "interactive",
                                fluidRow(
                                  box(
                                    title = "Defender 1 (LEFT)", status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE, width = 4, height = 320,
                                    htmlOutput("court1")
                                  ),
                                  
                                  box(
                                    title = "Matched Shots", status = "danger", solidHeader = TRUE,
                                    width = 4, height = 320,
                                    selectInput(inputId = "pairInput", "Choose a pair:", choices = 1:146),
                                    HTML('<center> <img width="200" height="150" src="image.png"/> </center>'),
                                    HTML( '<center> <h4>Stephen Curry 3PT Shots</h4> </center>' )
                                  ),
                                  
                                  box(
                                    title = "Defender 2 (RIGHT)", status = "info", solidHeader = TRUE,
                                    collapsible = TRUE, width = 4, height = 320,
                                    htmlOutput("court2")
                                  )
                                ),
                                fluidRow(
                                  box(status = "warning",
                                    # The id lets us use input$tabset on the server to find the current tab
                                    id = "box1Input", height = "110px",  width = 12,
                                    
                                    column( width = 1, offset = 0,
                                      htmlOutput("image1Input")
                                    ),
                                    
                                    column(width = 11, 
                                      
                                      tableOutput("result1Output")
                                             )
                                  )
              
                                  
                                ),
                                fluidRow(
                                  box(status = "info",
                                      # The id lets us use input$tabset on the server to find the current tab
                                      id = "box2Input", height = "110px",  width = 12,
                                      
                                      column(width = 1, offset = 0,
                                        htmlOutput("image2Input")
                                      ),
                                      
                                      column(width = 11,
                                        tableOutput("result2Output")  
                                      )
                                      
                                      
                                  )
                                )
                        ),
                        tabItem(tabName = "abstract",
                                HTML('<h1 style="text-align: center;"><span style="font-weight: 400;">NBA Shot Probability Analysis Using SportVU Data</span></h1>
<h2 style="text-align: center;"><strong>Abstract</strong></h2>
                                     <p style="text-align: center;"><span style="font-weight: 400;">The advent of SportVU player tracking data in the NBA has revolutionized how the sport can be analyzed, particularly on the defensive side. In this paper we use SportVU data to analyze how defensive positioning (distance from shooter, angle from shooter) might impact an individual shooter&rsquo;s shot probability. Focusing our analysis on Steph Curry, we perform a logistic regression on the shot outcome of 360 of Curry&rsquo;s 3 point attempts during the 2015-2016 NBA season, and follow by using propensity score matching to attempt to isolate for the effect of defensive positioning (left or right of Curry) on shot outcome. Both our logistic regression and our causal inference results indicate that defensive positioning on Curry&rsquo;s right (dominant) side is predictive of poorer shooting performance. Our project has several limitations (such as sample size) that caution reading too much into this result, but our results suggests that future work on defensive positioning could be worthwhile for defensive players and coaches.</span></p>')
                        ),
                        
                        tabItem(tabName = "sources",
                                HTML('<h1><span style="font-weight: 400;">References</span></h1>
<ol>
                                     <li style="font-weight: 400;"><a href="https://www.numberfire.com/nba/news/3855/is-there-a-strong-relationship-between-defender-distance-and-field-goal-percentage-in-the-nba"><span style="font-weight: 400;">https://www.numberfire.com/nba/news/3855/is-there-a-strong-relationship-between-defender-distance-and-field-goal-percentage-in-the-nba</span></a></li>
                                     <li style="font-weight: 400;"><a href="http://www.sloansportsconference.com/wp-content/uploads/2014/02/2014-SSAC-Quantifying-Shot-Quality-in-the-NBA.pdf"><span style="font-weight: 400;">http://www.sloansportsconference.com/wp-content/uploads/2014/02/2014-SSAC-Quantifying-Shot-Quality-in-the-NBA.pdf</span></a></li>
                                     <li style="font-weight: 400;"><a href="https://github.com/rajshah4/BasketballData/tree/master/2016.NBA.Raw.SportVU.Game.Logs"><span style="font-weight: 400;">https://github.com/rajshah4/BasketballData/tree/master/2016.NBA.Raw.SportVU.Game.Logs</span></a></li>
                                     <li style="font-weight: 400;"><a href="https://www.kaggle.com/drgilermo/nba-players-stats/data"><span style="font-weight: 400;">https://www.kaggle.com/drgilermo/nba-players-stats/data</span></a></li>
                                     <li style="font-weight: 400;"><a href="https://www.basketball-reference.com/"><span style="font-weight: 400;">https://www.basketball-reference.com/</span></a></li>
                                     <li style="font-weight: 400;"><a href="https://www.cbssports.com/nba/news/wide-open-3s-and-ranking-how-each-nba-team-fared-at-making-defenses-pay/"><span style="font-weight: 400;">https://www.cbssports.com/nba/news/wide-open-3s-and-ranking-how-each-nba-team-fared-at-making-defenses-pay/</span></a></li>
                                     </ol>')
                        ),
                        tabItem(tabName = "code",
                                h2("GutHub Repository:", a("GISP_final", href="https://github.com/nman28/GISP_final"))
                  
                        ),
                        tabItem(tabName = "conclusion",
                                HTML('<h1><span style="font-weight: 400;">Conclusion</span></h1>
<p><span style="font-weight: 400;">In this project our hope was to expand upon previous literature that used optical tracking data to analyze shot probability and shot defense. We focused on how to best defend individual players, first running a logistic regression to predict shot outcome (looking for variables with significant predictive power on shot outcome), and then performing a causation analysis on the impact of defender location (left or right) on Steph Curry&rsquo;s shot outcome. Both tests indicated that defensive positioning on Steph Curry&rsquo;s right was predictive of lower shooting percentages. Our project, however, suffered from several limitations; first, we did not account for strong defense that might cause Curry to avoid shooting in the first place. Second, in our causation analysis, we did not account for the moment leading up to the shot, which could both impact Curry&rsquo;s comfort when shooting and the position of the nearest defender. </span></p>
                                     <p><span style="font-weight: 400;">Despite these limitations, our results invite possible future work on the impact of defensive positioning on shot outcome. It is one thing for players to shoot worse when the defender is positioned to one side; it is another thing for defensive positioning to be a viable strategy (if you focus on staying on Curry&rsquo;s right side, how might he adapt)? Future work could include cataloging the moments leading up to the shot, perhaps using Play by Play data to note screens, using ball data to categorize catch-and-shoots, and using player acceleration data to identify shooting off the dribble. With a larger dataset, this analysis could be extended to additional players to see if there is a pattern in poorer shooting percentage with the defender on the shooter&rsquo;s dominant side. Regardless, we appreciated the opportunity to work with SportVU data that is changing what we can do in the landscape of NBA analytics.</span></p>')
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
  
  src1 <- reactive( {
    get_pic_left(input$pairInput, all_matches)
  })
  
  src2 <- reactive( {
    get_pic_right(input$pairInput, all_matches)
    
  })
  
  crt1 <- reactive( {
    paste0(input$pairInput, "L.png")
  })
  
  crt2 <- reactive( {
    paste0(input$pairInput, "R.png")
  })
  
  tbl1 <- reactive( {
    get_table_left(input$pairInput, df)
  })
  
  tbl2 <- reactive( {
    get_table_right(input$pairInput, df)
  })
  
  output$court1 <- renderText({
    c('<center>', '<img height="263.2" width="140" src="', crt1(), '">', '</center>')
  })
  
  output$court2 <- renderText({
    c('<center>', '<img height="263.2" width="140" src="', crt2(), '">', '</center>')
  })

  output$image1Input <- renderText({
    c('<img height="90" width="60" src="', src1(), '">')

  })
  
  output$image2Input <- renderText({
    c('<img height="90" width="60" src="', src2(), '">')
  })
  
  output$result1Output <- renderTable({
    tbl1()
  })
  
  output$result2Output <- renderTable({
    tbl2()
  })
  
}

shinyApp(ui = ui, server = server)
