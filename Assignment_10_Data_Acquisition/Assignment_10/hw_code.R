#Chad Huntebrinker

library(rvest)
library(dplyr)

url <- 'https://www.basketball-reference.com/leagues/NBA_2024_per_game.html'
nba_stats <- url %>% read_html() %>% html_table() %>% .[[1]]
nba_stats <- nba_stats %>% filter(Player != 'Player')
head(nba_stats)

#Assinment 2
nba_data = 
scaled_data <- scale(nba_data[, c("PTS", "AST", "TRB", "STL", "BLK", "FG.", "X3P.", "FT.", "TOV")])