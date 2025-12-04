#Chad Huntebrinker
#Used this at first to create a shot chart for specific players.
#Ultimatly, decided against using this. But still including it because it does look cool

library(shiny)
#https://github.com/sportsdataverse/hoopR
library(hoopR)
#hoopR R Package (Official API / SportsDataVerse)
#hoopR pulls shot location and other NBA data directly from the
#NBA Stats API with valid R functions.
#Example endpoint: nba_leaguedashplayershotlocations() — returns shot 
#distribution and percentages for players across different 
#court zones (restricted area, mid-range, corner 3, etc.).
library(tidyverse)
library(ggplot2)
library(plotly)

#Get current or most recent season shot location data
#(returns aggregate shot location metrics by court zone)
shot_data <- nba_leaguedashplayershotlocations(season = "2023-24")
df_shot_locations <- as_tibble(shot_data$ShotLocations)

player_df <- df_shot_locations %>% filter(PLAYER_NAME == "Stephen Curry")

p <- ggplot(player_df) +
  geom_point(aes(x = as.numeric(LOC_X), y = as.numeric(LOC_Y), color = as.numeric(FG_PCT)), size = 3) +
  labs(title = "NBA Player Shot Chart",
       subtitle = "Stephen Curry",
       x = "Court X Coordinate", 
       y = "Court Y Coordinate") +
  scale_color_viridis_c() +
  theme_minimal()
ggplotly(p, tooltip = c("PLAYER_NAME", "FG_PCT", "SHOT_ZONE_BASIC"))

options(hoopR.nba_headers = TRUE)

shots_raw <- nba_shotchartdetail(
  player_id = 201939,
  season = "2023-24",
  season_type = "Regular Season"
)

shots <- shots_raw$Shot_Chart_Detail %>%
  mutate(
    LOC_X = as.numeric(LOC_X),
    LOC_Y = as.numeric(LOC_Y),
    SHOT_MADE_FLAG = as.numeric(SHOT_MADE_FLAG),
    SHOT_DISTANCE = as.numeric(SHOT_DISTANCE)
  )
nba_half_court_plotly <- function() {
  
  #Three-point arc (centered at hoop: x=0, y=0)
  arc <- data.frame(
    x = 237.5 * cos(seq(-pi/2, pi/2, length.out = 300)),
    y = 237.5 * sin(seq(-pi/2, pi/2, length.out = 300))
  )
  
  #Free-throw circle
  ft <- data.frame(
    x = 60 * cos(seq(0, 2*pi, length.out = 300)),
    y = 143.5 + 60 * sin(seq(0, 2*pi, length.out = 300))
  )
  
  list(
    annotate("rect", xmin=-250, xmax=250, ymin=-47.5, ymax=422.5, fill=NA, color="black"),
    annotate("rect", xmin=-80, xmax=80, ymin=-47.5, ymax=143.5, fill=NA, color="black"),
    annotate("rect", xmin=-60, xmax=60, ymin=-47.5, ymax=143.5, fill=NA, color="black"),
    
    geom_path(data=ft, aes(x=x, y=y)),
    geom_path(data=arc, aes(x=x, y=y)),
    
    annotate("segment", x=-30, xend=30, y=-7.5, yend=-7.5),
    annotate("segment", x=-220, xend=-220, y=-47.5, yend=92.5),
    annotate("segment", x= 220, xend= 220, y=-47.5, yend=92.5)
  )
}

p <- ggplot(shots) +
  nba_half_court_plotly() +
  geom_point(
    aes(
      x = LOC_X,
      y = LOC_Y,
      color = SHOT_MADE_FLAG,
      text = paste(
        "Player:", PLAYER_NAME,
        "<br>Action:", ACTION_TYPE,
        "<br>Shot Zone:", SHOT_ZONE_BASIC,
        "<br>Result:", ifelse(SHOT_MADE_FLAG == 1, "Made", "Missed")
      )
    ),
    size = 3,
    alpha = 0.75
  ) +
  scale_color_gradient(low = "red", high = "green") +
  coord_fixed() +
  scale_y_continuous(limits = c(-50, 300)) +  #FIX FOR SQUISHING
  theme_void() +
  labs(
    title = "Stephen Curry Shot Chart (2023–24)",
    subtitle = "Regular Season"
  )

ggplotly(p, tooltip = "text")
