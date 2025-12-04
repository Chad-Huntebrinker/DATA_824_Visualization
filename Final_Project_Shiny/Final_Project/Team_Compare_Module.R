#Chad Huntebrinker

library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(tibble)
library(hoopR) #How we're getting the data
library(scales) #To convert into percentage

#-----------------------------
#UI
#-----------------------------
mod_team_compare_ui <- function(id) {
  ns <- NS(id)
  
  #Prefetch team names so UI populates quickly
  team_names <- tryCatch({
    hoopR::nba_teams()$team_name_full
  }, error = function(e) {
    character(0)
  })
  
  #Create UI Filters for what data to be shown
  fluidRow(
    column(
      width = 3,
      wellPanel(
        h4("Filters & Controls"),
        selectInput(
          ns("season"),
          "Season",
          choices = c("2020-21","2021-22","2022-23","2023-24","2024-25"),
          selected = "2024-25"
        ),
        
        selectInput(
          ns("team1"),
          "Team 1",
          choices = team_names,
          selected = ifelse(length(team_names) > 0, team_names[1], NULL)
        ),
        
        selectInput(
          ns("team2"),
          "Team 2",
          choices = team_names,
          selected = ifelse(length(team_names) > 1, team_names[2], NULL)
        ),
        
        hr(),
        
        h5("Counting stats"),
        checkboxGroupInput(
          ns("counting_stats"),
          label = NULL,
          choices = c(
            "PTS" = "PTS",
            "REB" = "REB",
            "OREB" = "OREB",
            "DREB" = "DREB",
            "AST" = "AST",
            "TOV" = "TOV",
            "STL" = "STL",
            "BLK" = "BLK",
            "FGM" = "FGM",
            "FGA" = "FGA",
            "FG3M" = "FG3M",
            "FG3A" = "FG3A",
            "FTM" = "FTM",
            "FTA" = "FTA",
            "PLUS_MINUS" = "PLUS_MINUS"
          ),
          selected = c("PTS","REB","AST")
        ),
        
        hr(),
        
        h5("Percentage stats (shown as %)"),
        checkboxGroupInput(
          ns("pct_stats"),
          label = NULL,
          choices = c(
            "FG%" = "FG_PCT",
            "3P%" = "FG3_PCT",
            "FT%" = "FT_PCT",
            "Win %" = "W_PCT"
          ),
          selected = c("FG_PCT","FG3_PCT")
        ),
        
        hr(),
        
        h5("Advanced metrics (computed)"),
        checkboxGroupInput(
          ns("adv_stats"),
          label = NULL,
          choices = c(
            "Possessions (est.)" = "Possessions",
            "Offensive Efficiency (PTS per poss *100)" = "Off_Eff",
            "PTS per Possession" = "PTS_per_Poss"
          ),
          selected = c("Possessions","Off_Eff")
        ),
        
        hr(),
        
        actionButton(ns("update"), "Update", class = "btn-primary"),
        br(), br(),
        helpText("Click Update to load stats for selected teams/season.")
      )
    ),
    
    #Main panel for Team IDs
    column(
      width = 9,
      fluidRow(
        column(
          width = 12,
          h4("Team IDs / Quick Summary"),
          tableOutput(ns("team_summary"))
        )
      ),
      
      #Season summary table
      fluidRow(
        column(
          width = 12,
          h4("Team Season Summary Table"),
          DTOutput(ns("team_stats_table"))
        )
      ),
      
      hr(),
      
      #Counting stats (like pts, assts, rebounds, etc.)
      fluidRow(
        column(
          width = 12,
          h4("Counting Stats (grouped)"),
          plotlyOutput(ns("counting_plot"), height = "380px")
        )
      ),
      
      #Percentage stats
      fluidRow(
        column(
          width = 12,
          h4("Percentage Stats (as %)"),
          plotlyOutput(ns("pct_plot"), height = "320px")
        )
      )
    ),
    
    #Our radar for head-to-head comparision
    h3("Radar Comparison"),
    plotlyOutput(ns("radar_plot"), height = "600px")
  )
}

#-----------------------------
#Server
#-----------------------------
mod_team_compare_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    #Load teams once
    teams_tbl <- reactiveVal(NULL)
    observe({
      teams_tbl( 
        tryCatch(hoopR::nba_teams(), error = function(e) NULL) )
    })
    
    #Get team id from full name
    team_id_from_name <- function(name) {
      tt <- teams_tbl()
      if (is.null(tt)) return(NA_character_)
      #Different hoopR versions may have different column names: try common ones
      if ("team_id" %in% colnames(tt)) {
        id <- tt$team_id[tt$team_name_full == name]
      }
      else if ("TEAM_ID" %in% colnames(tt)) {
        id <- tt$TEAM_ID[tt$team_name_full == name]
      }
      else {
        id <- NA_character_
      }
      if (length(id) == 0) return(NA_character_)
      id
    }
    
    #Fetch & summarize when Update pressed
    team_summary_df <- eventReactive(input$update, {
      
      req(input$season, input$team1, input$team2)
      
      id1 <- team_id_from_name(input$team1)
      id2 <- team_id_from_name(input$team2)
      team_ids <- c(id1, id2)
      
      if (any(is.na(team_ids))) {
        showNotification("Could not resolve one or more team IDs from hoopR::nba_teams()", type = "error")
        return(NULL)
      }
      
      #Fetch logs for each team separately
      logs_list <- lapply(team_ids, function(tid) {
        tryCatch({
          hoopR::nba_teamgamelogs(season = input$season, team_id = tid)$TeamGameLogs
        }, error = function(e) {
          NULL
        })
      })
      
      #If both are NULL, show notification
      if (all(vapply(logs_list, is.null, logical(1)))) {
        showNotification("No game logs returned from hoopR::nba_teamgamelogs() for the selected season/team(s).", type = "error")
        return(NULL)
      }
      
      #Bind rows and drop NULLs
      logs <- bind_rows(logs_list)
      
      if (nrow(logs) == 0) {
        showNotification("No game logs returned (empty).", type = "warning")
        return(NULL)
      }
      
      numeric_like <- names(logs)[sapply(logs, function(col) all(grepl("^[0-9\\.-]+$", col)))]
      logs[numeric_like] <- lapply(logs[numeric_like], \(x) as.numeric(x))
      
      #Ensure important columns exist
      want_numeric <- c("PTS","FG_PCT","FG3_PCT","FT_PCT","REB","AST","TOV",
                        "FGM","FGA","FG3M","FG3A","FTM","FTA","OREB","DREB",
                        "STL","BLK","PLUS_MINUS")
      present_numeric <- intersect(want_numeric, colnames(logs))
      logs[present_numeric] <- lapply(logs[present_numeric], function(x) as.numeric(as.character(x)))
      
      #Compute per-game possessions estimate per-row (needs FGA, FTA, OREB, TOV)
      if (all(c("FGA","FTA","OREB","TOV") %in% colnames(logs))) {
        logs <- logs %>%
          mutate(
            #Vouched by stats.NBA.com and ESPN
            #No need to divide by 2 we have team-level possessions, not game-level.
#https://fansided.com/2015/12/21/nylon-calculus-101-possessions/
            Possession_row = (FGA + 0.44 * FTA - OREB + TOV),
            Possession_row = as.numeric(Possession_row)
          )
      } else {
        logs$Possession_row <- NA_real_
      }
      
      #Compute WIN flag
      if ("WL" %in% colnames(logs)) {
        logs <- logs %>% mutate(WIN = ifelse(WL == "W", 1L, 0L))
      } else {
        logs$WIN <- NA_integer_
      }
      
      #Summarize team-level averages
      summary_df <- logs %>%
        group_by(TEAM_ID, TEAM_NAME) %>%
        summarise(
          Games = n(),
          Wins = sum(ifelse(is.na(WIN), 0, WIN), na.rm = TRUE),
          Losses = Games - Wins,
          PTS = if("PTS" %in% colnames(logs)) mean(PTS, na.rm = TRUE) else NA_real_,
          FG_PCT = if("FG_PCT" %in% colnames(logs)) mean(FG_PCT, na.rm = TRUE) else NA_real_,
          FG3_PCT = if("FG3_PCT" %in% colnames(logs)) mean(FG3_PCT, na.rm = TRUE) else NA_real_,
          FT_PCT = if("FT_PCT" %in% colnames(logs)) mean(FT_PCT, na.rm = TRUE) else NA_real_,
          REB = if("REB" %in% colnames(logs)) mean(REB, na.rm = TRUE) else NA_real_,
          AST = if("AST" %in% colnames(logs)) mean(AST, na.rm = TRUE) else NA_real_,
          TOV = if("TOV" %in% colnames(logs)) mean(TOV, na.rm = TRUE) else NA_real_,
          FGM = if("FGM" %in% colnames(logs)) mean(FGM, na.rm = TRUE) else NA_real_,
          FGA = if("FGA" %in% colnames(logs)) mean(FGA, na.rm = TRUE) else NA_real_,
          FG3M = if("FG3M" %in% colnames(logs)) mean(FG3M, na.rm = TRUE) else NA_real_,
          FG3A = if("FG3A" %in% colnames(logs)) mean(FG3A, na.rm = TRUE) else NA_real_,
          FTM = if("FTM" %in% colnames(logs)) mean(FTM, na.rm = TRUE) else NA_real_,
          FTA = if("FTA" %in% colnames(logs)) mean(FTA, na.rm = TRUE) else NA_real_,
          OREB = if("OREB" %in% colnames(logs)) mean(OREB, na.rm = TRUE) else NA_real_,
          DREB = if("DREB" %in% colnames(logs)) mean(DREB, na.rm = TRUE) else NA_real_,
          STL = if("STL" %in% colnames(logs)) mean(STL, na.rm = TRUE) else NA_real_,
          BLK = if("BLK" %in% colnames(logs)) mean(BLK, na.rm = TRUE) else NA_real_,
          PLUS_MINUS = if("PLUS_MINUS" %in% colnames(logs)) mean(PLUS_MINUS, na.rm = TRUE) else NA_real_,
          Possessions = if("Possession_row" %in% colnames(logs)) mean(Possession_row, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>%
        mutate(
          W_PCT = ifelse(Games > 0, Wins / Games, NA_real_),
          Off_Eff = ifelse(!is.na(Possessions) & Possessions > 0, (PTS / Possessions) * 100, NA_real_),
          PTS_per_Poss = ifelse(!is.na(Possessions) & Possessions > 0, PTS / Possessions, NA_real_)
        )
      
      #Keep column in order for UI
      summary_df <- summary_df %>%
        select(TEAM_ID, TEAM_NAME, Games, Wins, Losses, W_PCT, PTS, FG_PCT, FG3_PCT, FT_PCT,
               FGM, FGA, FG3M, FG3A, FTM, FTA, REB, OREB, DREB, AST, TOV, STL, BLK,
               Possessions, Off_Eff, PTS_per_Poss, PLUS_MINUS)
      
      summary_df
    }, ignoreNULL = TRUE)
    
    #Team summary: IDs
    output$team_summary <- renderTable({
      df <- team_summary_df()
      req(df)
      tibble::tibble(
        Team = c(input$team1, input$team2),
        TeamID = c(team_id_from_name(input$team1), team_id_from_name(input$team2))
      )
    })
    
    #Stats table, show only selected metrics (count, pct, adv)
    output$team_stats_table <- DT::renderDataTable({
      req(team_summary_df())
      df <- team_summary_df() %>% filter(TEAM_NAME %in% c(input$team1, input$team2))
      
      #Build selection list from inputs
      sel <- c()
      if (!is.null(input$counting_stats)) sel <- c(sel, input$counting_stats)
      if (!is.null(input$pct_stats)) sel <- c(sel, input$pct_stats)
      if (!is.null(input$adv_stats)) sel <- c(sel, input$adv_stats)
      
      #Some friendly rename mapping for display
      display_df <- df %>%
        select(TEAM_NAME, all_of(intersect(sel, colnames(df))))
      
      #Format percent columns (multiply by 100) for the pct group and W_PCT
      pct_cols <- intersect(c("FG_PCT","FG3_PCT","FT_PCT","W_PCT"), colnames(display_df))
      if (length(pct_cols) > 0) {
        display_df <- display_df %>%
          mutate(across(all_of(pct_cols), ~ round(.x * 100, 2)))
      }
      
      #Round numeric columns to sensible digits
      display_df <- display_df %>%
        mutate(across(where(is.numeric), ~ round(.x, 2)))
      
      DT::datatable(display_df, options = list(pageLength = 10), rownames = FALSE)
    })
    
    #Counting stats plot
    output$counting_plot <- renderPlotly({
      req(team_summary_df())
      df <- team_summary_df() %>% filter(TEAM_NAME %in% c(input$team1, input$team2))
      
      sel <- input$counting_stats
      if (is.null(sel) || length(sel) == 0) {
        #Show placeholder
        p <- plot_ly() %>% layout(title = "Select one or more counting stats on the left")
        return(p)
      }
      
      plot_df <- df %>%
        select(TEAM_NAME, all_of(intersect(sel, colnames(df)))) %>%
        pivot_longer(cols = -TEAM_NAME, names_to = "Stat", values_to = "Value")
      
      #Simple grouped bar chart
      plot_ly(plot_df, x = ~Stat, y = ~Value, color = ~TEAM_NAME, type = "bar") %>%
        layout(barmode = "group", yaxis = list(title = "Value"), xaxis = list(title = "Stat"))
    })
    
    #Percentage stats plot (as %)
    output$pct_plot <- renderPlotly({
      req(team_summary_df())
      df <- team_summary_df() %>% filter(TEAM_NAME %in% c(input$team1, input$team2))
      
      sel <- input$pct_stats
      if (is.null(sel) || length(sel) == 0) {
        p <- plot_ly() %>% layout(title = "Select one or more percentage stats on the left")
        return(p)
      }
      
      plot_df <- df %>%
        select(TEAM_NAME, all_of(intersect(sel, colnames(df)))) %>%
        pivot_longer(cols = -TEAM_NAME, names_to = "Stat", values_to = "Value")
      
      #Convert fraction -> percent 0-100 for display
      plot_df <- plot_df %>%
        mutate(Value = ifelse(is.na(Value), NA, Value * 100))
      
      plot_ly(plot_df, x = ~Stat, y = ~Value, color = ~TEAM_NAME, type = "bar") %>%
        layout(barmode = "group", yaxis = list(title = "Percent (%)"), xaxis = list(title = "Stat"))
    })
    
    #Create Radar plot
    output$radar_plot <- renderPlotly({
      req(team_summary_df())
      
      df <- team_summary_df() %>%
        filter(TEAM_NAME %in% c(input$team1, input$team2))
      
      #Combine selected metrics
      selected_metrics <- c(
        input$counting_stats,
        input$pct_stats,
        input$adv_stats
      )
      
      selected_metrics <- intersect(selected_metrics, colnames(df))
      
      if (length(selected_metrics) < 3) {
        return(
          plot_ly() %>% 
            layout(title = "Select at least 3 metrics to generate a radar chart.")
        )
      }
      
      #Convert to long format
      radar_df <- df %>%
        select(TEAM_NAME, all_of(selected_metrics)) %>%
        pivot_longer(
          cols = -TEAM_NAME,
          names_to = "Metric",
          values_to = "Value"
        )
      
      #Scale % columns to 0-100
      pct_cols <- c("FG_PCT","FG3_PCT","FT_PCT","W_PCT")
      radar_df <- radar_df %>%
        mutate(Value = ifelse(Metric %in% pct_cols, Value * 100, Value))
      
      #Plotly radar plot
      plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) %>%
        add_trace(
          r = radar_df$Value[radar_df$TEAM_NAME == input$team1],
          theta = radar_df$Metric[radar_df$TEAM_NAME == input$team1],
          name = input$team1
        ) %>%
        add_trace(
          r = radar_df$Value[radar_df$TEAM_NAME == input$team2],
          theta = radar_df$Metric[radar_df$TEAM_NAME == input$team2],
          name = input$team2
        ) %>%
        layout(
          polar = list(
            radialaxis = list(visible = TRUE)
          ),
          showlegend = TRUE
        )
    })
  })
}
