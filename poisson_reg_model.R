# load libraries ---- 

library(tidyverse)
library(slider)
library(clock)

library(tidymodels)
library(poissonreg)

library(rvest)
library(RSelenium)
library(robotstxt)

library(fs)

# collecting data 1 ----

numerals <- c(10:20, 11:21)

numerals <- sort(numerals) |>  
   as.character() |> 
   str_flatten()

numerals <- str_match_all(numerals, "\\d{4}") |>  
   unlist()

data_urls <- paste0("https://www.football-data.co.uk/mmz4281/", numerals, "/E0.csv")

file_names <- paste0("epl_season_", numerals, ".csv")

walk2(data_urls, file_names, download.file, mode = "wb")

file_move(file_names, "raw_data")

# collecting data 2 ----

page_urls <- paste0("https://understat.com/league/EPL/", 2014:2021)

football_xg_results <- 
   tibble(
      season = character(), 
      home_team = character(), 
      home_goals = character(), 
      home_xg = character(), 
      away_team = character(), 
      away_goals = character(), 
      away_xg = character()
   )

possibly_tibble <- possibly(tibble, otherwise = NULL)

binman::list_versions("chromedriver")

remote_driver <- rsDriver(
   browser = "firefox", 
   chromever = "114.0.5735.90", 
   port = free_port(),
   verbose = FALSE
)

remDr <- remote_driver$client

remDr$open()

for (url in page_urls) {
   
   remDr$navigate(url)
   
   repeat {
      
      html <- pluck(remDr$getPageSource(), 1) |> 
         read_html()
      
      temp_tbl <- possibly_tibble(
         
         season = html_element(html, ".calendar-date") |> 
            html_text(),
         home_team = html_elements(html, ".team-home a") |> 
            html_text(), 
         home_goals = html_elements(html, ".teams-goals .team-home") |> 
            html_text(),
         home_xg = html_elements(html, ".teams-xG .team-home") |> 
            html_text(),
         away_team = html_elements(html, ".team-away a") |> 
            html_text(),
         away_goals = html_elements(html, ".teams-goals .team-away") |> 
            html_text(), 
         away_xg = html_elements(html, ".teams-xG .team-away") |> 
            html_text()
      )
      
      football_xg_results <- bind_rows(football_xg_results, temp_tbl) 
      
      prev_page_button <- remDr$findElement(using = "css", ".calendar-prev")
      button_is_enabled <- pluck(prev_page_button$isElementEnabled(), 1)
      
      if (button_is_enabled == FALSE) break
      
      prev_page_button$clickElement() 
   }
}

remDr$close()

remote_driver$server$stop()

system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)

write_csv(football_xg_results, "raw_data_2/football_xg_results")

# Data cleaning 1 ----

football_xg_results <- read_csv("raw_data_2/football_xg_results")

football_xg_results |> 
   map_int(function(.x) sum(is.na(.x)))

football_xg_results <- football_xg_results |>  
   mutate(date = date_parse(season, format = "%A, %B %d, %Y")) |>  
   arrange(date, home_team)

football_xg_results <- football_xg_results |>  
   mutate(season = get_year(date) + (get_month(date) >= 8)) |>  
   select(-date)

# Data cleaning 2 ----

football_price_data <- dir("raw_data", full.names = TRUE) |>  
   map_dfr(function(.x) read_csv(.x, col_types = cols(.default = "c")))

football_price_data <- football_price_data  |>  
   select(date = Date, home_team = HomeTeam, away_team = AwayTeam,
          home_goals = FTHG, away_goals = FTAG, outcome = FTR, max_home_price = BbMxH,
          avg_home_price = BbAvH, max_draw_price = BbMxD, avg_draw_price = BbAvD,
          max_away_price = BbMxA, avg_away_price = BbAvA, B365H:BSA)

football_price_data |> 
   select(date:avg_away_price) |> 
   map_int(function(.x) sum(is.na(.x)))

football_price_data |> 
   select(date:avg_away_price) |>
   map_chr(function(.x) class(.x))

football_price_data <- football_price_data |> 
   relocate(outcome, .after = away_team) |> 
   mutate(outcome = as.factor(outcome)) |> 
   mutate(across(home_goals:avg_away_price, as.numeric))

football_price_data <- football_price_data |> 
   mutate(
      date = str_replace(date, "/10$", "/2010"),
      date = str_replace(date, "/11$", "/2011"),
      date = str_replace(date, "/12$", "/2012"),
      date = str_replace(date, "/13$", "/2013"),
      date = str_replace(date, "/14$", "/2014"),
      date = str_replace(date, "/15$", "/2015"),
      date = str_replace(date, "/16$", "/2016"),
      date = str_replace(date, "/17$", "/2017")
   )

football_price_data <- football_price_data |> 
   mutate(date = date_parse(date, format = "%d/%m/%Y"))

football_price_data <- football_price_data |> 
   mutate(across(B365H:BSA, as.numeric))

football_price_data <- football_price_data |> 
   rowwise() |> 
   mutate(max_home_price = max(c_across(ends_with("H")), na.rm = TRUE)) |> 
   ungroup()

football_price_data <- football_price_data |> 
   rowwise() |> 
   mutate(avg_home_price = mean(c_across(ends_with("H")), na.rm = TRUE)) |> 
   ungroup()

football_price_data <- football_price_data |> 
   rowwise() |> 
   mutate(max_draw_price = max(c_across(ends_with("D")), na.rm = TRUE)) |> 
   ungroup()

football_price_data <- football_price_data |> 
   rowwise() |> 
   mutate(avg_draw_price = mean(c_across(ends_with("D")), na.rm = TRUE)) |> 
   ungroup()

football_price_data <- football_price_data |> 
   rowwise() |> 
   mutate(max_away_price = max(c_across(ends_with("A")), na.rm = TRUE)) |> 
   ungroup()

football_price_data <- football_price_data |> 
   rowwise() |> 
   mutate(avg_away_price = mean(c_across(ends_with("A")), na.rm = TRUE)) |> 
   ungroup()

football_price_data <- football_price_data |> 
   select(date:avg_away_price) |> 
   drop_na()

football_price_data <- football_price_data |> 
   mutate(season = get_year(date) + (get_month(date) >= 8)) |> 
   relocate(season, .after = date)

football_xg_results <- football_xg_results |> 
   mutate(home_team = case_when(
      home_team == "Manchester City" ~ "Man City",
      home_team == "Manchester United" ~ "Man United",
      home_team == "Newcastle United" ~ "Newcastle",
      home_team == "Queens Park Rangers" ~ "QPR",
      home_team == "West Bromwich Albion" ~ "West Brom",
      home_team == "Wolverhampton Wanderers" ~ "Wolves",
      TRUE ~ home_team
   ))

football_xg_results <- football_xg_results |> 
   mutate(away_team = case_when(
      away_team == "Manchester City" ~ "Man City",
      away_team == "Manchester United" ~ "Man United",
      away_team == "Newcastle United" ~ "Newcastle",
      away_team == "Queens Park Rangers" ~ "QPR",
      away_team == "West Bromwich Albion" ~ "West Brom",
      away_team == "Wolverhampton Wanderers" ~ "Wolves",
      TRUE ~ away_team
   ))


football_data <- 
   football_price_data |> 
   left_join(football_xg_results, 
             by = c("season" = "season", "home_team" = "home_team", 
                    "away_team" = "away_team", "home_goals" = "home_goals", 
                    "away_goals" = "away_goals")) |> 
   arrange(date, home_team)

write_csv(football_data, "cleaned_data/football_data")

# Data Exploration ---- 

football_data |> 
   mutate(outcome = case_when(
      outcome == "H" ~ "Home",
      outcome == "D" ~ "Draw",
      outcome == "A" ~ "Away"
   )) |>
   ggplot(aes(x = outcome, fill = outcome)) + 
   geom_bar(colour = "black", alpha = 0.8) + 
   facet_wrap(facets = "season") + 
   coord_flip() + 
   labs(x = NULL, y = NULL, title = "Total of each outcome by season") + 
   guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
   scale_fill_manual(values = c("#009E73", "#E69F00", "#0072B2")) +
   theme_classic() + 
   theme(
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      legend.text = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 16)
   )

football_data |> 
   group_by(season) |> 
   summarise(home = sum(home_goals),
             away = sum(away_goals)) |> 
   ungroup() |>
   pivot_longer(home:away, names_to = "location", values_to = "goals") |> 
   mutate(location = factor(location, levels = c("away", "home"))) |> 
   ggplot(aes(x = location, y = goals)) + 
   geom_col(aes(fill = location), colour = "black", alpha = 0.6, width = 0.7) + 
   facet_wrap(facet = "season") + 
   coord_flip() + 
   labs(x = NULL, y = NULL, title = "Total goals home and away by season") +
   guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
   scale_fill_manual(values = c("#E69F00", "#0072B2")) +
   theme_classic() +
   theme(
      axis.text = element_text(face = "bold"),
      legend.text = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 16)
      
   )

football_data |> 
   pivot_longer(home_team:away_team, names_to = "location", values_to = "teams") |> 
   mutate(points = case_when(
      location == "home_team" & outcome == "H" ~ 3,
      location == "home_team" & outcome == "D" ~ 1,
      location == "home_team" & outcome == "A" ~ 0,
      location == "away_team" & outcome == "H" ~ 0,
      location == "away_team" & outcome == "D" ~ 1,
      location == "away_team" & outcome == "A" ~ 3
   )) |> 
   mutate(goals_scored = case_when(
      location == "home_team" ~ home_goals,
      location == "away_team" ~ away_goals
   )) |> 
   mutate(goals_conceded = case_when(
      location == "home_team" ~ away_goals,
      location == "away_team" ~ home_goals
   )) |> 
   group_by(season, teams) |> 
   mutate(match_number = seq(teams)) |> 
   mutate(total_points = cumsum(points),
          total_scored = cumsum(goals_scored),
          total_conceded = cumsum(goals_conceded),
          won = cumsum(points == 3),
          drawn = cumsum(points == 1),
          lost = cumsum(points == 0)) |> 
   ungroup() |> 
   filter(match_number == 38 & season == 2019) |>
   rename(GF = total_scored, GA = total_conceded) |> 
   mutate(GD = GF - GA) |> 
   mutate(played = match_number) |> 
   select(teams, played, won, drawn, lost, GF, GA, GD, total_points) |> 
   arrange(desc(total_points)) 

football_data |> 
   pivot_longer(home_team:away_team, names_to = "location", values_to = "teams") |> 
   mutate(goals_scored = case_when(
      location == "home_team" ~ home_goals,
      location == "away_team" ~ away_goals
   )) |> 
   mutate(goals_conceded = case_when(
      location == "home_team" ~ away_goals,
      location == "away_team" ~ home_goals
   )) |> 
   group_by(season, teams) |> 
   mutate(match_number = seq(teams)) |> 
   mutate(total_scored = cumsum(goals_scored),
          total_conceded = cumsum(goals_conceded),
          goal_diff = total_scored - total_conceded)|> 
   ungroup() |> 
   filter(season == 2019 & match_number == 38) |> 
   select(teams, goal_diff) |> 
   mutate(goal_supremacy = goal_diff - min(goal_diff)) |> 
   mutate(teams = fct_reorder(teams, goal_supremacy)) |> 
   ggplot(aes(x = teams, y = goal_supremacy)) + 
   geom_point(size = 2) +
   geom_segment(aes(x = teams, xend = teams, y = 0, yend = goal_supremacy), size = 1.25) + 
   coord_flip() +
   labs(x = NULL, y = NULL, title = "Goal Supremacy in the 2018-19 Season") +
   theme_classic()

team_order <- football_data |> 
   pivot_longer(home_team:away_team, names_to = "location", values_to = "teams") |> 
   mutate(points = case_when(
      location == "home_team" & outcome == "H" ~ 3,
      location == "home_team" & outcome == "D" ~ 1,
      location == "home_team" & outcome == "A" ~ 0,
      location == "away_team" & outcome == "H" ~ 0,
      location == "away_team" & outcome == "D" ~ 1,
      location == "away_team" & outcome == "A" ~ 3
   )) |> 
   group_by(season, teams) |> 
   mutate(match_number = seq(teams)) |> 
   mutate(total_points = cumsum(points)) |> 
   ungroup() |>
   filter(match_number == 38 & season == 2019) |>
   arrange(desc(total_points)) |> 
   pull(teams)

football_data |> 
   pivot_longer(home_team:away_team, names_to = "location", values_to = "teams") |> 
   mutate(match_outcome = case_when(
      location == "home_team" & outcome == "H" ~ "Won",
      location == "home_team" & outcome == "D" ~ "Drawn",
      location == "home_team" & outcome == "A" ~ "Lost",
      location == "away_team" & outcome == "H" ~ "Lost",
      location == "away_team" & outcome == "D" ~ "Drawn",
      location == "away_team" & outcome == "A" ~ "Won"
   )) |> 
   mutate(match_outcome = factor(
      match_outcome, levels = c("Lost", "Drawn", "Won"))
   ) |> 
   mutate(teams = factor(teams, levels = team_order)) |> 
   filter(season == 2019) |> 
   ggplot(aes(x = match_outcome)) +
   geom_bar(aes(fill = match_outcome), colour = "black", alpha = 0.8) + 
   coord_flip() +
   facet_wrap(facets = "teams") + 
   labs(y = "Totals", x = NULL, 
        title = "Breakdown of results by team for the 2018-19 season") + 
   guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
   scale_fill_manual(values = c("#009E73", "#E69F00", "#0072B2")) +
   theme_classic() +
   theme(
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      legend.text = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 16)
   )

# Feature engineering ----

football_data <- football_data |> drop_na() 

football_data_long <- football_data |> 
   pivot_longer(home_team:away_team, names_to = "location", values_to = "teams") |> 
   group_by(season, teams) |> 
   mutate(match_number = seq(teams)) |> 
   ungroup() |> 
   select(date, teams, match_number)

football_data <- football_data |> 
   left_join(
      football_data_long,
      by = c("date" = "date", "home_team" = "teams")
   ) |> 
   left_join(
      football_data_long,
      by = c("date" = "date", "away_team" = "teams"),
      suffix = c("_home", "_away")
   )

football_data <- football_data |> 
   rowwise() |> 
   mutate(
      home_goals_combined = (home_goals + home_xg) / 2,
      away_goals_combined = (away_goals + away_xg) / 2
   ) |> 
   ungroup()

football_data <- football_data |> 
   group_by(home_team, season) |> 
   mutate(
      avg_goals_home = slide_mean(home_goals_combined, before = 6),
      avg_conceded_home = slide_mean(away_goals_combined, before = 6)
   ) |> 
   mutate(
      avg_goals_home = lag(avg_goals_home),
      avg_conceded_home = lag(avg_conceded_home)
   ) |> 
   ungroup() |> drop_na()

football_data <- football_data |> 
   group_by(away_team, season) |> 
   mutate(
      avg_goals_away = slide_mean(away_goals_combined, before = 6),
      avg_conceded_away = slide_mean(home_goals_combined, before = 6)
   ) |> 
   mutate(
      avg_goals_away = lag(avg_goals_away),
      avg_conceded_away = lag(avg_conceded_away)
   ) |>    
   ungroup() |> drop_na()

football_data <- football_data |> 
   filter(match_number_home >= 6, match_number_away >= 6)

# Building a model ----

football_train <- football_data |> 
   filter(season < 2017)

football_test <- football_data |> 
   filter(season > 2016 & season < 2022)

model_home <- poisson_reg() |> 
   set_engine("glm") |> 
   fit(home_goals ~ avg_goals_home + avg_conceded_away, data = football_train)

model_away <- poisson_reg() |> 
   set_engine("glm") |> 
   fit(away_goals ~ avg_goals_away + avg_conceded_away, data = football_train)

home_preds <- predict(model_home, new_data = football_test)

away_preds <- predict(model_away, new_data = football_test)

match_preds <- football_test |> 
   select(date:away_goals, starts_with("max_")) |> 
   bind_cols(home_preds) |> 
   rename(home_pred = .pred) |> 
   bind_cols(away_preds) |> 
   rename(away_pred = .pred)

# Model evaluation ----

calculate_odds <- 
   function(outcome, pred_home_goals, pred_away_goals, max_goals = 9) {
      home_goals <- dpois(0:max_goals, pred_home_goals)
      away_goals <- dpois(0:max_goals, pred_away_goals)
      goal_matrix <- home_goals %o% away_goals
      if (outcome == "H") {
         home_prob <- sum(goal_matrix[lower.tri(goal_matrix)])
         return(home_prob)
      } else if (outcome == "A") {
         away_prob <- sum(goal_matrix[upper.tri(goal_matrix)])
         return(away_prob)
      } else if (outcome == "D") {
         draw_prob <- sum(diag(goal_matrix))
         return(draw_prob)
      }
   }

match_preds <- match_preds |> 
   rowwise() |> 
   mutate(
      home_prob = calculate_odds("H", home_pred, away_pred),
      draw_prob = calculate_odds("D", home_pred, away_pred),
      away_prob = calculate_odds("A", home_pred, away_pred)
   ) |> 
   ungroup()

match_preds <- match_preds |> 
   select(date:outcome, starts_with("max_"), ends_with("_prob")) |> 
   mutate(
      home_price = 1 / home_prob,
      draw_price = 1 / draw_prob,
      away_price = 1 / away_prob
   ) |> 
   select(date:outcome, ends_with("_price"))

results <- match_preds |> 
   mutate(home_bets = if_else(
      condition = home_price < max_home_price,
      true = (max_home_price * (outcome == "H")) - 1,
      false = 0
   )) |> 
   mutate(draw_bets = if_else(
      condition = draw_price < max_draw_price,
      true = (max_draw_price * (outcome == "D")) - 1,
      false = 0
   )) |> 
   mutate(away_bets = if_else(
      condition = away_price < max_away_price,
      true = (max_away_price * (outcome == "A")) - 1,
      false = 0
   )) 

results |> 
   group_by(season) |> 
   mutate(all_bets = home_bets + draw_bets + away_bets) |> 
   summarise(
      total_bets = sum(home_bets != 0),
      total_wins = sum(home_bets > 0),
      profit = sum(home_bets)
   ) 

results |> 
   filter(home_bets != 0) |> 
   mutate(
      year = get_year(date),
      month = get_month(date)
   ) |> 
   group_by(year, month) |> 
   mutate(profit = sum(home_bets)) |> 
   ungroup() |> 
   select(year, month, profit) |> 
   distinct() |> 
   mutate(
      profit = cumsum(profit),
      index = row_number()
   ) |> 
   ggplot(aes(x = index, y = profit)) +
   geom_line(size = 1.5) +
   labs(x = "Total Betting Days", y = "Total Profit", 
        title = "Profits for English Premier League") +
   theme_classic() + 
   theme(
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 16)
   )   









