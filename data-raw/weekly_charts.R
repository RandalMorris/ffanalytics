
suppressPackageStartupMessages(
  {
  library(ffanalytics)
  library(dplyr)
  library(ggplot2)
  library(readr)
    }
  )

NarFFL_Scoring <- AnalyticsFootball::scoring_rules[[paste0("NarFFL")]]

my_scrape <- ffanalytics::scrape_data(src = c("CBS", "ESPN", "FantasyPros", "FantasySharks", "FFToday",
                                              "NumberFire", "NFL"),
                         pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                         season = NULL, # NULL grabs the current season
                         week = NULL) # NULL grabs the current week


my_scrape = my_scrape %>%
  ffanalytics:::.scrape_sleeper(data = .) %>%
  ffanalytics:::.scrape_fantasyfootballnerd(data = .)

my_projections <<- ffanalytics::projections_table(my_scrape, avg_type = "average",
                                                  scoring_rules = NarFFL_Scoring) %>%
  ffanalytics::add_player_info() %>%
  ffanalytics::add_ecr() %>%
  ffanalytics::add_uncertainty() %>%
  tidyr::unite("first_name", first_name:last_name,sep = " ") %>% rename(name = first_name) %>%
  dplyr::select(-c(exp, pos,avg_type)) %>%
  dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, digits=2) %>%
  dplyr::arrange(position,pos_rank) %>%
  dplyr::select(c(id, name, age, team, position, points, sd_pts, floor, ceiling, dropoff, points_vor,
           floor_vor, ceiling_vor, rank, floor_rank, ceiling_rank, pos_rank, tier, pos_ecr,
           sd_ecr, uncertainty))

ffanalytics:::.Graphs()
