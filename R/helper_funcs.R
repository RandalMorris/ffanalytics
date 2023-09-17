


get_mfl_id = function(id_col = NULL, player_name = NULL, first = NULL,
                      last = NULL, pos = NULL, team = NULL) {
  l_p_info = list(
    player_name = player_name,
    first = first,
    last = last,
    pos = pos,
    team = team,
    id = NA
  )
  max_len = max(lengths(l_p_info))
  length_1 = lengths(l_p_info) == 1

  l_p_info[length_1] = lapply(l_p_info[length_1], function(x) {
    rep(x, max_len)
  })

  if(!is.null(player_name)) {
    if(is.null(first)) {
      l_p_info$first = sub("\\s+.*$", "", player_name)
    }

    if(is.null(last)) {
      l_p_info$last = sub(".*?\\s+", "", player_name)
    }
  }


  l_p_info = Filter(Negate(is.null), l_p_info)
  l_p_info = lapply(l_p_info, function(x) {
    x = rename_vec(toupper(x), unlist(pos_corrections))
    x = rename_vec(x, unlist(team_corrections))
    x = gsub("\\s+(defense|jr|sr|[iv]+)\\.?$", "", tolower(x))
    x = gsub("[[:punct:]]+|\\s+", "", x)
    x
  })

  if(!is.null(id_col)) {
    col_name = deparse(substitute(id_col))
    if(grepl("$", col_name, fixed = TRUE)) {
      col_name = sub(".+\\$", "", col_name)
    }
    l_p_info$id = player_ids$id[match(id_col, player_ids[[col_name]])]

    if(!anyNA(l_p_info$id)) {
      return(l_p_info$id)
    }
  }

  ref_table = player_table %>%
    mutate(across(where(is.character), tolower)) %>%
    transmute(id = id,
              player_name = paste(first_name, last_name),
              player_name = gsub("\\s+(defense|jr|sr|[iv]+)\\.?$", "", player_name),
              player_name = gsub("[[:punct:]]|\\s+", "", player_name),
              last = gsub("\\s+(defense|jr|sr|[iv]+)\\.?$", "", last_name),
              last = gsub("[[:punct:]]|\\s+", "", last),
              first = gsub("\\s+(defense|jr|sr|[iv]+)\\.?$", "", first_name),
              first = gsub("[[:punct:]]|\\s+", "", first),
              pos = rename_vec(toupper(position), unlist(pos_corrections)),
              pos = tolower(pos),
              team = rename_vec(toupper(team), unlist(team_corrections)),
              team = tolower(team))

  # If pos = DST, replace by team name
  if("pos" %in% names(l_p_info)) {
    l_p_info$id = ifelse(
      l_p_info$pos == "dst",
      ref_table$id[match(l_p_info$team, ref_table$team)],
      l_p_info$id
    )
  }

  col_combos = list(
    c("player_name", "pos", "team"),
    c("last", "pos", "team"),
    c("player_name", "team"),
    c("player_name", "pos"),
    # c("last", "team"),
    c("first", "pos", "team")
  )
  combo_idx = vapply(col_combos, function(x) {
    all(x %in% names(l_p_info))
  }, logical(1L))

  for(combo in col_combos[combo_idx]) {
    id_idx = is.na(l_p_info$id)

    l_p_info_vec = do.call(paste0, l_p_info[combo])[id_idx]
    ref_table_vec = do.call(paste0, ref_table[combo])

    # Removing dups from reftable
    ref_dups = ref_table_vec[duplicated(ref_table_vec)]
    keep_in_ref = !ref_table_vec %in% ref_dups
    ref_table_vec = ref_table_vec[keep_in_ref]


    l = lapply(l_p_info_vec, function(y) {
      which(ref_table_vec %in% y)
    })
    l[lengths(l) != 1] = NA_integer_
    match_vec = unlist(l)

    l_p_info$id[id_idx] = ref_table$id[keep_in_ref][match_vec]

  }
  l_p_info$id

}

get_scrape_year <- function(date) {
  if(missing(date)) {
    date = Sys.Date()
  }
  date = as.POSIXlt(date)
  cal_year = date$year + 1900L
  cal_month = date$mon + 1L

  if(cal_month %in% 1:3) {
    cal_year - 1L
  } else {
    cal_year
  }
}

rename_vec = function(x, new_names, old_names = NULL) {
  if(is.null(old_names)) {
    old_names = names(new_names)
    if(is.null(names(new_names))) {

      message = paste0("Must supply old_names argument, or "
                       , deparse(substitute(new_names))
                       , " needs to be a named vector with the "
                       , "old names  as the named portion")
      stop(message)
    }
  }

  idx = match(x, old_names)
  x[!is.na(idx)] = new_names[omit_NA(idx)]
  x
}

omit_NA = function(x) {
  x[!is.na(x)]
}

row_sd = function(x, na.rm = FALSE) {
  if(is.data.frame(x)) {
    x = do.call(cbind, x)
  }
  dim_x = dim(x)

  if(na.rm && anyNA(x)) {
    n_minus_1 = dim_x[2] - .rowSums(is.na(x), dim_x[1], dim_x[2]) - 1L
  } else {
    n_minus_1 = dim_x[2] - 1
  }

  r_mean = .rowMeans(x, dim_x[1], dim_x[2], na.rm = na.rm)

  r_var = .rowSums((x - r_mean)^2 / n_minus_1, dim_x[1], dim_x[2], na.rm = na.rm)
  r_sd = sqrt(r_var)
  r_sd[n_minus_1 <= 1] = NA
  r_sd
}

impute_and_score_sources = function(data_result, scoring_rules) {
  scoring_objs = make_scoring_tables(scoring_rules)

  data_result = impute_via_rates_and_mean(data_result, scoring_objs)
  data_result = impute_bonus_cols(data_result, scoring_objs$scoring_tables)

  data_result[] = source_points(data_result, scoring_rules, return_data_result = TRUE)
  data_result
}


# Returns new player_id table
update_player_id_table = function(player_id_table = NULL, id_column, value) {

}

get_pos_src_from_scrape = function(data_result) {
  data_by_pos_src = lapply(data_result, function(x) {
    split(x, x$data_src)
  })
  src_pos = stack(lapply(data_by_pos_src, names))
  split(as.character(src_pos$ind), src_pos$values)
}

# TODO: This may be supersceeded by caching at the scrape level
extract_src_scrapes_from_scrape = function(data_result) {
  pos_src = get_pos_src_from_scrape(data_result)

  lapply(setNames(names(pos_src), names(pos_src)), function(x) {
    positions = setNames(pos_src[[x]], pos_src[[x]])
    lapply(positions, function(pos) {
      data_result[[pos]][data_result[[pos]]$data_src == x,]
    })
  })
}




#Functions for weekly charts

.scrape_fantasyfootballnerd = function(week = ffanalytics:::get_scrape_week(), data = my_scrape) {

  cat("",sep = "\n")
  cat("Scraping Fantasy Nerd Weekly")
  FantasyNerds_QB = read.csv(paste0("/home/randal/Projects/AnalyticsFootball/data/FantasyNerds_Week",week,"_QB_Projections.csv"),na = "0")
  FantasyNerds_RB = read.csv(paste0("/home/randal/Projects/AnalyticsFootball/data/FantasyNerds_Week",week,"_RB_Projections.csv"),na = "0")
  FantasyNerds_WR = read.csv(paste0("/home/randal/Projects/AnalyticsFootball/data/FantasyNerds_Week",week,"_WR_Projections.csv"),na = "0")
  FantasyNerds_TE = read.csv(paste0("/home/randal/Projects/AnalyticsFootball/data/FantasyNerds_Week",week,"_TE_Projections.csv"),na = "0")

  FantasyNerds_QB$position = "QB"
  FantasyNerds_RB$position = "RB"
  FantasyNerds_WR$position = "WR"
  FantasyNerds_TE$position = "TE"

  FantasyNerds_Final = plyr::rbind.fill(FantasyNerds_QB, FantasyNerds_RB, FantasyNerds_WR,
                                        FantasyNerds_TE)
  names(FantasyNerds_Final) = tolower(names(FantasyNerds_Final))

  rm(FantasyNerds_QB, FantasyNerds_RB, FantasyNerds_WR, FantasyNerds_TE)

  FantasyNerds_Final = FantasyNerds_Final %>%
    mutate(src_id = gsub("[[:punct:]]|\\s+", "-", tolower(player)),
           data_src = "FantasyNerds") %>%
    mutate(src_id = gsub("--", "-", tolower(src_id))) %>%
    mutate(id = ffanalytics:::player_ids$id[match(.$src_id,ffanalytics:::player_ids$numfire_id)]) %>%
    mutate(id2 = ffanalytics:::player_ids$id[match(.$src_id,ffanalytics:::player_ids$fantasypro_id)]) %>%
    mutate(id = coalesce(id, id2)) %>%
    select(-c(id2))

  stat_cols = c(id = "id", src_id = "src_id", data_src = "data_src",
                player = "player", team = "team", pos = "position",
                pass_att = "pass.att", pass_comp = "pass.cmp",
                pass_yds = "pass.yds", pass_tds = "pass.td", pass_int = "int",
                rush_att = "rush.att", fumbles_lost = "fmbl",
                rush_yds = "rush.yds", rush_tds = "rush.td",
                rec = "rec", rec_yds ="rec.yds",rec_tds = "rec.td")  %>%
    plyr::ldply(., data.frame) %>%
    dplyr::rename(., match = .id, raw = X..i..)
  typ_cols = cols(id = col_character(), player = col_character(), team = col_character(),
                  pos = col_character(), pass_att = col_double(), pass_comp = col_double(),
                  pass_yd = col_double(), pass_td = col_double(),
                  rush_yd = col_double(), rush_td = col_double(),
                  rec = col_double(), rec_yd = col_double(), rec_td = col_double(),  fumbles_lost = col_double(),
                  data_src = col_character(), src_id = col_character())

  FantasyNerds_Final = FantasyNerds_Final %>% select(stat_cols$raw) %>%
    type_convert(., col_types = typ_cols)

  names(FantasyNerds_Final) = stat_cols$match[match(names(FantasyNerds_Final), stat_cols$raw)]

  FantasyNerds_Final = split(FantasyNerds_Final,FantasyNerds_Final$pos)


  for(scr_pos in names(data)) {
    data[[scr_pos]] = bind_rows(data[[scr_pos]], FantasyNerds_Final[[scr_pos]])
  }
  return(data)
}

.scrape_sleeper = function(data = my_scrape) {
  ### Get Sleeper Data
  cat("",sep = "\n")
  cat("Scraping Sleeper Weekly")
  baseurl="https://api.sleeper.com/projections/nfl/2023/"
  positions= "&position[]=QB&position[]=RB&position[]=WR&position[]=TE,&position[]=K,&position[]=DEF"
  url = paste0(baseurl,ffanalytics:::get_scrape_week(),"?season_type=regular",positions)

  sleeper_ids <- ffscrapr::sleeper_players() %>%
    filter(status == "Active", pos %in% c("QB", "RB", "WR", "TE"), team != "FA") %>%
    dplyr::rename(src_id = player_id, name = player_name) %>%
    mutate(data_src = "Sleeper",id = ffanalytics:::player_ids$id[match(.$src_id,ffanalytics:::player_ids$sleeper_id)]) %>%
    select(id, data_src, src_id, name, pos, team)

  Sleeper = httr::VERB(verb = "GET",
                       url = url) %>%
    httr::content("parsed", "application/json") %>%
    lapply(., unlist) %>% dplyr::bind_rows(.) %>% select(-c(player.team, team))
  names(Sleeper) <- gsub(x = names(Sleeper), pattern = "player.|stats.|metadata.", replacement = "")

  Sleeper = Sleeper %>% dplyr::rename(src_id = id) %>%
    inner_join(sleeper_ids, by = c("src_id" = "src_id")) %>% relocate(c(id,data_src, name, pos, team), .before = gp) %>%

    select(-c(rec_0_4,rec_5_9,rec_10_19,rec_20_29, rec_30_39, rec_40p),
           -starts_with("adp_"),-starts_with("pts_"), -starts_with("xp"), -starts_with("fg"),
           -starts_with("idp_"), -ends_with("_fd"), -starts_with("bonus_"), -starts_with("injury_"))


  stat_cols = c(id = "id", src_id = "src_id", data_src = "data_src", player = "name", team = "team",
                pos = "pos", pass_comp = "pass_cmp", pass_att = "pass_att", pass_yds = "pass_yd",
                pass_comp_pct = "cmp_pct",
                pass_tds = "pass_td", pass_int = "pass_int",rush_att = "rush_att",
                rush_yds = "rush_yd", rush_tds = "rush_td", fumbles_lost =  "fum_lost",
                two_pts = "two_pts", rec = "rec",
                rec_yds ="rec_yd",rec_tds = "rec_td") %>%
    plyr::ldply(., data.frame) %>%
    dplyr::rename(., match = .id, raw = X..i..)

  typ_cols = cols(id = col_character(), player = col_character(), team = col_character(),
                  pos = col_character(), cmp_pct = col_double(), pass_cmp = col_double(),
                  pass_att = col_double(), pass_yd = col_double(), pass_td = col_double(),
                  rush_att = col_double(), rush_yd = col_double(), rush_td = col_double(),
                  rec = col_double(), rec_att = col_double(), rec_yd = col_double(), rec_td = col_double(),
                  data_src = col_character(), src_id = col_character())

  Sleeper = Sleeper %>% dplyr::mutate(two_pts = sum(as.integer(c_across(ends_with("_2pt"))))) %>%
    select(stat_cols$raw) %>% type_convert(., col_types = typ_cols)

  names(Sleeper) <- stat_cols$match[match(names(Sleeper), stat_cols$raw)]
  Sleeper = Sleeper[!is.na(names(Sleeper))]
  Sleeper = split(Sleeper, Sleeper$pos)

  for(scr_pos in names(data)) {
    data[[scr_pos]] = bind_rows(data[[scr_pos]], Sleeper[[scr_pos]])
  }

  return(data)
}



#'Create Graphs for user
.Graphs <- function(){


  .graph_func(pos="QB", pos_txt="Quarter Back", rlim = 32, xlim=25)
  .graph_func(pos="RB", pos_txt="Running Back", rlim = 75, xlim=25)
  .graph_func(pos="WR", pos_txt="Wide Receiver", rlim = 75, xlim=25)
  .graph_func(pos="TE", pos_txt="Tight End", rlim = 40, xlim=25)
  .graph_func(pos="K", pos_txt="Kicker", rlim = 32, xlim=25)
  .graph_func(pos="DST", pos_txt="Defense", rlim = 32, xlim=25)

}

#'Base Graph Set

.graph_func <- function(pos, pos_txt, rlim, xlim){

  season = ffanalytics:::get_scrape_year()
  varweek = ffanalytics:::get_scrape_week()
  usecase = "NarFFL"

  ffproj <<- my_projections %>%
    dplyr::filter(position == pos & !is.na(floor)) %>%
    dplyr::slice_head(n = rlim)

  #geom_point(aes(x=Total_Points), size =2, shape=1, color = "black") +
  fp_plot <<- ggplot(ffproj, aes(x=points, y=pos_rank, color=factor(tier))) +
    geom_errorbarh(aes(xmin=floor,xmax=ceiling), height=.3) +
    geom_point(size=5,color="white")+
    geom_text(aes(x=points,label=round(points,0)),check_overlap = T,size=4) +
    geom_text(aes(x=ceiling, label=paste0(pos_rank, " | ",name," | ",uncertainty),
                  hjust=-.25, vjust=(.5), angle=(0), size=1),check_overlap = T, size=4) +
    geom_text(aes(x=mean(points), y= 0, label=paste("Average Uncertainty = ",
                                                    round(mean(uncertainty,na.rm = TRUE),2),sep = "")),color="black") +
    geom_text(aes(x=round(max(points),0), y= -2,
                  label=paste("Mean Projection | Rank | Player | Uncertainty",sep = ""),
                  size=1),check_overlap = T,color="black") +
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = "lightblue",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
      ,plot.title = element_text(hjust = 0.5)
      #,panel.grid.major.x = element_blank()
      #,panel.grid.minor.y = element_blank()
      ,panel.border=element_rect(color="grey",fill=NA)
      ,legend.position = "none") +
    scale_y_reverse() +
    ylab("Average Rank") + xlab("Median FPTS Projection") +
    coord_cartesian(xlim=c(0,(max(ffproj$ceiling)+xlim)))


  fp_plot + labs(title = paste(season, "Season Week", varweek, pos_txt, "Projections", usecase, date()))

  ggsave(paste0(getwd(),"/Visualizations/",season,
                "/Week ",varweek,"/",pos_txt,".png"),
         device = "png",width = 14, height = 11,dpi = 320, scale = 1, units = "in")

}




#' Cleans Raw Site Data by removing one off site columns
#' Adds Player Info
#'
#'
.RawDataClean <- function(data = my_scrape, position=c("QB", "RB","WR","TE","K","DST")){
  for (x in 1:4) {

    data[[position[x]]] = data[[position[x]]] %>%
      group_by(id) %>%
      mutate(pass_att_avg = round(mean(pass_att[!is.na(pass_att)]), 2),
             pass_comp_avg = round(mean(pass_comp[!is.na(pass_comp)]), 2),
             rush_att_avg = round(mean(rush_att[!is.na(rush_att)]), 2),
             fumbles_lost_avg = round(mean(fumbles_lost[!is.na(fumbles_lost)]), 2),
             games = 17) %>%
      mutate(pass_att = coalesce(pass_att, pass_att_avg),
             pass_comp = coalesce(pass_comp, pass_comp_avg),
             rush_att = coalesce(rush_att, rush_att_avg),
             fumbles_lost = coalesce(fumbles_lost, fumbles_lost_avg),) %>%
      mutate(pass_yds_g = round(pass_yds/games, 2),
             rush_avg = round(rush_yds/rush_att, 2),
             rec_yds_g = round(rec_yds/games, 2),) %>%
      select(-c(pass_att_avg, pass_comp_avg, rush_att_avg))

    rbind(data[[position[x]]]) %>%
      add_player_info() %>%
      mutate(player= paste(first_name,last_name)) %>%
      assign(paste(position[x]), ., envir = .GlobalEnv)

    if (position[x] == "QB"){
      keeps_qb <- c("id", "player", "team.y", "pos", "data_src",
                    "pass_att", "pass_comp",  "pass_yds", "pass_int", "pass_tds",
                    "rush_att", "rush_yds", "rush_avg", "rush_tds",
                    "fumbles_lost","two_pts")
      QB <- subset(QB, select=keeps_qb) %>%
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        rename("name" = 2, "team" = 3, "position" = 4) %>%
        add_column(pass_ypa = 0,pass_ypc =0, .before = "pass_tds") %>%
        dplyr::group_by(name,data_src) %>%
        mutate(rush_avg   = case_when(rush_att > 0 ~ round((rush_yds/rush_att),2), rush_att < 1 ~ 0, FALSE ~ 0),
               pass_ypa   = case_when(pass_att > 0 ~ round((pass_yds/pass_att),2), pass_att < 1 ~ 0, FALSE ~ 0),
               pass_ypc   = case_when(pass_comp > 0 ~ round((pass_yds/pass_comp),2), pass_comp < 1 ~ 0, FALSE ~ 0),
               pass_comp_pct= case_when(pass_att > 0 ~ round((pass_comp/pass_att),2)*100, pass_att < 1 ~ 0, FALSE ~ 0),
               rush_tds   = case_when(rush_tds > 0 ~ rush_tds, rush_tds < 1 ~ rush_tds, FALSE ~ 0),) %>%
        ungroup() %>%
        assign(paste(position[x]), ., envir = .GlobalEnv)
    } else if (position[x] == "RB"){
      keeps_rec <- c("id", "player", "team.y", "pos", "data_src",
                     "rush_att", "rush_yds", "rush_avg", "rush_tds",
                     "rec", "rec_yds", "rec_yds_g", "rec_tds",
                     "fumbles_lost","two_pts")
      RB <-subset(RB, select=keeps_rec) %>%
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        rename("name" = 2, "team" = 3, "position" = 4) %>%
        add_column(rush_ypc = 0, .before = "rush_tds") %>% dplyr::group_by(name,data_src) %>%
        mutate(rush_avg   = case_when(rush_att > 0 ~ round((rush_yds/rush_att),2), rush_att < 1 ~ 0, FALSE ~ 0),
               rush_ypc   = case_when(rush_att > 0 ~ round((rush_yds/rush_att),2), rush_att < 1 ~ 0, FALSE ~ 0)) %>%
        ungroup() %>%
        assign(paste(position[x]), ., envir = .GlobalEnv)
    } else if (position[x] == "WR"){
      keeps_rec <- c("id", "player", "team.y", "pos", "data_src",
                     "rush_att", "rush_yds", "rush_avg", "rush_tds",
                     "rec", "rec_yds", "rec_tds",
                     "fumbles_lost","two_pts")
      WR <- subset(WR, select=keeps_rec) %>%
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        rename("name" = 2, "team" = 3, "position" = 4) %>%
        add_column(rush_ypc = 0, .before = "rush_tds") %>% dplyr::group_by(name,data_src) %>%
        mutate(rush_avg   = case_when(rush_att > 0 ~ round((rush_yds/rush_att),2), rush_att < 1 ~ 0, FALSE ~ 0),
               rush_ypc   = case_when(rush_att > 0 ~ round((rush_yds/rush_att),2), rush_att < 1 ~ 0, FALSE ~ 0)) %>%
        ungroup() %>%
        assign(paste(position[x]), ., envir = .GlobalEnv)
    } else if (position[x] == "TE"){
      keeps_te <- c("id", "player", "team.y", "pos", "data_src",
                    "rush_att", "rush_yds", "rush_tds",
                    "rec", "rec_tgt", "rec_yds", "rec_tds",
                    "fumbles_lost")
      TE <- subset(TE, select=keeps_te) %>%
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        rename("name" = 2, "team" = 3, "position" = 4) %>%
        dplyr::group_by(name,data_src) %>%
        ungroup() %>%
        assign(paste(position[x]), ., envir = .GlobalEnv)
    }
  }

  for (x in 5:6) {

    if (position[x] == "K"){
      keeps_k <- c("id", "player", "team", "pos", "data_src",
                   "fg_att", "fg", "fg_miss", "FG%", "xp_att", "xp", "xp_miss", "XP%")
      K <- data[[position[x]]] %>%
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        mutate(fg_miss = fg_att-fg, 'FG%' = round((fg/fg_att)*100,2),
               xp_miss=xp_att-xp, 'XP%'=round((xp/xp_att)*100,2)) %>%
        subset(., select=keeps_k) %>%
        rename("name" = 2, "team" = 3, "position" = 4) %>%
        assign(paste(position[x]), ., envir = .GlobalEnv)
    }

    if (position[x] == "DST"){
      keeps_dst <- c("id", "player", "team", "position", "data_src",
                     "dst_int", "dst_safety", "dst_sacks", "dst_tackles", "dst_fum_rec", "dst_fum_force",
                     "dst_td", "dst_pts_allowed", "dst_pts_allowed_g", "dst_pass_yds_allowed",
                     "dst_rush_yds_allowed","dst_yds_allowed","dst_avg_yds_allowed")
      DST <- data[[position[x]]] %>%
        mutate(position = "DST") %>%
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        subset(., select=keeps_dst) %>%
        rename("name" = 2, "team" = 3, "position" = 4) %>%
        assign(paste(position[x]), ., envir = .GlobalEnv)
    }
  }

  #create detail data file
  DataAllOff <<- plyr::rbind.fill(QB,RB,WR,TE,K,DST) %>%
    filter(position %in% c("QB", "RB","WR","TE","K","DST")) %>%
    mutate_all(~replace(., is.na(.), 0))
  write.csv(DataAllOff, file = paste(getwd(),"/Data/Raw Site Data.csv", sep=""), row.names=FALSE)

}
