library(tidyverse)
library(data.table)
library(ncaahoopR)
library(sqldf)
library(cluster)    
library(factoextra) 
library(gridExtra)

## get_boxscore function (credit lbenz, ncaahoopR)
get_boxscore <- function(game_id) {
  url <- paste0("https://www.espn.com/mens-college-basketball/boxscore?gameId=", game_id)
  webpage <- xml2::read_html(url)
  
  # Grab team names. Away team is always listed first.
  pagetext <- rvest::html_text(webpage)
  matchup <- unlist(strsplit(pagetext, "-"))[[1]][1]
  away_name <- unlist(strsplit(matchup, " vs. "))[1]
  away_name <- stringr::str_trim(away_name)
  home_name <- unlist(strsplit(matchup, " vs. "))[2]
  home_name <- stringr::str_trim(home_name)
  
  # General tidying and splitting of columns.
  away <- rvest::html_table(webpage)[[2]]
  away <- away[1:(nrow(away) - 1),]
  away <- away[-6,]
  away <- tidyr::separate(away, 'FG', c("FGM", "FGA"), sep = "-")
  away <- tidyr::separate(away, '3PT', c("3PTM", "3PTA"), sep = "-")
  away <- tidyr::separate(away, 'FT', c("FTM", "FTA"), sep = "-")
  away_totals <- away[nrow(away):nrow(away),]
  away_totals$Position <- NA
  away <- head(away, -1)
  away$Position <- substr(away$Starters, nchar(away$Starters), nchar(away$Starters))
  away$Starters <- substr(away$Starters, 0, (nchar(away$Starters)-1)/2)
  away <- rbind(away, away_totals)
  rownames(away) <- NULL
  colnames(away)[1] <- "player"
  colnames(away)[18] <- "position"
  away <- away[, c(1, 18, 2:(ncol(away)-1))]
  away$starter <- F
  away$starter[1:5] <- T
  
  home <- rvest::html_table(webpage)[[3]]
  home <- home[1:(nrow(home) - 1),]
  home <- home[-6,]
  home <- tidyr::separate(home, 'FG', c("FGM", "FGA"), sep = "-")
  home <- tidyr::separate(home, '3PT', c("3PTM", "3PTA"), sep = "-")
  home <- tidyr::separate(home, 'FT', c("FTM", "FTA"), sep = "-")
  home_totals <- home[nrow(home):nrow(home),]
  home_totals$Position <- NA
  home <- head(home, -1)
  home$Position <- substr(home$Starters, nchar(home$Starters), nchar(home$Starters))
  home$Starters <- substr(home$Starters, 0, (nchar(home$Starters)-1)/2)
  home <- rbind(home, home_totals)
  rownames(home) <- NULL
  colnames(home)[1] <- "player"
  colnames(home)[18] <- "position"
  home <- home[, c(1, 18, 2:(ncol(home)-1))]
  home$starter <- F
  home$starter[1:5] <- T
  
  for(i in 3:18) {
    home[,i] <- as.numeric(home[,i])
    away[,i] <- as.numeric(away[,i])
  }
  
  
  results <- list(away, home)
  names(results) <- c(away_name, home_name)
  results$home_team <- home_name ##
  results$away_team <- away_name ##
  
  
  return(results)
}

## get ACC teams 
acc_teams <- ncaa_colors %>% 
  filter(conference == 'ACC') %>% 
  select(ncaa_name, espn_name, conference) %>% 
  data.frame()

## get ACC team schedules
acc_team_schedules <- data.frame()
## loop through 
for(team_name in acc_teams$espn_name){
  print(team_name)
  team_schedule <- get_schedule(team_name) %>% 
    filter(!is.na(team_score) & !is.na(opp_score)) %>% 
    mutate(team = team_name)
  acc_team_schedules <- suppressWarnings(bind_rows(acc_team_schedules, 
                                                   team_schedule))
}

## get box scores for all games 
## vector of all games played by ACC teams
games <- acc_team_schedules[!is.na(acc_team_schedules$record), 'game_id']
## loop through games and collect box scores
box_scores <- data.frame()
for (g in games){
  print(paste('Getting box score for game ', g, sep = ''))
  ## get individual game box score
  bs <- get_boxscore(g)
  ## separate the 2 teams' box scores 
  team_1 <- bs[[1]] %>% cbind(game_id = g)
  team_2 <- bs[[2]] %>% cbind(game_id = g)
  ## get team names
  team_1_name <- names(bs)[1]
  team_2_name <- names(bs)[2]
  ## add team names 
  team_1 <- team_1 %>% mutate(team_name = team_1_name)
  team_2 <- team_2 %>% mutate(team_name = team_2_name)
  ## one box score total per game
  tmp_bs <- suppressWarnings(bind_rows(team_1, team_2))
  tmp_bs <- tmp_bs %>% filter(player != 'TEAM')
  ## combine individual game with all box scores for all games collected 
  box_scores <- suppressWarnings(bind_rows(box_scores, tmp_bs)) 
  print(uniqueN(box_scores$game_id)) ## print number of games looped through 
  print(dim(box_scores)) ## data check: if number of cols = 21, data is correct
}

## remove duplicates from box scores (teams play against each other)
## filter to only ACC teams 
box_scores <- box_scores %>% 
  distinct() %>% 
  filter(team_name %in% acc_teams$ncaa_name | 
           team_name %in% acc_teams$espn_name)

## get team rosters
acc_team_rosters <- data.frame()
## loop through teams to get rosters 
for(team_name in acc_teams$espn_name){
  print(team_name)
  team_roster <- get_roster(team_name) %>% 
    mutate(team = team_name)
  acc_team_rosters <- suppressWarnings(bind_rows(acc_team_rosters, 
                                                 team_roster))
}

## clean rosters data 
acc_team_rosters <- suppressWarnings(acc_team_rosters %>% 
  mutate(team_name_box_score = case_when( # fix team names to match box_scores df 
    team == 'UVA' ~ 'Virginia', 
    team == 'Pitt' ~ 'Pittsburgh', 
    team == 'UNC' ~ 'North Carolina', 
    TRUE ~ team)) %>% 
  filter(team_name_box_score %in% box_scores$team_name) %>% 
  mutate(name2 = name) %>% 
  separate(name2, into = c('first', 'last', 'suffix'), sep = ' ') %>% 
    mutate(first_initial = substr(first, 1, 1), 
           suffix = ifelse(is.na(suffix), '', suffix)) %>% 
    mutate(box_score_name = paste(first_initial, '. ', last, ' ', suffix, sep = ''), 
           box_score_name = trimws(box_score_name), 
           box_score_name = case_when( # fix player names where no join
             box_score_name == 'A. Karim Coulibaly' ~ 'A. Coulibaly', 
             box_score_name == 'P. Horne' ~ 'P.J. Horne', 
             box_score_name == 'C. Bryce' ~ 'C.J. Bryce', 
             box_score_name == 'M. Walker' ~ 'M.J. Walker', 
             box_score_name == 'K. Smith' ~ 'K.J. Smith', 
             box_score_name == 'A. Taylor' ~ 'A.J. Taylor', 
             TRUE ~ box_score_name)))

## add height and weight to box_scores df
box_scores <- suppressWarnings(sqldf("select bs.*, 
                a.height, a.weight, a.name as player_full_name, a.team as team_name2
             from box_scores bs 
             left join acc_team_rosters a 
              on bs.team_name = a.team_name_box_score 
              and bs.player = a.box_score_name 
              and bs.position = a.position") %>% 
  mutate(weight = str_replace_all(weight, ' lbs', ''), 
         weight = as.numeric(weight)) %>% 
  separate(height, c('height_ft', 'height_in'), sep = "' ") %>% 
  mutate(height_in = str_replace_all(height_in, '"', ''), 
         height_in = as.numeric(height_in), 
         height_ft = as.numeric(height_ft)) %>% 
  filter(!is.na(height_in) & !is.na(height_ft) & !is.na(weight)) %>% 
  mutate(height = height_ft * 12 + height_in) %>% 
  select(-height_in, -height_ft) %>% 
  rename(height_in = height, 
         weight_lbs = weight))

## aggregate player box scores to 1 row per player 
player_data <- box_scores %>% 
  group_by(player, player_full_name, team_name, team_name2) %>% 
  ## season totals 
  summarise(gp = n(), 
            mins_total = sum(MIN, na.rm = TRUE), 
            fgm_total = sum(FGM, na.rm = TRUE), 
            fga_total = sum(FGA, na.rm = TRUE), 
            fg3m_total = sum(`3PTM`, na.rm = TRUE), 
            fg3a_total = sum(`3PTA`, na.rm = TRUE), 
            reb_total = sum(REB, na.rm = TRUE), 
            ast_total = sum(AST, na.rm = TRUE), 
            stl_total = sum(STL, na.rm = TRUE), 
            to_total = sum(TO, na.rm = TRUE), 
            pts_total = sum(PTS, na.rm = TRUE), 
            wt_total = sum(weight_lbs, na.rm = TRUE), 
            ht_total = sum(height_in, na.rm = TRUE)) %>% 
  data.frame() %>% ungroup() %>% 
  ## per game averages
  transmute(player = player, 
            player_full_name = player_full_name, 
            team_name = team_name,
            team_name2 = team_name2, 
            mpg = mins_total / gp, 
            fgp = fgm_total / fga_total, 
            fg3p = fg3m_total / fg3a_total, 
            fg3a = fg3a_total / fga_total, # percent of FG's that are 3s
            rpg = reb_total / gp, 
            apg = ast_total / gp, 
            tog = to_total / gp, 
            ppg = pts_total / gp, 
            weight = wt_total / gp, 
            height = ht_total / gp) %>% 
  filter(mpg >= 10) %>% ## min 10mpg for players
  ## fix NA values in 3p fg pct 
  mutate(fg3p = ifelse(is.na(fg3p), 0, fg3p))

## end data collection for clustering 

#### BEGIN CLUSTERING 
## scale data
scale_df <- player_data[,5:ncol(player_data)]
## df: data input for kmeans clustering 
df <- scale(scale_df)

## quick clustering of 2:5 clusters 
k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

## compare 2:5 clusters visually 
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
## plot 2:5 clusters
grid.arrange(p1, p2, p3, p4, nrow = 2)

### 3 or 4 clusters looks best 

set.seed(123)
## compute WSS function
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

## cluster for 1:15 clusters to compare results
k_vals <- 1:15

## extract wss for 2-15 clusters
wss_values <- map_dbl(k_vals, wss)

## plot clusters WSS values 
plot(k_vals, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

## plot again for optimal number of clusters
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

## 4 clusters looks to be best 

avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

## Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
## plot for optimal number of clusters based on silhouette width
fviz_nbclust(df, kmeans, method = "silhouette")

## 4 clusters has best silhouette score 

## use 4 clusters for final output 
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)

## viz of final cluster output (4 clusters)
fviz_cluster(final, data = df)

## compute cluster means
suppressWarnings(player_data %>%
  mutate(cluster = final$cluster) %>%
  group_by(cluster) %>%
  summarise_all("mean"))

## add cluster to player_data
player_data <- player_data %>%
  mutate(cluster = final$cluster)

## GET PLAYER SHOT DATA FOR SHOT CHART MAPPING 
games <- acc_team_schedules[!is.na(acc_team_schedules$record), 'game_id']
## loop through games and collect box scores
shot_data <- data.frame()
for (g in games){
  print(paste('Getting shot data score for game ', g, sep = ''))
  # print(g)
  tmp_shot_data <- get_shot_locs(g)
  # tmp_shot_data$game_id <- g
  if(!is_null(tmp_shot_data)){
    tmp_shot_data <- tmp_shot_data %>% mutate(game_id = g)
  } else {
    next()
  }
  shot_data <- rbind(shot_data, tmp_shot_data)
  shot_data <- shot_data %>%
    filter(team_name %in% player_data$team_name | team_name %in% player_data$team_name2)
}

games <- acc_team_schedules[!is.na(acc_team_schedules$record), 'game_id'] %>% unique()
## loop through games and collect box scores
shot_data <- data.frame()
for (game_nbr in 1:length(games)){
  ## get game_id to find shot data for
  game_id <- games[game_nbr]
  ## print status 
  print(paste('Getting shot data for game ', game_id, '. Game ', game_nbr, ' of ', length(games),
              sep = ''))
  ## get shot data for individual game
  tmp_shot_data <- suppressMessages(get_shot_locs(game_id))
  ## if shot data is available, add game_id as a column
  if(!is_null(tmp_shot_data)){
    tmp_shot_data <- tmp_shot_data %>% mutate(game_id = game_id)
    print('Game has shot data available.')
  } else {
    print('No shot data available for game.')
  }
  
  ## combine individual game shot data to all games
  shot_data <- bind_rows(shot_data, tmp_shot_data)
  
  ## only ACC teams 
  shot_data <- shot_data %>%
  filter(team_name %in% player_data$team_name | team_name %in% player_data$team_name2)
}


## make copy of shot data
shot_chart_data <- shot_data
## put all shots on one side of the court
shot_chart_data[shot_chart_data$y > 47, 'x'] <- 50 - shot_chart_data[shot_chart_data$y > 47, 'x']
shot_chart_data[shot_chart_data$y > 47, 'y'] <- 94 - shot_chart_data[shot_chart_data$y > 47, 'y']
shot_chart_data <- sqldf("select c.*, p.cluster
                   from shot_chart_data c 
                   join player_data p 
                    on c.shooter = p.player_full_name and c.team_name = p.team_name") %>% distinct()

## get teams in each game and scores 
teams_in_games <- acc_team_schedules %>% 
  # filter(team %in% player_data$team_name) %>% 
  mutate(opponent = paste0(opponent, ': ', opp_score), 
         team = paste0(team, ': ', team_score)) %>% 
  select(game_id, date, team1 = opponent, team2 = team) %>% 
  gather(key = 'team', value = 'g', -game_id, -date) %>% 
  select(-team) %>% 
  distinct() %>% 
  group_by(game_id, date) %>%
  summarise(teams = toString(g)) %>% distinct() %>% 
  separate(teams, c('team1', 'team2'), sep = ", ") %>% 
  separate(team1, c('team1', 'team1_score'), sep = ": ") %>% 
  separate(team2, c('team2', 'team2_score'), sep = ": ")

## add game scores to shot_chart_data
shot_chart_data <- sqldf("select s.*, t.team1, t.team1_score, t.team2, t.team2_score 
             from shot_chart_data s 
             left join teams_in_games t 
              on s.game_id = t.game_id") %>% data.frame() %>% 
  mutate(opponent = ifelse(team_name == team1, 
                           team2, team1), 
         opponent_score = ifelse(team_name == team1, 
                                 team2_score, team1_score), 
         team = ifelse(team_name == team1, 
                       team1, team2), 
         team_score = ifelse(team_name == team1, 
                             team1_score, team2_score), 
         win_loss = ifelse(team_score < opponent_score, "L", "W")) %>% 
  ## drop columns
  select(-team1, -team1_score, -team2, -team2_score) %>% 
  mutate(row_nbr = 1:n(),
    shot_id = paste0('s', row_nbr))

## player data long format
player_data_long <- player_data %>% 
  gather(key = 'stat', value = 'value', 
         -player, -player_full_name, -team_name, -team_name2, -cluster)

## write output files for tableau
fwrite(shot_chart_data, "Desktop/shot_chart_data.csv")
fwrite(player_data, "Desktop/player_clustering_data.csv")
fwrite(player_data_long, "Desktop/player_clustering_long_data.csv")

