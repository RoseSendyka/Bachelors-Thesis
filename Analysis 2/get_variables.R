# Note. The variable "Perseverance" is referred to as "grit" in the code.

pooled_data <- read.csv("pooled_data.csv")
usernames <- unique(pooled_data$username)


# variables
start_date <- character()
account_age_days <- numeric()

number_of_games <- numeric()
games_per_day <- numeric()

elo_progress_day <- numeric()

n_friends_G <- numeric() # general 
n_friends_US <- numeric() # user-specific

grit <- numeric()

grit_friend_G <- numeric() 
grit_not_friend_G <- numeric() 

grit_friend_US <- numeric()
grit_not_friend_US <- numeric()


# get cutoff values for the general threshold for friendship

## total_games
total_games_vector <- sort(unique(pooled_data$total_games))
t <- as.numeric(table(pooled_data$total_games)/total_games_vector)

# create a vector of total_games without repeated entries
total_games_no_repeats <- rep(total_games_vector[1], t[1])
for (i in 2:length(total_games_vector)) {
  total_games_no_repeats <- c(total_games_no_repeats, rep(total_games_vector[i], t[i]))
}

# cutoff total games
perc_played <- sort(total_games_no_repeats)
cutoff_games_99 <- perc_played[0.99*(length(perc_played))] # 4

# elo difference
perc_elo <- sort(pooled_data$elo_diff)
cutoff_elo_95 <- perc_elo[0.95*(length(perc_elo))] # 239



# for each player
for (i in 1:length(usernames)) {
  data <- pooled_data[pooled_data$username==usernames[i],]
  
  ## Start date
  start_date[i] <- data$game_time_from[1]
  
  ## Account age in days
  year_month_day <- substr(data$game_time_from, 1, 10)
  start <- year_month_day[1]
  end <- year_month_day[length(year_month_day)]
  date_diff <- as.Date(as.character(end), format="%Y-%m-%d")-
    as.Date(as.character(start), format="%Y-%m-%d")
  account_age_days[i] <- as.numeric(date_diff) 
  
  ## Number_of games
  number_of_games[i] <- length(data$id)
  
  ## Games per day
  games_per_day[i] <- length(data$id)/as.numeric(date_diff)
  
  ## Elo progress per day
  elo_incr <- data$player_elo[length(data$player_elo)] -  data$player_elo[1] 
  elo_progress_day[i] <- elo_incr/as.numeric(date_diff)
  
  
  ## Number of friends
  
  # Elo difference
  data$elo_diff <- abs(data$player_elo - data$opponent_elo)
  
  # Dataframe: opponents, number of games, mean Elo difference
  opponents <- unique(data$opponent)
  
  opp_total_games <- numeric()
  opp_mean_elo_diff <- numeric()
  
  for (j in 1:length(opponents)) {
    user_opp <- data[data$opponent==opponents[j],]
    opp_total_games[j] <- mean(user_opp$total_games)
    opp_mean_elo_diff[j] <- mean(user_opp$elo_diff)
  }
  
  ## General threshold
  friend_games_bin <- ifelse(opp_total_games > cutoff_games_99, 1, 0)
  friend_elo_diff_bin <- ifelse(opp_mean_elo_diff > cutoff_elo_95, 1, 0)
  
  opponents_friend_G <- ifelse(friend_games_bin==1 & friend_elo_diff_bin==1, 1,0)
  n_friends_G[i] <- sum(opponents_friend_G)
  
  # create friend_G
  friend_G <- numeric()
  for (k in 1:length(opponents)) {
    data_opp_id <- which(data$opponent == opponents[k])
    friend_G[data_opp_id] <- opponents_friend_G[k]
  }
  
  data <- data.frame(data,friend_G)
  
  ## User-specific threshold
  percentile_n_played <- rank(opp_total_games)/length(opp_total_games)
  percentile_mean_elo <- rank(opp_mean_elo_diff)/length(opp_mean_elo_diff)
  
  opponents_friend_US <- ifelse(percentile_n_played>.99 & percentile_mean_elo>.95, 1, 0)
  n_friends_US[i] <- sum(opponents_friend_US)
  
  # create friend_US
  friend_US <- numeric()
  for (k in 1:length(opponents)) {
    data_opp_id <- which(data$opponent == opponents[k])
    friend_US[data_opp_id] <- opponents_friend_US[k]
  }
  
  data <- data.frame(data,friend_US)
  
  # grit
  # create quit
  cutoff <- 300  # 5 minutes
  data$quit=ifelse(data$time_since_last_game<cutoff,0,1)
  
  # grit function
  grit_fn <- function (p,q) {
    return(log((p/(1-p))/(q/(1-q))))
  }
  
  # grit overall
  quit_win <- length(data[data$quit==1&data$player_score==1,]$id)
  n_win <- length(data[data$player_score==1,]$id)
  p <- quit_win/n_win
  
  quit_loss <- length(data[data$quit==1&data$player_score==0,]$id)
  n_loss <- length(data[data$player_score==0,]$id)
  q <- quit_loss/n_loss
  
  grit[i] <- grit_fn(p,q)
  
  
  # grit friends G
  data_friend <- data[data$friend_G==1,]
  
  quit_win <- length(data_friend[data_friend$quit==1&data_friend$player_score==1,]$id)
  n_win <- length(data_friend[data_friend$player_score==1,]$id)
  p <- quit_win/n_win
  
  quit_loss <- length(data_friend[data_friend$quit==1&data_friend$player_score==0,]$id)
  n_loss <- length(data_friend[data_friend$player_score==0,]$id)
  q <- quit_loss/n_loss
  
  grit_friend_G[i] <-  grit_fn(p,q)
  
  # grit not friends G
  data_not_friend <- data[data$friend_G==0,]
  
  quit_win <- length(data_not_friend[data_not_friend$quit==1&data_not_friend$player_score==1,]$id)
  n_win <- length(data_not_friend[data_not_friend$player_score==1,]$id)
  p <- quit_win/n_win
  
  quit_loss <- length(data_not_friend[data_not_friend$quit==1&data_not_friend$player_score==0,]$id)
  n_loss <- length(data_not_friend[data_not_friend$player_score==0,]$id)
  q <- quit_loss/n_loss
  
  grit_not_friend_G[i] <-  grit_fn(p,q)
  
  # grit friends US
  data_friend <- data[data$friend_US==1,]
  
  quit_win <- length(data_friend[data_friend$quit==1&data_friend$player_score==1,]$id)
  n_win <- length(data_friend[data_friend$player_score==1,]$id)
  p <- quit_win/n_win
  
  quit_loss <- length(data_friend[data_friend$quit==1&data_friend$player_score==0,]$id)
  n_loss <- length(data_friend[data_friend$player_score==0,]$id)
  q <- quit_loss/n_loss
  
  grit_friend_US[i] <-  grit_fn(p,q)
  
  # grit not friends US
  data_not_friend <- data[data$friend_US==0,]
  
  quit_win <- length(data_not_friend[data_not_friend$quit==1&data_not_friend$player_score==1,]$id)
  n_win <- length(data_not_friend[data_not_friend$player_score==1,]$id)
  p <- quit_win/n_win
  
  quit_loss <- length(data_not_friend[data_not_friend$quit==1&data_not_friend$player_score==0,]$id)
  n_loss <- length(data_not_friend[data_not_friend$player_score==0,]$id)
  q <- quit_loss/n_loss
  
  grit_not_friend_US[i] <-  grit_fn(p,q)
}


as <- data.frame(usernames, 
                                 start_date, 
                                 account_age_days,
                                 number_of_games,
                                 games_per_day,
                                 elo_progress_day,
                                 n_friends_G,
                                 n_friends_US,
                                 grit,
                                 grit_friend_G,
                                 grit_friend_US,
                                 grit_not_friend_G,
                                 grit_not_friend_US)


write.csv(data_variables, "data_variables.csv")



