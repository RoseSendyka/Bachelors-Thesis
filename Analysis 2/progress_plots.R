pooled_data <- read.csv("pooled_data.csv")
usernames <- unique(pooled_data$username)

for (i in 1:length(usernames)) {
  data <- pooled_data[pooled_data$username==usernames[i],]
  
  title <- paste("User ", i, sep = "")

  plot(data$player_elo, pch = 19, col = "blue", frame.plot = F, cex = 0.4, 
       xlab = "Game", ylab = "Elo ranking", main = title)
}
