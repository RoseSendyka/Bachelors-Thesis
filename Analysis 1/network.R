install.packages("igraph")
library(igraph)

# month data
data <- read.csv("2023-10-network-all.csv")  
# 2 days data
data <- read.csv("2023-10-network-1.csv")   # 1-2 Oct 2023

N <- 10000   # N = 1000 used for month data
names <- sample(data$White, N)
M <- matrix(rep(0,N^2), N,N)
elo <- numeric()

for (i in 1:N) {
  data2 <- data[data$White == names[i],]
  elo[i] <- mean(data2$WhiteElo)
  for (j in 1:N) {
    v <- grepl(names[j], data2$Black) # logical vector of matches
    M[i,j] <- sum(v)
    }
}


names_elo <- data.frame(names, elo)
max(M)
hist(M[M>0])

# move everything to lower triangle
l <- M[lower.tri(M)]
u <- M[upper.tri(M)]
M1 <- matrix(0, nrow = N, ncol = N)
lu <- l + u
M1[lower.tri(M1, diag = F)] <- lu

# remove nodes with no connections
row_full <- which(rowSums(M1[])>0) #non-zero rows;  which(rowSums(M1[])>10) was used in another analysis
col_full <- which(colSums(M1[])>0) #non-zero columns;  which(colSums(M1[])>10) was used in another analysis

x_full <- c(row_full, col_full)
x_full <- unique(x_full)  # combined - non-zero participants

names_elo <- names_elo[x_full,]

names_full <- names[x_full]
M2 <- M1[x_full,x_full]


remaining <- length(M2[1,])
title <- paste(N," -> ", remaining, " participants", sep="")


# graph of non-zero participants
graph <- graph_from_adjacency_matrix(M2, mode='undirected', weighted = T)

# coulour by elo
# 16 coulours
colours <- c("#253494", "#225EA8", "#1D91C0", "#41B6C4","#7FCDBB", "#C7E9B4",
             "#EDF8B1","#FFFFCC", "#FFEDA0", "#FED976",
             "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")

#  split into 16 categories
min <- min(names_elo_n$elo)
max <- max(names_elo_n$elo)
alpha <- (max - min)/16
a <- 0:16
b <- min + alpha*a

names_elo <- names_elo[,1:2]
cat <- cut(names_elo$elo, breaks=b, labels=1:16)
names_elo <- data.frame(names_elo, cat)


# plot
V(graph)$cat <- names_elo$cat
my_color <- colours[as.numeric(as.factor(V(graph)$cat))]

plot(graph,
     vertex.color = my_color,
     vertex.label=NA,
     edge.color = 'gray77',
     edge.width= E(graph_n)$weight,
     vertex.size = 2,
     layout=layout.fruchterman.reingold, main=title)


# plot only large clusters
cls <- clusters(graph)
remaining <- sum(cls$csize[cls$csize>10])
title <- title <- paste(N," -> ", remaining, " participants", sep="")
graph_clusters <- delete_vertices(graph, V(graph)[cls$membership %in% which(cls$csize < 10)])
plot(graph_clusters,
     vertex.color = my_color,
     vertex.label=NA,
     edge.color = 'gray77',
     edge.width= E(graph_n)$weight,
     vertex.size = 2,
     layout=layout.fruchterman.reingold, main=title)

# descriptives
hist(M[M>0])
table(M2)
max(M1)
hist(M2[M2>0])


