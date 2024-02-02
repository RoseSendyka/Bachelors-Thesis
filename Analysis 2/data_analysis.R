# mediation
install.packages("lavaan") 
library("lavaan")

# Sobel test
install.packages('bda') 
library('bda')

# correlation matrix
install.packages("rstatix")
library("rstatix")

# GML assumption checks
install.packages("MASS")
library("MASS")
install.packages("car") # homoskedaticity
library("car")
install.packages("psych") # skew
library("psych")

# bootstrapping
install.packages("boot.pval")
library("boot.pval")
install.packages("semhelpinghands")
library("semhelpinghands")

# plotting
install.packages("ggplot2")
library("ggplot2")
install.packages("ggExtra")
library("ggExtra")
install.packages("gplots")
library(gplots)


# Note. The variable "Perseverance" is referred to as "grit" in the code.



df <- read.csv("data_variables.csv")  # N = 237

# Data cleaning
# change Inf and -Inf to NA in GRIT_FRIEND_G
for (i in 1:length(df$grit_friend_G)) {
  if (is.na(df$grit_friend_G[i]) == T) {
    df$grit_friend_G[i] <- NA
  } else if (df$grit_friend_G[i] == "Inf") {
    df$grit_friend_G[i] <- NA
  } else if (df$grit_friend_G[i] == "-Inf") {
    df$grit_friend_G[i] <- NA
  }
}

# change Inf and -Inf to NA in GRIT_FRIEND_US
for (i in 1:length(df$grit_friend_US)) {
  if (is.na(df$grit_friend_US[i]) == T) {
    df$grit_friend_US[i] <- NA
  } else if (df$grit_friend_US[i] == "Inf") {
    df$grit_friend_US[i] <- NA
  } else if (df$grit_friend_US[i] == "-Inf") {
    df$grit_friend_US[i] <- NA
  }
}


# Change day to month
df$elo_progress_month <- df$elo_progress_day*30
df$games_per_month <- df$games_per_day*30
df$account_age_months <- df$account_age_days/30


# Descriptives
variables <- c("Number of friends (general)",
               "Number of friends (user-specific)",
               "Account age (months)",
               "Total number of games", 
               "Number of games per month", 
               "Elo progress per month", 
               "Grit", 
               "Grit with friends (general)",
               "Grit with friends (user-specific)")

mean <- c(mean(df$n_friends_G), 
          mean(df$n_friends_US),
          mean(df$account_age_months),
          mean(df$number_of_games),
          mean(df$games_per_month),
          mean(df$elo_progress_month),
          mean(df$grit), 
          mean(na.omit(df$grit_friend_G)),
          mean(na.omit(df$grit_friend_US)))

median <- c(median(df$n_friends_G), 
            median(df$n_friends_US),
            median(df$account_age_months),
            median(df$number_of_games),
            median(df$games_per_month),
            median(df$elo_progress_month),
            median(df$grit), 
            median(na.omit(df$grit_friend_G)),
            median(na.omit(df$grit_friend_US)))

sd <- c(sd(df$n_friends_G),
        sd(df$n_friends_US),
        sd(df$account_age_months),
        sd(df$number_of_games),
        sd(df$games_per_month),
        sd(df$elo_progress_month),
        sd(df$grit), 
        sd(na.omit(df$grit_friend_G)),
        sd(na.omit(df$grit_friend_US)))

descriptives <- data.frame(variables, mean, median, sd)


# Correlations n_friends_G, n_friends_US, account age, number of games
# Normality check
shapiro.test(df$account_age_months)
shapiro.test(df$number_of_games)

cor_mat(df[,c(8,9,4,5)], method = "spearman")
cor_pmat(df[,c(8,9,4,5)], method = "spearman")


# skew
hist(df$number_of_games)
skew(df$number_of_games)  # 3.389407
hist(df$account_age_months)
skew(df$account_age_months) # 0.7909488
hist(df$elo_progress_month)
skew(df$elo_progress_month) # 2.785196
hist(df$n_friends_G)
skew(df$n_friends_G) # 6.588445
hist(df$n_friends_US)
skew(df$n_friends_US) # 3.11064
hist(df$games_per_month)
skew(df$games_per_month) # 2.579294
hist(df$grit)
skew(df$grit) # -0.6237865
hist(df$grit_friend_G)
skew(df$grit_friend_G) # -0.1619201
hist(df$grit_friend_US)
skew(df$grit_friend_US) # -0.09860724


## Friends (General) 

#GLM Assumption checks
# 1. Normality
# distribution of studentized residuals
sresid <- studres(lm_progress_friends_G) 
hist(sresid, freq=FALSE, breaks = 100,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
# Q-Q plot
plot(lm_progress_friends_G,which=2)
# skew
hist(df$elo_progress_month)
skew(df$elo_progress_month) # 2.785196
hist(df$n_friends_G)
skew(df$n_friends_G) # 6.588445
# Shapiro-Wilk tests
shapiro.test(df$n_friends_G)
shapiro.test(df$elo_progress_month)
shapiro.test(df$games_per_month)
shapiro.test(df$grit)

# 2. Homoscedasticity and linearity
# plot fitted x residuals
plot(lm_progress_friends_G,which=1) 
#  scale-location plot 
plot(lm_progress_friends_G,which=3) 
# Breusch-Pagan test (homoscedasticity)
ncvTest(lm_progress_friends_G)

# 3. Independence
durbinWatsonTest(lm_progress_friends_G)

# 4. Outliers
plot(lm_progress_friends_G,which=5)
cooksdist <- cooks.distance(lm_progress_friends_G)
cooksdist[cooksdist>1]


# Regression friends -> skill progress
lm_progress_friends_G = lm(elo_progress_month~n_friends_G, data = df)
summary(lm_progress_friends_G)

# bootstrapped
set.seed(1)
boot_summary(lm_progress_friends_G)

# plot R
plot(df$n_friends_G,df$elo_progress_month)
abline(lm_progress_friends_G)

# plot ggplot
p <- ggplot(df, aes(x=n_friends_G, y=elo_progress_month)) +
  geom_point() +
  geom_abline(slope = coef(lm_progress_friends_G)[["n_friends_G"]], 
              intercept = coef(lm_progress_friends_G)[["(Intercept)"]])+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  xlab("Number of Friends (General)") + 
  ylab("Skill Progress")
p1 <- ggMarginal(p, type="histogram")
plot(p1)


# Mediation through games per day, grit
mediation_G <- '
# outcome model 
elo_progress_month ~ c*n_friends_G + b1*games_per_month + b2*grit

# mediator models
games_per_month ~ a1*n_friends_G 
grit ~ a2*n_friends_G

# indirect effects (IDE)
games_per_dayIDE  := a1*b1
gritIDE  := a2*b2
sumIDE := (a1*b1) + (a2*b2)

# total effect
total := c + (a1*b1) + (a2*b2)
grit ~~ games_per_month # model correlation between mediators
'
mediation_G_results <- sem(mediation_G, data = df)
med_G_res <- summary(mediation_G_results, standardized = TRUE, fit.measures = TRUE)

# bootstapped
set.seed(1)
mediation_G_results_boot <- sem(mediation_G, data = df, se="bootstrap")
summary(mediation_G_results_boot, standardized = TRUE, fit.measures = TRUE)
standardizedSolution_boot_ci(mediation_G_results_boot)


# Sobel tests 
mediation.test(df$grit,df$n_friends_G,df$elo_progress_month)
mediation.test(df$games_per_month,df$n_friends_G,df$elo_progress_month)


# t-test grit
people_with_friends <- df[df$n_friends_G>0,]
length(people_with_friends$X) # N = 82

participants <- people_with_friends[is.na(people_with_friends$grit_friend_G)==F,]
length(participants$X) # N = 42

grit_friend <- as.numeric(na.omit(participants$grit_friend_G))
grit_not_friend <- as.numeric(na.omit(participants$grit_not_friend_G))

# assumptions
# 1. Normally distributed data
shapiro.test(grit_friend)
shapiro.test(grit_not_friend)
hist(grit_not_friend)

# 2. Homogeneity of variance
labels <- c(rep("F", length(grit_friend)), 
            rep("notF", length(grit_not_friend)))
grit_c <- c(grit_friend, grit_not_friend)

grit_df <- data.frame(labels,grit_c)

var.test(grit_c ~ labels, data = grit_df)

# t-test
t.test(grit_friend, grit_not_friend, alternative = "greater", paired = T)


## Friends (User-Specific)

#GLM Assumption checks
# 1. Normality
# distribution of studentized residuals
sresid <- studres(lm_progress_friends_US) 
hist(sresid, freq=FALSE, breaks = 100,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
# Q-Q plot
plot(lm_progress_friends_US,which=2)
# skew
hist(df$elo_progress_month)
skew(df$elo_progress_month) # 2.785196
hist(df$n_friends_US)
skew(df$n_friends_US) # 3.11064
# Shapiro-Wilk test
shapiro.test(df$n_friends_US)

# 2. Homoscedasticity and linearity
# plot fitted x residuals
plot(lm_progress_friends_US,which=1) 
#  scale-location plot 
plot(lm_progress_friends_US,which=3)
# Breusch-Pagan test (homoscedasticity)
ncvTest(lm_progress_friends_US)

# 3. Independence
durbinWatsonTest(lm_progress_friends_US)

# 4. Outliers
plot(lm_progress_friends_US,which=5) 
cooksdist <- cooks.distance(lm_progress_friends_US)
cooksdist[cooksdist>1]


# regression friends -> grit
lm_progress_friends_US = lm(elo_progress_month~n_friends_US, data = df)
summary(lm_progress_friends_US)
# bootstrapped
set.seed(1)
boot_summary(lm_progress_friends_US)

# plot
p <- ggplot(df, aes(x=n_friends_US, y=elo_progress_month)) +
  geom_point() +
  geom_abline(slope = coef(lm_progress_friends_US)[["n_friends_US"]], 
              intercept = coef(lm_progress_friends_US)[["(Intercept)"]])+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  xlab("Number of Friends (User-Specific)") + 
  ylab("Skill Progress")
p1 <- ggMarginal(p, type="histogram")
plot(p1)

# mediation through games per day, grit
mediation_US <- '
# outcome model 
elo_progress_month ~ c*n_friends_US + b1*games_per_month + b2*grit

# mediator models
games_per_month ~ a1*n_friends_US 
grit ~ a2*n_friends_US

# indirect effects (IDE)
games_per_dayIDE  := a1*b1
gritIDE  := a2*b2
sumIDE := (a1*b1) + (a2*b2)

# total effect
total := c + (a1*b1) + (a2*b2)
grit ~~ games_per_month # model correlation between mediators
'
mediation_US_results <- sem(mediation_US, data = df)
summary(mediation_US_results, standardized = TRUE, fit.measures = TRUE)

# bootstrapped
set.seed(1)
mediation_US_results_boot <- sem(mediation_US, data = df, se="bootstrap")
summary(mediation_US_results_boot, standardized = TRUE, fit.measures = TRUE)
standardizedSolution_boot_ci(mediation_US_results_boot)


# follow-up regression frequency of practice ~ n_friends 
lm_freq_friends = lm(games_per_month~n_friends_US, data = df)
summary(lm_freq_friends)              # Multiple R-squared:  0.03063
plot(df$n_friends_US, df$games_per_month)
abline(lm_freq_friends)

cor.test(df$n_friends_US, df$number_of_games)
cor.test(df$games_per_month, df$number_of_games)


lm_conf_1 <- lm(games_per_month~number_of_games, data = df)
lm_conf_2 <- lm(games_per_month~number_of_games+n_friends_US, data = df)
summary(lm_conf_1) # Multiple R-squared:  0.3553
summary(lm_conf_2) # n_friends no longer a significant predictor after adding n_games
                 # Multiple R-squared:  0.3571 -> increase of 0.0018


# Sobel tests 
mediation.test(df$games_per_month,df$n_friends_US,df$elo_progress_month)
mediation.test(df$grit,df$n_friends_US,df$elo_progress_month)


# t-test grit

people_with_friends <- df[df$n_friends_US>0,]
length(people_with_friends$X) # N = 118

participants <- people_with_friends[is.na(people_with_friends$grit_friend_US)==F,]
length(participants$X) # = 57

grit_friend <- as.numeric(na.omit(participants$grit_friend_US))
grit_not_friend <- as.numeric(na.omit(participants$grit_not_friend_US))

# assumptions
# 1. Normally distributed data
shapiro.test(grit_friend)
shapiro.test(grit_not_friend)

# 2. Homogeneity of variance
labels <- c(rep("F", length(grit_friend)), 
            rep("notF", length(grit_not_friend)))
grit_c <- c(grit_friend, grit_not_friend)

grit_df <- data.frame(labels,grit_c)

var.test(grit_c ~ labels, data = grit_df)


# t-test grit
t.test(grit_friend, grit_not_friend, alternative = "greater", paired = T)


# plot
# G
people_with_friends <- df[df$n_friends_G>0,]
participants <- people_with_friends[is.na(people_with_friends$grit_friend_G)==F,]
grit_friend_G <- as.numeric(na.omit(participants$grit_friend_G))
grit_not_friend_G <- as.numeric(na.omit(participants$grit_not_friend_G))

# US
people_with_friends <- df[df$n_friends_US>0,]
participants <- people_with_friends[is.na(people_with_friends$grit_friend_US)==F,]
grit_friend_US <- as.numeric(na.omit(participants$grit_friend_US))
grit_not_friend_US <- as.numeric(na.omit(participants$grit_not_friend_US))

labels <- c(rep("A_Friend (general)", length(grit_friend_G)), 
            rep("B_Not friend (general)", length(grit_not_friend_G)), 
            rep("C_Friend (user-specific)", length(grit_friend_US)), 
            rep("D_Not friend (user-specific)", length(grit_not_friend_US)))
grit_c <- c(grit_friend_G, grit_not_friend_G, grit_friend_US, grit_not_friend_US)

grit_df <- data.frame(labels,grit_c)

# plot means and CIs
plotmeans(grit_c ~ labels, data = grit_df, frame = FALSE, n.label = F,
          mean.labels = F, connect = FALSE,
          pch = 19, 
          col=c("#00A600", "blue","#00A600","blue"), barcol="black")
abline(h=0, col = "grey", lty = 2)
legend(3.35,-0.28, legend=c("With friends", "With strangers"),
       fill=c("#00A600", "blue"), cex=1,
       box.lty=0)



