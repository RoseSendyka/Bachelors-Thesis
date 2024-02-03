# Bachelors-Thesis
 The code and data used in the Bachelor's Thesis "Check-Mates: The Role of Friendship in Online Chess Learning Motivation and Progress" by Rose Sendyka at the University of Amsterdam.
 
### Abstract
Social influences have been found to be of vital importance in learning motivation and outcomes across a variety of contexts. This study used online chess players’ data to investigate the relationship between friendships and chess skill progress on the gaming platform Lichess. First, a network analysis using a dataset of over 94 million games played over the period of one month was conducted to map the structure of player interactions (Analysis 1). Indications of friendship relationships were found. Next, data from 237 online chess players were used to investigate whether having more friendships on the site predicted higher chess skill progress, mediated through motivation (Analysis 2). Motivation was considered as the frequency of practice and perseverance after a loss. The number of friendships did not predict higher chess skill progress. No mediating effects of motivation were found. However, the number of friendships predicted the users’ frequency of practice. The users' tendency to persevere after a loss was the same when playing with friends and strangers. Applications of the findings for the improvement of education were discussed.



## Analysis 1
### Code
#### Python code
  The file 2023-10-network.py contains the code used for parsing the data of all games played in October 2023 on lichess.org. The original dataset is available at https://database.lichess.org/.


#### R code
  The file network.R contains the code used in the network analyses.

### Data

  The file 2023-10-network-all.csv contains usernames and Elo rankings of opponents from all games played in October 2023.

  The file 2023-10-network-1.csv contains usernames and Elo rankings of opponents from games played between 1-10-2023 and 2-10-2023. 
  
  The file Network_1000_usernames_elo.csv contains the usernames and Elo rankings of 1000 randomly selected users. The file Matrix_all_1000.csv contains the matrix mapping games played between those users over the month of October 2023.
  
  The file Network_2days_10000_usernames_elo.csv contains the usernames and Elo rankings of 10000 randomly selected users. The file Matrix_2days_10000.csv contains the matrix mapping the games played between those users between 1-10-2023 and 2-10-2023.



## Analysis 2
### Code
#### Python code

  The file get_games.py contains the code used in obtaining participant data from the lichess.org Application Programming Interface. The file usernames.txt contains the list of usernames from which participants' usernames were randomly selected.

#### R code

  The file get_variables.R contains the code for obtaining users' values on the variables used in the study.

  The file data_analysis.R contains the code used for the statistical analysis of the data.

  The file progress_plots.R contains the code used for visualizing users' Elo ranking progress.

### Data

  The file pooled_data.csv contains the gaming histories of the 237 participants used in the study.
  
  The file data_variables.csv contains the users' values on the variables used in the study.

