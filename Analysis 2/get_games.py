import berserk
import dotenv # from python-dotenv

import requests
import os
import berserk
import random
import pandas as pd
import numpy as np
from datetime import date
from tqdm import tqdm
import matplotlib.pyplot as plt

with open('LICHESS_API_TOKEN.txt', "r") as f:
  api_token = f.read()

with open("usernames.txt", "r") as file:
    users = file.read().split(",")

users_sample = random.sample(users, k=20)

# opening a session to access lichess data:
session = berserk.TokenSession(api_token)
client = berserk.Client(session)

# rapid
# rapid information dataframes:
rapid_ratings = pd.DataFrame(columns = ['username', "year", 	"month", 	"day", 	"rating"])
rapid_games = pd.DataFrame(columns = ['username', "games"])

for user in tqdm(users_sample):
    try:
      user_history = client.users.get_rating_history(user)
      if len(user_history[2]["points"]) > 50: # only extract data if this user has more than 50 rated rapid games.

        # rating data...
        user_rapid_ratings = pd.DataFrame(user_history[2]["points"])
        user_rapid_ratings.insert(0, "username", user)
        user_rapid_ratings['month'] += 1  # because months in lichess API start at 0 we have to increment by 1
        rapid_ratings = pd.concat([rapid_ratings, user_rapid_ratings])

        # rapid games information:
        rapid_list = list(client.games.export_by_player(user, evals=True, clocks=True, perf_type="rapid"))
        rapid_games.loc[len(rapid_games)] = [user, rapid_list]

    except KeyError:
      pass
    except berserk.exceptions.ResponseError:
      pass

names = []
from logging import exception
for i in range(len(rapid_games)):
  username = rapid_games.iloc[i].username
  names.append(username)
  print(username)
  list_of_game_info = []
  for game in rapid_games.iloc[i].games:
    try:
      if 'aiLevel' in game['players']['white'] or 'aiLevel' in game['players']['black']: #its possible that there will be other crap to exclude weird stuff
        pass
      else:
        game_id = game['id']
        game_time_from = str(game['createdAt'])[:19]
        game_time_to = str(game['lastMoveAt'])[:19]
        game_p_white = game['players']['white']['user']['id']
        game_p_black = game['players']['black']['user']['id']
        game_elo_white = game['players']['white']['rating']
        game_elo_black = game['players']['black']['rating']
        if 'winner' in game:
          game_winner = game['winner']
        else:
          game_winner = 'draw'
        if game_p_white.lower() == username.lower():
          color = 'white'
          player_elo = game_elo_white
          opponent = game_p_black
          opponent_elo = game_elo_black
          player_score = {'black': 0, 'draw': 0.5, 'white': 1}[game_winner]
        elif game_p_black.lower() == username.lower():
          color = 'black'
          player_elo = game_elo_black
          opponent = game_p_white
          opponent_elo = game_elo_white
          player_score = {'white': 0, 'draw': 0.5, 'black': 1}[game_winner]
        else:
          print('ERROR, not white not black HELP')
        list_of_game_row = [game_id, game_time_from, game_time_to, username, opponent, player_elo, opponent_elo, player_score]
        list_of_game_info.append(list_of_game_row)
    except Exception as e:
      print(e)
      print(game)

  player_df = pd.DataFrame(list_of_game_info, columns=['id', 'game_time_from', 'game_time_to', 'username', 'opponent', 'player_elo', 'opponent_elo', 'player_score'])
  player_df = player_df.sort_values(['opponent', 'game_time_to']).reset_index(drop=True)
  player_df['previous_games'] = 0
  player_df['total_games'] = 0
  player_df['time_since_last_game'] = None
  player_df['time_since_opponent'] = None
  for ii in range(len(player_df)):
    player_df.loc[ii,'total_games'] = player_df['opponent'].value_counts()[player_df.loc[ii,'opponent']]
  for ii in range(1, len(player_df)):
    if player_df.loc[ii,'opponent'] == player_df.loc[ii-1,'opponent']:
      player_df.loc[ii,'previous_games'] = player_df.loc[ii-1,'previous_games']+1
      player_df.loc[ii,'time_since_opponent'] = (pd.to_datetime(player_df.loc[ii,'game_time_from'])-pd.to_datetime(player_df.loc[ii-1,'game_time_to'])).total_seconds()
  player_df = player_df.sort_values(['game_time_to']).reset_index(drop=True)
  for ii in range(1, len(player_df)):
    player_df.loc[ii,'time_since_last_game'] = (pd.to_datetime(player_df.loc[ii,'game_time_from'])-pd.to_datetime(player_df.loc[ii-1,'game_time_to'])).total_seconds()
  player_df.to_csv(f"{username}.csv",index=False)


names_series = pd.Series(names)
print(names_series)

names_series.to_csv("usernames_sample.csv")
