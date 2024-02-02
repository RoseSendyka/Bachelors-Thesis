import pandas as pd
def parseline(line):
    if line[0] == '[':
        return line.split(' ')[0][1:],line.split('"')[1]
    return('', '')

with open("oct2023_1.pgn", "r") as file:
    games = []
    count = 0
    gamedict = {}
    for line in file:
        name,value = parseline(line)
        gamedict[name] = value
        count += 1
        if parseline(line)[0] == 'Event' and len(gamedict)>5:
            newdict = {k: gamedict[k] for k in ('White', 'Black', 'WhiteElo')}
            games.append(newdict)
            gamedict = {}
        #if count > 100000000:
        #   break
data = pd.DataFrame(games)
data.to_csv("2023-10-network-all.csv")
