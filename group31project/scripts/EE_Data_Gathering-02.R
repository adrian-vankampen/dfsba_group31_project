# ==============================================================================
# GETTING MORE DATA FROM ESPORTSEARNINGS.COM
# ==============================================================================
#
# After some EDA on the initial data gathered from the EsportsEarnings.com website (see "scripts/EE_Data_Gathering-01.R"), let's find more specific information about the top games.
#
# (We use the same "build_df" function as in the first data-gathering script)

source("scripts/Functions.R")

top_prizes <- read.csv("data/top_prizes.csv") %>%
  rename(Rank = X)

# ------------------------------------------------------------------------------
# 100 HIGHEST EARNING PLAYERS OF TOP 10 GAMES
# ------------------------------------------------------------------------------

for (i in seq(1:10)) {
  gameId <- top_prizes[i, "id"]
  message(paste("Searching top eanring players' data for gameID ", gameId, sep=""))
  gameIdPar <- paste("&gameid=", gameId, sep = "")
  
  
  # Nicer file-names
  
  gameName <- make_nicer_name(top_prizes[i, "GameName"])
  
  temp <- build_df(methods_list$topplayearn_bygame,
                      parameters = gameIdPar)
  
  write.csv(temp, paste("data/top_players_by_game/", gameName, sep=""))
  
  df_name <- paste("tp_", gameName, sep = "")
  
  assign(df_name,
         temp)
  
  
  rm(gameId, gameIdPar, gameName, temp, df_name, i) # For a tidy environment...
}

# ------------------------------------------------------------------------------
# 100 HIGHEST EARNING TEAMS OF TOP 10 GAMES
# ------------------------------------------------------------------------------

for (i in seq(1:10)) {
  gameId <- top_prizes[i, "id"]
  message(paste("Searching top earning teams' data for gameID ", gameId, sep=""))
  gameIdPar <- paste("&gameid=", gameId, sep = "")
  
  
  # Nicer file-names
  
  gameName <- make_nicer_name(top_prizes[i, "GameName"])
  
  temp <- build_df(methods_list$topteamsearn_bygame,
                   parameters = gameIdPar)
  
  write.csv(temp, paste("data/top_teams_by_game/", gameName, sep=""))
  
  df_name <- paste("tt_", gameName, sep = "")
  
  assign(df_name,
         temp)
  
  rm(gameId, gameIdPar, gameName, temp, df_name, i) # For a tidy environment...
}