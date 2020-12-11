# ==============================================================================
# GETTING INITIAL DATA FROM ESPORTSEARNINGS.COM
# ==============================================================================
#
source(file = here::here("scripts/setup.R"))
source(file = here::here("scripts/Functions.R"))

# For info about the "build_df" function, see the corresponding script.
# 
# ------------------------------------------------------------------------------
# TOURNAMENTS PRIZES
# ------------------------------------------------------------------------------
# 
# CAUTION: will take more than 412 seconds to compute, DO NOT RUN.
# To test, set "offset" to a small number (i.e. between 100 and 500)

df.tournaments <- build_df(method = methods_list$recenttournaments,
                           offset = 50000) 

# Save the resulting dataframe in a csv file to access it later in other parts of the project.

write.csv(df.tournaments, "data/EsportsEarnings.com/EE_tournaments.csv", row.names = FALSE)

# 
# ------------------------------------------------------------------------------
# TEAM EARNINGS
# ------------------------------------------------------------------------------

# CAUTION: will take more than 12 seconds to compute

df.team_earnings <- build_df(method = methods_list$topteamsearning,
                             offset = 2000)

# Save the resulting data-frame in a csv file to access it in other parts of the project.

write.csv(df.team_earnings, "data/EsportsEarnings.com/EE_teams_earnings.csv", row.names = FALSE)

# 
# ------------------------------------------------------------------------------
# TEAM EARNINGS
# ------------------------------------------------------------------------------

# CAUTION: will take more than ... lots of seconds to compute

df.players_earnings1 <- build_df(method = methods_list$topplayersearning,
                             offset = 50000)

# Save the resulting data-frame in a csv file to access it in other parts of the project.

write.csv(df.players_earnings, "~data/EsportsEarnings/EE_players_earnings.csv")

# ------------------------------------------------------------------------------
# TOTAL PRIZE MONEY BY GAME
# ------------------------------------------------------------------------------
#
# To focus our project, we might want to focus on games that have the highest financial aspects (i.e. largest overall prize money).
#
# To do so, we first need all the games' IDs. 
# We can get them through the EE_tournaments.csv data-set that we already downloaded.

gameIDs <- read.csv("data/EsportsEarnings.com/EE_tournaments.csv") %>%
  pull(GameId) %>%
  unique()

# We then create a tibble that will contain each game's information by using the "LookupGameById" method's output format.

df.games_earnings <- tibble("id" = gameIDs,
                            "GameName" = "",
                            "TotalUSDPrize" = 0,
                            "TotalTournaments" = 0,
                            "TotalPlayers" = 0)

temp_df <- data.frame()

# We then use our function for each game ID. This method does not require an offset since it outputs a single row per request.

# CAUTION: will take more than 480 seconds to compute.
# To test, specify a range for gameIDs in the for-loop condition (e.g. for(i in gameIDs[1:5]) )

for(i in gameIDs) {
  
  gameIDpar <- paste(c("&gameid=", i), collapse="")
  
  temp_df <- build_df(method = methods_list$gameid,
                      parameters = gameIDpar)
  
  # print(temp_df) # Prints the output of each request, for testing purposes only.
  
  df.games_earnings[df.games_earnings$id == i, "GameName"] <- temp_df[1,"GameName"]
  df.games_earnings[df.games_earnings$id == i, "TotalUSDPrize"] <- temp_df[1,"TotalUSDPrize"]
  df.games_earnings[df.games_earnings$id == i, "TotalTournaments"] <- temp_df[1,"TotalTournaments"]
  df.games_earnings[df.games_earnings$id == i, "TotalPlayers"] <- temp_df[1,"TotalPlayers"]

}

# Save the resulting data-frame in a csv file to access it later in other parts of the project.
df.games_earnings %>% 
  write.csv("data/EsportsEarnings.com/EE_games_earnings.csv", row.names = FALSE)


# From here, we can do Exploratory Data Analysis on these few data-sets to determine which additional data would be of interest.
# ==============================================================================
# 
# 
# ------------------------------------------------------------------------------
# EARNINGS FOR LEAGUE OF LEGENDS
# ------------------------------------------------------------------------------
# 
# CAUTION: will take more than 3 seconds to compute
df.teams <- read.csv("data/EsportsEarnings.com/EE_games_earnings.csv")
lolid <- df.teams[df.teams$GameName=="League of Legends", "id"]

lolIDpar <- paste(c("&gameid=", lolid), collapse="")
df.lol_team_earnings <- build_df(method = methods_list$topteamsearn_bygame,
                                         offset = 1000,
                                         parameters = lolIDpar)

# # Save the resulting data-frame in a csv file to access it later in other parts of the project.

write.csv(df.lol_team_earnings, "data/EsportsEarnings/EE_lol_teams_earnings.csv")

# 
# ------------------------------------------------------------------------------
# EARNINGS FOR DOTA 2
# ------------------------------------------------------------------------------
# 
# Gets Dota 2's id

df.teams <- read.csv("data/EsportsEarnings.com/EE_games_earnings.csv")
dota2id <- df.teams[df.teams$GameName=="Dota 2", "id"]

dota2IDpar <- paste(c("&gameid=", dota2id), collapse="")

# CAUTION: will take more than 3 seconds to compute

df.dota2_team_earnings <- build_df(method = methods_list$topteamsearn_bygame,
                                             offset = 1000,
                                             parameters = dota2IDpar)

# Save the resulting data-frame in a csv file to access it later in other parts of the project.

write.csv(df.dota2_team_earnings, "data/EsportsEarnings/EE_dota2_teams_earnings.csv")


#############################################
########### TEST AREA #######################
#############################################
#
# These are not the droids that you are looking for...