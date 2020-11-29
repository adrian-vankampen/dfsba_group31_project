# ==============================================================================
# GETTING DATA FROM ESPORTSEARNINGS.COM
# ==============================================================================
#
# This script pulls different data-sets from www.esportsearnings.com through API.
# Documentation on how to do so was accessed from here: https://www.esportsearnings.com/apidocs
# Here is a brief summary of the documentation:
#
# -------------------------------------------------------------------------------
#
# API requests are of the following form:
# http://api.esportsearnings.com/v<versionnumber>/<methodname>?apikey=<apikey>&format=<format>
#
# Methods indicate which type of data we wish to pull. This can be any of the 12 listed in methods_list here.
#

methods_list <- list("player" = "LookupPlayerById",
                    "playertourn" = "LookupPlayerTournaments",
                    "topplayersearning" =  "LookupHighestEarningPlayers",
                    "gameid" = "LookupGameById",
                    "topplayearn_bygame" = "LookupHighestEarningPlayersByGame",
                    "recenttournaments" = "LookupRecentTournaments",
                    "tournamentid" = "LookupTournamentById",
                    "tresults" = "LookupTournamentResultsByTournamentId",
                    "tresults_team" = "LookupTournamentTeamResultsByTournamentId",
                    "tplayersteam" = "LookupTournamentTeamPlayersByTournamentId",
                    "topteamsearning" = "LookupHighestEarningTeams",
                    "topteamsearn_bygame" = "LookupHighestEarningTeamsByGame")

# Not all methods require exactly the same input, nor produce the same outputs. There are however common elements to all methods that need to be provided:
# - Version:
# The site's API is in beta, hence the version is 0

# - API Key:
# We created an account to obtain an API key:
#
# account id : adrian1811
# API key : 377aeceec830e58cdce6d6f09a365a6c2e438a01160f1ac4a85b1272106dcea7
#
#
# - Format:
# The output format can be any of json (by default), xml or csv. For our purposes, we will prefer csv.
#
# - Offset:
# Some methods can output large datasets. For each API query however, the website outputs only the first 100 rows. To get the full data sets, one must make several queries with a special parameter named "offset" in order to get all the rows, 100 rows at a time.
#
#
# Since we are going to build several data-sets from several methods, we write a function that will be usable for most cases:
# ------------------------------------------------------------------------------
# Function that builds a data-frame by iterating queries done through the API.
# ------------------------------------------------------------------------------

build_df <- function(method,
                     versionnumber = "0",
                     apikey = "377aeceec830e58cdce6d6f09a365a6c2e438a01160f1ac4a85b1272106dcea7",
                     format = "csv",
                     offset = FALSE,
                     parameters = "")
  # "parameters" contains additional method-specific parameters
{
  # To write the address for the appropriate request, we use a list that we will paste later on as a single characters string.
  
  api_list <- list(
    "domain" = "http://api.esportsearnings.com/v",
    "versionnumber" = versionnumber,
    "methodsep" = "/",
    "methodname" = method,
    "apikeysep" = "?apikey=",
    "apikey" = apikey,
    "formatsep" = "&format=",
    "format" = format,
    "parameters" = parameters
  ) # Additional method-specific parameters, see documentation.
  
  # Here is the actual request!!!
  
  url <- paste(api_list, collapse = "")
  df <- read.csv(url)
  
  Sys.sleep(1.1) # This will be useful for calling multiple iterations of "no offset" methods.
  
  # All this work for that df!
  #
  # Now, by default, "offset = FALSE". However, if the method outputs large data-sets (as described above), we add the "offset" parameter (which corresponds to the maximum offset that we want) when calling the function.
  # N.B. if everything works as intended, we wouldn't need to specify the maximum offset. However, we prefer to specify it explicitly in order to avoid long loops if needed, for instance when testing the code. If we want the full dataset, then we just provide a very large number as maximum offset.
  
  if (offset!=FALSE) {
    
    for (step in seq(from = 100, to = offset, by = 100)) {
      url <- paste(c(url,
                     "&offset=",
                     as.character(step)),
                   collapse = "")
      df.next <- try(read.csv(url),
                     silent = TRUE)
      if (inherits(df.next, "try-error"))
        break
      
      df <- rbind(df, df.next)
      Sys.sleep(1.1) 
      # We can only access the API at most once a second.
      # This is the reason why the function can work very slowly for large data-sets. Unfortunately, we cannot do anything about that as of this API version.
    }
  }
  
  return(df)
  
}

# ==============================================================================
# THE ACTUAL DATA GATHERING
# ==============================================================================
#
# Now that we have built this function, we can use it to gather datasets that might be of interest for us.
# 
# ------------------------------------------------------------------------------
# TOURNAMENTS PRIZES
# ------------------------------------------------------------------------------
# 
# CAUTION: will take more than 412 seconds to compute, DO NOT RUN.
# To test, set "offset" to a small number (i.e. between 100 and 500)

df.tournaments <- build_df(method = methods_list$recenttournaments,
                           offset = 41200) 

# Save the resulting dataframe in a csv file to access it later in other parts of the project.

write.csv(df.tournaments, "data/EsportsEarnings/EE_tournaments.csv")

# 
# ------------------------------------------------------------------------------
# TEAM EARNINGS
# ------------------------------------------------------------------------------

# CAUTION: will take more than 12 seconds to compute

df.team_earnings <- build_df(method = methods_list$topteamsearning,
                             offset = 1200)

# Save the resulting data-frame in a csv file to access it in other parts of the project.

write.csv(df.team_earnings, "data/EsportsEarnings/EE_teams_earnings.csv")


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

df.total_earnings <- tibble("id" = gameIDs,
                            "GameName" = "",
                            "TotalUSDPrize" = 0,
                            "TotalTournaments" = 0,
                            "TotalPlayers" = 0)

temp_df <- data.frame()

# We then use our function for each game ID. This method does not require an offset since it outputs a single row per request.

# CAUTION: will take more than 480 seconds to compute.
# To test, specify a range for gameIDs in the for-loop condition (e.g. for(i in gameIDs[1:5]) )

for(i in gameIDs[1:5]) {
  
  gameIDpar <- paste(c("&gameid=", i), collapse="")
  
  temp_df <- build_df(method = methods_list$gameid,
                      parameters = gameIDpar)
  
  # print(temp_df) # Prints the output of each request, for testing purposes only.
  
  df.total_earnings[df.total_earnings$id == i, "GameName"] <- temp_df[1,"GameName"]
  df.total_earnings[df.total_earnings$id == i, "TotalUSDPrize"] <- temp_df[1,"TotalUSDPrize"]
  df.total_earnings[df.total_earnings$id == i, "TotalTournaments"] <- temp_df[1,"TotalTournaments"]
  df.total_earnings[df.total_earnings$id == i, "TotalPlayers"] <- temp_df[1,"TotalPlayers"]

}

# Save the resulting data-frame in a csv file to access it later in other parts of the project.

write.csv(df.total_earnings, "data/EsportsEarnings.com/EE_games_earnings.csv")


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