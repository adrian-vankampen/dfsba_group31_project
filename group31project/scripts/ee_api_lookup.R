# This script pulls data from www.esportsearnings.com through API requests (hopefully).
#
# API requests are of the following form:
# http://api.esportsearnings.com/v<versionnumber>/<methodname>?apikey=<apikey>&format=<format>
#
# The site's API is in beta, hence the version is 0

versionnumber <- "0"

# account : adrian1811
# API key : 377aeceec830e58cdce6d6f09a365a6c2e438a01160f1ac4a85b1272106dcea7

# Methods indicate which type of data we wish to pull. This can be any of the 12 listed in methods_list hereunder.
#
# See https://www.esportsearnings.com/apidocs for documentation on how to use them.
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


# Finally, format can be any of json (by default), xml or csv. For our purposes, we will prefer csv.

format <- "csv"

# From those elements, we can build our API query in the form of a list first :


# Function that builds a data-frame by iterating queries done through the API.

build_full_df_by_API <- function(method, 
                             apikey="377aeceec830e58cdce6d6f09a365a6c2e438a01160f1ac4a85b1272106dcea7",
                             format="csv",
                             maxoffset=100,
                             parameters="") # Additional method-specific parameters
  {

  api_list <- list("domain" = "http://api.esportsearnings.com/v",
                   "versionnumber" = versionnumber,
                   "methodsep" = "/",
                   "methodname" = method,
                   "apikeysep" = "?apikey=",
                   "apikey" = apikey,
                   "formatsep" = "&format=",
                   "format" = format,
                   "parameters" = parameters) # Additional method-specific parameters, see documentation.
  
  # Pre-assign the output to the first query (100 observations)
  url <- paste(api_list, collapse="")
  df <- read.csv(url)
  
  # Iterate queries until the maxoffset set by the function call (default 100)
  
  for (offset in seq(from = 100, to = maxoffset, by = 100)) {
    url <- paste(c(url,
                   "&offset=", 
                   as.character(offset)), 
                 collapse = "")
    df.next <- try(read.csv(url),
                   silent=TRUE)
    if (inherits(df.next, "try-error")) break
    
    df <- rbind(df, df.next)
    Sys.sleep(1.5) # We can only request an API once a second (due website's limitations)
  }
  
  return(df)
}
  

# 
# ### Teams earnings overall
# 
# # CAUTION: will take more than 12 seconds to compute
# 
# # df.team_earnings <- build_full_df_by_API(method = methods_list$topteamsearning,
#                                          maxoffset = 1200)
# 
# # write.csv(df.team_earnings, "data/EE_teams_earnings.csv")
# 
# 
# ### Tournaments prize cash overall
# 
# # CAUTION: will take more than 412 seconds to compute
# 
# # df.tournaments <- build_full_df_by_API(method = methods_list$recenttournaments,
#                                        maxoffset = 41200) 
# 
# # write.csv(df.tournaments, "data/EE_tournaments.csv")
# 
# 
# 
# 
# ### Team earnings for League of Legends
# 
# # CAUTION: will take more than 3 seconds to compute
# 
# df.lol_team_earnings <- build_full_df_by_API(method = methods_list$topteamsearn_bygame,
#                                          maxoffset = 300,
#                                          parameters = "&gameid=164")
# 
# write.csv(df.lol_team_earnings, "data/EE_lol_teams_earnings.csv")
# 
# 
# ### Team earnings for Dota 2
# 
# # CAUTION: will take more than 3 seconds to compute
# 
# df.dota2_team_earnings <- build_full_df_by_API(method = methods_list$topteamsearn_bygame,
#                                              maxoffset = 100,
#                                              parameters = "&gameid=231")
# 
# write.csv(df.dota2_team_earnings, "data/EE_dota2_teams_earnings.csv")
# 
#
# Find all charts about team earnings by game
#



#############################################
########### TEST AREA #######################
#############################################
# Trying team earnings for League of Legends#


versionnumber <- "0"
method <- methods_list$topteamsearn_bygame
apikey <- "377aeceec830e58cdce6d6f09a365a6c2e438a01160f1ac4a85b1272106dcea7"
format <- "csv"

api_list_lol <- list("domain" = "http://api.esportsearnings.com/v",
                 "versionnumber" = versionnumber,
                 "methodsep" = "/",
                 "methodname" = method,
                 "apikeysep" = "?apikey=",
                 "apikey" = apikey,
                 "formatsep" = "&format=",
                 "format" = format,
                 "parameters" = "") # Additional method-specific parameters, see documentation.


# Pre-assign the output to the first query (100 observations)

games <- read.csv("data/EE_tournaments.csv") %>%
  distinct(GameId)

for(g in games$GameId[1:10]){
  api_list_lol$parameters <- paste(c("&gameid=", g), collapse="")
  df.temp <- build_full_df_by_API(method = api_list_lol$methodname, maxoffset=1000, parameters = api_list_lol$parameters)
  write.csv(df.temp, paste(c("data/EE_", g, "_teams_earnings.csv"), collapse=""))
  Sys.sleep(1.5)
}

  df6 <- build_full_df_by_API(method = api_list_lol$methodname, maxoffset=1000, parameters = api_list_lol$parameters)

