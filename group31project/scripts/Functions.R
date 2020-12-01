# ==============================================================================
# FUNCTION USED TO GATHER DATA FROM API
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
# FUNCTION USED MAKE NICE NAMES
# ==============================================================================
#
# This function makes nicer names, for instance for writing multiple csv files.

make_nicer_name <- function(sample){
  nice_name <- sample %>% 
    gsub(" ", "", .) %>% # Remove white-spaces
    gsub("[^A-Za-z0-9]", "_", .) # Remove special characters and replace with an underscore
  
  return(nice_name)
}