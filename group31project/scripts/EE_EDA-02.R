# ==============================================================================
# EDA for EsportsEarnings.com (EE) data-sets
# ==============================================================================
#
# Hi again!

library(tidyverse)
library(scales)
library(lubridate)

# We would like to see the evolution of tournament prize pools in time.

tournaments <- read.csv("data/EsportsEarnings.com/EE_tournaments.csv")

games <- read.csv("data/EsportsEarnings.com/EE_games_earnings.csv") %>%
  rename(GameId = id)

# To improve the understanding of our tournaments data, let's display the name of the game

tournaments1 <- tournaments %>%
  left_join(games, by = "GameId") %>%
  rename(TotalUSDPrize = TotalUSDPrize.x, X = X.x) %>%
  select(X:GameId,
         GameName,
         TournamentName:TotalUSDPrize) %>%
  mutate(StartDate = as_date(StartDate), EndDate = as_date(EndDate))

monthly_tourn <- tournaments1 %>%
  group_by(Month_Yr = floor_date(EndDate, "month")) %>%
  summarize(number = n(),
            totalPrize = sum(TotalUSDPrize))

monthly_tourn

tourn_nb <- monthly_tourn %>% ggplot(aes(x = Month_Yr)) +
  geom_line(aes(y=number)) +
  ylab("Number of tournaments per month") +
  xlab("Month")

tourn_nb

tourn_money <- monthly_tourn %>% 
  ggplot(aes(x = Month_Yr)) +
  geom_point(aes(y=totalPrize)) +
  ylab("Total prizes won") +
  xlab("Month")

tourn_money



# ==============================================================================


o_tourn <- games %>%
  select(GameId, GameName, TotalTournaments) %>%
  arrange(desc(TotalTournaments)) %>%
  filter(TotalTournaments > quantile(TotalTournaments, p=0.90, na.rm=TRUE))

facetnb <- 4

tourn_bygame <- tournaments1 %>%
  filter(GameId %in% o_tourn$GameId[1:facetnb]) %>%
  group_by(Month_Yr = floor_date(EndDate, "month"), GameId) %>%
  summarize(number= n())

game_ev_tourn <- tourn_bygame %>%
  ggplot(aes(x=Month_Yr, y=number)) +
  geom_line() +
  xlab("") +
  ylab("Tournaments per month") +
  facet_wrap(tourn_bygame$GameId)

game_ev_tourn

# ==============================================================================

o_prizes <- games %>%
  select(GameId, GameName, TotalUSDPrize) %>%
  filter(TotalUSDPrize > quantile(TotalUSDPrize, p=0.90, na.rm=TRUE)) %>%
  arrange(desc(TotalUSDPrize))

tourn_bygame2 <- tournaments1 %>%
  filter(GameId %in% o_prizes$GameId[1:9]) %>%
  group_by(Month_Yr = floor_date(EndDate, "month"), GameId) %>%
  summarize(prize = sum(TotalUSDPrize))

game_ev_prize <- tourn_bygame2 %>%
  ggplot(aes(x=Month_Yr, y=prize/1000000)) +
  geom_line() +
  xlab("") +
  ylab("Prizes per month (in mio USD)") +
  facet_trelliscope(tourn_bygame2$GameId, 
                    as_plotly = TRUE)

game_ev_prize

# ==============================================================================
#
# We would like to see the evolution of players' participation in tournaments.
# This information is trickier to find though. For that, we could lookup a selection of tournaments and count the number of players

print(games$GameName[games$GameId %in% tourn_bygame2$GameId])