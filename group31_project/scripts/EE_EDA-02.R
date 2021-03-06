# ==============================================================================
# EDA for EsportsEarnings.com (EE) data-sets
# ==============================================================================
#
# Hi again!

library(tidyverse)
library(scales)
library(lubridate)
library(plotly)

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


tourn_bygame <- tournaments1 %>%
  filter(GameId %in% o_tourn$GameId) %>%
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
  summarize(prize = sum(TotalUSDPrize)) %>%
  mutate(GameId = factor(GameId, 
                          levels = games$GameId,
                          labels = games$GameName))
  # mutate(GameName = games$GameName[games$GameId == tourn_bygame2$GameId])


game_ev_prize <- tourn_bygame2 %>%
  ggplot(aes(x=Month_Yr, y=prize/1000000)) +
  geom_line() +
  xlab("") +
  ylab("Prizes per month (in mio USD)") +
  facet_wrap(tourn_bygame2$GameId, labeller = labeller(game.labels))

ggplotly(game_ev_prize)

game_ev_prize

fig_ev_prize <- plot_ly(
  type = "bar",
  x = tourn_bygame2$Month_Yr,
  y = tourn_bygame2$prize/1000000,
  text = tourn_bygame2$GameId,
  hoverinfo = "text"
)

fig_ev_prize

# ==============================================================================