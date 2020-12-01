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
