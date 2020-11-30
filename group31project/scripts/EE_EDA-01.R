# ==============================================================================
# EDA for EsportsEarnings.com (EE) data-sets
# ==============================================================================
#
# Hello There!

library(tidyverse)
library(scales)

setwd("C:/Users/adria/Desktop/projects/dfsba_group31_project/group31project")

# General Earnings in Esports

earnings_pergame <- read.csv("data/EsportsEarnings.com/EE_games_earnings.csv")

# ------------------------------------------------------------------------------

o_prizes <- earnings_pergame %>%
  select(GameName, TotalUSDPrize) %>%
  arrange(TotalUSDPrize, desc=TRUE) %>%
  filter(TotalUSDPrize > quantile(TotalUSDPrize, p=0.95, na.rm=TRUE))

o_prizes %>%
  ggplot(aes(x=reorder(GameName, TotalUSDPrize), y=TotalUSDPrize/1000000)) +
  geom_bar(stat="sum") +
  scale_y_continuous(labels = comma) +
  ylab("Total Prizes (in mio of USD)") +
  coord_flip()

# ------------------------------------------------------------------------------

o_tourn <- earnings_pergame %>%
  select(GameName, TotalTournaments) %>%
  arrange(TotalTournaments, desc=TRUE) %>%
  filter(TotalTournaments > quantile(TotalTournaments, p=0.95, na.rm=TRUE))

o_tourn %>%
  ggplot(aes(x=reorder(GameName, TotalTournaments), y=TotalTournaments)) +
  geom_bar(stat="sum") +
  coord_flip()

# ------------------------------------------------------------------------------

o_players <- earnings_pergame %>%
  select(GameName, TotalPlayers) %>%
  arrange(TotalPlayers, desc=TRUE) %>%
  filter(TotalPlayers > quantile(TotalPlayers, p=0.95, na.rm=TRUE))

o_players %>%
  ggplot(aes(x=reorder(GameName, TotalPlayers), y=TotalPlayers)) +
  geom_bar(stat="sum") +
  coord_flip()
