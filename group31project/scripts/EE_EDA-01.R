# ==============================================================================
# EDA for EsportsEarnings.com (EE) data-sets
# ==============================================================================
#
# Hello There!

library(tidyverse)
library(scales)

# General Earnings in Esports

earnings_pergame <- read.csv("data/EsportsEarnings.com/EE_games_earnings.csv")

# ------------------------------------------------------------------------------
# MOST PRIZE MONEY WON
# ------------------------------------------------------------------------------

o_prizes <- earnings_pergame %>%
  select(id, GameName, TotalUSDPrize) %>%
  filter(TotalUSDPrize > quantile(TotalUSDPrize, p=0.90, na.rm=TRUE)) %>%
  arrange(desc(TotalUSDPrize))

write.csv(o_prizes, "data/top_prizes.csv")

o_prizes %>%
  ggplot(aes(x=reorder(GameName, TotalUSDPrize), y=TotalUSDPrize/1000000)) +
  geom_bar(stat="sum") +
  scale_y_continuous(labels = comma) +
  ylab("Total Prizes (in mio of USD)") +
  coord_flip()

# ------------------------------------------------------------------------------
# MOST TOURNAMENTS ORGANIZED
# ------------------------------------------------------------------------------

o_tourn <- earnings_pergame %>%
  select(id, GameName, TotalTournaments) %>%
  arrange(desc(TotalTournaments)) %>%
  filter(TotalTournaments > quantile(TotalTournaments, p=0.90, na.rm=TRUE))

write.csv(o_tourn, "data/top_nb_tourn.csv")

o_tourn %>%
  ggplot(aes(x=reorder(GameName, TotalTournaments), y=TotalTournaments)) +
  geom_bar(stat="sum") +
  coord_flip()

# ------------------------------------------------------------------------------
# MOST PROFESSIONAL PLAYERS
# ------------------------------------------------------------------------------

o_players <- earnings_pergame %>%
  select(id, GameName, TotalPlayers) %>%
  arrange(desc(TotalPlayers)) %>%
  filter(TotalPlayers > quantile(TotalPlayers, p=0.90, na.rm=TRUE))

write.csv(o_players, "data/top_nb_players.csv")

o_players %>%
  ggplot(aes(x=reorder(GameName, TotalPlayers), y=TotalPlayers)) +
  geom_bar(stat="sum") +
  coord_flip()
