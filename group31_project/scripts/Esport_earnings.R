# ==============================================================================
# EDA for EsportsEarnings.com (EE) data-sets
# ==============================================================================
#
# Hi again!

library(tidyverse)
library(scales)
library(lubridate)
library(plotly)

source(here::here("scripts/setup.R"))

# ==============================================================================
# Import data sources
# ==============================================================================

tournaments <- read_csv(file = here::here("data/EsportsEarnings.com/EE_tournaments.csv"))

# ==============================================================================

games <- read_csv(file = here::here("data/EsportsEarnings.com/EE_games_earnings.csv")) %>%
  rename(GameId = id) # We add this to harmonize variables name across datasets

# ==============================================================================

teams <- read_csv(file = here::here("data/EsportsEarnings.com/EE_teams_earnings.csv"))


# ==============================================================================
# Data set tidying for analysis of "tops"
# ==============================================================================

o_tourn <- games %>%
  select(GameId, GameName, TotalTournaments) %>%
  arrange(desc(TotalTournaments)) %>%
  filter(TotalTournaments > quantile(TotalTournaments, p=0.95, na.rm=TRUE))

# ==============================================================================

o_prizes <- games %>%
  select(GameId, GameName, TotalUSDPrize) %>%
  filter(TotalUSDPrize > quantile(TotalUSDPrize, p=0.95, na.rm=TRUE)) %>%
  arrange(desc(TotalUSDPrize))

# ==============================================================================

o_players <- games %>%
  select(GameId, GameName, TotalPlayers) %>%
  arrange(desc(TotalPlayers)) %>%
  filter(TotalPlayers > quantile(TotalPlayers, p=0.95, na.rm=TRUE))


# ==============================================================================
# EDA Plots
# ==============================================================================

rankg_by_tournnb <- o_tourn %>%
  ggplot(aes(x=(GameName %>% 
                  reorder(TotalTournaments)), 
             y=TotalTournaments, 
             fill = TotalTournaments)) +
  geom_bar(stat="sum", show.legend = FALSE) +
  xlab("") +
  ylab("Number of tournaments") +
  coord_flip()

# Wrap with ggplotly to make tooltips on hover

rankg_by_tournnb <- ggplotly(rankg_by_tournnb,
                             height = 600,
                             tooltip = "y")

# ==============================================================================

rankg_by_prizes <- o_prizes %>%
  ggplot(aes(x=reorder(GameName, TotalUSDPrize), y=TotalUSDPrize, fill = TotalUSDPrize)) +
  geom_bar(stat="sum", show.legend = FALSE) +
  ylab("Total Prizes (in USD)") +
  xlab("") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  coord_flip()

# Wrap with ggplotly to make tooltips on hover

rankg_by_prizes <- ggplotly(rankg_by_prizes,
                            height = 600,
                            tooltip = "y")

# ==============================================================================

rankg_by_playernb <- o_players %>%
  ggplot(aes(x=reorder(GameName, TotalPlayers), y=TotalPlayers, fill = TotalPlayers)) +
  geom_bar(stat="sum", show.legend = FALSE) +
  xlab("") +
  ylab("Number of competing players") +
  coord_flip()

# Wrap with ggplotly to make tooltips on hover

rankg_by_playernb <- ggplotly(rankg_by_playernb,
                              height = 600,
                              tooltip = "y")