# ==============================================================================
# EDA for twitchtracker.com (Twitch) data-set
# ==============================================================================
#
# This script aims to clean the Twitch data-set and give an overview of it.

library(tidyverse)
library(lubridate)
library(readr)
library(kableExtra)
library(naniar)
library(questionr)
library(quantmod)
library(plotly)


# General Statistics of the Twitch platform

data1 <- read_delim(file = here::here("data/Twitch.csv"), ";", 
                    escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# ------------------------------------------------------------------------------

# Data cleaning

TwitchData <- data1 %>%
  # set a name for each variable
  rename(Date = X1, Avg_concur_viewers = X2, 
         Avg_concur_channels = X3, Hours_watched = X4, 
         Active_streamers = X5, Hours_streamed = X6) %>%
  # modify the format of certain variable
  mutate(Date = mdy(Date), 
         Hours_watched = parse_number(Hours_watched)) %>%
  # Replace value with NA (in our dataframe, n/a are considered as character)
  replace_with_na(replace = list(Active_streamers = "n/a")) %>%
  mutate(Viewers_per_streamer = Hours_watched*1000000/Hours_streamed) %>%
  # Convert the column from character to double
  mutate(Active_streamers = parse_number(Active_streamers))

# ------------------------------------------------------------------------------

# Data overview

TwitchData %>% 
  kbl(caption = "Twitch statistics") %>%  
  kable_paper(full_width = F) %>% 
  scroll_box(width = "100%", height = "300px")

# NA's overview
freq.na(TwitchData)

# ------------------------------------------------------------------------------

# Visualization

# ------------------------------------------------------------------------------
# AVERAGE CONCURENT VIEWERS
# ------------------------------------------------------------------------------

TwitchData %>% 
    ggplot(aes(x = Date, y = Avg_concur_viewers)) +
    geom_col() +
    ylab("Average concurent viewers")

# ------------------------------------------------------------------------------
# AVERAGE CONCURENT CHANNELS
# ------------------------------------------------------------------------------

TwitchData %>% 
    ggplot(aes(x = Date, y = Avg_concur_channels)) +
    geom_col() +
    ylab("Average concurent channels")

# ------------------------------------------------------------------------------
# TOTAL NUMBER OF HOURS WATCHED
# ------------------------------------------------------------------------------

TwitchData %>% 
    ggplot(aes(x = Date, y = Hours_watched)) +
    geom_col() +
    ylab("Hours watched")

# ------------------------------------------------------------------------------
# TOTAL NUMBER OF ACTIVE STREAMERS
# ------------------------------------------------------------------------------

TwitchData %>% 
  ggplot(aes(x = Date, y = Active_streamers)) +
  geom_col(na.rm = TRUE) +
  ylab("Active streamers")

# ------------------------------------------------------------------------------
# TOTAL NUMBER OF HOURS STREAMED
# ------------------------------------------------------------------------------

TwitchData %>% 
  ggplot(aes(x = Date, y = Hours_streamed)) +
  geom_col() +
  ylab("Hours streamed")

# ------------------------------------------------------------------------------
# VIEWERS PER STREAMER ON AVERAGE
# ------------------------------------------------------------------------------

TwitchData %>% 
  ggplot(aes(x = Date, y = Viewers_per_streamer)) +
  geom_col(mapping = NULL) +
  ylab("Average concurent viewers")

# ------------------------------------------------------------------------------

bf <- TwitchData %>% separate(Date, into = c("year", "month", "day")) %>%
  mutate(day = NULL) %>% 
  unite(Date, year, month, sep = "-") %>% 
  arrange(Date) %>% mutate(ID = 1:99)
bf$ID <- seq.int(nrow(bf))


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

bf <- bf %>%
  accumulate_by(~ID)

p <- ggplot(bf, aes(ID, Hours_streamed, frame = frame)) +
  geom_line()

fig <- ggplotly(p, height = 400) %>%
  layout(
    title = "Hours watched",
    yaxis = list(
      title = "Total ammount",
      zeroline = F,
      tickprefix = "$"
    ),
    xaxis = list(
      title = "Month",
      zeroline = F, 
      showgrid = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Month "
    )
  )

fig

