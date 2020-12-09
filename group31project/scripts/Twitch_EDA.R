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
         Hours_watched = parse_number(Hours_watched)*1000000) %>%
  # Replace value with NA (in our dataframe, n/a are considered as character)
  replace_with_na(replace = list(Active_streamers = "n/a")) %>%
  mutate(Viewers_per_streamer = Hours_watched/Hours_streamed) %>%
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

plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Avg_concur_viewers) %>%
  layout(xaxis = list(title="Date"),
         title = "Average concurent viewers")

# ------------------------------------------------------------------------------
# AVERAGE CONCURENT CHANNELS
# ------------------------------------------------------------------------------

plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Avg_concur_channels) %>%
  layout(xaxis = list(title="Date"),
         title = "Average concurent channels")

# ------------------------------------------------------------------------------
# TOTAL NUMBER OF HOURS WATCHED
# ------------------------------------------------------------------------------

plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Hours_watched) %>%
  layout(xaxis = list(title="Date"),
         title = "Total Hours watched")

# ------------------------------------------------------------------------------
# TOTAL NUMBER OF ACTIVE STREAMERS
# ------------------------------------------------------------------------------

plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Active_streamers) %>%
  layout(xaxis = list(title="Date"),
         title = "Total number of active streamers")

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

fig <- plot_ly(TwitchData, x = ~TwitchData$Date, y = ~TwitchData$Hours_watched, name = 'Hours watched', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = ~TwitchData$Hours_streamed, name = 'Hours streamed', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = ~TwitchData$Viewers_per_streamer, name = 'Viewers per streamer', type = 'scatter', mode = 'lines')

fig

# ------------------------------------------------------------------------------

# Dynamic plot

# Create function to set the frame 

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


# Arrange the actual data-set

bf <- TwitchData %>% separate(Date, into = c("year", "month", "day")) %>%
  mutate(day = NULL) %>% 
  unite(Date, year, month, sep = "-") %>% 
  arrange(Date) %>% mutate(ID = 1:99) %>% 
  accumulate_by(~ID)

# Build the dynamic plot
ggplotly(ggplot(bf, aes(ID, Hours_streamed, frame = frame)) +
                  geom_line(), height = 400) %>%
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
    frame = 20, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Month "
  )
)