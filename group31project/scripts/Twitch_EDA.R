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
library(quantmod) # dont
library(plotly)
library(numform)


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
  # shows values in a more aesthetic way
  mutate(Avg_concur_viewers = case_when(Avg_concur_viewers >= 1000000000 ~ f_bills(Avg_concur_viewers, prefix = "$", digits = 12),
                                                     Avg_concur_viewers >= 1000000 & Avg_concur_viewers < 1000000000 ~ f_mills(Avg_concur_viewers, prefix = "$", digits = -4),
                                                     Avg_concur_viewers < 1000000 & Avg_concur_viewers >= 1000 ~ f_thous(Avg_concur_viewers, prefix = "$", digits = 6)),
         Avg_concur_channels = case_when(Avg_concur_channels >= 1000000000 ~ f_bills(Avg_concur_channels, prefix = "$", digits = 12),
                                         Avg_concur_channels >= 1000000 & Avg_concur_channels < 1000000000 ~ f_mills(Avg_concur_channels, prefix = "$", digits = 9),
                                         Avg_concur_channels < 1000000 & Avg_concur_channels >= 1000 ~ f_thous(Avg_concur_channels, prefix = "$", digits = -2)),
         Hours_watched = case_when(Hours_watched >= 1000000000 ~ f_bills(Hours_watched, prefix = "$", digits = -7),
                                   Hours_watched >= 1000000 & Hours_watched < 1000000000 ~ f_mills(Hours_watched, prefix = "$", digits = -3),
                                   Hours_watched < 1000000 & Hours_watched >= 1000 ~ f_thous(Hours_watched, prefix = "$", digits = 6)),
         Active_streamers = case_when(Active_streamers >= 1000000000 ~ f_bills(Active_streamers, prefix = "$", digits = 12),
                                      Active_streamers >= 1000000 & Active_streamers < 1000000000 ~ f_mills(Active_streamers, prefix = "$", digits = -4),
                                      Active_streamers < 1000000 & Active_streamers >= 1000 ~ f_thous(Active_streamers, prefix = "$", digits = 6)),
         Hours_streamed = case_when(Hours_streamed >= 1000000000 ~ f_bills(Hours_streamed, prefix = "$", digits = 12),
                                    Hours_streamed >= 1000000 & Hours_streamed < 1000000000 ~ f_mills(Hours_streamed, prefix = "$", digits = -4),
                                    Hours_streamed < 1000000 & Hours_streamed >= 1000 ~ f_thous(Hours_streamed, prefix = "$", digits = 6)),
         Viewers_per_streamer = round(Viewers_per_streamer, digits = 1)) %>%
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
        y = TwitchData$Avg_concur_channels,
        height = 400) %>%
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

plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Hours_streamed) %>%
  layout(xaxis = list(title="Date"),
         title = "Total number of hours streamers")

# ------------------------------------------------------------------------------
# VIEWERS PER STREAMER ON AVERAGE
# ------------------------------------------------------------------------------

plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Viewers_per_streamer) %>%
  layout(xaxis = list(title="Date"),
         title = "Viewers per streamer on average")

# ------------------------------------------------------------------------------
# RATIO BTW WATCHED AND STREAMED
# ------------------------------------------------------------------------------

# On the same plot

plot_ly(data = TwitchData, x = ~TwitchData$Date, y = ~TwitchData$Hours_watched
        ,type = "scatter", mode = "lines", width = 800
        ,name = "Hours watched") %>%
  add_trace(x = ~TwitchData$Date, y = ~TwitchData$Hours_streamed, yaxis = "y2", name = "Hours streamed") %>%
  add_trace(x = ~TwitchData$Date, y = ~TwitchData$Viewers_per_streamer, yaxis = "y3", name = "Viewers per steamers")%>%
  layout(
    yaxis = list(
      showline = FALSE, side = "left"
      ,title = ""
      , color = "blue"
    )
    ,yaxis2 = list(
      showline = FALSE
      ,overlaying = "y"
      ,title = ""
      , anchor = "free"
      , color = "orange"
    )
    ,yaxis3 = list(
      showline = FALSE,
      side = "right",
      overlaying = "y"
      ,title = ""
      , color = "green"
    )
    ,xaxis = list(
      showline = FALSE, zeroline = FALSE, dtick = 0, title = "Date"
    )
    ,showlegend = TRUE
    ,margin = list(
      pad = 30, b = 90, l = 150, r = 90
    )
    ,legend = list(orientation = "v")
    , title = "Comparaison between watched and streamed"
  )

# With a facet_wrap

library(plotly)
fig1 <- plot_ly(TwitchData, x = ~Date, 
                y = ~Hours_watched) %>%
  add_lines(name = ~"Hours watched")

fig2 <- plot_ly(TwitchData, x = ~Date,
                y = ~Hours_streamed) %>%
        add_lines(name = ~"Hours streamed")

fig3 <- plot_ly(TwitchData, x = ~Date,
                y = ~Viewers_per_streamer) %>%
  add_lines(name = ~"Viewers per streamer")

fig <- subplot(fig1, fig2, fig3)

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
    frame = 30, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Month "
  )
)
