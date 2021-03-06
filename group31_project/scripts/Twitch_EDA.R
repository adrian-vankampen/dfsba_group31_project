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
library(dygraphs)

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

# Summary

summary(TwitchData)

# NA's overview
freq.na(TwitchData)

# ------------------------------------------------------------------------------

# Visualization

# Select bottom with shiny

library(shiny)

shiny_twitch <- TwitchData %>%
  pivot_longer(c(Avg_concur_viewers, Avg_concur_channels, Hours_watched, Active_streamers, Hours_streamed, Viewers_per_streamer),
               names_to = "Variable",
               values_to = "Value")

ui<- fluidPage(
  titlePanel("Interactive plot for the Twitch statistics"),
  selectInput(inputId = "Variable",
              label="Choose a Variable",
              choices = c("Avg_concur_viewers", "Avg_concur_viewers","Hours_watched", "Active_streamers", "Hours_streamed", "Viewers_per_streamer")),
  mainPanel(
    h1("Statistics over time"),
    textOutput("Selected_variable"),
    plotlyOutput(outputId ="PlotlyVariable")
  ))

server <- function(input,output)
{
  data <- reactive({
    shiny_twitch %>% filter(Variable == input$Variable)})
  
  output$Selected_variable <- renderUI({
    paste("You have selected", input$Variable)
  })
  output$PlotlyVariable <- renderPlotly({
    plot_ly(data(), x = ~Date, y = ~Value ,type = "bar", width = 600
            ,name = "Number", height = 400) %>% 
      layout(
        yaxis = list(showline = FALSE, side = "right" ,title = "", color = "blue")
        ,xaxis = list( showline = FALSE, zeroline = FALSE, dtick = 0, title = "Date")
        ,showlegend = TRUE ,margin = list(pad = 30, b = 30, l = 150, r = 30)
        ,legend = list(orientation = "bottomright"))
    
  })
}

shinyApp(ui, server)


# Adrian

library(shiny)

shiny_twitch <- TwitchData %>%
  pivot_longer(c(Avg_concur_viewers, Avg_concur_channels, Hours_watched, Active_streamers, Hours_streamed, Viewers_per_streamer),
               names_to = "Variable",
               values_to = "Value")

inputPanel(
  selectInput(
    "Variable",
    label = "Choose a Variable :",
    choices = unique(shiny_twitch$Variable),
    selected = shiny_twitch$Variable[1]
  )
)

renderDygraph({
  
  plot_data <- shiny_twitch %>%
    filter(Variable == shiny_twitch$Variable[siny_twitch$Variable == input$GameNameBoth]) %>%
    group_by(Month_Yr = floor_date(EndDate, "month")) %>%
    summarize(prize = sum(TotalUSDPrize),
              tournb = n())
  
  prize_cash_xts <- xts(plot_data$prize, 
                        order.by = plot_data$Month_Yr)
  tourn_nb_xts <- xts(plot_data$tournb, 
                      order.by = plot_data$Month_Yr)
  
  both <- cbind(prize_cash_xts, tourn_nb_xts)
  dygraph(both) %>%
    dyRangeSelector() %>%
    dyAxis("y2", label = "Number of tournaments", independentTicks = TRUE) %>%
    dySeries("prize_cash_xts", label = "Prize Cash (in USD)") %>%
    dySeries("tourn_nb_xts", label = "Number of tournaments", axis = "y2") %>%
    dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyLegend(width = 500) %>%
    dyOptions(labelsKMB = TRUE)
})

server <- function(input,output)
{
  data <- reactive({
    shiny_twitch %>% filter(Variable == input$Variable)})
  
  output$Selected_variable <- renderUI({
    paste("You have selected", input$Variable)
  })
  output$PlotlyVariable <- renderPlotly({
    plot_ly(data(), x = ~Date, y = ~Value ,type = "bar", width = 600
            ,name = "Number", height = 400) %>% 
      layout(
        yaxis = list(showline = FALSE, side = "right" ,title = "", color = "blue")
        ,xaxis = list( showline = FALSE, zeroline = FALSE, dtick = 0, title = "Date")
        ,showlegend = TRUE ,margin = list(pad = 30, b = 30, l = 150, r = 30)
        ,legend = list(orientation = "bottomright"))
    
  })
}

# ------------------------------------------------------------------------------
# AVERAGE CONCURENT VIEWERS
# ------------------------------------------------------------------------------

aaaa <- plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Avg_concur_viewers,
        height = 400) %>%
  layout(xaxis = list(title="Date"),
         title = "Average concurent viewers")

# ------------------------------------------------------------------------------
# AVERAGE CONCURENT CHANNELS
# ------------------------------------------------------------------------------

b <- plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Avg_concur_channels,
        height = 400) %>%
  layout(xaxis = list(title="Date"),
         title = "Average concurent channels")

# ------------------------------------------------------------------------------
# TOTAL NUMBER OF HOURS WATCHED
# ------------------------------------------------------------------------------

c <- plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Hours_watched,
        height = 400) %>%
  layout(xaxis = list(title="Date"),
         title = "Total Hours watched")

# ------------------------------------------------------------------------------
# TOTAL NUMBER OF ACTIVE STREAMERS
# ------------------------------------------------------------------------------

d <- plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Active_streamers,
        height = 400) %>%
  layout(xaxis = list(title="Date"),
         title = "Total number of active streamers")

# ------------------------------------------------------------------------------
# TOTAL NUMBER OF HOURS STREAMED
# ------------------------------------------------------------------------------

e <- plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Hours_streamed,
        height = 400) %>%
  layout(xaxis = list(title="Date"),
         title = "Total number of hours streamed")

# ------------------------------------------------------------------------------
# VIEWERS PER STREAMER ON AVERAGE
# ------------------------------------------------------------------------------

f <- plot_ly(type = "bar",
        x = TwitchData$Date, 
        y = TwitchData$Viewers_per_streamer,
        height = 400) %>%
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

# shiny's substitute

watched_xts <- xts(TwitchData$Hours_watched, order.by = TwitchData$Date)
streamed_xts <- xts(TwitchData$Hours_streamed, order.by = TwitchData$Date)
viewers_xts <- xts(TwitchData$Viewers_per_streamer, order.by = TwitchData$Date)

all <- cbind(watched_xts, streamed_xts, viewers_xts)

dygraph(all, height = 300) %>%
  dyRangeSelector() %>%
  dySeries("watched_xts", axis = "y") %>%
  dySeries("streamed_xts", axis = "y") %>%
  dySeries("viewers_xts", axis = "y2") %>%
  dyAxis("y2", label = "Viewers per stream", independentTicks = TRUE) %>%
  dyLegend(show = "follow") %>%
  dyOptions(labelsKMB = TRUE, logscale = TRUE)
