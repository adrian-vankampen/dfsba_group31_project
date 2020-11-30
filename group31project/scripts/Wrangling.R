library(tidyverse)
library(lubridate)
library(readr)

options(scipen = 999) # remove the scientific notation

setwd("~/GitHub/dfsba_group31_project/group31project/data")

#https://twitchtracker.com/statistics

data1 <- read_delim("Twitch.csv", ";", escape_double = FALSE, 
                   col_names = FALSE, trim_ws = TRUE)

TwitchData <- data1 %>% 
  rename(date = X1, "Avg. concur. viewers" = X2, "Avg. concur. channels" = X3, Time_watched = X4, "Active streamers" = X5, Hours_streamed = X6) %>% 
  mutate(date = mdy(date), Time_watched = parse_number(Time_watched)) %>%
  mutate(day = NULL, "Viewers per streamer" = Time_watched*1000000/Hours_streamed)

TwitchData4 <- data1 %>% 
  rename(date = X1, Avg_concur_viewers = X2, Avg_concur_channels = X3, Hours_watched = X4, Active_streamers = X5, Hours_streamed = X6) %>% 
  mutate(date = mdy(date), Hours_watched = parse_number(Hours_watched))%>%
  mutate(Viewers_per_streamer = Hours_watched*1000000/Hours_streamed)

(vizu_1_Hours_watched <- TwitchData4 %>% 
                        ggplot(aes(x = date, y = Hours_watched)) +
                        geom_col())

(vizu_1_Avg_concur_viewers <- TwitchData4 %>% 
  ggplot(aes(x = date, y = Avg_concur_viewers)) +
  geom_col())

(vizu_1_Avg_concur_channels <- TwitchData4 %>% 
  ggplot(aes(x = date, y = Avg_concur_channels)) +
  geom_col())

(vizu_1_Active_streamers <- TwitchData4 %>% 
  ggplot(aes(x = date, y = Active_streamers)) +
  geom_col())

(vizu_1_Hours_streamed <- TwitchData4 %>% 
  ggplot(aes(x = date, y = Hours_streamed)) +
  geom_col())

(vizu_1_Viewers_per_streamer <- TwitchData4 %>% 
  ggplot(aes(x = date, y = Viewers_per_streamer)) +
  geom_col())

#https://platform.newzoo.com/companies/public-revenues

data2 <- read_delim("NZ_CompaniesPublicRevenues2.csv", ";", escape_double = FALSE,
                    col_names = FALSE, trim_ws = TRUE)

CompaniesPublicRevenues <- data2 %>%
  rename("Company name" = X1, "Region of HQ" = X2, Q1 = X3, Q2 = X4, Q3 = X5, Q4 = X6, Q1b = X7, Q2b = X8) %>%
  mutate(Q1 = parse_number(Q1), Q2 = parse_number(Q2), 
         Q3 = parse_number(Q3), Q4 = parse_number(Q4), 
         Q1b = parse_number(Q1b), Q2b = parse_number(Q2b)) %>%
  mutate(Q1 = ifelse(Q1 < 10 , Q1*1000, Q1), 
         Q2 = ifelse(Q2 < 10 , Q2*1000, Q2), 
         Q3 = ifelse(Q3 < 10 , Q3*1000, Q3), 
         Q4 = ifelse(Q4 < 10 , Q4*1000, Q4),
         Q1b = ifelse(Q1b < 10 , Q1b*1000, Q1b),
         Q2b = ifelse(Q2b < 10 , Q2b*1000, Q2b)) %>%
  mutate(First_semester_of_2019 = Q1 + Q2, 
         Second_semester_of_2019 = Q3 + Q4, 
         Total_in_2019 = Q1 + Q2 + Q3 + Q4, 
         First_semester_in_2020 = Q1b + Q2b) %>%
  mutate(Q1_grate = 100*(Q1b - Q1)/Q1, 
         Q2_grate = 100*(Q2b - Q2)/Q2, 
         Semester2_grate = 100*(Second_semester_of_2019 - First_semester_of_2019)/First_semester_of_2019,
         Semester3_grate = 100*(First_semester_in_2020 - Second_semester_of_2019)/Second_semester_of_2019,
         First_semester_grate = 100*(First_semester_in_2020 - First_semester_of_2019)/First_semester_of_2019)

#https://platform.newzoo.com/companies/investments

data3 <- read_delim("NZ_CompaniesInvestmentscsv1.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

data4 <- read_delim("NZ_CompaniesInvestmentscsv2.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

CompaniesInvestments <- data3 %>% full_join(data4) %>%
  rename(Date = X1, "Investment type" = X2, Sectors = X3, Emptycolumn = X4, Investee = X5, Amount = X6) %>%
  separate(Date, into = c("Day", "Month", "Year")) %>%
  mutate(Emptycolumn = NULL, Sectors = str_replace_all(Sectors, ",.", ", "))

mutate(Emptycolumn = NULL, 
         Amount = parse_number(str_replace_all(Amount, c("K" = "000", "M" = "0000", "\\$" = ""))),
         Sectors = str_replace_all(Sectors, ",.", ", "))

#kaggle
data5 <- read.csv("Esport_Earnings.csv")
data6 <- read.csv("GeneralEsportData.csv")
data7 <- read.csv("HistoricalEsportData.csv")
data8 <- read.csv("Managerial_and_Decision_Economics_2013_Video_Games_Dataset.csv")
