# ==============================================================================
# EDA for platform.newzoo.com (NZ) data-sets
# ==============================================================================
#
# This script aims to clean the NZ data-set and give an overview of it.
# It is composed of two data-sets : one regarding the income and the other about investment.

library(tidyverse)
library(lubridate)
library(readr)
library(kableExtra)
library(plotly)
library(numform)


# Income of the 25 largest video game companies

data1 <- read_delim(file = here::here("data/NZ_CompaniesPublicRevenues2.csv"), ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# ------------------------------------------------------------------------------

# Data cleaning

options(scipen = 999)

# Creating a numeric vector for X3 to X8 in data1

# X3 as numeric

numeric_Q1 <- c()
for(i in seq_along(data1$X3)){
  x <- parse_number(data1$X3[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X3[i], -1 , -1), 'KMB'))))
  numeric_Q1 <- c(numeric_Q1, x)
}

# X4 as numeric

numeric_Q2 <- c()
for(i in seq_along(data1$X4)){
  x <- parse_number(data1$X4[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X4[i], -1 , -1), 'KMB'))))
  numeric_Q2 <- c(numeric_Q2, x)
}

# X5 as numeric

numeric_Q3 <- c()
for(i in seq_along(data1$X5)){
  x <- parse_number(data1$X5[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X5[i], -1 , -1), 'KMB'))))
  numeric_Q3 <- c(numeric_Q3, x)
}

# X6 as numeric

numeric_Q4 <- c()
for(i in seq_along(data1$X6)){
  x <- parse_number(data1$X6[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X6[i], -1 , -1), 'KMB'))))
  numeric_Q4 <- c(numeric_Q4, x)
}

# X7 as numeric

numeric_Q1_2020 <- c()
for(i in seq_along(data1$X7)){
  x <- parse_number(data1$X7[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X7[i], -1 , -1), 'KMB'))))
  numeric_Q1_2020 <- c(numeric_Q1_2020, x)
}

# X8 as numeric

numeric_Q2_2020 <- c()
for(i in seq_along(data1$X8)){
  x <- parse_number(data1$X8[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X8[i], -1 , -1), 'KMB'))))
  numeric_Q2_2020 <- c(numeric_Q2_2020, x)
}

# Data-set for computation 

CompRev <- data1 %>%
  # set a name for each variable
  rename(Company_name = X1, Region_of_HQ = X2, Q1 = X3, 
         Q2 = X4, Q3 = X5, Q4 = X6, 
         Q1_2020 = X7, Q2_2020 = X8) %>%
  # Replacing the data by its numeric value
  mutate(Q1 = numeric_Q1, Q2 = numeric_Q2,
         Q3 = numeric_Q3, Q4 = numeric_Q4,
         Q1_2020 = numeric_Q1_2020, Q2_2020 = numeric_Q2_2020) %>%
  # compute total and growth
  mutate(Total_2019 = Q1 + Q2 + Q3 + Q4, .after = Q4) %>%
  mutate(Half_2020 = Q1_2020 + Q2_2020, .after = Q2_2020) %>%
  mutate(Q1_grate = 100*(Q1_2020 - Q1)/Q1, .after = Q1_2020) %>% 
  mutate(Q2_grate = 100*(Q2_2020 - Q2)/Q2, .after = Q2_2020)

# Data-set to display (more aesthetic)

CompRev2 <- CompRev %>%
  mutate(Q1 = data1$X3, Q2 = data1$X4, Q3 = data1$X5,
         Q4 = data1$X6, Q1_2020 = data1$X5, Q2_2020 = data1$X6,
         Total_2019 = ifelse(Total_2019 >= 1000000000, 
                             f_bills(Total_2019, prefix = "$", digits = 12), 
                             f_mills(Total_2019, prefix = "$", digits = 9)),
         Half_2020 = ifelse(Half_2020 >= 1000000000, 
                            f_bills(Half_2020, prefix = "$", digits = 12), 
                            f_mills(Half_2020, prefix = "$", digits = 9)),
         Q1_grate = paste0(round(Q1_grate, digits = 2), "%"), 
         Q2_grate = paste0(round(Q2_grate, digits = 2), "%"))

# ------------------------------------------------------------------------------

# Data overview

CompRev2 %>%
  rename("Q1 " = Q1_2020, "Q2 " = Q2_2020, "Full year" = Total_2019, 
         "YoY growth" = Q1_grate,"Half year " = Half_2020, 
         "YoY growth " = Q2_grate) %>%
  kbl(caption = "Game revenu per company") %>%
  kable_paper(full_width = F) %>% 
  add_header_above(c(" " = 2, "2019" = 5, "2020" = 5)) %>%
  scroll_box(width = "100%", height = "300px")

# ------------------------------------------------------------------------------

# Visualization

# ------------------------------------------------------------------------------
# Total income in 2019 per company
# ------------------------------------------------------------------------------

# With different colors

# Set the factor to display the values sorted 

plot_CompRev <- CompRev %>%
  mutate(Company_name = factor(CompRev$Company_name, 
                               levels = unique(CompRev$Company_name)[order(CompRev$Total_2019, decreasing = FALSE)]))

plot_ly(data = plot_CompRev,
        x = plot_CompRev$Total_2019,
        y = ~plot_CompRev$Company_name,
        color = ~plot_CompRev$Company_name,
        colors = "Dark2",
        type = "bar",
        height = 400) %>% 
  layout(title = "Income per company",
         xaxis = list(title = "Revenue"),
         yaxis = list(title = ""),
         showlegend = FALSE)

# ------------------------------------------------------------------------------
# Total income in 2020 per company
# ------------------------------------------------------------------------------

plot_CompRev_2020 <- CompRev %>%
  mutate(Company_name = factor(CompRev$Company_name, 
                               levels = unique(CompRev$Company_name)[order(CompRev$Half_2020, decreasing = FALSE)]))

plot_ly(data = plot_CompRev_2020,
        x = plot_CompRev_2020$Half_2020,
        y = ~plot_CompRev_2020$Company_name,
        color = ~plot_CompRev_2020$Company_name,
        colors = "Dark2",
        type = "bar",
        height = 400) %>% 
  layout(title = "Income per company",
         xaxis = list(title = "Revenue"),
         yaxis = list(title = ""),
         showlegend = FALSE)

# ------------------------------------------------------------------------------
# Total income in 2019 per Region
# ------------------------------------------------------------------------------

CompRev_HQ2019 <- CompRev %>% 
  mutate(Region_of_HQ = factor(CompRev$Region_of_HQ, 
                               levels = unique(CompRev$Region_of_HQ)[order(CompRev$Total_2019, decreasing = FALSE)])) 

plot_CompRev_HQ2019 <- CompRev_HQ2019 %>%
  group_by(Region_of_HQ) %>% 
  summarize(total = sum(Total_2019), 
            number = sum(Total_2019 > 0), 
            mean = mean(Total_2019),
            median = median(Total_2019))

plot_ly(data = plot_CompRev_HQ2019,
        x = plot_CompRev_HQ2019$Region_of_HQ,
        y = ~plot_CompRev_HQ2019$mean,
        color = ~plot_CompRev_HQ2019$Region_of_HQ,
        colors = "Dark2",
        type = "bar",
        name = "Total income",
        height = 400) %>% 
  add_trace(y = ~plot_CompRev_HQ2019$total, name = 'Mean') %>%
  layout(title = "Income per Region",
         xaxis = list(title = "Revenue"),
         yaxis = list(title = ""),
         margin = list(b = 0),
         barmode = "group",
         showlegend = FALSE)

# ------------------------------------------------------------------------------
# Total income in 2020 per Region
# ------------------------------------------------------------------------------

CompRev_HQ2020 <- CompRev %>% 
  mutate(Region_of_HQ = factor(CompRev$Region_of_HQ, 
                               levels = unique(CompRev$Region_of_HQ)[order(CompRev$Total_2019, decreasing = FALSE)])) 

plot_CompRev_HQ2020 <- CompRev_HQ2020 %>%
  group_by(Region_of_HQ) %>% 
  summarize(total = sum(Half_2020),
            number = sum(Half_2020 > 0), 
            mean = mean(Half_2020),
            median = median(Half_2020))

plot_ly(data = plot_CompRev_HQ2020,
        x = plot_CompRev_HQ2020$Region_of_HQ,
        y = ~plot_CompRev_HQ2020$mean,
        color = ~plot_CompRev_HQ2020$Region_of_HQ,
        colors = "Dark2",
        type = "bar",
        name = "Total income",
        height = 400) %>% 
  add_trace(y = ~plot_CompRev_HQ2020$total, name = 'Mean') %>%
  layout(title = "Income per Region",
         xaxis = list(title = "Revenue"),
         yaxis = list(title = ""),
         margin = list(b = 0),
         barmode = "group",
         showlegend = FALSE)

# ------------------------------------------------------------------------------
# YoY Revenue growth per company
# ------------------------------------------------------------------------------

YoY <- CompRev %>%
  mutate(Q1_grate = (round(Q1_grate, digits = 2)),
         Company_name = factor(CompRev$Company_name, 
                               levels = unique(CompRev$Company_name)[order(CompRev$Q1_grate, decreasing = TRUE)]))

# Q1 growth rate
plot1 <- plot_ly(YoY, x = ~Company_name, y = ~Q1_grate, type = "bar", name = 'YoY growth for Q1 (%)')

# Q1 growth rate
plot2 <- plot_ly(YoY, x = ~Company_name, y = ~Q2_grate, type = "bar", name = 'Yoy growth for Q2 (%)')

plot <- subplot(plot1, plot2)

plot

# ------------------------------------------------------------------------------

# Different investment between 2018 and 2020


data3 <- read_delim(file = here::here("data/NZ_CompaniesInvestmentscsv1.csv"), 
                    ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

data4 <- read_delim(file = here::here("data/NZ_CompaniesInvestmentscsv2.csv"), 
                    ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# ------------------------------------------------------------------------------

# Data cleaning

CompaniesInvestments <- data3 %>%
  # combine the 2 data-sets
  full_join(data4) %>%
  # set a name for each variable
  rename(Date = X1, "Investment type" = X2, Sectors = X3, 
         Emptycolumn = X4, Investee = X5, Amount = X6) %>%
  # modify the format of certain variable
  mutate(Date = dmy(Date), Emptycolumn = NULL, Sectors = str_replace_all(Sectors, ",.", ", "))

# ------------------------------------------------------------------------------

# Data overview

CompaniesInvestments %>% 
  kbl(caption = "Ammount of different investmen") %>%
  kable_paper(full_width = F) %>% 
  scroll_box(width = "100%", height = "300px")

# ------------------------------------------------------------------------------

# Visualization

options(scipen = 999)

numeric_amount <- c()
for(i in seq_along(CompaniesInvestments$Amount)){
  x <- parse_number(CompaniesInvestments$Amount[i])*(10 ** (3*as.integer(regexpr(str_sub(CompaniesInvestments$Amount[i], -1 , -1), 'KMB'))))
  numeric_amount <- c(numeric_amount, x)
}



Monthly_investment <- CompaniesInvestments %>% 
  mutate(Amount = numeric_amount, Date = floor_date(Date, "month")) %>% 
  group_by(Date) %>% 
  summarize(total = sum(Amount), 
            number = sum(Amount > 0), 
            mean = mean(Amount))

plot_ly(data = Monthly_investment,
        x = Monthly_investment$Date,
        y = ~Monthly_investment$total,
        type = "bar",
        height = 400) %>% 
  layout(title = "Monthly investment",
         xaxis = list(title = "Date"),
         yaxix = list(title = "Amount"),
         showlegend = FALSE)


investment_type <- CompaniesInvestments %>%
  rename(type = "Investment type") %>%
  mutate(Amount = numeric_amount) %>%
  group_by(type) %>%
  summarize(total = sum(Amount), 
            number = sum(Amount > 0), 
            mean = mean(Amount)) %>%
  arrange(desc(number))

# add median, remove total and use mean
sector_influence <- CompaniesInvestments %>%
  mutate(Amount = numeric_amount) %>%
  separate(Sectors, into = c("s1", "s2", "s3", "s4", "s5"), sep = ",") %>%
  mutate(s2 = str_replace(s2, " ", ""),
         s3 = str_replace(s3, " ", ""),
         s4 = str_replace(s4, " ", ""),
         s5 = str_replace(s5, " ", "")) %>%
  pivot_longer(c(s1, s2, s3, s4, s5), names_to = "var", values_to = "Sectors") %>%
  drop_na(Sectors) %>%
  group_by(Sectors) %>%
  summarize(number = sum(Amount > 0),
            total = sum(Amount), 
            mean = mean(Amount),
            median = median(Amount)) %>%
  arrange(desc(total))


