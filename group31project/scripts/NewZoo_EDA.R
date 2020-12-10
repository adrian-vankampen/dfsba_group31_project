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

# Income of the 25 largest video game companies

data1 <- read_delim(file = here::here("data/NZ_CompaniesPublicRevenues2.csv"), ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# ------------------------------------------------------------------------------

# Data cleaning

CompaniesPublicRevenues <- data1 %>%
  # set a name for each variable
  rename(Company_name = X1, Region_of_HQ = X2, Q1 = X3, 
         Q2 = X4, Q3 = X5, Q4 = X6, 
         Q1_2020 = X7, Q2_2020 = X8) %>%
  # modify the format of certain variable   
  mutate(Q1 = parse_number(Q1), Q2 = parse_number(Q2),
         Q3 = parse_number(Q3), Q4 = parse_number(Q4),
         Q1_2020 = parse_number(Q1_2020), Q2_2020 = parse_number(Q2_2020)) %>%
  # put every variable in the same unit
  mutate(Q1 = ifelse(Q1 < 10 , Q1*1000, Q1), 
         Q2 = ifelse(Q2 < 10 , Q2*1000, Q2), 
         Q3 = ifelse(Q3 < 10 , Q3*1000, Q3),
         Q4 = ifelse(Q4 < 10 , Q4*1000, Q4),
         Q1_2020 = ifelse(Q1_2020 < 10 , Q1_2020*1000, Q1_2020),
         Q2_2020 = ifelse(Q2_2020 < 10 , Q2_2020*1000, Q2_2020)) %>%
  # compute total and growth
  mutate(Total_2019 = Q1 + Q2 + Q3 + Q4, .after = Q4) %>%
  mutate(Half_2020 = Q1_2020 + Q2_2020, .after = Q2_2020) %>%
  mutate(Q1_grate = 100*(Q1_2020 - Q1)/Q1, .after = Q1_2020) %>% 
  mutate(Q2_grate = 100*(Q2_2020 - Q2)/Q2, .after = Q2_2020)

# ------------------------------------------------------------------------------

# Data overview 

CompRev %>%
  rename("Q1 " = Q1_2020, "Q2 " = Q2_2020, "Full year" = Total_2019, 
         "YoY growth (%)" = Q1_grate,"Half year " = Half_2020, 
         "YoY growth (%) " = Q2_grate) %>%
  kbl(caption = "Game revenu per company") %>%
  kable_paper(full_width = F) %>% 
  add_header_above(c(" " = 2, "2019" = 5, "2020" = 5)) %>%
  scroll_box(width = "100%", height = "300px")

# ------------------------------------------------------------------------------

# Visualization

# ------------------------------------------------------------------------------
# Total income in 2019 per company
# ------------------------------------------------------------------------------

options(scipen = 999)

numeric_Q1 <- c()
for(i in seq_along(data1$X3)){
  x <- parse_number(data1$X3[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X3[i], -1 , -1), 'KMB'))))
  numeric_Q1 <- c(numeric_Q1, x)
}

numeric_Q2 <- c()
for(i in seq_along(data1$X4)){
  x <- parse_number(data1$X4[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X4[i], -1 , -1), 'KMB'))))
  numeric_Q2 <- c(numeric_Q2, x)
}

numeric_Q3 <- c()
for(i in seq_along(data1$X5)){
  x <- parse_number(data1$X5[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X5[i], -1 , -1), 'KMB'))))
  numeric_Q3 <- c(numeric_Q3, x)
}

numeric_Q4 <- c()
for(i in seq_along(data1$X6)){
  x <- parse_number(data1$X6[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X6[i], -1 , -1), 'KMB'))))
  numeric_Q4 <- c(numeric_Q4, x)
}

numeric_Q1_2020 <- c()
for(i in seq_along(data1$X7)){
  x <- parse_number(data1$X7[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X7[i], -1 , -1), 'KMB'))))
  numeric_Q1_2020 <- c(numeric_Q1_2020, x)
}

numeric_Q2_2020 <- c()
for(i in seq_along(data1$X8)){
  x <- parse_number(data1$X8[i])*(10 ** (3*as.integer(regexpr(str_sub(data1$X8[i], -1 , -1), 'KMB'))))
  numeric_Q2_2020 <- c(numeric_Q2_2020, x)
}

# With different colors

CompRev <- data1 %>%
  # set a name for each variable
  rename(Company_name = X1, Region_of_HQ = X2, Q1 = X3, 
         Q2 = X4, Q3 = X5, Q4 = X6, 
         Q1_2020 = X7, Q2_2020 = X8) %>%
  # modify the format of certain variable   
  mutate(Q1 = numeric_Q1, Q2 = numeric_Q2,
         Q3 = numeric_Q3, Q4 = numeric_Q4,
         Q1_2020 = numeric_Q1_2020, Q2_2020 = numeric_Q2_2020) %>%
  # compute total and growth
  mutate(Total_2019 = Q1 + Q2 + Q3 + Q4, .after = Q4) %>%
  mutate(Half_2020 = Q1_2020 + Q2_2020, .after = Q2_2020) %>%
  mutate(Q1_grate = 100*(Q1_2020 - Q1)/Q1, .after = Q1_2020) %>% 
  mutate(Q2_grate = 100*(Q2_2020 - Q2)/Q2, .after = Q2_2020)

plot_CompRev <- CompaniesPublicRevenues %>%
  mutate(Company_name = factor(CompaniesPublicRevenues$Company_name, 
                               levels = unique(CompaniesPublicRevenues$Company_name)[order(CompaniesPublicRevenues$Total_2019, decreasing = FALSE)]))

plot_ly(data = plot_CompRev,
        x = plot_CompRev$Total_2019,
        y = ~plot_CompRev$Company_name,
        color = ~plot_CompRev$Company_name,
        colors = "Dark2",
        type = "bar",
        height = 400) %>% 
  layout(title = "Sales per city",
         xaxis = list(title = "City"),
         showlegend = FALSE)

# ------------------------------------------------------------------------------
# YoY Revenue growth per company
# ------------------------------------------------------------------------------

test <- CompaniesPublicRevenues %>%
  mutate(Q1_grate = (round(Q1_grate, digits = 2)),
         Company_name = factor(CompaniesPublicRevenues$Company_name, 
                               levels = unique(CompaniesPublicRevenues$Company_name)[order(CompaniesPublicRevenues$Q1_grate, decreasing = TRUE)]))

# Q1 growth rate
plot1 <- plot_ly(test, x = ~Company_name, y = ~Q1_grate, type = "bar", name = 'YoY growth for Q1 (%)')

# Q1 growth rate
plot2 <- plot_ly(test, x = ~Company_name, y = ~Q2_grate, type = "bar", name = 'Yoy growth for Q2 (%)')

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

o_Monthly_investment <- Monthly_investment %>% 
  ggplot(aes(x = Date, y = total)) +
  geom_col()+
  ylab("Monthly investment")

ggplotly(o_Monthly_investment, height = 400)


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
