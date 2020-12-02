# ==============================================================================
# EDA for newzoo.com (NZ) data-sets
# ==============================================================================
#
# This script aims to clean the NZ data-set and give an overview of it.




#https://platform.newzoo.com/companies/public-revenues

data2 <- read_delim("NZ_CompaniesPublicRevenues2.csv", ";", escape_double = FALSE,
                    col_names = FALSE, trim_ws = TRUE)

CompaniesPublicRevenues <- data2 %>%
  rename(Company_name = X1, Region_of_HQ = X2, Q1_2019 = X3, Q2_2019 = X4, Q3_2019 = X5, Q4_2019 = X6, Q1_2020 = X7, Q2_2020 = X8) %>%
  mutate(Q1_2019 = parse_number(Q1_2019), Q2_2019 = parse_number(Q2_2019), 
         Q3_2019 = parse_number(Q3_2019), Q4_2019 = parse_number(Q4_2019), 
         Q1_2020 = parse_number(Q1_2020), Q2_2020 = parse_number(Q2_2020)) %>%
  mutate(Q1_2019 = ifelse(Q1_2019 < 10 , Q1_2019*1000, Q1_2019), 
         Q2_2019 = ifelse(Q2_2019 < 10 , Q2_2019*1000, Q2_2019), 
         Q3_2019 = ifelse(Q3_2019 < 10 , Q3_2019*1000, Q3_2019), 
         Q4_2019 = ifelse(Q4_2019 < 10 , Q4_2019*1000, Q4_2019),
         Q1_2020 = ifelse(Q1_2020 < 10 , Q1_2020*1000, Q1_2020),
         Q2_2020 = ifelse(Q2_2020 < 10 , Q2_2020*1000, Q2_2020)) %>%
  mutate(First_semester_of_2019 = Q1_2019 + Q2_2020, 
         Second_semester_of_2019 = Q3_2019 + Q4_2019, 
         Total_in_2019 = Q1_2019 + Q2_2019 + Q3_2019 + Q4_2019, 
         First_semester_in_2020 = Q1_2020 + Q2_2020) %>%
  mutate(Q1_grate = 100*(Q1_2020 - Q1_2019)/Q1_2019, 
         Q2_grate = 100*(Q2_2020 - Q2_2019)/Q2_2019, 
         Semester2_grate = 100*(Second_semester_of_2019 - First_semester_of_2019)/First_semester_of_2019,
         Semester3_grate = 100*(First_semester_in_2020 - Second_semester_of_2019)/Second_semester_of_2019,
         First_semester_grate = 100*(First_semester_in_2020 - First_semester_of_2019)/First_semester_of_2019)


CompaniesPublicRevenues %>% kbl() %>%  kable_classic() %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))


kbl(CompaniesPublicRevenues) %>%
  kable_classic() %>%
  add_header_above(c(" " = 2, "2019" = 4, "2020" = 2, "Other" = 9))

#https://platform.newzoo.com/companies/investments

data3 <- read_delim("NZ_CompaniesInvestmentscsv1.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

data4 <- read_delim("NZ_CompaniesInvestmentscsv2.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

CompaniesInvestments <- data3 %>% full_join(data4) %>%
  rename(Date = X1, "Investment type" = X2, Sectors = X3, Emptycolumn = X4, Investee = X5, Amount = X6) %>%
  mutate(Date = dmy(Date)) %>%
  mutate(Emptycolumn = NULL, Sectors = str_replace_all(Sectors, ",.", ", "))

mutate(Emptycolumn = NULL, 
       Amount = parse_number(str_replace_all(Amount, c("K" = "000", "M" = "0000", "\\$" = ""))),
       Sectors = str_replace_all(Sectors, ",.", ", "))

#kaggle
data5 <- read.csv("Esport_Earnings.csv")
data6 <- read.csv("GeneralEsportData.csv")
data7 <- read.csv("HistoricalEsportData.csv")
data8 <- read.csv("Managerial_and_Decision_Economics_2013_Video_Games_Dataset.csv")
