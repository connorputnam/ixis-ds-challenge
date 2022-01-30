## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(lubridate)
library(readxl)


## -----------------------------------------------------------------------------------------------------------------------------------------------
### Loading in Data
cart_dta <- read_csv(here("Data", "DataAnalyst_Ecom_data_addsToCart.csv"))
counts_dta <- read_csv(here("Data", "DataAnalyst_Ecom_data_sessionCounts.csv"))

### Loading in the sheets created in the combined workbook
### Although this is only two sheets and mapping throuh them is not really necessary,
### I wanted to show I know how to use the purrr package

sheet_names <- excel_sheets(here("Data", "reference_tables_prefered.xlsx")) ### saving the sheet names
reference_dfs <- sheet_names %>%
  map(function(sheet) {
    read_xlsx(here("Data", "reference_tables_prefered.xlsx"), sheet = sheet) %>% ### mapping through the sheets
      as.data.frame()
  })

first_sheet <- reference_dfs[[1]]
second_sheet <- reference_dfs[[2]]


## -----------------------------------------------------------------------------------------------------------------------------------------------
options(scipen = 999)
first_sheet %>%
  mutate_at(vars(month), as.factor) %>%
  mutate(month_abb = month.abb[month]) %>%
  ggplot(aes(x = month_abb, y = Sessions, group = dim_deviceCategory)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ dim_deviceCategory) +
  ggtitle("Total Number of Sessions by Device Type") +
  theme(axis.text.x = element_text(angle = 45))

first_sheet %>%
  mutate_at(vars(month), as.factor) %>%
  mutate(month_abb = month.abb[month]) %>%
  ggplot(aes(x = month_abb, y = Transactions, group = dim_deviceCategory)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ dim_deviceCategory) +
  ggtitle("Total Number of Transactions by Device Type") +
  theme(axis.text.x = element_text(angle = 45))

first_sheet %>%
  mutate_at(vars(month), as.factor) %>%
  mutate(month_abb = month.abb[month]) %>%
  ggplot(aes(x = month_abb, y = QTY, group = dim_deviceCategory)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ dim_deviceCategory) +
  ggtitle("Total QTY by Device Type") +
  theme(axis.text.x = element_text(angle = 45))

## -----------------------------------------------------------------------------------------------------------------------------------------------
cart_dta %>%
  mutate(date = make_date(dim_year, dim_month)) %>%
  ggplot(aes(x = date, y = addsToCart)) +
  geom_bar(stat = "identity")

cart_dta %>%
  mutate(seasons = case_when(dim_month == 12 | dim_month == 1 | dim_month == 2 ~ "Winter",
                             dim_month == 9 | dim_month == 10 | dim_month == 11 ~ "Fall",
                             dim_month == 6 | dim_month == 7 | dim_month == 8 ~ "Summer",
                             dim_month == 3 | dim_month == 4 | dim_month == 5 ~ "Spring", 
                             TRUE ~ as.character(dim_month))) %>%
  ggplot(aes(x = seasons, y = addsToCart)) +
  geom_bar(stat = "identity")


## -----------------------------------------------------------------------------------------------------------------------------------------------
sum_count_compare <- second_sheet_counts %>%
  group_by(dim_deviceCategory, month) %>%
  summarise(num_sessions = sum(sessions),
            num_transactions = sum(transactions),
            monthly_QTY = sum(QTY),
            monthly_ECR = num_transactions / num_sessions)
sum_count_compare

