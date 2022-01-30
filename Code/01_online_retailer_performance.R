## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(readr)
library(openxlsx)
library(writexl)
library(knitr)
library(lubridate)


## ------------------------------------------------------------------------------------------------------------------------------------------------
cart_dta <- read_csv(here("Data", "DataAnalyst_Ecom_data_addsToCart.csv"))
counts_dta <- read_csv(here("Data", "DataAnalyst_Ecom_data_sessionCounts.csv"))

source(here("Code", "seasons_function.R"))


## ------------------------------------------------------------------------------------------------------------------------------------------------
first_sheet <- counts_dta %>%
  mutate_at(vars(dim_date), mdy) %>% ### using lubridate to convert to class date 
  mutate(month = month(dim_date)) %>% ### extracting the month
  group_by(dim_deviceCategory, month) %>% ### assignemnt asks to create a Month 8 Device aggregation
  summarise(Sessions = sum(sessions),  ### Now time to compute the four desired metrics
            Transactions = sum(transactions),
            QTY = sum(QTY),
            ECR = Transactions / Sessions)
first_sheet


## ------------------------------------------------------------------------------------------------------------------------------------------------
second_sheet_counts <- counts_dta %>%
  mutate_at(vars(dim_date), mdy) %>%
  arrange(desc(dim_date)) %>% ### arranging for a logic check
  filter(dim_date >= max(dim_date) - (months(2) - days(1))) %>% ### most recent two months 5/1 - 6/30
  mutate(month = month(dim_date)) %>%                           ### I chose to subtract a day to not include 4/30
  group_by(month) %>% ### This questions asks for a month aggregation
  summarise(Sessions = sum(sessions),
            Transactions = sum(transactions),
            QTY = sum(QTY),
            ECR = Transactions / Sessions) %>%
  arrange(desc(month)) ### another logic check
second_sheet_counts


## ------------------------------------------------------------------------------------------------------------------------------------------------
### get the cart data in the same format as the count data for a later merge
second_sheet_cart <- cart_dta %>%
  arrange(desc(dim_year), desc(dim_month)) %>%
  slice(1:2)
second_sheet_cart


## ------------------------------------------------------------------------------------------------------------------------------------------------
### Merging
second_sheet <- left_join(second_sheet_counts, second_sheet_cart, by = c("month" = "dim_month")) %>%
  select(-dim_year)
second_sheet


## ------------------------------------------------------------------------------------------------------------------------------------------------
### Add to dataframes to workbook via the openxlsx package
wb <- createWorkbook()
addWorksheet(wb, "month_device")
addWorksheet(wb, "month_over_month")
writeData(wb, 1, first_sheet)
writeData(wb, 2, second_sheet)
saveWorkbook(wb, file = here("Data", "reference_tables.xlsx"))


## ------------------------------------------------------------------------------------------------------------------------------------------------
### This is personally how I like to do this task.
### Find it more straight forward than the openxlsx package, and does not contains not dependencies
writexl::write_xlsx(list(month_device = first_sheet, 
                         month_over_month = second_sheet), 
                    here("Data", "reference_tables_prefered.xlsx"))


## ------------------------------------------------------------------------------------------------------------------------------------------------
purl("01_online_retailer_performance.RMD")

