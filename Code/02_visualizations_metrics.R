## ----setup, include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(RColorBrewer)
library(cowplot)
library(zoo)
library(knitr)


## ------------------------------------------------------------------------------------
### Loading in Data
cart_dta <- read_csv(here("Data", "DataAnalyst_Ecom_data_addsToCart.csv"))
counts_dta <- read_csv(here("Data", "DataAnalyst_Ecom_data_sessionCounts.csv"))

### Loading in the sheets created in the combined workbook
### Although this is only two sheets and mapping through them is not really necessary,
### I wanted to show I know how to use the purrr package

sheet_names <- excel_sheets(here("Data", "reference_tables_prefered.xlsx")) ### saving the sheet names
reference_dfs <- sheet_names %>%
  map(function(sheet) {
    read_xlsx(here("Data", "reference_tables_prefered.xlsx"), sheet = sheet) %>% ### mapping through the sheets
      as.data.frame()
  })

first_sheet <- reference_dfs[[1]]
second_sheet <- reference_dfs[[2]]


## ------------------------------------------------------------------------------------
options(scipen = 999)

facet_data <- counts_dta %>%
  mutate_at(vars(dim_date), mdy) %>%
  mutate(year_month = as.yearmon(dim_date, "%m/%Y")) %>% ### Creating a year-month abbreviation 
  mutate_at(vars(year_month), factor) %>%
  group_by(dim_deviceCategory, year_month) %>%
  summarise(sum_sessions = sum(sessions),          ### Want to compare the sum of sessions 
            sum_transactions = sum(transactions))  ### and transactions by device type

session_plot <- ggplot(facet_data, aes(x = year_month, y = sum_sessions, 
                          group = dim_deviceCategory, fill = dim_deviceCategory)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ dim_deviceCategory) +
    theme(axis.title.x = element_blank(), ### Going to share a title and axis with another plot
          axis.text.x = element_blank()) + 
    scale_fill_brewer(palette = "Dark2") + ### making sure to use a color blind friendly palette
    theme(legend.position = "none") + ### going to share a legend with another plot
    ylab("Number of Sessions")

transaction_plot <- ggplot(facet_data, aes(x = year_month, y = sum_transactions, 
                              group = dim_deviceCategory, fill = dim_deviceCategory)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ dim_deviceCategory) +
    theme(axis.text.x = element_text(angle = 90, ### Formating to make the plot look good
                                     vjust = .5,
                                     size = 5,
                                     face = "bold")) +
    scale_fill_brewer(palette = "Dark2") +
    theme(legend.position = "none") +
    xlab("Date") +
    ylab("Number of Transactions") +
    theme(strip.background = element_blank(), 
          strip.text.x = element_blank()) ### Don't need two facet titles, same thing

combined_plot <- plot_grid(session_plot, transaction_plot, ncol = 1, align = "v")
combined_plot
ggsave(here("Results", "combined_plot.png"), plot = combined_plot)


## ------------------------------------------------------------------------------------
### Was curious if there was a postive realtions between the two variables, looks like it!
ggplot(counts_dta, aes(sessions, transactions, color = QTY)) +
  geom_point() +
  scale_color_distiller(palette = "Oranges", direction = 1) +
  theme_minimal() +
  ggtitle("Relationship between Sessions and Transactions") +
  xlab("Number of Sessions") +
  ylab("Number of Transactions")


## ------------------------------------------------------------------------------------
cart_dta %>%
  mutate(date = make_date(dim_year, dim_month)) %>%
  ggplot(aes(x = date, y = addsToCart)) +
  geom_bar(stat = "identity", fill = "#1B9E77") +
  theme_minimal() +
  xlab("Date") +
  ylab("Amount Added to Cart") +
  ggtitle("Overall Number of Items in Cart")

### Going to create seasons using meteorological seasonal cutoffs
seasonal_plot <- cart_dta %>%
  mutate(seasons = case_when(dim_month == 12 | dim_month == 1 | dim_month == 2 ~ "Winter", ### Classifying Winter
                             dim_month == 9 | dim_month == 10 | dim_month == 11 ~ "Fall", ### Classifying Fall
                             dim_month == 6 | dim_month == 7 | dim_month == 8 ~ "Summer", ### Classifying Summer
                             dim_month == 3 | dim_month == 4 | dim_month == 5 ~ "Spring", ### Classifying Spring
                             TRUE ~ as.character(dim_month))) %>%
  mutate(seasons = factor(seasons, levels = c("Winter", "Spring", "Summer", "Fall"))) %>% ### putting the season in order
  ggplot(aes(x = seasons, y = addsToCart, fill = seasons)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Season") +
  ylab("Amount Added to Cart") +
  ggtitle("Overall Number of Items in Cart") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2")
seasonal_plot

ggsave(here("Results", "seasonal_plot.png"), plot = seasonal_plot)


## ------------------------------------------------------------------------------------
top_browsers <- counts_dta %>%
  group_by(dim_browser) %>%
  summarise(sum_sessions = sum(sessions)) %>%
  arrange(desc(sum_sessions)) %>%
  group_by(dim_browser = factor(c(dim_browser[1:7],  ### Taking the top 7 browsers
                                     rep("Other",  ### Next couple lines condense the remaining to an "Other" category
                                     n() - 7)),
                                     levels = c(dim_browser[1:7], "Other"))) %>%
  summarise(sum_sessions = sum(sum_sessions)) %>%
  mutate(percent_sessions = sum_sessions / sum(sum_sessions))

top_browser_plot <- ggplot(top_browsers, aes(dim_browser, sum_sessions)) +
  geom_bar(stat = "identity", fill = "#7570B3") +
  coord_flip() +
  scale_x_discrete(limits = rev) + ### The largest bar should go on top
  theme_minimal() +
  ggtitle("Most Popular Browser") +
  ylab("Number if Sessions") +
  xlab("Browser Name") +
  theme(axis.text.y = element_text(angle = 45)) ### Just so text fits nicely
top_browser_plot

ggsave(here("Results", "top_browser_plot.png"), plot = top_browser_plot)


## ------------------------------------------------------------------------------------
### Cart Abandonment Rate

### Start by calculating the monthly transactions
car_count <- counts_dta %>% 
  mutate_at(vars(dim_date), mdy) %>%
  mutate(month = month(dim_date),
         year = year(dim_date)) %>% 
  group_by(year, month) %>%
  summarise(monthly_transactions = sum(transactions))

### Then formatting the monthly add to cart numbers to match the monthly transactions creates
car_cart <- cart_dta %>%
  mutate(date = make_date(dim_year, dim_month)) %>%
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(year, month)

car_plot <- left_join(car_count, car_cart, by = c("month", "year")) %>%
  mutate(CAR = monthly_transactions / addsToCart) %>% ### calculating the cart abandonment rate
  ggplot(aes(date, CAR)) +
  geom_line(color = "#D95F02") +
  theme_minimal() +
  ylab("Cart Abandonment Rate") +
  xlab("Date") +
  ggtitle("Increase in Cart Abandonment Rate")
car_plot

ggsave(here("Results", "car_plot.png"), plot = car_plot)


## ------------------------------------------------------------------------------------
purl("02_visualizations_metrics.RMD")

