library(tidyverse)
library(lubridate)

rm(list = ls())
example_data <- read_excel("./Data/2019Week03.xlsx")
example_data$start_date <- lubridate::dmy(example_data$Start_Date)


# example_data <- tibble(
#   Name = c("Carl", "Jonathan", "Andy", "Sophie"),
#   monthly_cost = c(20, 15, 45, 30),
#   lenght_of_contract = c(24, 6, 12, 12),
#   start_date = c("2018-12-13", "2019-02-22", "2018-10-17", "2018-11-19") %>% ymd()
# )

monthly_costs <- example_data %>%
  rename(monthly_cost =Monthly_Cost) %>%
  rename(lenght_of_contract = Contract_Length) %>%
  rename(start_date = Start_Date) %>%
  group_by(Name) %>%
  nest()  %>%
  mutate(month_date =
           map(data,
               #have start at 0, because I want this month included, and have to subtract 1 from the length of the contract because the contract end on that month and not the next
               ~.x$start_date %m+% months(seq(0,.x$lenght_of_contract-1)))
  )    %>%
  unnest(cols = c(data, month_date)) %>%
  mutate(first_of_month = floor_date(month_date, "month"))  %>%
  group_by(first_of_month)   %>%
  summarise(monthly_cost = sum(monthly_cost))

summ_monthly_costs <- monthly_costs %>%
  group_by(first_of_month)   %>%
  summarise(monthly_cost = sum(monthly_cost))

summ_monthly_costs

summ_monthly_costs %>%
  ggplot(aes(first_of_month, monthly_cost)) +
  geom_col(fill = "skyblue") +
  theme_light()
