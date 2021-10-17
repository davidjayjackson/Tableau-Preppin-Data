library(tidyverse)
library(scales)
library(readxl)
library(janitor)
library(lubridate)

### read data
rm(list=ls())
df <- read_excel("./PD 2021Wk1.xlsx")
df <- clean_names(df)
### Task 1: Seperate store_bike field into two cols: store and bike
df <- separate(df,store_bike,into = c("store","bike"),sep="-")
df$bike <- gsub("Graval","Gravel",df$bike)
df$bike <- gsub("Gravle","Gravel",df$bike)
df$bike <- gsub("Mountaen","Mountain",df$bike)
df$bike <- gsub("Rood","Road",df$bike)
df$bike <- gsub("Rowd","Road",df$bike)
barplot(table(df$bike))


###  dplyr::recode
table(df$bike)
df$bike <- recode(df$bike,"Graval" = "Gravel",
                    "Gravle" = "Gravel",
                    "Mountaen" = "Mountain",
                    "Rood" =  "Road",
                    "Rowd" = "Road")


table(df$bike)

df %>% case_when (
  bike == "Graval" ~"Gravel",
  bike == "Gravle" ~"Gravel",
  bike == "Mountaen" ~ "Mountain",
  bike == "Rood" ~  "Road",
  bike == "Rowd" ~ "Road",
  TRUE ~NA
)
##  Task 5 and 5 Days of Month and Quarter

df$day_of_month <- day(df$date)
df$quarter <- quarter(df$date)
df <- df %>% filter(order_id >=11)
write.csv(df,file="Week01.csv",row.names = F)

## Calculate daily average sale price of bikes sold
df$Month <- month(df$date)
average_sales <- df %>% group_by(quarter,Month,day_of_month) %>%
  summarise(bike_average = mean(bike_value),
            running_total = cumsum(bike_value))

## Plotof sales by Quarter
df$total <- cumsum(df$bike_value)

ggplot(df) + geom_col(aes(x=day_of_month,y=total)) +
  facet_wrap(~Month+quarter,ncol=1)









