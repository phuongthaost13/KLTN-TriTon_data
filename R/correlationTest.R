#install.packages("rpart")

library(rpart)
library(readr)
library(dplyr)
library(ggplot2)
Sys.setlocale("LC_ALL", "Vietnamese")

## import needed library
te <- read_csv("data/TE2.csv", locale = locale(encoding = "UTF-8"))
overview <- read_csv("data/overview.csv", locale = locale(encoding = "UTF-8"))
landProperty <- read_csv("data/landProperty.csv", locale = locale(encoding = "UTF-8"))


## select variables that are needed for the correlation analysis
te_choosen <- te %>% 
  select(village, record, plant, plant.year, plant.price)

overview_choosen <- overview %>% 
  select(!full.time)

landProperty_choosen <- landProperty %>% 
  select(!c(land.increase, land.decrease))

x <- left_join(te_choosen, overview_choosen, by = "record")
y <- right_join(te_choosen, overview_choosen, by = "record")
z <- x %>% 
  left_join(landProperty_choosen, by = "record") %>% 
  select(!c(village.x, village.y))


w <- z %>% select(!c(ethnic, village, soil.characteristic, land.slope, water.source, water.status, record, religion))
q <- z %>%  select(plant, soil.characteristic)
cor <- dx2y(z)

