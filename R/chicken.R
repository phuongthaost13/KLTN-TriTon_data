library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(readr) # always use readr::read_csv for reading non-english text file
options(stringsAsFactors = FALSE)
Sys.setlocale("LC_ALL", "Vietnamese") ## DCm cái này quan trọng lắm

## https://www.r-bloggers.com/2016/06/escaping-from-character-encoding-hell-in-r-on-windows/


dt <- read_csv("data/TE2.csv", locale = locale(encoding = "UTF-8")) ## Te2.csv là file csv UTF-8 encoding
dt <- dt %>% 
  filter(!(plant == ""))

plant_total <- dt %>% 
  count(plant)

plant_percent <- dt %>% 
  group_by(plant) %>% 
  summarise(count = n()) %>% 
  mutate(plant = factor(plant),
         percent = count/sum(count),
         label = scales::percent(percent)) %>% 
  arrange(desc(percent))

ggplot(plant_percent, aes(x=reorder(plant,percent), y=percent, label=label))+
  geom_bar(stat = "identity")+
  geom_text(hjust = -0.3)+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Cây trồng") + ylab("Phần trăm")

common.plant <- dt %>% 
  group_by(plant) %>% 
  count() %>% 
  filter(!(n < 5))

common.plant.dt <- dt %>% 
  filter(plant %in% common.plant[[1]])

plant_history <- common.plant.dt %>% 
  group_by(plant.year,plant) %>% 
  summarise(count = n()) 

ggplot(plant_history, aes(x = plant.year, y = count, color = plant)) +
  geom_line(size = 1, position = position_jitter())+
  theme_bw() + theme(legend.position = "none")+
  ggrepel::geom_text_repel(aes(x = plant.year, y = count, label = plant), data = plant_history)+
  annotate("point", x = 1993, y = 2.5, colour = "black", size = 3)+
  annotate("text", x = 1993, y = 2.5, label = "1993", colour = "black", vjust = -0.5)+
  xlab("Năm trồng") + ylab("Tổng mô hình xuất hiện")

new_plant <- dt %>% 
  filter(plant == "bưởi"|plant == "bơ"|plant == "mãng cầu",
         plant.year >= 2010)

ggplot(new_plant, aes(x = plant.year, y = yield, color = plant)) +
  geom_jitter(size = 3)+
  theme_bw() +
  ggrepel::geom_text_repel(aes(x = plant.year, y = yield, label = plant), data = new_plant)

ggplot(plant_history, aes(x = plant.year, y = count, color = plant)) +
  geom_jitter(size = 1)+
  theme_bw() + theme(legend.position = "none")+
  facet_wrap(~plant)+
  xlim(c(1970,2018))

