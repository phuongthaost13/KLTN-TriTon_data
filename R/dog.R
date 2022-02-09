## 04022022
## Phuong Thao
## Data exploring: Tri Ton, An Giang Data, from project Bay Nui An Giang

library(dplyr)
library(ggplot2)
library(Benchmarking)
library(frontier)

dt <- read.csv("data/TE.csv", stringsAsFactors = F)
dt <- dt %>% 
  filter(!(plant == ""))

## Histogram show frequency of plants appearing in the NLKH model
plant <- within(dt,
                plant <- factor(plant, 
                                levels = names(sort(table(plant),
                                                    decreasing = FALSE))))

ggplot(plant)+
  geom_bar(aes(x = plant), binwidth =1)+
  coord_flip()

## 

## remove plants that 
common.plant <- dt %>% 
  group_by(plant) %>% 
  count() %>% 
  filter(!(n < 5))

common.plant.dt <- dt %>% 
  filter(plant %in% common.plant[[1]])

plant_history <- common.plant.dt %>% 
  group_by(plant.year,plant) %>% 
  count()

ggplot(plant_history, aes(x = plant.year, y = n, color = plant)) +
  geom_line(size = 1, position = position_jitter())+
  theme_bw() + theme(legend.position = "none")+
  ggrepel::geom_text_repel(aes(x = plant.year, y = n, label = plant), data = plant_history)
#facet_wrap(~plant)#+
xlim(c(1970,2018))

which(dt$plant.Number >= 30000)


## tam vong data
tam.vong <- dt[dt$plant == "tam vong",]


prodCD <- lm(log(yield) ~ log(plant.year) + log(area) + log(plant.number), data = tam.vong)
summary(prodCD)

prodSfa <- sfa(log(yield) ~ log(plant.year) + log(area) + log(plant.number), data = tam.vong)
summary(prodSfa)

plot(sort(efficiencies(prodSfa)))


## xoai data
xoai <- dt[dt$plant == "xoai",]

prodCD <- lm(log(yield) ~ log(plant.year) + log(area) + log(plant.number), data = xoai)
summary(prodCD)

prodSfa <- sfa(log(yield) ~ log(plant.year) + log(area) + log(plant.number), data = xoai)
summary(prodSfa)

plot(sort(efficiencies(prodSfa)))


## xoai c data
xoai.c <- dt[dt$plant == "xoai c",]

prodCD <- lm(log(yield) ~ log(plant.year) + log(area) + log(plant.number), data = xoai.c)
summary(prodCD)

prodSfa <- sfa(log(yield) ~ log(plant.year) + log(area) + log(plant.number), data = xoai.c)
summary(prodSfa)

plot(sort(efficiencies(prodSfa)))

## buoi
buoi <- dt[dt$plant == "xoai c",]

prodCD <- lm(log(yield) ~ log(plant.year) + log(area) + log(plant.number), data = buoi)
summary(prodCD)

prodSfa <- sfa(log(yield) ~ log(plant.year) + log(area) + log(plant.number), data = buoi)
summary(prodSfa)

plot(sort(efficiencies(prodSfa)))


ggplot(economic_plant_plot) +
  geom_bar(mapping = aes(x = village, fill = plant), position = "fill", binwidth = 1, alpha = 1.5)+
  theme_minimal()


## Barplot shows percentage of plants appearing in the NLKH model
plant <- within(dt,
                plant <- factor(plant, 
                                levels = names(sort(table(plant),
                                                    decreasing = FALSE))))

ggplot(plant)+
  geom_bar(aes(x = plant, stat(prop), group = 1))+
  coord_flip()