## 04022022
## Phuong Thao
## Data exploring: Tri Ton, An Giang Data, from project Bay Nui An Giang

library(dplyr)
library(ggplot2)
library(frontier)
library(readr)

dt <- read_csv("data/TE2.csv")
dt <- dt %>% 
  filter(!(plant == ""))

## TE calculate

yield_dt <- dt %>% 
  filter(!(yield == 0 | plant.year == 0 | area == 0 | plant.number == 0))

## Tầm vông TE
tam_vong <- yield_dt[yield_dt$plant == "tầm vông",]

prodSfa_tam_vong <- sfa(log(yield) ~ log(area) + log(plant.number) | plant.year, data = tam_vong)
tam_vong_summary <- summary(prodSfa_tam_vong)
range(efficiencies(prodSfa_tam_vong))

## Xoài TE
xoai <- yield_dt[yield_dt$plant == "xoài",]

prodSfa_xoai <- sfa(log(yield) ~  log(area) + log(plant.number) | plant.year , data = xoai)
xoai_summary <- summary(prodSfa_xoai)
range(efficiencies(prodSfa_xoai))

## Xoài cát TE
xoai_c <- yield_dt[yield_dt$plant == "xoài c",]

prodSfa_xoai_c <- sfa(log(yield) ~  log(area) + log(plant.number) | plant.year , data = xoai_c)
xoai_c_summary <- summary(prodSfa_xoai_c)
range(efficiencies(prodSfa_xoai_c))


## Barplot shows percentage of plants appearing in the NLKH model
plant <- within(dt,
                plant <- factor(plant, 
                                levels = names(sort(table(plant),
                                                    decreasing = FALSE))))

ggplot(plant)+
  geom_bar(aes(x = plant, stat(prop), group = 1))+
  coord_flip()