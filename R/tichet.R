library(dplyr)
library(ggplot2)

dt <- readxl::read_xlsx("data/rawTriTon(Thao).xlsx", sheet = 2, na = c("", 0))

## collapse text by group in data frame
## https://stackoverflow.com/questions/22756372/collapse-text-by-group-in-data-frame

dt %>% 
  group_by(index) %>% 
  summarise(model = paste(plantType, collapse = "+"),
            area = mean(area),
            plantDensity = sum(plantNumber)/mean(area),
            profit = mean(annualIncome, na.rm = T) - mean(annualCost, na.rm = T))

            