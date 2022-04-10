## 15032022
## Phạm Phương Thảo
## TE for the teacher

library(dplyr)
library(readr)
library(frontier)
library(lmtest)
library(ggplot2)
Sys.setlocale("LC_ALL", "Vietnamese") ## DCm cái này quan trọng lắm

## import TE data
dt <- read_csv(here::here("data/TE2.csv"), locale = locale(encoding = "UTF-8")) ## Te2.csv là file csv UTF-8 encoding
general_info <- read_csv(here::here("data/generalInfo.csv"), locale = locale(encoding = "UTF-8"))
landProperty <- read_csv(here::here("data/landProperty.csv"), locale = locale(encoding = "UTF-8")) ## Te2.csv là file csv UTF-8 encoding

dt <- dt %>% filter(!(plant == ""))


## data processing
TE <- dt %>% 
  left_join(general_info, by = "record") %>% 
  group_by(record) %>% 
  summarise(family.income = mean(family.income, na.rm = TRUE),
            NLKH.income = mean(NLKH.income, na.rm = TRUE),
            plant.number = sum(plant.number, na.rm = TRUE),
            fer.annual = sum(fer.annual, na.rm = TRUE),
            chem.annual = sum(chem.annual, na.rm = TRUE),
            harvest.annual = sum(harvest.annual, na.rm = TRUE),
            cost.total = mean(cost.total, na.rm = TRUE),
            cost.ini.total = mean(cost.ini.total, na.rm = TRUE),
            area = mean(area, na.rm = TRUE),
            age = mean(age, na.rm = TRUE),
            education = mean(education, na.rm = TRUE),
            family.member = mean(family.member, na.rm = TRUE),
            labor.total = mean(labor.total, na.rm = TRUE),
            labor.agri = mean(labor.agri, na.rm = TRUE),
            agri.working = mean(agri.working, na.rm = TRUE),
            loan = mean(loan, na.rm = TRUE),
            training = mean(training, na.rm = TRUE)) %>% 
  mutate(NLKH.income.percent = NLKH.income/family.income,
         NLKH.income.area = NLKH.income/area,
         plant.density = plant.number/area,
         fer.area = fer.annual/area,
         chem.area = chem.annual/area,
         harvest.area = harvest.annual/area,
         labor.area = labor.agri/area,
         cost.total.area = cost.total/area,
         cost.ini.total.area = cost.ini.total/area,
         labor.agri.percent = labor.agri/labor.total
         ) %>% 
  filter(NLKH.income.area < 80, NLKH.income.area > 0) %>% 
  left_join(landProperty, by = "record")
  

TE <- TE[ ,!sapply(TE, is.character)]

TE[is.na(TE) == TRUE] <- 0

TE <- TE + 0.001

#TE$water.source <- as.factor(TE$water.status)
#TE$education <- as.factor(TE$education.status)
#TE$loan <- as.factor(TE$loan)

TE <- TE %>% 
  mutate(fer.dummy = case_when(fer.area > 0 ~ 1,
                               TRUE ~ 0),
         chem.dummy = case_when(chem.area > 0 ~ 1,
                                TRUE ~ 0),
         water.status = case_when(water.source == 3.001 ~ 0,
                                  TRUE ~ 1),
         education.status = case_when(education == 1.001 ~ 0,
                                      TRUE ~ 1))

## create log var for better model convenience
TE$logIncome  <- log(TE$NLKH.income.area)
TE$logFer <- log(TE$fer.area)
TE$logChem  <- log(TE$chem.area)
TE$logLab <- log(TE$labor.area)


## TE
summary(SFA.no.int)
eff <- efficiencies(SFA.no.int)

