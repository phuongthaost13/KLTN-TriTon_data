## 10122021
## Create new data set that contains only Tri Ton data
## Extract the file to .csv file (the original file is .xlsx)
## Pham Phuong Thao


library(ggplot2)
library(dplyr)
library(stringi)
library(tidyselect)

## The data file contains a title row and a column names row that is not appropriate
## for my analysis, so I used skip = 2 and col.names = FALSE
AnGiangData <- readxl::read_xlsx("data/rawAnGiangData.xlsx", sheet = 1, skip = 2, col_names = FALSE)
str(AnGiangData)

## Giving header for the data set
## I have created a separated file containing all the names for all the columns
variable <- read.table("data/variable.txt")
colnames(AnGiangData) <- variable$V1

## It is not safe to work with Vietnamese text, so I change all of them to Latin letters
## uppercase all the text
AnGiangData <- AnGiangData %>% 
  mutate(across(everything(), stri_trans_general, "latin-ascii")) %>% 
  mutate(across(everything(), toupper))

## decide columns' data type
dataType <- read.table("data/dataType.txt", header = F)

charIndex <- which(dataType$V1 == "character")
AnGiangData[,charIndex] <- lapply(AnGiangData[,charIndex], as.character)

numIndex <- which(dataType$V1 == "numeric")
AnGiangData[,numIndex] <- lapply(AnGiangData[,numIndex], as.numeric)

facIndex <- which(dataType$V1 == "factor")
AnGiangData[,facIndex] <- lapply(AnGiangData[,facIndex], factor)


## removing duplicated columns
AnGiangData <- AnGiangData %>% 
  select(!c(income.2018y)) %>% 
  slice(1:181)

  
## dealing with NA values

### area.total == area.NLKH + area.forestry
### check for this condition to detect any suspected wrong value
### Blank cells (NA) to zero
suspectedWrongValue <- list()

areaForestry <- AnGiangData %>% 
  select(area.total:area.forestry) %>% 
  mutate(area.forestry = tidyr::replace_na(area.forestry,0)) 

suspectedWrongValue[["area.forestry"]] <- with(areaForestry, which((area.total != area.NLKH + area.forestry)))


## unit conversion



## plantModel
plantModel <- AnGiangData %>% 
  select(model.total:model2.intercrop3.yield.unit)
write.csv(plantModel,"data/plantModel.csv", row.names = F)
