library(dplyr)
library(data.table)

## plantModel.csv contains data about all the plant models that are being collected in the survey
plantModel <- fread("data/plantModel.csv")


### "plant yield"
yieldIndex <- grep(pattern = "yield$", x = names(plantModel), value = FALSE)
yieldUnitIndex <- yieldIndex + 1
yieldTableIndex <- c(rbind(yieldIndex, yieldUnitIndex))

yieldTable <- data.frame(yield = unlist(plantModel[,..yieldIndex]),
                         yieldUnit = unlist(plantModel[,..yieldUnitIndex]))

## using case_when
yieldUnitConversion <- yieldTable %>% 
  mutate(yield = case_when(
    yieldUnit == "KG" ~ round(yield/1000, 2),
    TRUE ~ yield
  ),
        yieldUnit = case_when(
          yieldUnit == "KG" ~ "TAN",
          TRUE ~ yieldUnit
        ))

## https://stackoverflow.com/questions/37145863/splitting-a-data-frame-into-equal-parts
yieldToDataframe <- split(yieldUnitConversion, rep(1:ceiling(2171/181), each = 181, length.out = 2172))
yieldTable <- do.call(cbind.data.frame, yieldToDataframe)
names(yieldTable) <- names(plantModel[, ..yieldTableIndex]) 

### "plant number"
numberIndex <- grep(pattern = "number$", x = names(plantModel), value = FALSE)
numberUnitIndex <- numberIndex + 1
numberTableIndex <- c(rbind(numberIndex, numberUnitIndex))

numberTable <- data.frame(number = unlist(plantModel[, ..numberIndex]),
                          numberUnit = unlist(plantModel[, ..numberUnitIndex]))

str(numberTable)
summary(numberTable)

numberTable <- setDT(numberTable)
numberTable[, .N, by = numberTable$numberUnit]

numberToDataframe <- split(numberTable, rep(1:ceiling(2171/181), each = 181, length.out = 2172))
numberTable <- do.call(cbind.data.frame, numberToDataframe)
names(yieldTable) <- names(plantModel[, ..numberTableIndex]) 
  
### "plant year"


