## 04022022
## Phuong Thao
## Data exploring: Tri Ton, An Giang Data, from project Bay Nui An Giang

library(dplyr)
library(ggplot2)
library(Benchmarking)

dt <- read.csv("data/TE.csv", stringsAsFactors = F)


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

ggplot(x, aes(x = plant.Year, y = n, color = plant)) +
  geom_line(size = 1, position = position_jitter())+
  theme_bw() + theme(legend.position = "none")+
  ggrepel::geom_text_repel(aes(x = plant.Year, y = n, label = plant), data = x)
  #facet_wrap(~plant)#+
  xlim(c(1970,2018))

which(dt$plant.Number >= 30000)

x <- common.plant.dt %>% 
  group_by(plant.Year,plant) %>% 
  count()

## tam vong data
tam.vong <- dt[dt$plant == "tam vong",]


## model sfa
x <- with(tam.vong, cbind(area, plant.number, plant.year))
y <- matrix(tam.vong$yield)

tam.vong.Sfa <- sfa(log(x), log(y))
summary(tam.vong.Sfa)

## firm technical efficiency
e <- residuals(tam.vong.Sfa)
s2 <- sigma2.sfa(tam.vong.Sfa)
lambda <- lambda.sfa(tam.vong.Sfa)

mustar <- -e*lambda^2/(1+lambda^2)
sstar <- lambda/(1+lambda^2)*sqrt(s2)
teJ <- exp(-mustar - sstar*dnorm(mustar/sstar)/pnorm(mustar/sstar))
estar <- e*lambda/sqrt(s2)
euJ <- sstar*(dnorm(estar)/(1 - pnorm(estar)) - estar)
teJJ <- exp(-euJ)
all.equal(teJ, teJJ)

teMode <- exp(pmin(0, -mustar))

teBC <- pnorm(mustar/sstar - sstar)/pnorm(mustar/sstar) *
  exp((sstar^2/2 - mustar))

cor(cbind(teBC = c(teBC), teMode=c(teMode), teJ=c(teJ)))

mean(teBC)

sum(tam.vong$yield*teBC/sum(tam.vong$yield))

hist(te)
plot(sort(teBC))
